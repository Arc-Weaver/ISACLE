{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Synthesis renderer for the ISA IR.
--
-- An instruction body is run through 'Isacle.ISA.Build.ISABuild' to produce an
-- 'InstrIR' (the source of truth); 'renderSynth' lowers that IR into the
-- per-instruction request structures the CPU pass
-- ('Isacle.ISA.Backend.SynthCPU') assembles into register files, memory ports
-- and the execution sequencer.  No instruction body is interpreted here, and no
-- 'WireId' is ever smuggled into a value — wires are minted only during
-- lowering ('Isacle.ISA.Backend.Lower').
module Isacle.ISA.Backend.Synth
    ( -- * Collected per-instruction outputs
      SynthResult(..)
    , RegWriteReq(..)
    , ScalarWriteReq(..)
    , RegReadReq(..)
    , MemWriteReq(..)
    , MemReadReq(..)
    , FlagWriteReq(..)
      -- * Rendering
    , RenderCtx(..)
    , renderSynth
      -- * Combinational helpers reused by the CPU pass
    , buildMatch
    , extractFieldSig
    ) where

import Prelude hiding (Word)
import Data.Either (partitionEithers)
import Data.List (nub)
import qualified Data.Map.Strict as Map

import qualified Hdl.Net as N
import Hdl.Types (Signal(..), KnownDom)
import Hdl.Monad (Hdl(..))
import Isacle.ISA.Types (CPUFlag(..))
import Isacle.ISA.Encoding
import Isacle.ISA.IR
import Isacle.ISA.Backend.Lower

-- ---------------------------------------------------------------------------
-- Per-instruction output signals (consumed by SynthCPU's arbiters/sequencer).
-- Every "wire" is now a typed 'Signal' value — no NetM, no WireId.
-- ---------------------------------------------------------------------------

-- | Register-file (indexed) write request — guarded by 'rwMatchWire'.
data RegWriteReq s dom = RegWriteReq
    { rwMatchWire :: s dom Bool
    , rwRfName    :: String
    , rwIdxWire   :: s dom ()
    , rwDataWire  :: s dom ()
    }

-- | Scalar-register write request (PC, SP, …) — guarded by 'swMatchWire'.
data ScalarWriteReq s dom = ScalarWriteReq
    { swMatchWire :: s dom Bool
    , swRegName   :: String
    , swDataWire  :: s dom ()
    }

-- | Register-file read request (index signal + the read-result signal).
data RegReadReq s dom = RegReadReq
    { rrRfName  :: String
    , rrIdxWire :: s dom ()
    , rrOutWire :: s dom ()
    }

-- | Data-memory write request.
data MemWriteReq s dom = MemWriteReq
    { mwMatchWire :: s dom Bool
    , mwAddrWire  :: s dom ()
    , mwDataWire  :: s dom ()
    }

-- | Data-memory read request.  'mrResultWire' is the signal the body consumed
-- for the read result; the CPU sequencer produces it from 'mrBusWire' (the bus,
-- directly for a single read, or via a per-cycle select + holding latch).
data MemReadReq s dom = MemReadReq
    { mrMatchWire  :: s dom Bool
    , mrAddrWire   :: s dom ()
    , mrBusWire    :: s dom ()
    , mrResultWire :: s dom ()
    }

-- | Flag write: set one status-register bit when the instruction fires.
data FlagWriteReq s dom = FlagWriteReq
    { fwMatchWire :: s dom Bool
    , fwRegName   :: String
    , fwBitPos    :: Int
    , fwValueWire :: s dom ()
    }

-- | All combinational outputs of one instruction.
data SynthResult s dom = SynthResult
    { srMatchWire    :: Maybe (s dom Bool)
    , srRegWrites    :: [RegWriteReq s dom]
    , srScalarWrites :: [ScalarWriteReq s dom]
    , srRegReads     :: [RegReadReq s dom]
    , srMemWrites    :: [MemWriteReq s dom]
    , srMemReads     :: [MemReadReq s dom]
    , srFlagWrites   :: [FlagWriteReq s dom]
    }

-- ---------------------------------------------------------------------------
-- Render context
-- ---------------------------------------------------------------------------

-- | Resolution the CPU pass supplies for one instruction slot.  Leaves are
-- already-resolved signals; @rcReadRes@ is the per-read result the sequencer
-- drives (the CPU renders twice — see 'SynthCPU').
data RenderCtx s dom = RenderCtx
    { rcInstrWire  :: s dom ()                  -- ^ instruction word (field source)
    , rcReadScalar :: String -> s dom ()        -- ^ scalar register reader
    , rcDataBus    :: s dom ()                  -- ^ data_rd_data
    , rcCodeBus    :: s dom ()                  -- ^ code read bus (LPM/2nd word)
    , rcReadRes    :: ReadTok -> s dom ()       -- ^ per-read result (from sequencer)
    , rcGetFlag    :: String -> Int -> s dom () -- ^ status-bit reader
    , rcRegCount   :: String -> Int             -- ^ register-file entry count
    , rcIrqVector  :: Maybe (s dom ())          -- ^ irq_vector (in an IRQ body)
    , rcWordW      :: Int                        -- ^ data word width (write clamp)
    }

-- ---------------------------------------------------------------------------
-- Render
-- ---------------------------------------------------------------------------

-- | Lower one instruction's 'InstrIR' into a 'SynthResult'.
--
-- @mBase@ is the base match condition: 'Nothing' to derive it from the encoding
-- (normal instructions), or @Just w@ to seed it (e.g. @irq_pending@).
renderSynth :: forall s m dom. (Hdl s m, Signal s, KnownDom dom)
            => RenderCtx s dom -> Maybe (s dom Bool) -> InstrIR -> m (SynthResult s dom)
renderSynth ctx mBase ir = do
    let instrW = rcInstrWire ctx
        mEnc   = fmap parseEncoding (iirEncoding ir)
        col    = mconcat (map collectS (iirStmts ir))
                   <> maybe mempty collectE (iirGate ir)
        fieldKeys = nub (colFields col)
        regReads  = nub (colRegReads col)   -- (rf, field, scale, offset)
        idxKeys   = nub (colIdx col)        -- (field, scale, offset) used as an index

    -- Field-extraction signals (shared by immediates and register-file indices).
    fieldPairs <- mapM (\k -> (,) k <$> extractFieldNamed mEnc instrW (iirMnemonic ir) k)
                       fieldKeys
    let fieldMap = Map.fromList fieldPairs

    -- Base match signal — computed early so it can stand in as a fallback.
    base <- case mBase of
        Just w  -> pure w
        Nothing -> case (mEnc, iirMnemonic ir) of
            (Just e, Just nm) -> named ("match_" ++ nm) (buildMatch e instrW)
            (Just e, Nothing) -> pure (buildMatch e instrW)
            (Nothing, _)      -> pure (sigLitW 1 1)
    -- Data fallback for missing field/index/read lookups (an undriven 0).
    let dflt = sigLitW 0 1 :: s dom ()

    -- Register-file index signals.  Offset 0 is the raw field; a non-zero offset
    -- (sub-range encoding, e.g. AVR @ldi@ → R16..R31) adds a constant.
    idxPairs <- mapM (\(k, sc, off) ->
                    if null k
                        then do  -- constant index: a literal, no field
                            s <- named (mnemPrefix (iirMnemonic ir) ("r" ++ show off ++ "_idx"))
                                       (sigLitW (fromIntegral off) (rcWordW ctx))
                            pure ((k, sc, off), s)
                        else do
                            let fw = Map.findWithDefault dflt k fieldMap
                            if sc == 1 && off == 0
                                then pure ((k, sc, off), fw)
                                else do
                                    let ext    = sigPrim1 (N.PResize (rcWordW ctx)) fw
                                        scaled = if sc == 1 then ext
                                                 else sigPrim2 N.PMul ext (sigLitW (fromIntegral sc) (rcWordW ctx))
                                        s0     = if off == 0 then scaled
                                                 else sigPrim2 N.PAdd scaled (sigLitW (fromIntegral off) (rcWordW ctx))
                                    s <- named (mnemPrefix (iirMnemonic ir) (k ++ "_idx")) s0
                                    pure ((k, sc, off), s))
                   idxKeys
    let idxMap = Map.fromList idxPairs
        idxWire k sc off = Map.findWithDefault dflt (k, sc, off) idxMap

    -- One register-file read per distinct (file, index field, scale, offset),
    -- resolved inline via 'regBankRead'.
    regTriples <- mapM (\(rf, k, sc, off) -> do
                            out <- regBankRead "cpu_state" rf (rcRegCount ctx rf) (idxWire k sc off)
                            out' <- named (rf ++ "_" ++ slotTag k off) out
                            pure ((rf, k, sc, off), out', RegReadReq rf (idxWire k sc off) out'))
                       regReads
    let regOutMap   = Map.fromList [ (key, o) | (key, o, _) <- regTriples ]
        regReadReqs = [ r | (_, _, r) <- regTriples ]

    -- Read-result resolution: code reads alias the code bus; data reads use the
    -- per-read signal the sequencer drives ('rcReadRes').
    let codeToks = [ t | SReadCode (ReadTok t) _ <- iirStmts ir ]
        readResSig (ReadTok t)
            | t `elem` codeToks = rcCodeBus ctx
            | otherwise         = rcReadRes ctx (ReadTok t)

    -- Read a view register: concatenate its constant-index entry reads, the
    -- first (low) entry least significant.  Pure 'Signal' composition.
    let readView file ew idxs =
            let total = ew * length idxs
                entrySig idx = Map.findWithDefault dflt (file, "", 1, idx) regOutMap
                parts = [ let z = sigPrim1 (N.PResize total) (entrySig idx)
                          in if p == 0 then z
                             else sigPrim2 N.PShiftL z (sigLitW (fromIntegral (p * ew)) total)
                        | (p, idx) <- zip [0 :: Int ..] idxs ]
            in case parts of
                 []       -> sigLitW 0 (max total 1)
                 (x : xs) -> foldl (sigPrim2 N.POr) x xs
        lctx = LowerCtx
            { lcReadReg = \ref -> case ref of
                  RegScalar n                    -> rcReadScalar ctx n
                  RegFile rf (FieldRef k) sc off -> Map.findWithDefault dflt (rf, k, sc, off) regOutMap
                  RegEntries file ew idxs        -> readView file ew idxs
            , lcField     = \(FieldRef k) -> Map.findWithDefault dflt k fieldMap
            , lcReadRes   = readResSig
            , lcReadFlag  = \f -> rcGetFlag ctx (cpuFlagReg f) (cpuFlagBit f)
            , lcIrqVector = maybe dflt id (rcIrqVector ctx)
            , lcMnemonic  = iirMnemonic ir
            }

    -- irqGate refines the match condition.
    matchW <- case iirGate ir of
        Nothing -> pure base
        Just g  -> sigPrim2 N.PAnd base <$> lowerExpr_ lctx g

    r <- renderInstr lctx ir

    -- Map the lowered 'Rendered' into request structures (pure now).
    let splitW (RegWrite (RegScalar n) w)                 = Left  (ScalarWriteReq matchW n w)
        splitW (RegWrite (RegFile rf (FieldRef k) sc off) w) =
            Right (RegWriteReq matchW rf (idxWire k sc off) w)
        splitW (RegWrite (RegEntries{}) _) =
            error "view-register write should have been fanned out in renderInstr"
        (scalarWs, regWs) = partitionEithers (map splitW (rRegWrites r))

        memWrites = [ MemWriteReq matchW a (sigPrim1 (N.PResize (rcWordW ctx)) d)
                    | (a, d) <- rMemWrites r ]
        memReads  = [ MemReadReq matchW a (rcDataBus ctx) (readResSig (ReadTok t))
                    | (ReadTok t, a) <- rMemReads r ]
        flagWrites = [ FlagWriteReq matchW (cpuFlagReg f) (cpuFlagBit f) w
                     | (f, w) <- rFlagWrites r ]
        jumpWs = [ ScalarWriteReq (sigPrim2 N.PAnd matchW cond) (regRefName rr) tgt
                 | Jump rr cond tgt <- rJumps r ]

    pure SynthResult
        { srMatchWire    = Just matchW
        , srRegWrites    = regWs
        , srScalarWrites = scalarWs ++ jumpWs
        , srRegReads     = regReadReqs
        , srMemWrites    = memWrites
        , srMemReads     = memReads
        , srFlagWrites   = flagWrites
        }

regRefName :: RegRef w -> String
regRefName (RegScalar n)      = n
regRefName (RegFile  n _ _ _) = n
regRefName (RegEntries n _ _) = n

-- | Name a register-file slot: a field key (with optional @_pN@ offset) or, for
-- a constant index (empty key), the index number @rN@.
slotTag :: String -> Int -> String
slotTag k off
    | null k    = "r" ++ show off
    | off == 0  = k
    | otherwise = k ++ "_p" ++ show off

-- | Prefix a name with the instruction mnemonic when one is present.
mnemPrefix :: Maybe String -> String -> String
mnemPrefix mnem s = maybe s (\m -> m ++ "_" ++ s) mnem

-- | Extract a field's bits from the instruction word (or a 0 literal when the
-- encoding lacks it), named @\<mnemonic\>_\<key\>@.
extractFieldNamed :: (Hdl s m, Signal s)
                  => Maybe EncodingInfo -> s dom () -> Maybe String -> String -> m (s dom ())
extractFieldNamed mEnc instrW mnem k = do
    let sig = case mEnc >>= Map.lookup k . encFields of
                Just bits -> extractFieldSig bits instrW
                Nothing   -> sigLitW 0 1
    case mnem of { Just nm -> named (nm ++ "_" ++ k) sig; _ -> pure sig }

-- ---------------------------------------------------------------------------
-- Pre-pass: collect field keys and register-file reads from the IR
-- ---------------------------------------------------------------------------

data Collected = Collected
    { colRegReads :: [(String, String, Int, Int)] -- (register file, index field, scale, offset)
    , colFields   :: [String]                     -- field keys (immediates + indices)
    , colIdx      :: [(String, Int, Int)]         -- (index field, scale, offset) used as a regfile index
    }

instance Semigroup Collected where
    Collected a b c <> Collected d e f = Collected (a ++ d) (b ++ e) (c ++ f)
instance Monoid Collected where
    mempty = Collected [] [] []

-- | Collect the field key and index wire a register /reference/ needs.  A write
-- destination contributes its index field (so it gets extracted) and its index
-- wire, but — unlike a read — needs no read port.
-- | An empty field key denotes a constant register index (the index is the
-- offset, lowered to a literal): no field is extracted for it.
fieldsOf :: String -> [String]
fieldsOf k = [ k | not (null k) ]

collectRef :: RegRef w -> Collected
collectRef (RegScalar _)               = mempty
collectRef (RegEntries _ _ _)          = mempty  -- view writes fan out to file slots in renderInstr
collectRef (RegFile _ (FieldRef k) s o)  = Collected [] (fieldsOf k) [(k, s, o)]

collectE :: IExpr w -> Collected
collectE e = case e of
    ILit _                                  -> mempty
    IField (FieldRef k)                     -> Collected [] [k] []
    IReadReg (RegScalar _)                  -> mempty
    IReadReg (RegFile rf (FieldRef k) s o)  -> Collected [(rf, k, s, o)] (fieldsOf k) [(k, s, o)]
    -- A view read needs each constituent entry as a constant-index file read.
    IReadReg (RegEntries file _ idxs)       ->
        mconcat [ Collected [(file, "", 1, i)] [] [("", 1, i)] | i <- idxs ]
    IReadRes _                              -> mempty
    IFlagRead _                             -> mempty
    IIrqVector                              -> mempty
    IBin _ a b                              -> collectE a <> collectE b
    IUn _ a                                 -> collectE a
    IMux c t f                              -> collectE c <> collectE t <> collectE f
    IReinterpret a                          -> collectE a
    IResize a                               -> collectE a
    ISignExt a                              -> collectE a
    IZeroExt a                              -> collectE a
    ITrunc a                                -> collectE a
    IIsZero a                               -> collectE a
    ISlice _ _ a                            -> collectE a
    INamed _ a                              -> collectE a

collectS :: IStmt -> Collected
collectS s = case s of
    SReadMem  _ a      -> collectE a
    SReadCode _ a      -> collectE a
    SWriteReg r e      -> collectRef r <> collectE e
    SWriteMem a d      -> collectE a <> collectE d
    SWriteFlag _ e     -> collectE e
    SJumpIf r c t      -> collectRef r <> collectE c <> collectE t

-- ---------------------------------------------------------------------------
-- Combinational helpers (pure Signal, shared with the CPU pass)
-- ---------------------------------------------------------------------------

-- | The combinational instruction-match signal: @(instr AND mask) == value@.
buildMatch :: Signal s => EncodingInfo -> s dom () -> s dom Bool
buildMatch enc instrW =
    sigPrim2 N.PEq
        (sigPrim2 N.PAnd instrW (sigLitW (encMask enc) (encTotalBits enc)))
        (sigLitW (encValue enc) (encTotalBits enc))

-- | Extract non-contiguous field bits, reassembling MSB-first via PSlice/PConcat.
extractFieldSig :: Signal s => [Int] -> s dom () -> s dom ()
extractFieldSig []   _      = sigLitW 0 1
extractFieldSig [bp] instrW = sigPrim1 (N.PSlice bp bp) instrW
extractFieldSig bps  instrW =
    foldr1 (sigPrim2 N.PConcat) [ sigPrim1 (N.PSlice bp bp) instrW | bp <- bps ]
