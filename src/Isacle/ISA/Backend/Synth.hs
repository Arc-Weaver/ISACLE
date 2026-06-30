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
      -- * Netlist helpers reused by the CPU pass
    , buildMatchWire
    , extractFieldNetM
    ) where

import Prelude hiding (Word)
import Control.Monad (foldM)
import Data.Either (partitionEithers)
import Data.List (nub)
import qualified Data.Map.Strict as Map

import Hdl.Net (NetM, WireId, freshWire, hintWire)
import qualified Hdl.Net as N
import Isacle.ISA.Backend.Wire (comb2, litW, sliceW, resizeW, andW, eqW)
import Isacle.ISA.Types (CPUFlag(..))
import Isacle.ISA.Encoding
import Isacle.ISA.IR
import Isacle.ISA.Backend.Lower

-- ---------------------------------------------------------------------------
-- Per-instruction output types (consumed by SynthCPU's arbiters/sequencer)
-- ---------------------------------------------------------------------------

-- | Register-file (indexed) write request — guarded by 'rwMatchWire'.
data RegWriteReq = RegWriteReq
    { rwMatchWire :: WireId
    , rwRfName    :: String
    , rwIdxWire   :: WireId
    , rwDataWire  :: WireId
    } deriving (Show)

-- | Scalar-register write request (PC, SP, …) — guarded by 'swMatchWire'.
data ScalarWriteReq = ScalarWriteReq
    { swMatchWire :: WireId
    , swRegName   :: String
    , swDataWire  :: WireId
    } deriving (Show)

-- | Register-file read request; the CPU pass emits the 'NMem' read port driving
-- 'rrOutWire' from 'rrIdxWire'.
data RegReadReq = RegReadReq
    { rrRfName  :: String
    , rrIdxWire :: WireId
    , rrOutWire :: WireId
    } deriving (Show)

-- | Data-memory write request.
data MemWriteReq = MemWriteReq
    { mwMatchWire :: WireId
    , mwAddrWire  :: WireId
    , mwDataWire  :: WireId
    } deriving (Show)

-- | Data-memory read request.  'mrResultWire' is the wire the body consumed for
-- the read result; the CPU sequencer drives it from 'mrBusWire' (directly for a
-- single read, or via a per-cycle select + holding latch for multi-reads).
data MemReadReq = MemReadReq
    { mrMatchWire  :: WireId
    , mrAddrWire   :: WireId
    , mrBusWire    :: WireId
    , mrResultWire :: WireId
    } deriving (Show)

-- | Flag write: set one status-register bit when the instruction fires.
data FlagWriteReq = FlagWriteReq
    { fwMatchWire :: WireId
    , fwRegName   :: String
    , fwBitPos    :: Int
    , fwValueWire :: WireId
    } deriving (Show)

-- | All combinational outputs of one instruction.
data SynthResult = SynthResult
    { srMatchWire    :: Maybe WireId
    , srRegWrites    :: [RegWriteReq]
    , srScalarWrites :: [ScalarWriteReq]
    , srRegReads     :: [RegReadReq]
    , srMemWrites    :: [MemWriteReq]
    , srMemReads     :: [MemReadReq]
    , srFlagWrites   :: [FlagWriteReq]
    } deriving (Show)

-- ---------------------------------------------------------------------------
-- Render context
-- ---------------------------------------------------------------------------

-- | Resolution the CPU pass supplies for one instruction slot.
data RenderCtx = RenderCtx
    { rcInstrWire  :: WireId                    -- ^ instruction word (field source)
    , rcReadScalar :: String -> NetM WireId     -- ^ scalar register reader
    , rcDataBus    :: WireId                    -- ^ data_rd_data
    , rcCodeBus    :: WireId                    -- ^ code read bus (LPM/2nd word)
    , rcGetFlag    :: String -> Int -> NetM WireId  -- ^ status-bit reader
    , rcIrqVector  :: Maybe WireId              -- ^ irq_vector (in an IRQ body)
    , rcWordW      :: Int                       -- ^ data word width (write clamp)
    }

-- ---------------------------------------------------------------------------
-- Render
-- ---------------------------------------------------------------------------

-- | Lower one instruction's 'InstrIR' into a 'SynthResult'.
--
-- @mBase@ is the base match condition: 'Nothing' to derive it from the encoding
-- (normal instructions), or @Just w@ to seed it (e.g. @irq_pending@).
renderSynth :: RenderCtx -> Maybe WireId -> InstrIR -> NetM SynthResult
renderSynth ctx mBase ir = do
    let instrW = rcInstrWire ctx
        mEnc   = fmap parseEncoding (iirEncoding ir)
        col    = mconcat (map collectS (iirStmts ir))
                   <> maybe mempty collectE (iirGate ir)
        fieldKeys = nub (colFields col)
        regReads  = nub (colRegReads col)   -- (rf, field, offset)
        idxKeys   = nub (colIdx col)        -- (field, offset) used as a regfile index

    -- Field-extraction wires (shared by immediates and register-file indices).
    fieldPairs <- mapM (\k -> (,) k <$> extractFieldNamed mEnc instrW (iirMnemonic ir) k)
                       fieldKeys
    let fieldMap = Map.fromList fieldPairs

    -- Base match wire — computed early so it can stand in as a fallback wire.
    base <- case mBase of
        Just w  -> pure w
        Nothing -> case mEnc of
            Just e  -> buildMatchWire e instrW
            Nothing -> litW 1 1
    case (mBase, iirMnemonic ir) of
        (Nothing, Just nm) -> hintWire base ("match_" ++ nm)
        _                  -> pure ()

    -- Register-file index wires.  Offset 0 is the raw field; a non-zero offset
    -- (sub-range encoding, e.g. AVR @ldi@ → R16..R31) adds a constant, producing
    -- a real adder so the index reaches the high half of the file.
    idxPairs <- mapM (\(k, sc, off) ->
                    if null k
                        then do  -- constant index: a literal, no field
                            w <- litW (fromIntegral off) (rcWordW ctx)
                            hintWire w (mnemPrefix (iirMnemonic ir) ("r" ++ show off ++ "_idx"))
                            pure ((k, sc, off), w)
                        else do
                            let fw = Map.findWithDefault base k fieldMap
                            if sc == 1 && off == 0
                                then pure ((k, sc, off), fw)
                                else do
                                    ext    <- resizeW (rcWordW ctx) fw
                                    scaled <- if sc == 1 then pure ext
                                              else comb2 N.PMul ext =<< litW (fromIntegral sc) (rcWordW ctx)
                                    s <- if off == 0 then pure scaled
                                         else comb2 N.PAdd scaled =<< litW (fromIntegral off) (rcWordW ctx)
                                    hintWire s (mnemPrefix (iirMnemonic ir) (k ++ "_idx"))
                                    pure ((k, sc, off), s))
                   idxKeys
    let idxMap = Map.fromList idxPairs
        idxWire k sc off = Map.findWithDefault base (k, sc, off) idxMap

    -- One read port per distinct (register file, index field, scale, offset).
    regTriples <- mapM (\(rf, k, sc, off) -> do
                            outW <- freshWire
                            hintWire outW (rf ++ "_" ++ slotTag k off)
                            pure ((rf, k, sc, off), outW, RegReadReq rf (idxWire k sc off) outW))
                       regReads
    let regOutMap   = Map.fromList [ (key, o) | (key, o, _) <- regTriples ]
        regReadReqs = [ r | (_, _, r) <- regTriples ]

    -- Read-result wires: data reads get a fresh wire (driven by the sequencer);
    -- code reads alias the code bus directly.
    tokPairs <- fmap concat $ mapM (\s -> case s of
                    SReadMem  (ReadTok t) _ -> do { w <- freshWire; pure [(t, w)] }
                    SReadCode (ReadTok t) _ -> pure [(t, rcCodeBus ctx)]
                    _                       -> pure []) (iirStmts ir)
    let tokMap = Map.fromList tokPairs

    let -- Read a view register: concatenate its constant-index entry reads, the
        -- first (low) entry least significant.
        readView file ew idxs = do
            let total = ew * length idxs
                entryWire idx = Map.findWithDefault base (file, "", 1, idx) regOutMap
            parts <- mapM (\(p, idx) -> do
                rz <- resizeW total (entryWire idx)
                if p == 0 then pure rz
                          else comb2 N.PShiftL rz =<< litW (fromIntegral (p * ew)) total)
                (zip [0 :: Int ..] idxs)
            case parts of
                []       -> litW 0 (max total 1)
                (x : xs) -> foldM (comb2 N.POr) x xs
    let lctx = LowerCtx
            { lcReadReg = \ref -> case ref of
                  RegScalar n                 -> rcReadScalar ctx n
                  RegFile rf (FieldRef k) sc off -> pure (Map.findWithDefault base (rf, k, sc, off) regOutMap)
                  RegEntries file ew idxs     -> readView file ew idxs
            , lcField     = \(FieldRef k) -> pure (Map.findWithDefault base k fieldMap)
            , lcReadRes   = \(ReadTok t)  -> pure (Map.findWithDefault (rcDataBus ctx) t tokMap)
            , lcReadFlag  = \f -> rcGetFlag ctx (cpuFlagReg f) (cpuFlagBit f)
            , lcIrqVector = pure (maybe base id (rcIrqVector ctx))
            , lcMnemonic  = iirMnemonic ir
            }

    -- irqGate refines the match condition.
    matchW <- case iirGate ir of
        Nothing -> pure base
        Just g  -> andW base =<< lowerExpr_ lctx g

    r <- renderInstr lctx ir

    -- Map the lowered Rendered into request structures.
    let splitW (RegWrite (RegScalar n) w)                 = Left  (ScalarWriteReq matchW n w)
        splitW (RegWrite (RegFile rf (FieldRef k) sc off) w) =
            Right (RegWriteReq matchW rf (idxWire k sc off) w)
        splitW (RegWrite (RegEntries{}) _) =
            error "view-register write should have been fanned out in renderInstr"
        (scalarWs, regWs) = partitionEithers (map splitW (rRegWrites r))

    memWrites <- mapM (\(a, d) -> do
                          dc <- clampW (rcWordW ctx) d
                          pure (MemWriteReq matchW a dc)) (rMemWrites r)

    let memReads = [ MemReadReq matchW a (rcDataBus ctx)
                                (Map.findWithDefault (rcDataBus ctx) t tokMap)
                   | (ReadTok t, a) <- rMemReads r ]
        flagWrites = [ FlagWriteReq matchW (cpuFlagReg f) (cpuFlagBit f) w
                     | (f, w) <- rFlagWrites r ]

    jumpWs <- mapM (\(Jump rr condW tgtW) -> do
                       g <- andW matchW condW
                       pure (ScalarWriteReq g (regRefName rr) tgtW)) (rJumps r)

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

-- | Extract a field's bits from the instruction word (or a fresh wire when the
-- encoding lacks it), named @\<mnemonic\>_\<key\>@.
extractFieldNamed :: Maybe EncodingInfo -> WireId -> Maybe String -> String -> NetM WireId
extractFieldNamed mEnc instrW mnem k = do
    w <- case mEnc of
        Just e  -> maybe freshWire (`extractFieldNetM` instrW) (Map.lookup k (encFields e))
        Nothing -> freshWire
    case mnem of { Just nm -> hintWire w (nm ++ "_" ++ k); _ -> pure () }
    pure w

clampW :: Int -> WireId -> NetM WireId
clampW = resizeW

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
-- Netlist helpers (shared with the CPU pass)
-- ---------------------------------------------------------------------------

-- | Build the combinational instruction-match signal: @(instr AND mask) == value@.
buildMatchWire :: EncodingInfo -> WireId -> NetM WireId
buildMatchWire enc instrW = do
    anded <- comb2 N.PAnd instrW =<< litW (encMask enc) (encTotalBits enc)
    eqW anded =<< litW (encValue enc) (encTotalBits enc)

-- | Extract non-contiguous field bits, reassembling MSB-first via PSlice/PConcat.
extractFieldNetM :: [Int] -> WireId -> NetM WireId
extractFieldNetM []   _      = litW 0 1
extractFieldNetM [bp] instrW = sliceW bp bp instrW
extractFieldNetM bps  instrW = mapM (\bp -> sliceW bp bp instrW) bps >>= foldBits
  where
    foldBits [w]    = pure w
    foldBits (w:ws) = foldBits ws >>= comb2 N.PConcat w
    foldBits []     = litW 0 1
