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
import Data.Either (partitionEithers)
import Data.List (nub)
import qualified Data.Map.Strict as Map

import Hdl.Net (NetM, WireId, NetNode(..), freshWire, emit, hintWire)
import qualified Hdl.Net as N
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
        regReads  = nub (colRegReads col)

    -- Field-extraction wires (shared by immediates and register-file indices).
    fieldPairs <- mapM (\k -> (,) k <$> extractFieldNamed mEnc instrW (iirMnemonic ir) k)
                       fieldKeys
    let fieldMap = Map.fromList fieldPairs

    -- One read port per distinct (register file, index field).
    regTriples <- mapM (\(rf, k) -> do
                            outW <- freshWire
                            hintWire outW (rf ++ "_" ++ k)
                            let idxW = Map.findWithDefault outW k fieldMap
                            pure ((rf, k), outW, RegReadReq rf idxW outW))
                       regReads
    let regOutMap   = Map.fromList [ ((rf,k), o) | ((rf,k), o, _) <- regTriples ]
        regReadReqs = [ r | (_, _, r) <- regTriples ]

    -- Read-result wires: data reads get a fresh wire (driven by the sequencer);
    -- code reads alias the code bus directly.
    tokPairs <- fmap concat $ mapM (\s -> case s of
                    SReadMem  (ReadTok t) _ -> do { w <- freshWire; pure [(t, w)] }
                    SReadCode (ReadTok t) _ -> pure [(t, rcCodeBus ctx)]
                    _                       -> pure []) (iirStmts ir)
    let tokMap = Map.fromList tokPairs

    -- Base match wire.
    base <- case mBase of
        Just w  -> pure w
        Nothing -> case mEnc of
            Just e  -> buildMatchWire e instrW
            Nothing -> litW 1 1
    case (mBase, iirMnemonic ir) of
        (Nothing, Just nm) -> hintWire base ("match_" ++ nm)
        _                  -> pure ()

    let lctx = LowerCtx
            { lcReadReg = \ref -> case ref of
                  RegScalar n             -> rcReadScalar ctx n
                  RegFile rf (FieldRef k) -> pure (Map.findWithDefault base (rf, k) regOutMap)
            , lcField     = \(FieldRef k) -> pure (Map.findWithDefault base k fieldMap)
            , lcReadRes   = \(ReadTok t)  -> pure (Map.findWithDefault (rcDataBus ctx) t tokMap)
            , lcReadFlag  = \f -> rcGetFlag ctx (cpuFlagReg f) (cpuFlagBit f)
            , lcIrqVector = pure (maybe base id (rcIrqVector ctx))
            , lcMnemonic  = iirMnemonic ir
            }

    -- irqGate refines the match condition.
    matchW <- case iirGate ir of
        Nothing -> pure base
        Just g  -> do
            gw <- lowerExpr_ lctx g
            o  <- freshWire; emit $ NComb o N.PAnd [base, gw]; pure o

    r <- renderInstr lctx ir

    -- Map the lowered Rendered into request structures.
    let splitW (RegWrite (RegScalar n) w)             = Left  (ScalarWriteReq matchW n w)
        splitW (RegWrite (RegFile rf (FieldRef k)) w) =
            Right (RegWriteReq matchW rf (Map.findWithDefault base k fieldMap) w)
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
                       g <- freshWire; emit $ NComb g N.PAnd [matchW, condW]
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
regRefName (RegScalar n)   = n
regRefName (RegFile  n _)  = n

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
clampW w d = do { o <- freshWire; emit $ NComb o (N.PResize w) [d]; pure o }

litW :: Integer -> Int -> NetM WireId
litW v w = do { o <- freshWire; emit $ NComb o (N.PLit v w) []; pure o }

-- ---------------------------------------------------------------------------
-- Pre-pass: collect field keys and register-file reads from the IR
-- ---------------------------------------------------------------------------

data Collected = Collected
    { colRegReads :: [(String, String)]   -- (register file, index field)
    , colFields   :: [String]             -- field keys (immediates + indices)
    }

instance Semigroup Collected where
    Collected a b <> Collected c d = Collected (a ++ c) (b ++ d)
instance Monoid Collected where
    mempty = Collected [] []

collectE :: IExpr w -> Collected
collectE e = case e of
    ILit _                              -> mempty
    IField (FieldRef k)                 -> Collected [] [k]
    IReadReg (RegScalar _)              -> mempty
    IReadReg (RegFile rf (FieldRef k))  -> Collected [(rf, k)] [k]
    IReadRes _                          -> mempty
    IFlagRead _                         -> mempty
    IIrqVector                          -> mempty
    IBin _ a b                          -> collectE a <> collectE b
    IUn _ a                             -> collectE a
    IResize a                           -> collectE a
    ISignExt a                          -> collectE a
    IZeroExt a                          -> collectE a
    ITrunc a                            -> collectE a
    IIsZero a                           -> collectE a
    ISlice _ _ a                        -> collectE a
    INamed _ a                          -> collectE a

collectS :: IStmt -> Collected
collectS s = case s of
    SReadMem  _ a      -> collectE a
    SReadCode _ a      -> collectE a
    SWriteReg _ e      -> collectE e
    SWriteMem a d      -> collectE a <> collectE d
    SWriteFlag _ e     -> collectE e
    SJumpIf _ c t      -> collectE c <> collectE t

-- ---------------------------------------------------------------------------
-- Netlist helpers (shared with the CPU pass)
-- ---------------------------------------------------------------------------

-- | Build the combinational instruction-match signal: @(instr AND mask) == value@.
buildMatchWire :: EncodingInfo -> WireId -> NetM WireId
buildMatchWire enc instrW = do
    maskW <- freshWire; emit $ NComb maskW (N.PLit (encMask enc) (encTotalBits enc)) []
    andW  <- freshWire; emit $ NComb andW  N.PAnd [instrW, maskW]
    valW  <- freshWire; emit $ NComb valW  (N.PLit (encValue enc) (encTotalBits enc)) []
    out   <- freshWire; emit $ NComb out   N.PEq  [andW, valW]
    pure out

-- | Extract non-contiguous field bits, reassembling MSB-first via PSlice/PConcat.
extractFieldNetM :: [Int] -> WireId -> NetM WireId
extractFieldNetM []  _      = do { o <- freshWire; emit $ NComb o (N.PLit 0 1) []; pure o }
extractFieldNetM [bp] instrW = do { o <- freshWire; emit $ NComb o (N.PSlice bp bp) [instrW]; pure o }
extractFieldNetM bps instrW = do
    bits <- mapM (\bp -> do { o <- freshWire; emit $ NComb o (N.PSlice bp bp) [instrW]; pure o }) bps
    foldBits bits
  where
    foldBits [w]    = pure w
    foldBits (w:ws) = do { rest <- foldBits ws; o <- freshWire; emit $ NComb o N.PConcat [w, rest]; pure o }
    foldBits []     = do { o <- freshWire; emit $ NComb o (N.PLit 0 1) []; pure o }
