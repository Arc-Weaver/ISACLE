{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
-- | Per-instruction synthesis backend for 'MonadALU'.
--
-- 'SynthM' interprets one instruction body, building NetNode IR fragments:
-- encoding fields are extracted via 'PSlice'/'PConcat' nodes; ALU ops become
-- 'NComb' nodes; register and memory accesses are recorded as request
-- structures for the CPU-level synthesis pass ('Isacle.ISA.Backend.SynthCPU')
-- to wire into the register files and memories.
--
-- /Signal representation/: wire identifiers ('WireId') are stored inside the
-- 'Unsigned' wrapper that 'MonadALU' returns, so instruction bodies compile
-- unchanged.  Concrete arithmetic on those wrappers (e.g. flag equality
-- checks in the instruction body) produces incorrect synthesis results; this
-- is an acknowledged limitation until the typeclass grows signal-level flag
-- operations.
module Isacle.ISA.Backend.Synth
    ( -- * Context (provided per instruction by the CPU synthesis pass)
      SynthCtx(..)
      -- * Collected per-instruction outputs
    , SynthResult(..)
    , RegWriteReq(..)
    , ScalarWriteReq(..)
    , RegReadReq(..)
    , MemWriteReq(..)
    , MemReadReq(..)
    , FlagWriteReq(..)
      -- * Synthesis monad
    , SynthM
      -- * Runner
    , runSynthM
    , evalSynthM
    ) where

import Prelude hiding (Word)
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Control.Monad.Reader
import Control.Monad.State.Strict

import GHC.TypeLits (KnownNat, natVal)
import Hdl.Bits
import Hdl.Net (NetM, WireId, NetNode(..), freshWire, emit)
import qualified Hdl.Net as N
import Isacle.ISA.Types
import Isacle.ISA.ALU
import Isacle.ISA.Encoding

-- ---------------------------------------------------------------------------
-- Context
-- ---------------------------------------------------------------------------

-- | Context supplied by the CPU synthesis pass for one instruction slot.
data SynthCtx alu = SynthCtx
    { scAlu       :: alu
    , scInstrWire :: WireId
      -- ^ instruction word wire (sliced to extract field bits)
    , scReadReg   :: String -> WireId -> NetM WireId
      -- ^ scalar-register reader: @rfname → (ignored) → current-value wire@.
      --   Only called for scalar registers (no \':\' in key).
      --   Register-file reads are collected as 'RegReadReq' instead.
    , scReadMem   :: WireId -> NetM WireId
      -- ^ data-memory reader: @addr-wire → data-wire@.
      --   Typically returns a pre-allocated NInput wire; the caller routes the
      --   address separately using 'srMemReads'.
    , scReadCode  :: WireId -> NetM WireId
      -- ^ code-memory reader (for LPM-style instructions).
    , scGetFlag   :: String -> Int -> NetM WireId
      -- ^ flag bit reader: @regName → bitPos → 1-bit output wire@.
      --   Emits a PSlice node to extract the bit from the status register.
    }

-- ---------------------------------------------------------------------------
-- Per-instruction output types
-- ---------------------------------------------------------------------------

-- | Register-file (indexed) write request — guarded by 'rwMatchWire'.
data RegWriteReq = RegWriteReq
    { rwMatchWire :: WireId   -- ^ 1 when this instruction is selected
    , rwRfName    :: String   -- ^ register file name
    , rwIdxWire   :: WireId   -- ^ register index signal (from field extraction)
    , rwDataWire  :: WireId   -- ^ data-to-write signal
    } deriving (Show)

-- | Scalar-register write request (PC, SP, …) — guarded by 'swMatchWire'.
data ScalarWriteReq = ScalarWriteReq
    { swMatchWire :: WireId
    , swRegName   :: String   -- ^ register name (no \':\')
    , swDataWire  :: WireId
    } deriving (Show)

-- | Register-file read request.  The CPU synthesis pass emits the 'NMem' node
-- (deferred, after the write arbiter is known) using 'rrOutWire' as the
-- read-data output wire.
data RegReadReq = RegReadReq
    { rrRfName  :: String
    , rrIdxWire :: WireId   -- ^ read address signal
    , rrOutWire :: WireId   -- ^ pre-allocated read-data wire
    } deriving (Show)

-- | Data-memory write request.
data MemWriteReq = MemWriteReq
    { mwMatchWire :: WireId
    , mwAddrWire  :: WireId
    , mwDataWire  :: WireId
    } deriving (Show)

-- | Data-memory read request.  'mrAddrWire' lets the CPU pass build an
-- address mux; 'scReadMem' already returned the shared data-in wire.
data MemReadReq = MemReadReq
    { mrMatchWire :: WireId
    , mrAddrWire  :: WireId
    } deriving (Show)

-- | Flag write request: set one bit of a status register when the instruction fires.
data FlagWriteReq = FlagWriteReq
    { fwMatchWire :: WireId
    , fwRegName   :: String   -- ^ status register name
    , fwBitPos    :: Int      -- ^ bit position (0 = LSB)
    , fwValueWire :: WireId   -- ^ 1-bit value wire
    } deriving (Show)

-- ---------------------------------------------------------------------------
-- Monad state
-- ---------------------------------------------------------------------------

data SynthSt = SynthSt
    { ssEncoding     :: Maybe EncodingInfo
    , ssMatchWire    :: Maybe WireId
    , ssFieldWires   :: Map String WireId   -- field char → extracted wire
    , ssRegWrites    :: [RegWriteReq]
    , ssScalarWrites :: [ScalarWriteReq]
    , ssRegReads     :: [RegReadReq]
    , ssMemWrites    :: [MemWriteReq]
    , ssMemReads     :: [MemReadReq]
    , ssFlagWrites   :: [FlagWriteReq]
    }

initSynthSt :: SynthSt
initSynthSt = SynthSt Nothing Nothing Map.empty [] [] [] [] [] []

-- ---------------------------------------------------------------------------
-- Result
-- ---------------------------------------------------------------------------

-- | All combinational outputs produced by running one instruction body.
-- The CPU synthesis pass ('synthHarvardCPU') assembles these into hardware.
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
-- Monad
-- ---------------------------------------------------------------------------

newtype SynthM (alu :: Type) (wordW :: Nat) (addrW :: Nat)
               (codeWordW :: Nat) (codeAddrW :: Nat) a
    = SynthM (ReaderT (SynthCtx alu) (StateT SynthSt NetM) a)
    deriving newtype (Functor, Applicative, Monad)

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

-- | Return the cached field wire for @k@, or extract it from the instruction
-- word using the stored 'EncodingInfo' and cache the result.
getFieldWire :: String
             -> ReaderT (SynthCtx alu) (StateT SynthSt NetM) WireId
getFieldWire k = do
    ctx <- ask
    st  <- get
    case Map.lookup k (ssFieldWires st) of
        Just w  -> return w
        Nothing -> do
            w <- lift $ lift $ case ssEncoding st of
                Nothing  -> freshWire
                Just enc -> case Map.lookup k (encFields enc) of
                    Nothing  -> freshWire
                    Just bps -> extractFieldNetM bps (scInstrWire ctx)
            modify $ \s -> s { ssFieldWires = Map.insert k w (ssFieldWires s) }
            return w

-- | Build the combinational instruction-match signal from fixed bits:
-- @(instrWord AND mask) == value@.
buildMatchWire :: EncodingInfo -> WireId -> NetM WireId
buildMatchWire enc instrW = do
    maskW <- freshWire
    emit $ NComb maskW (N.PLit (encMask enc) (encTotalBits enc)) []
    andW  <- freshWire
    emit $ NComb andW  N.PAnd [instrW, maskW]
    valW  <- freshWire
    emit $ NComb valW  (N.PLit (encValue enc) (encTotalBits enc)) []
    out   <- freshWire
    emit $ NComb out   N.PEq  [andW, valW]
    return out

-- | Extract non-contiguous field bits from an instruction word, reassembling
-- them MSB-first into a single output wire via 'PSlice'/'PConcat'.
extractFieldNetM :: [Int] -> WireId -> NetM WireId
extractFieldNetM [] _ = do
    out <- freshWire; emit $ NComb out (N.PLit 0 1) []; return out
extractFieldNetM [bp] instrW = do
    out <- freshWire; emit $ NComb out (N.PSlice bp bp) [instrW]; return out
extractFieldNetM bps instrW = do
    bits <- mapM (\bp -> do
        out <- freshWire
        emit $ NComb out (N.PSlice bp bp) [instrW]
        return out) bps
    foldBits bits
  where
    foldBits [w]    = return w
    foldBits (w:ws) = do
        rest <- foldBits ws
        out  <- freshWire
        emit $ NComb out N.PConcat [w, rest]
        return out
    foldBits [] = do
        out <- freshWire; emit $ NComb out (N.PLit 0 1) []; return out

-- | Decompose @"rfname:fieldChar"@ into @(rfname, fieldChar)@.
splitRegKey :: String -> (String, String)
splitRegKey key = let (rf, rest) = break (== ':') key in (rf, drop 1 rest)

-- | True for register-file keys (contain \':'\), false for scalar keys.
isRegFileKey :: String -> Bool
isRegFileKey = elem ':'

-- | Map 'ALUPrim' to 'N.PrimOp'.  'PNot' is handled separately (unary).
toPrimOp :: ALUPrim -> N.PrimOp
toPrimOp PAdd         = N.PAdd
toPrimOp PSub         = N.PSub
toPrimOp PAnd         = N.PAnd
toPrimOp POr          = N.POr
toPrimOp PXor         = N.PXor
toPrimOp PNot         = N.PNot
toPrimOp PShiftL      = N.PShiftL
toPrimOp PShiftR      = N.PShiftR
toPrimOp PArithShiftR = N.PShiftR  -- approximation
toPrimOp PMul         = N.PMul
toPrimOp PMulSigned   = N.PMul     -- approximation

-- ---------------------------------------------------------------------------
-- MonadALU instance
-- ---------------------------------------------------------------------------

instance KnownNat wordW => MonadALU (SynthM alu wordW addrW codeWordW codeAddrW) where
    type AluDef   (SynthM alu wordW addrW codeWordW codeAddrW) = alu
    type Word     (SynthM alu wordW addrW codeWordW codeAddrW) = Unsigned wordW
    type DataAddr (SynthM alu wordW addrW codeWordW codeAddrW) = Unsigned addrW

    cpu sel     = SynthM (asks (sel . scAlu))
    cpuFlag sel = SynthM (asks (sel . scAlu))

    mnemonic _ = return ()
    doc      _ = return ()

    encoding s = SynthM $ do
        ctx <- ask
        let enc = parseEncoding s
        modify $ \st -> st { ssEncoding = Just enc }
        matchW <- lift $ lift $ buildMatchWire enc (scInstrWire ctx)
        modify $ \st -> st { ssMatchWire = Just matchW }

    register sel field = SynthM $ do
        ctx <- ask
        let CPURegFile rfname = sel (scAlu ctx)
            k = fieldKey field
        _ <- getFieldWire k
        return (CPURegister (rfname ++ ":" ++ k))

    registerWithOffset sel field offset = SynthM $ do
        ctx <- ask
        let CPURegFile rfname = sel (scAlu ctx)
            k      = fieldKey field
            newKey = k ++ "+" ++ show offset
        fieldW <- getFieldWire k
        offsetW <- lift $ lift $ do
            litW <- freshWire
            emit $ NComb litW (N.PLit (fromIntegral offset) 5) []
            addW <- freshWire
            emit $ NComb addW N.PAdd [fieldW, litW]
            return addW
        modify $ \s -> s { ssFieldWires = Map.insert newKey offsetW (ssFieldWires s) }
        return (CPURegister (rfname ++ ":" ++ newKey))

    immediate field = SynthM $ do
        let k = fieldKey field
        w <- getFieldWire k
        return (Unsigned (fromIntegral w))

    -- | Register-file read: allocate a result wire, record the pending read.
    -- Scalar-register read: call 'scReadReg' to get the NReg output wire.
    readReg (CPURegister key)
        | isRegFileKey key = SynthM $ do
            let (_, fkey) = splitRegKey key
            st <- get
            case Map.lookup fkey (ssFieldWires st) of
                Nothing      -> return (Unsigned 0)
                Just idxWire -> do
                    let (rfname, _) = splitRegKey key
                    rdOutW <- lift $ lift freshWire
                    modify $ \s -> s { ssRegReads =
                        RegReadReq rfname idxWire rdOutW : ssRegReads s }
                    return (Unsigned (fromIntegral rdOutW))
        | otherwise = SynthM $ do
            ctx      <- ask
            dataWire <- lift $ lift $ scReadReg ctx key 0
            return (Unsigned (fromIntegral dataWire))

    -- | Register-file write: record a 'RegWriteReq'.
    -- Scalar-register write: record a 'ScalarWriteReq'.
    writeReg (CPURegister key) (Unsigned datWire)
        | isRegFileKey key = SynthM $ do
            let (rfname, fkey) = splitRegKey key
            st <- get
            case (ssMatchWire st, Map.lookup fkey (ssFieldWires st)) of
                (Just matchW, Just idxWire) ->
                    modify $ \s -> s { ssRegWrites =
                        RegWriteReq matchW rfname idxWire (fromIntegral datWire)
                        : ssRegWrites s }
                _ -> return ()
        | otherwise = SynthM $ do
            st <- get
            case ssMatchWire st of
                Nothing     -> return ()
                Just matchW ->
                    modify $ \s -> s { ssScalarWrites =
                        ScalarWriteReq matchW key (fromIntegral datWire)
                        : ssScalarWrites s }

    readMem (Unsigned addrWire) = SynthM $ do
        ctx <- ask
        st  <- get
        case ssMatchWire st of
            Just matchW ->
                modify $ \s -> s { ssMemReads =
                    MemReadReq matchW (fromIntegral addrWire) : ssMemReads s }
            Nothing -> return ()
        dataWire <- lift $ lift $ scReadMem ctx (fromIntegral addrWire)
        return (Unsigned (fromIntegral dataWire))

    writeMem (Unsigned addrWire) (Unsigned datWire) = SynthM $ do
        st <- get
        case ssMatchWire st of
            Nothing     -> return ()
            Just matchW -> do
                let dw = fromIntegral (natVal (Proxy @wordW))
                clamped <- lift $ lift $ do
                    out <- freshWire
                    emit $ NComb out (N.PResize dw) [fromIntegral datWire]
                    return out
                modify $ \s -> s { ssMemWrites =
                    MemWriteReq matchW (fromIntegral addrWire) clamped
                    : ssMemWrites s }

    getFlag (CPUFlag { cpuFlagReg = regName, cpuFlagBit = bitPos }) = SynthM $ do
        ctx <- ask
        w <- lift $ lift $ scGetFlag ctx regName bitPos
        return (Unsigned (fromIntegral w))

    setFlag (CPUFlag { cpuFlagReg = regName, cpuFlagBit = bitPos }) (Unsigned valW) = SynthM $ do
        st <- get
        case ssMatchWire st of
            Nothing     -> return ()
            Just matchW ->
                modify $ \s -> s { ssFlagWrites =
                    FlagWriteReq matchW regName bitPos (fromIntegral valW) : ssFlagWrites s }

    absJumpIf (CPURegister key) (Unsigned condW) (Unsigned targetW) = SynthM $ do
        st <- get
        case ssMatchWire st of
            Nothing     -> return ()
            Just matchW -> do
                gatedW <- lift $ lift $ do
                    out <- freshWire
                    emit $ NComb out N.PAnd [matchW, fromIntegral condW]
                    return out
                modify $ \s -> s { ssScalarWrites =
                    ScalarWriteReq gatedW key (fromIntegral targetW) : ssScalarWrites s }

    signExtendBits = \(v :: Unsigned k) -> go v Proxy
      where
        go :: forall k n. (KnownNat k, KnownNat n)
           => Unsigned k -> Proxy n -> SynthM alu wordW addrW codeWordW codeAddrW (Unsigned n)
        go (Unsigned srcW) proxy = SynthM $ lift $ lift $ do
            let dstW = fromIntegral (natVal proxy) :: Int
            out <- freshWire
            emit $ NComb out (N.PSignedResize dstW) [fromIntegral srcW]
            return (Unsigned (fromIntegral out))

    isZero = \(v :: Unsigned n) -> go v Proxy
      where
        go :: KnownNat n => Unsigned n -> Proxy n -> SynthM alu wordW addrW codeWordW codeAddrW (Unsigned 1)
        go (Unsigned wa) proxy = SynthM $ lift $ lift $ do
            let bw    = fromIntegral (natVal proxy) :: Int
                w     = fromIntegral wa :: WireId
            zeroW <- freshWire
            emit $ NComb zeroW (N.PLit 0 bw) []
            out   <- freshWire
            emit $ NComb out N.PEq [w, zeroW]
            return (Unsigned (fromIntegral out))

    aluOp PNot (Unsigned wa) _ = SynthM $ lift $ lift $ do
        out <- freshWire
        emit $ NComb out N.PNot [fromIntegral wa]
        return (Unsigned (fromIntegral out))
    aluOp p (Unsigned wa) (Unsigned wb) = SynthM $ lift $ lift $ do
        out <- freshWire
        emit $ NComb out (toPrimOp p) [fromIntegral wa, fromIntegral wb]
        return (Unsigned (fromIntegral out))

    litC = \v -> go v Proxy
      where
        go :: forall n. KnownNat n
           => Integer -> Proxy n
           -> SynthM alu wordW addrW codeWordW codeAddrW (Unsigned n)
        go v proxy = SynthM $ lift $ lift $ do
            let bw = fromIntegral (natVal proxy)
            out <- freshWire
            emit $ NComb out (N.PLit v bw) []
            return (Unsigned (fromIntegral out))

-- ---------------------------------------------------------------------------
-- MonadHarvardALU instance
-- ---------------------------------------------------------------------------

instance KnownNat wordW => MonadHarvardALU (SynthM alu wordW addrW codeWordW codeAddrW) where
    type CodeAddr (SynthM alu wordW addrW codeWordW codeAddrW) = Unsigned codeAddrW
    type CodeWord (SynthM alu wordW addrW codeWordW codeAddrW) = Unsigned codeWordW

    readCode (Unsigned addrWire) = SynthM $ do
        ctx      <- ask
        dataWire <- lift $ lift $ scReadCode ctx (fromIntegral addrWire)
        return (Unsigned (fromIntegral dataWire))

-- ---------------------------------------------------------------------------
-- Runner
-- ---------------------------------------------------------------------------

-- | Run one instruction body, collecting all combinational side effects.
runSynthM :: alu
          -> WireId                             -- ^ instruction word wire
          -> (String -> WireId -> NetM WireId)  -- ^ scalar register reader
          -> (WireId -> NetM WireId)             -- ^ data memory reader
          -> (WireId -> NetM WireId)             -- ^ code memory reader
          -> (String -> Int -> NetM WireId)      -- ^ flag bit reader: regName → bitPos → 1-bit wire
          -> SynthM alu wordW addrW codeWordW codeAddrW ()
          -> NetM SynthResult
runSynthM aluRec instrWire readRegFn readMemFn readCodeFn getFlagFn (SynthM m) = do
    let ctx = SynthCtx aluRec instrWire readRegFn readMemFn readCodeFn getFlagFn
    (_, st) <- runStateT (runReaderT m ctx) initSynthSt
    return SynthResult
        { srMatchWire    = ssMatchWire st
        , srRegWrites    = reverse (ssRegWrites st)
        , srScalarWrites = reverse (ssScalarWrites st)
        , srRegReads     = reverse (ssRegReads st)
        , srMemWrites    = reverse (ssMemWrites st)
        , srMemReads     = reverse (ssMemReads st)
        , srFlagWrites   = reverse (ssFlagWrites st)
        }

-- | Run a 'SynthM' action in a dummy context, threading through the current
-- 'NetM' state.  Used by the CPU synthesis pass to extract register/flag names
-- from ISADef fields (which only read the ALU record and emit no nodes).
evalSynthM :: alu -> SynthM alu wordW addrW codeWordW codeAddrW a -> NetM a
evalSynthM aluRec (SynthM m) = do
    let ctx = SynthCtx aluRec 0 (\_ _ -> return 0) (const (return 0))
                               (const (return 0)) (\_ _ -> return 0)
    (a, _) <- runStateT (runReaderT m ctx) initSynthSt
    return a
