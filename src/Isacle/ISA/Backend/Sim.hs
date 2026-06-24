{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Isacle.ISA.Backend.Sim
    ( -- * Simulation state
      SimState(..)
    , SimCPU(..)
    , emptySim
      -- * Simulation monad
    , SimM
      -- * Runners
    , runInstr
    , execInstr
    ) where

import Prelude hiding (Word)
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import GHC.TypeLits (natVal)
import Control.Monad.Reader
import Control.Monad.State.Strict

import Hdl.Bits
import Isacle.ISA.Types
import Isacle.ISA.ALU
import Isacle.ISA.Encoding

-- ---------------------------------------------------------------------------
-- Simulation state
-- ---------------------------------------------------------------------------

-- | CPU register and flag state, keyed by name.
--
--   Register file entries use composite keys: @\"GPR:5\"@ for GPR index 5.
--   Single registers use their declared name: @\"SP\"@, @\"PC\"@.
data SimCPU = SimCPU
    { scRegs  :: Map String Integer
    , scFlags :: Map String Bit
    } deriving (Show, Eq)

-- | Full simulation state: CPU state plus separate data and code memories,
-- and the encoding parsed from the current instruction body.
data SimState = SimState
    { ssCPU      :: SimCPU
    , ssDataMem  :: IntMap Integer   -- ^ byte-addressable data memory
    , ssCodeMem  :: IntMap Integer   -- ^ word-addressable code memory (Harvard)
    , ssEncoding :: Maybe EncodingInfo -- ^ parsed by 'encoding' call in instr body
    } deriving (Show)

emptySim :: SimState
emptySim = SimState (SimCPU Map.empty Map.empty) IntMap.empty IntMap.empty Nothing

-- ---------------------------------------------------------------------------
-- Simulation monad
-- ---------------------------------------------------------------------------

-- | Context supplied for one instruction execution: the ALU handle record
--   and the raw instruction word.  Field values are extracted from the word
--   via the 'EncodingInfo' stored in 'SimState' (set by 'encoding').
data SimCtx alu = SimCtx
    { sxAlu       :: alu
    , sxInstrWord :: Integer   -- ^ raw instruction word to decode
    }

-- | Simulation backend for 'MonadALU'.
newtype SimM (alu :: Type) (wordW :: Nat) (addrW :: Nat)
             (codeWordW :: Nat) (codeAddrW :: Nat) a
    = SimM (ReaderT (SimCtx alu) (State SimState) a)
    deriving newtype (Functor, Applicative, Monad)

-- ---------------------------------------------------------------------------
-- MonadALU instance
-- ---------------------------------------------------------------------------

instance MonadALU (SimM alu wordW addrW codeWordW codeAddrW) where
    type AluDef   (SimM alu wordW addrW codeWordW codeAddrW) = alu
    type Word     (SimM alu wordW addrW codeWordW codeAddrW) = Unsigned wordW
    type DataAddr (SimM alu wordW addrW codeWordW codeAddrW) = Unsigned addrW

    cpu sel    = SimM (asks (sel . sxAlu))
    cpuFlag sel = SimM (asks (sel . sxAlu))

    -- | Parse the encoding string and store in 'SimState'; field extraction
    --   in 'register' and 'immediate' then reads it from state.
    encoding s = SimM $ do
        let enc = parseEncoding s
        modify $ \st -> st { ssEncoding = Just enc }

    -- | Extract the register index from the instruction word via the stored
    --   encoding, then return a typed register handle.
    register sel field = SimM $ do
        ctx <- ask
        st  <- get
        let CPURegFile rfname = sel (sxAlu ctx)
            k   = fieldKey field
            idx = case ssEncoding st of
                Nothing  -> 0
                Just enc -> case Map.lookup k (encFields enc) of
                    Nothing  -> 0
                    Just bps -> extractField bps (sxInstrWord ctx)
        return (CPURegister (rfname ++ ":" ++ show idx))

    registerWithOffset sel field offset = SimM $ do
        ctx <- ask
        st  <- get
        let CPURegFile rfname = sel (sxAlu ctx)
            k   = fieldKey field
            idx = case ssEncoding st of
                Nothing  -> 0
                Just enc -> case Map.lookup k (encFields enc) of
                    Nothing  -> 0
                    Just bps -> extractField bps (sxInstrWord ctx)
        return (CPURegister (rfname ++ ":" ++ show (idx + fromIntegral offset)))

    -- | Extract an immediate value from the instruction word via the stored encoding.
    immediate field = SimM $ do
        ctx <- ask
        st  <- get
        let k = fieldKey field
        return $ Unsigned $ case ssEncoding st of
            Nothing  -> 0
            Just enc -> case Map.lookup k (encFields enc) of
                Nothing  -> 0
                Just bps -> extractField bps (sxInstrWord ctx)

    mnemonic _ = return ()
    doc      _ = return ()

    -- | Read a register; mask the stored integer to the declared bit width.
    readReg :: forall w. KnownNat w => CPURegister w -> SimM alu wordW addrW codeWordW codeAddrW (Unsigned w)
    readReg (CPURegister name) = SimM $ do
        st <- get
        let raw  = Map.findWithDefault 0 name (scRegs (ssCPU st))
            mask = (1 `shiftL` fromIntegral (natVal (Proxy @w))) - 1
        return (fromInteger (raw .&. mask))

    -- | Write a register; mask to the declared bit width before storing.
    writeReg :: forall w. KnownNat w => CPURegister w -> Unsigned w -> SimM alu wordW addrW codeWordW codeAddrW ()
    writeReg (CPURegister name) val = SimM $ do
        let w    = fromIntegral (natVal (Proxy @w)) :: Int
            mask = (1 `shiftL` w) - 1
            Unsigned raw = val
        modify $ \st -> st { ssCPU = (ssCPU st)
            { scRegs = Map.insert name (raw .&. mask) (scRegs (ssCPU st)) } }

    readMem (Unsigned addr) = SimM $ do
        st <- get
        return (Unsigned (IntMap.findWithDefault 0 (fromIntegral addr) (ssDataMem st)))

    writeMem (Unsigned addr) val = SimM $ do
        let Unsigned v = val
        modify $ \st -> st { ssDataMem = IntMap.insert (fromIntegral addr) v (ssDataMem st) }

    getFlag (CPUFlag name) = SimM $ do
        st <- get
        return (Map.findWithDefault Lo name (scFlags (ssCPU st)))

    setFlag (CPUFlag name) b = SimM $
        modify $ \st -> st { ssCPU = (ssCPU st)
            { scFlags = Map.insert name b (scFlags (ssCPU st)) } }

    -- | ALU operation; result is masked to @w@ bits so PNot is correct.
    aluOp :: forall w. KnownNat w => ALUPrim -> Unsigned w -> Unsigned w -> SimM alu wordW addrW codeWordW codeAddrW (Unsigned w)
    aluOp p (Unsigned a) (Unsigned b) = do
        let w    = fromIntegral (natVal (Proxy @w)) :: Int
            mask = (1 `shiftL` w) - 1
            raw  = case p of
                PAdd         -> a + b
                PSub         -> a - b
                PAnd         -> a .&. b
                POr          -> a .|. b
                PXor         -> xor a b
                PNot         -> complement a      -- mask applied below
                PShiftL      -> a `shiftL` fromIntegral b
                PShiftR      -> a `shiftR` fromIntegral b
                PArithShiftR -> arithShiftR w a (fromIntegral b)
                PMul         -> a * b
                PMulSigned   -> signedMul w a b
        return (fromInteger (raw .&. mask))


-- ---------------------------------------------------------------------------
-- MonadHarvardALU instance
-- ---------------------------------------------------------------------------

instance MonadHarvardALU (SimM alu wordW addrW codeWordW codeAddrW) where
    type CodeAddr (SimM alu wordW addrW codeWordW codeAddrW) = Unsigned codeAddrW
    type CodeWord (SimM alu wordW addrW codeWordW codeAddrW) = Unsigned codeWordW

    readCode (Unsigned addr) = SimM $ do
        st <- get
        return (Unsigned (IntMap.findWithDefault 0 (fromIntegral addr) (ssCodeMem st)))

-- ---------------------------------------------------------------------------
-- Runners
-- ---------------------------------------------------------------------------

-- | Run one instruction body against an ALU record and raw instruction word,
--   returning the updated simulation state.
runInstr :: alu                    -- ^ ALU record (handles for register names)
         -> Integer                -- ^ raw instruction word to decode
         -> SimM alu wordW addrW codeWordW codeAddrW ()
         -> SimState
         -> SimState
runInstr aluRec instrWord (SimM m) st =
    execState (runReaderT m (SimCtx aluRec instrWord)) st

-- | 'runInstr' starting from 'emptySim' — useful for unit-testing single
--   instruction bodies in isolation.
execInstr :: alu
          -> Integer
          -> SimM alu wordW addrW codeWordW codeAddrW ()
          -> SimState
execInstr aluRec instrWord m = runInstr aluRec instrWord m emptySim

-- ---------------------------------------------------------------------------
-- Internal ALU helpers
-- ---------------------------------------------------------------------------

-- | Arithmetic (sign-preserving) right shift of an @w@-bit value.
arithShiftR :: Int -> Integer -> Int -> Integer
arithShiftR w a n =
    let sign = a `shiftR` (w - 1)   -- 0 or 1
        mask = (1 `shiftL` w) - 1
        extended = if sign == 1 then a - (1 `shiftL` w) else a
    in (extended `shiftR` n) .&. mask

-- | Signed multiply: interpret both operands as @w@-bit two's complement.
signedMul :: Int -> Integer -> Integer -> Integer
signedMul w a b =
    let half  = 1 `shiftL` (w - 1)
        toSigned x = if x >= half then x - (1 `shiftL` w) else x
    in toSigned a * toSigned b
