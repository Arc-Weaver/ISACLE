-- NB: NoImplicitPrelude is active from cabal common-options.
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Isacle.System.Periph
    ( -- * Signal operations record
      PeriphOps(..)
    , nullOps
      -- * Bus interface adapter
    , BusIface(..)
    , nullBusIface
      -- * Peripheral definition monad
    , PeriphDef
    , runPeriphDef
      -- * Signal-level register operations
    , onWrite
    , onWriteStrobe
    , onRead
      -- * Register / field declarations (metadata)
    , field
    , field8
    , register
      -- * Spec types
    , PeriphSpec(..)
    , FieldSpec(..)
    , BitField(..)
    , RegWidth(..)
    , RegAccess(..)
    , specSize
      -- * BitField helpers
    , bitF
    , bitsF
      -- * Memory peripheral phantom types
    , RAM
    , ROM
      -- * Block memory peripheral defs (isacle-hdl backend)
    , blockRamDef
    , blockRomDef
    ) where

import Prelude
import Data.Kind (Type)
import Data.Word (Word8, Word32)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader

import Isacle.Hdl.Prim (Unsigned)
import Isacle.System.Spec (NullSig(..))

-- ---------------------------------------------------------------------------
-- Register access and width
-- ---------------------------------------------------------------------------

data RegAccess = ReadOnly | ReadWrite | WriteOnly deriving (Show, Eq)

data RegWidth = RW8 | RW16 | RW32 deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Field specification (two-level: register → bit-fields)
-- ---------------------------------------------------------------------------

-- | A named bit range inside a register.
data BitField = BitField
    { bfLoBit  :: Word8
    , bfHiBit  :: Word8
    , bfAccess :: RegAccess
    , bfName   :: String
    , bfDesc   :: String
    } deriving (Show)

-- | A memory-mapped register at a byte offset.
data FieldSpec = FieldSpec
    { fieldOffset    :: Word8
    , fieldWidth     :: RegWidth
    , fieldAccess    :: RegAccess
    , fieldName      :: String
    , fieldDesc      :: String
    , fieldBitFields :: [BitField]
    } deriving (Show)

-- | Structural description of a peripheral (analysis path only).
newtype PeriphSpec = PeriphSpec
    { psFields :: [FieldSpec]
    } deriving (Show)

-- | Address window size in bytes, derived from the highest declared field.
specSize :: PeriphSpec -> Word32
specSize (PeriphSpec []) = 0
specSize (PeriphSpec fs) = maximum
    [ fromIntegral (fieldOffset f) + widthBytes (fieldWidth f) | f <- fs ]
  where
    widthBytes RW8  = 1
    widthBytes RW16 = 2
    widthBytes RW32 = 4

-- ---------------------------------------------------------------------------
-- PeriphOps: injected register-creation operations
-- ---------------------------------------------------------------------------

-- | Operations record injected into the 'PeriphDef' monad.
--
-- Two concrete values exist:
--
--   * 'nullOps' — spec / documentation interpreter; all operations are no-ops.
--   * @'hdlOps'@ (in "Isacle.System.HdlCircuit") — isacle-hdl backend.
data PeriphOps (sig :: Type -> Type) dat = PeriphOps
    { -- | Create a clocked register.
      -- @sigReg initVal writeEnable writeData@ → current register value.
      sigReg :: dat -> sig Bool -> sig dat -> sig dat
      -- | Create a block RAM/ROM.
      -- @sigBlockMem size initVals writeEnable writeAddr writeData readAddr@
    , sigBlockMem :: Int -> [Integer]
                  -> sig Bool -> sig (Unsigned 32) -> sig dat -> sig (Unsigned 32)
                  -> sig dat
      -- | Address less-than: @sigAddrLt addr limit@ is True when @addr < limit@.
    , sigAddrLt :: sig (Unsigned 32) -> Word32 -> sig Bool
      -- | Zero signal (initial read-data accumulator).
    , sigZero :: sig dat
      -- | Combinational AND of two Bool signals.
    , sigAnd  :: sig Bool -> sig Bool -> sig Bool
      -- | Combinational mux: @sigMux sel thenSig elseSig@.
    , sigMux  :: sig Bool -> sig dat -> sig dat -> sig dat
    }

-- | Spec / documentation ops: all hardware operations are no-ops.
nullOps :: PeriphOps NullSig dat
nullOps = PeriphOps
    { sigReg      = \_ _ _ -> NullSig
    , sigBlockMem = \_ _ _ _ _ _ -> NullSig
    , sigAddrLt   = \_ _ -> NullSig
    , sigZero     = NullSig
    , sigAnd      = \_ _ -> NullSig
    , sigMux      = \_ _ _ -> NullSig
    }

-- ---------------------------------------------------------------------------
-- BusIface: decomposed bus adapter (backend-agnostic)
-- ---------------------------------------------------------------------------

-- | Decomposed bus interface passed to 'runPeriphDef'.
--
-- Each backend constructs one of these from its native bus representation:
--
--   * isacle-hdl: 'hdlBusIface' in "Isacle.System.HdlCircuit"
--   * spec / null: 'nullBusIface'
data BusIface sig dat = BusIface
    { biWrData    :: sig dat
      -- ^ Write data (valid only when 'biWrEqAddr' returns True for some offset).
    , biWrEqAddr  :: Word32 -> sig Bool
      -- ^ @biWrEqAddr off@ — True iff a write is targeting byte offset @off@.
    , biRdEqAddr  :: Word32 -> sig Bool
      -- ^ @biRdEqAddr off@ — True iff a read is targeting byte offset @off@.
    , biWrEn      :: sig Bool
      -- ^ Global write enable (not address-decoded; used by block-memory peripherals).
    , biRelWrAddr :: sig (Unsigned 32)
      -- ^ Write address relative to peripheral base (used by block-memory peripherals).
    , biRelRdAddr :: sig (Unsigned 32)
      -- ^ Read address relative to peripheral base (used by block-memory peripherals).
    }

-- | 'BusIface' for the spec / documentation path; all signals are 'NullSig'.
nullBusIface :: BusIface NullSig dat
nullBusIface = BusIface
    { biWrData    = NullSig
    , biWrEqAddr  = \_ -> NullSig
    , biRdEqAddr  = \_ -> NullSig
    , biWrEn      = NullSig
    , biRelWrAddr = NullSig
    , biRelRdAddr = NullSig
    }

-- ---------------------------------------------------------------------------
-- PeriphDef: peripheral definition monad
-- ---------------------------------------------------------------------------

data PeriphEnv sig dat = PeriphEnv
    { peOps :: PeriphOps sig dat
    , peBus :: BusIface sig dat
    }

data PeriphAccum sig dat = PeriphAccum
    { paFields :: [FieldSpec]
    , paRdData :: sig dat
    }

-- | Peripheral description monad.
--
-- @p@   — phantom peripheral kind tag (e.g. @GPIO@, @UART@).
-- @sig@ — signal family; 'NullSig' for spec, @Sig dom@ for isacle-hdl.
-- @dat@ — bus data type.
-- @a@   — return type (the peripheral's physical output signals).
newtype PeriphDef (p :: Type) (sig :: Type -> Type) dat a = PeriphDef
    { unPeriphDef :: ReaderT (PeriphEnv sig dat) (State (PeriphAccum sig dat)) a }
    deriving newtype (Functor, Applicative, Monad)

-- | Run a peripheral definition.
runPeriphDef
    :: PeriphOps sig dat
    -> BusIface sig dat
    -> PeriphDef p sig dat a
    -> (a, sig dat, PeriphSpec)
runPeriphDef ops bus def =
    let env      = PeriphEnv { peOps = ops, peBus = bus }
        initAcc  = PeriphAccum { paFields = [], paRdData = sigZero ops }
        (a, acc) = runState (runReaderT (unPeriphDef def) env) initAcc
    in (a, paRdData acc, PeriphSpec (paFields acc))

-- ---------------------------------------------------------------------------
-- Signal-level circuit operations
-- ---------------------------------------------------------------------------

-- | Declare a registered output at @offset@.
onWrite
    :: Word32
    -> dat
    -> PeriphDef p sig dat (sig dat)
onWrite off initVal = PeriphDef $ do
    PeriphEnv { peOps = ops, peBus = bus } <- ask
    let wen  = biWrEqAddr bus off
        wdat = biWrData bus
        reg  = sigReg ops initVal wen wdat
    pure (sigMux ops wen wdat reg)

-- | Like 'onWrite' but also returns a write-strobe.
onWriteStrobe
    :: Word32
    -> dat
    -> PeriphDef p sig dat (sig dat, sig Bool)
onWriteStrobe off initVal = PeriphDef $ do
    PeriphEnv { peOps = ops, peBus = bus } <- ask
    let wen  = biWrEqAddr bus off
        wdat = biWrData bus
        reg  = sigReg ops initVal wen wdat
    pure (sigMux ops wen wdat reg, wen)

-- | Wire @sig@ into the read-data mux at @offset@.
onRead
    :: Word32
    -> sig dat
    -> PeriphDef p sig dat ()
onRead off sig = PeriphDef $ do
    PeriphEnv { peOps = ops, peBus = bus } <- ask
    lift $ modify $ \acc ->
        let prev = paRdData acc
        in acc { paRdData = sigMux ops (biRdEqAddr bus off) sig prev }

-- ---------------------------------------------------------------------------
-- Structural metadata declarations
-- ---------------------------------------------------------------------------

field :: RegWidth -> RegAccess -> Word8 -> String -> String
      -> PeriphDef p sig dat ()
field width acc off name desc = PeriphDef $ lift $ modify $ \a ->
    a { paFields = paFields a ++ [FieldSpec off width acc name desc []] }

field8 :: RegAccess -> Word8 -> String -> String -> PeriphDef p sig dat ()
field8 = field RW8

register :: RegWidth -> Word8 -> String -> String -> [BitField]
         -> PeriphDef p sig dat ()
register width off name desc bfs = PeriphDef $ lift $ modify $ \a ->
    a { paFields = paFields a ++ [FieldSpec off width ReadWrite name desc bfs] }

bitF :: RegAccess -> Word8 -> String -> String -> BitField
bitF acc b = BitField b b acc

bitsF :: RegAccess -> Word8 -> Word8 -> String -> String -> BitField
bitsF acc lo hi = BitField lo hi acc

-- ---------------------------------------------------------------------------
-- Memory peripheral phantom types and block memory defs
-- ---------------------------------------------------------------------------

-- | Phantom type tag for a synchronous block RAM peripheral.
data RAM

-- | Phantom type tag for a read-only ROM peripheral.
data ROM

-- | Block RAM peripheral for the isacle-hdl path.
-- Occupies @size@ entries of the bus address space starting at the peripheral
-- base.  Write is synchronous; read is asynchronous (combinational).
blockRamDef :: Int -> [Integer] -> PeriphDef p sig dat ()
blockRamDef size initVals = PeriphDef $ do
    PeriphEnv { peOps = ops, peBus = bus } <- ask
    let relRd  = biRelRdAddr bus
        relWr  = biRelWrAddr bus
        rdSel  = sigAddrLt ops relRd (fromIntegral size)
        wrEn'  = sigAnd ops (biWrEn bus) (sigAddrLt ops relWr (fromIntegral size))
        rdData = sigBlockMem ops size initVals wrEn' relWr (biWrData bus) relRd
    lift $ modify $ \acc ->
        acc { paRdData = sigMux ops rdSel rdData (paRdData acc) }

-- | ROM peripheral for the isacle-hdl path.
-- Read is purely combinational; ignores all writes.
blockRomDef :: Int -> [Integer] -> PeriphDef p sig dat ()
blockRomDef size initVals = PeriphDef $ do
    PeriphEnv { peOps = ops, peBus = bus } <- ask
    let relRd  = biRelRdAddr bus
        rdSel  = sigAddrLt ops relRd (fromIntegral size)
        noWr   = sigAddrLt ops relRd 0   -- unsigned < 0 is always False
        rdData = sigBlockMem ops size initVals noWr relRd (sigZero ops) relRd
    lift $ modify $ \acc ->
        acc { paRdData = sigMux ops rdSel rdData (paRdData acc) }
