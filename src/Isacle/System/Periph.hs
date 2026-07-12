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
    , liftHdl
    , runPeriphDef
      -- * Signal-level register operations
    , onWrite
    , onWriteStrobe
    , onRead
      -- * Typed field + logic in one (PE2)
    , regField
    , roField
      -- * Register handles (PE3): declare + writeAction + readAction
    , Reg(..)
    , declareReg
    , declareRegUnsigned
    , declareRegSigned
    , writeAction
    , readAction
      -- * Register / field declarations (metadata)
    , field
    , field8
    , fieldOf
    , fieldRec
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
    , romCombDef
    ) where

import Prelude
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Word (Word8, Word32)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader
import GHC.TypeLits (natVal)

import Hdl.Net (Repr(..))
import Hdl.Prim (Unsigned)
import Hdl.Sig (HdlType, hdlRepr, Width, GFields)
import Isacle.Layout (bitLayout, layoutSize, layoutPlacements, plPos, plHi, plName)
import GHC.Generics (Generic, Rep)
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
    , fieldRepr      :: Repr        -- ^ how software should interpret the bits
                                    --   (signed/unsigned/…); drives C-header types.
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
-- | The __stateful__ operations are monadic (they build clocked hardware in the
-- backend monad @m@); the __combinational__ operations stay pure.  The isacle-hdl
-- backend ('hdlOps') realises the stateful ops with the real 'Hdl' 'register' /
-- 'ram' / 'rom' — no 'SExpr'.  The spec pass ('nullOps') realises them as pure
-- no-ops in any monad, so it emits no hardware.
data PeriphOps (sig :: Type -> Type) (m :: Type -> Type) dat = PeriphOps
    { -- | Create a clocked register (monadic).
      -- @sigReg initVal writeEnable writeData@ → current register value.
      sigReg :: dat -> sig Bool -> sig dat -> m (sig dat)
      -- | Create a block RAM/ROM (monadic).
      -- @sigBlockMem size initVals writeEnable writeAddr writeData readAddr@
    , sigBlockMem :: Int -> [Integer]
                  -> sig Bool -> sig (Unsigned 32) -> sig dat -> sig (Unsigned 32)
                  -> m (sig dat)
      -- | Create a read-only ROM with a __combinational__ lookup (monadic).
    , sigRom :: Int -> [Integer] -> sig (Unsigned 32) -> m (sig dat)
      -- | Address less-than: @sigAddrLt addr limit@ is True when @addr < limit@.
    , sigAddrLt :: sig (Unsigned 32) -> Word32 -> sig Bool
      -- | Zero signal (initial read-data accumulator).
    , sigZero :: sig dat
      -- | Combinational AND of two Bool signals.
    , sigAnd  :: sig Bool -> sig Bool -> sig Bool
      -- | Combinational mux: @sigMux sel thenSig elseSig@.
    , sigMux  :: sig Bool -> sig dat -> sig dat -> sig dat
      -- | Attach a human-readable name hint to a signal (monadic).
    , sigHint :: String -> sig dat -> m (sig dat)
    }

-- | Spec / documentation ops: every operation is a pure no-op in any monad, so
-- running a peripheral with these emits no hardware — only field metadata is
-- collected (in the 'PeriphDef' state).
nullOps :: Applicative m => PeriphOps NullSig m dat
nullOps = PeriphOps
    { sigReg      = \_ _ _ -> pure NullSig
    , sigBlockMem = \_ _ _ _ _ _ -> pure NullSig
    , sigRom      = \_ _ _ -> pure NullSig
    , sigAddrLt   = \_ _ -> NullSig
    , sigZero     = NullSig
    , sigAnd      = \_ _ -> NullSig
    , sigMux      = \_ _ _ -> NullSig
    , sigHint     = \_ s  -> pure s
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

data PeriphEnv sig m dat = PeriphEnv
    { peOps :: PeriphOps sig m dat
    , peBus :: BusIface sig dat
    }

data PeriphAccum sig dat = PeriphAccum
    { paFields :: [FieldSpec]
    , paRdData :: sig dat
    }

-- | Peripheral description monad — a transformer over the backend monad @m@.
--
-- @p@   — phantom peripheral kind tag (e.g. @GPIO@, @UART@).
-- @sig@ — signal family; 'NullSig' for spec, @Sig dom@ for isacle-hdl.
-- @m@   — backend monad ('NetM' for isacle-hdl; any 'Applicative' for the spec
--         pass).  Lift its 'Hdl' operations in with 'liftHdl'.
-- @dat@ — bus data type.
-- @a@   — return type (the peripheral's physical output signals).
newtype PeriphDef (p :: Type) (sig :: Type -> Type) (m :: Type -> Type) dat a = PeriphDef
    { unPeriphDef :: ReaderT (PeriphEnv sig m dat) (StateT (PeriphAccum sig dat) m) a }
    deriving newtype (Functor, Applicative, Monad, MonadFix)

-- | Lift a backend ('Hdl') computation into a peripheral definition — the seam
-- where a peripheral runs @register@/@ram@/@rom@/… of the abstract HDL monad.
liftHdl :: Monad m => m a -> PeriphDef p sig m dat a
liftHdl = PeriphDef . lift . lift

-- | Run a peripheral definition in the backend monad @m@.
runPeriphDef
    :: Monad m
    => PeriphOps sig m dat
    -> BusIface sig dat
    -> PeriphDef p sig m dat a
    -> m (a, sig dat, PeriphSpec)
runPeriphDef ops bus def = do
    let env     = PeriphEnv { peOps = ops, peBus = bus }
        initAcc = PeriphAccum { paFields = [], paRdData = sigZero ops }
    (a, acc) <- runStateT (runReaderT (unPeriphDef def) env) initAcc
    pure (a, paRdData acc, PeriphSpec (paFields acc))

-- ---------------------------------------------------------------------------
-- Signal-level circuit operations
-- ---------------------------------------------------------------------------

-- | Declare a named registered output at @offset@.
-- The name is attached to the register flip-flop output in the HDL backend.
onWrite
    :: Monad m => String   -- ^ register name (used as a VHDL signal hint)
    -> Word32
    -> dat
    -> PeriphDef p sig m dat (sig dat)
onWrite name off initVal = PeriphDef $ do
    PeriphEnv { peOps = ops, peBus = bus } <- ask
    let wen  = biWrEqAddr bus off
        wdat = biWrData bus
    reg0 <- lift (lift (sigReg ops initVal wen wdat))
    reg  <- lift (lift (sigHint ops name reg0))
    pure (sigMux ops wen wdat reg)

-- | Like 'onWrite' but also returns a write-strobe.
onWriteStrobe
    :: Monad m => String   -- ^ register name (used as a VHDL signal hint)
    -> Word32
    -> dat
    -> PeriphDef p sig m dat (sig dat, sig Bool)
onWriteStrobe name off initVal = PeriphDef $ do
    PeriphEnv { peOps = ops, peBus = bus } <- ask
    let wen  = biWrEqAddr bus off
        wdat = biWrData bus
    reg0 <- lift (lift (sigReg ops initVal wen wdat))
    reg  <- lift (lift (sigHint ops name reg0))
    pure (sigMux ops wen wdat reg, wen)

-- | Wire @sig@ into the read-data mux at @offset@.
onRead
    :: Monad m => Word32
    -> sig dat
    -> PeriphDef p sig m dat ()
onRead off sig = PeriphDef $ do
    PeriphEnv { peOps = ops, peBus = bus } <- ask
    lift $ modify $ \acc ->
        let prev = paRdData acc
        in acc { paRdData = sigMux ops (biRdEqAddr bus off) sig prev }

-- | PE2: declare a /typed/ read-write register AND wire its write register and
-- read-back mux in one call. The field's name, byte offset, width and
-- representation are single-sourced — the metadata ('fieldOf') and the logic
-- ('onWrite'/'onRead') agree by construction, instead of repeating the offset
-- across a separate metadata/logic pair. Returns the register's value signal.
--
-- > setpoint <- regField @(Signed 8) 0 "SETPOINT" "target value" 0
regField :: forall a p sig m dat. (HdlType a, Monad m)
         => Word8 -> String -> String -> dat
         -> PeriphDef p sig m dat (sig dat)
regField off name desc initVal = do
    fieldOf @a ReadWrite off name desc
    val <- onWrite name (fromIntegral off) initVal
    onRead (fromIntegral off) val
    pure val

-- | PE2: declare a /typed/ read-only register AND wire its read mux from a
-- peripheral-supplied signal, single-sourcing the field metadata and the read
-- logic. For status/input registers the hardware drives.
roField :: forall a p sig m dat. (HdlType a, Monad m)
        => Word8 -> String -> String -> sig dat
        -> PeriphDef p sig m dat ()
roField off name desc sig = do
    fieldOf @a ReadOnly off name desc
    onRead (fromIntegral off) sig

-- ---------------------------------------------------------------------------
-- PE3: register handles — declare / writeAction / readAction as separate,
--      composable actions so arbitrary 'liftHdl' logic can sit between the
--      value the CPU wrote and the value it reads back.
-- ---------------------------------------------------------------------------

-- | A declared register: its byte offset, width, representation and name.
-- Produced by 'declareRegUnsigned' / 'declareRegSigned'; wired by 'writeAction'
-- (write side) and 'readAction' (read side).  Splitting a register into a handle
-- plus separate write/read actions lets any HDL processing sit between them:
--
-- > reg     <- declareRegUnsigned 8 "CTRL"
-- > written <- writeAction reg            -- what the CPU wrote (a clocked register)
-- > back    <- liftHdl (process written)  -- arbitrary HDL processing
-- > readAction reg back                   -- what the CPU reads back at CTRL
data Reg dat = Reg
    { regOffset :: Word32   -- ^ byte offset within the peripheral window
    , regWidth  :: RegWidth
    , regRepr   :: Repr
    , regName   :: String
    } deriving (Show)

-- | Declare a register at the next free byte offset, recording its metadata and
-- returning a handle.  The register defaults to read-write; wiring 'writeAction'
-- and/or 'readAction' realises its hardware.
declareReg :: Monad m => RegWidth -> Repr -> String -> PeriphDef p sig m dat (Reg dat)
declareReg width repr name = PeriphDef $ do
    off <- lift $ gets (nextOffset . paFields)
    lift $ modify $ \a ->
        a { paFields = paFields a ++ [FieldSpec off width ReadWrite name "" repr []] }
    pure (Reg (fromIntegral off) width repr name)
  where
    nextOffset = foldl (\o f -> o + widthBytes (fieldWidth f)) 0
    widthBytes RW8 = 1
    widthBytes RW16 = 2
    widthBytes RW32 = 4

-- | Declare an unsigned register of the given bit-width (8/16/32) at the next
-- free offset.
declareRegUnsigned :: Monad m => Int -> String -> PeriphDef p sig m dat (Reg dat)
declareRegUnsigned bits = declareReg (rwOfBits bits) RUnsigned

-- | Declare a signed register of the given bit-width (8/16/32) at the next free
-- offset — the datapath is interpreted signed (C-header @int8_t@ etc.).
declareRegSigned :: Monad m => Int -> String -> PeriphDef p sig m dat (Reg dat)
declareRegSigned bits = declareReg (rwOfBits bits) RSigned

rwOfBits :: Int -> RegWidth
rwOfBits 8  = RW8
rwOfBits 16 = RW16
rwOfBits 32 = RW32
rwOfBits n  = error ("declareReg: unsupported width " ++ show n
                     ++ " bits (expected 8, 16, or 32)")

-- | The write side of a register: a clocked register that captures bus writes to
-- this register's offset (initialised to 0), returning its current value.  Feed
-- the result on to 'liftHdl' logic and/or straight to 'readAction'.  The
-- flip-flop carries the register's name with a @_q@ suffix (its stored Q output),
-- leaving the bare name for the bus-visible read value wired by 'readAction'.
writeAction :: (Num dat, Monad m) => Reg dat -> PeriphDef p sig m dat (sig dat)
writeAction reg = onWrite (regName reg ++ "_q") (regOffset reg) 0

-- | The read side of a register: name @sig@ with the register's name and wire it
-- into the read-data mux at this register's offset — the value the CPU reads back
-- carries the register's name in the emitted HDL.
readAction :: Monad m => Reg dat -> sig dat -> PeriphDef p sig m dat ()
readAction reg sig = PeriphDef $ do
    PeriphEnv { peOps = ops, peBus = bus } <- ask
    named <- lift (lift (sigHint ops (regName reg) sig))
    lift $ modify $ \acc ->
        acc { paRdData = sigMux ops (biRdEqAddr bus (regOffset reg)) named (paRdData acc) }

-- ---------------------------------------------------------------------------
-- Structural metadata declarations
-- ---------------------------------------------------------------------------

-- | Core field declaration: explicit width + representation.
fieldFull :: Monad m => RegWidth -> Repr -> RegAccess -> Word8 -> String -> String
          -> PeriphDef p sig m dat ()
fieldFull width repr acc off name desc = PeriphDef $ lift $ modify $ \a ->
    a { paFields = paFields a ++ [FieldSpec off width acc name desc repr []] }

-- | Untyped register declaration: the bits are treated as unsigned.
field :: Monad m => RegWidth -> RegAccess -> Word8 -> String -> String
      -> PeriphDef p sig m dat ()
field width = fieldFull width RUnsigned

field8 :: Monad m => RegAccess -> Word8 -> String -> String -> PeriphDef p sig m dat ()
field8 = field RW8

-- | Typed register declaration: the register's software type is given by an
-- 'HdlType', so its /width/ ('Width') and /representation/ ('hdlRepr',
-- e.g. @Signed 8@ → signed) are recorded in the metadata.  This is what lets a
-- documentation / C-header generator emit the correct C type (@int8_t@ vs
-- @uint8_t@ …) — the bus is just wires, the interpretation lives here.
--
-- > fieldOf @(Signed 8) ReadWrite 0 "SETPOINT" "Target value"
fieldOf :: forall a p sig m dat. (HdlType a, Monad m)
        => RegAccess -> Word8 -> String -> String -> PeriphDef p sig m dat ()
fieldOf = fieldFull (regWidthBits (fromIntegral (natVal (Proxy @(Width a)))))
                    (hdlRepr (Proxy @a))
  where
    regWidthBits :: Int -> RegWidth
    regWidthBits 8  = RW8
    regWidthBits 16 = RW16
    regWidthBits 32 = RW32
    regWidthBits n  = error ("fieldOf: unsupported register width "
                             ++ show n ++ " bits (expected 8, 16, or 32)")

-- | Typed register declaration whose **bit-fields are derived from a record
-- 'HdlType'** — so a CPU flag register and a peripheral control register share
-- the same mechanism (define the record once; the bit-field layout, width, and
-- representation all come from it).  Bits are MSB-first in field order (matching
-- the 'Hdl.Types.genericToBits' packing).
--
-- > data Ctrl = Ctrl { enable :: Bit, mode :: Unsigned 2, irq :: Bit }
-- >   deriving (Generic, HdlType)
-- > fieldRec @Ctrl ReadWrite 0 "CTRL" "control register"
fieldRec :: forall a p sig m dat. (HdlType a, Generic a, GFields (Rep a), Monad m)
         => RegAccess -> Word8 -> String -> String -> PeriphDef p sig m dat ()
fieldRec acc off name desc = PeriphDef $ lift $ modify $ \st ->
    st { paFields = paFields st
                 ++ [FieldSpec off width acc name desc (hdlRepr (Proxy @a)) bfs] }
  where
    -- The bit-field layout is the record's bit-position layout (C5/C2),
    -- single-sourced through the shared address-mapping helper.
    layout = bitLayout (Proxy @a)
    width  = case layoutSize layout of
        8  -> RW8; 16 -> RW16; 32 -> RW32
        n  -> error ("fieldRec: unsupported register width " ++ show n)
    bfs = [ BitField (fromIntegral (plPos p)) (fromIntegral (plHi p)) acc (plName p) ""
          | p <- layoutPlacements layout ]

register :: Monad m => RegWidth -> Word8 -> String -> String -> [BitField]
         -> PeriphDef p sig m dat ()
register width off name desc bfs = PeriphDef $ lift $ modify $ \a ->
    a { paFields = paFields a ++ [FieldSpec off width ReadWrite name desc RUnsigned bfs] }

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
blockRamDef :: Monad m => Int -> [Integer] -> PeriphDef p sig m dat ()
blockRamDef size initVals = PeriphDef $ do
    PeriphEnv { peOps = ops, peBus = bus } <- ask
    let relRd  = biRelRdAddr bus
        relWr  = biRelWrAddr bus
        rdSel  = sigAddrLt ops relRd (fromIntegral size)
        wrEn'  = sigAnd ops (biWrEn bus) (sigAddrLt ops relWr (fromIntegral size))
    rdData <- lift (lift (sigBlockMem ops size initVals wrEn' relWr (biWrData bus) relRd))
    lift $ modify $ \acc ->
        acc { paRdData = sigMux ops rdSel rdData (paRdData acc) }

-- | ROM peripheral for the isacle-hdl path (synchronous block memory).
blockRomDef :: Monad m => Int -> [Integer] -> PeriphDef p sig m dat ()
blockRomDef size initVals = PeriphDef $ do
    PeriphEnv { peOps = ops, peBus = bus } <- ask
    let relRd  = biRelRdAddr bus
        rdSel  = sigAddrLt ops relRd (fromIntegral size)
        noWr   = sigAddrLt ops relRd 0   -- unsigned < 0 is always False
    rdData <- lift (lift (sigBlockMem ops size initVals noWr relRd (sigZero ops) relRd))
    lift $ modify $ \acc ->
        acc { paRdData = sigMux ops rdSel rdData (paRdData acc) }

-- | Combinational ROM peripheral: @readAddr@ → @readData@ the /same/ cycle (via
-- 'sigRom' → a combinational @NRom@).  This is what an instruction/code ROM must
-- be — the CPU fetches combinationally (the opcode must be stable the cycle the
-- address is driven), unlike a data-bus block ROM which may be synchronous.
romCombDef :: Monad m => Int -> [Integer] -> PeriphDef p sig m dat ()
romCombDef size initVals = PeriphDef $ do
    PeriphEnv { peOps = ops, peBus = bus } <- ask
    let relRd  = biRelRdAddr bus
        rdSel  = sigAddrLt ops relRd (fromIntegral size)
    rdData <- lift (lift (sigRom ops size initVals relRd))
    lift $ modify $ \acc ->
        acc { paRdData = sigMux ops rdSel rdData (paRdData acc) }
