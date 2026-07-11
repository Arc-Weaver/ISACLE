-- | System-level DSL for the isacle-hdl backend.
--
-- Provides an API close to the ExampleAPI target:
--
-- @
-- mySystem (uart0Rx, gpio0In) = runSystemDSL $ do
--     uart0 <- createUart "uart0" uart0Rx
--     gpio0 <- createGpio "gpio0" gpio0In
--
--     bh <- createHarvardCPU \@16 \@16 \@16 "cpu" myCPUDef myISA romWords
--     (uart0Tx, gpio0Port, gpio0Ddr) <- createBus "databus" bh $ do
--         uart0' <- attachPeripheral 0x100 uart0
--         gpio0' <- attachPeripheral 0x300 gpio0
--         return (uartTxLine uart0', gpioPort gpio0', gpioDdr gpio0')
--
--     return (uart0Tx, gpio0Port, gpio0Ddr)
-- @
--
-- 'attachPeripheral' is generic: the output type is determined by the token
-- type, so GHC infers the right thing without any cast.
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecursiveDo                #-}
module Isacle.System.SystemDSL
    ( -- * System-building typeclass + its netlist instance
      SysDSL
    , SysNet
    , runSystemDSL
      -- * Peripheral tokens (opaque; carry name + PeriphDef)
    , PeriphToken
      -- * Bus sub-monad
    , BusDSL
      -- (the circuit-level peripheral handle 'Bus' and 'BusMaster' live in
      --  "Isacle.System.Bus" — import it directly; not re-exported here to avoid
      --  colliding with the spec-level 'Isacle.System.BusDef.Bus'.)
      -- * Bus protocols (signalling)
    , SimpleBus(..)    -- re-exported from Isacle.System.BusArch
    , attachPeripheral
      -- * System-level operations
    , createBus
    , createHarvardCPU
    , createL1Cache
    , createCachedCPU
      -- * Interrupts
    , IrqDriver(..)
    , createIrq
    , noIrq
      -- * Design runner
    , execSystemDSL
    , runSystemDesign
    , runSystemDesignWith
      -- * Deferred file loading (resolved by the IO interpreter)
    , loadFile
    , loadFileBytes
    , FileEnv
      -- * Peripheral constructors
    , createUart
    , createGpio
    , createTimer
    , createRamp
    , createRam
    , RomImage(..)
    , createRom
      -- * Utilities
    , sigFalse
    , sigTrue
    , sysInput
    , sysOutput
      -- * System documentation
    , SysDoc(..)
    , BusSection(..)
    , PeriphEntry(..)
    ) where

import Prelude
import Data.Kind (Type)
import Data.Word (Word32, Word8)
import Control.Monad (forM)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class (lift)
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Hdl.Monad as Hdl
import Data.Proxy (Proxy(..))
import GHC.Generics (Generic, Rep)
import GHC.TypeLits (natVal, KnownNat, type (<=))

import Hdl.Net
import Hdl.Sig
import Hdl.Prim (Unsigned)
import Hdl.Entity (EntityDef)
import Hdl.IO (entity, instanceOf)
import Hdl.Class (inputS, outputS, connectSig, freshSig)
import Isacle.System.Periph
import Isacle.System.Bus (Bus(..), BusMaster(..))
import Isacle.System.BusArch
    (BusArch(..), SimpleBus(..), MasterReq(..), SlaveResp(..), BusChild)
import Isacle.System.HdlCircuit
    ( hdlOps, busPortIface, HdlPhys(..)
    , GpioPhys(..), UartPhys(..), TimerPhys(..)
    )
import Isacle.Periph.Interrupt (interruptArbiter)
import Isacle.Periph.GPIO  (gpioDef, GPIO)
import Isacle.Periph.UART  (uartDefWithFSM, UART)
import Isacle.Periph.Timer (timerDefWithFSM, Timer)
import Isacle.Periph.Ramp  (rampDefWithFSM, Ramp)
import Isacle.ISA.Chip   (Chip(..))
import Isacle.ISA.Backend.SynthCPU (synthHarvardCPU', CpuMemIface(..))
import Isacle.ISA.Backend.SynthVnCPU (synthVonNeumannCPU', VnMemIface(..))
import Isacle.Cache.Config (CacheConfig)
import Isacle.Cache.L1 (CacheHandle(..), synthL1Cache)

-- ---------------------------------------------------------------------------
-- System documentation types
-- ---------------------------------------------------------------------------

-- | One peripheral in a bus section.
data PeriphEntry = PeriphEntry
    { peName :: String
    , peBase :: Word32
    , peSpec :: PeriphSpec
    } deriving (Show)

-- | A named memory bus and its attached peripherals.
data BusSection = BusSection
    { bsName    :: String
    , bsEntries :: [PeriphEntry]
    } deriving (Show)

-- | Top-level system documentation produced by 'runSystemDSL'.
data SysDoc = SysDoc
    { sdBuses :: [BusSection]
    , sdCPUs  :: [String]
    } deriving (Show)

emptySysDoc :: SysDoc
emptySysDoc = SysDoc [] []

-- | Resolved file contents, keyed by path.  Populated by the IO interpreter
-- ('Isacle.System.Emit.writeSystem') before the real elaboration pass; empty
-- during pure runs, so 'loadFile' yields @""@ then.
type FileEnv = Map.Map FilePath BS.ByteString

-- | Internal build state threaded through 'SysNet': the accumulated 'SysDoc',
-- the resolved file environment (read-only, seeded by the runner), and the list
-- of paths requested via 'loadFile' (accumulated so the IO interpreter knows
-- what to read in its harvest pass).
data SysBuild = SysBuild
    { sbDoc  :: SysDoc
    , sbEnv  :: FileEnv
    , sbReqs :: [FilePath]
    }

emptyBuild :: SysBuild
emptyBuild = SysBuild emptySysDoc Map.empty []

emptyBuildWith :: FileEnv -> SysBuild
emptyBuildWith env = emptyBuild { sbEnv = env }

-- ---------------------------------------------------------------------------
-- PeriphToken
-- ---------------------------------------------------------------------------

-- | Opaque handle produced by 'createUart' / 'createGpio' / 'createTimer'.
--
-- @a@ is the concrete physical-output type specific to the peripheral:
--
--   * UART  → @(Sig dom Bool, Sig dom Bool, Sig dom Bool)@ (TX, rxIrq, txIrq)
--   * GPIO  → @(Sig dom dat, Sig dom dat)@                 (PORT, DDR)
--   * Timer → @(Sig dom Bool, Sig dom Bool)@               (ovf, cmp)
--
-- 'attachPeripheral' is generic over @a@, so the right output type is
-- inferred from the token without any explicit annotation.
--
-- 'ptAddrSize' is the peripheral's address window in bytes.  For
-- register-mapped peripherals set it to 0 and the window is derived
-- automatically from 'specSize'.  Memory peripherals (RAM, ROM) must
-- provide an explicit size.
-- @proto@ is the bus protocol (signalling) the peripheral is attached over.  It
-- is unified with the bus's protocol at 'attachPeripheral', so a peripheral that
-- /requires/ a capability (e.g. a stalling FIFO carrying @'BusArch' proto,
-- 'Cap' proto ~ ''Stalling'@) can only be attached to a bus whose master speaks
-- it — the signalling is in the type.  Register peripherals are protocol-
-- agnostic (any @proto@), so they attach to any bus.
data PeriphToken (proto :: Type) p dom dat a = PeriphToken
    { ptName     :: String
    , ptDef      :: PeriphDef p (Sig dom) dat a
    , ptAddrSize :: Word32
    }

-- ---------------------------------------------------------------------------
-- SysNet monad
-- ---------------------------------------------------------------------------

-- | The netlist/synthesis __instance__ of the 'SysDSL' class.  Wraps 'NetM'
-- with the accumulated 'SysBuild' (system documentation, resolved file
-- environment, requested file paths).  'NetM' lives only here (and the emitter),
-- never in the 'SysDSL' class surface.
newtype SysNet a = SysNet (StateT SysBuild NetM a)
    deriving newtype (Functor, Applicative, Monad, MonadFix)

-- | The system-building surface, as a __typeclass__.  Its methods are the
-- system operations (create a bus, attach/​instantiate a CPU, a peripheral, …);
-- 'SysNet' is the concrete backend instance that realises them via 'NetM'.
-- User system code is written @'SysDSL' m => … m …@ and interpreted by whichever
-- instance runs it.  Signals stay concrete 'Sig'; only the assembly monad @m@ is
-- abstract, and 'NetM' never appears in this class.
class MonadFix m => SysDSL m where
    createBus        :: (BusArch proto, KnownDom dom, HdlType dat, Num dat, Num (Sig dom dat))
                     => String -> BusDSL proto dom dat a
                     -> m (Bus proto dom (Unsigned 32) dat, a)
    createHarvardCPU :: forall core addrW codeWordW codeAddrW proto cproto (dom :: Type) dat alu.
                        ( KnownDom dom, KnownNat addrW, KnownNat codeWordW, KnownNat codeAddrW
                        , addrW <= 32, codeAddrW <= 32, HdlType dat
                        , HdlType core, Generic core, GFields (Rep core)
                        , BusMaster proto, BusMaster cproto )
                     => String
                     -> Chip core alu (Width dat) addrW codeWordW codeAddrW
                     -> Bus cproto dom (Unsigned 32) (Unsigned codeWordW)
                     -> Bus proto  dom (Unsigned 32) dat
                     -> IrqDriver dom codeAddrW
                     -> m ()
    createIrq        :: forall (dom :: Type) caw. (KnownDom dom, KnownNat caw, Num (Sig dom (Unsigned caw)))
                     => Sig dom Bool -> [(Sig dom Bool, Integer)] -> m (IrqDriver dom caw)
    noIrq            :: forall (dom :: Type) caw. (KnownDom dom, KnownNat caw, Num (Sig dom (Unsigned caw)))
                     => m (IrqDriver dom caw)
    createUart       :: (KnownDom dom, HdlType dat, Num dat, Num (Sig dom dat))
                     => String -> Sig dom Bool -> m (PeriphToken proto UART dom dat (UartPhys dom))
    createGpio       :: (Num dat, Num (Sig dom dat))
                     => String -> Sig dom dat -> m (PeriphToken proto GPIO dom dat (GpioPhys dom dat))
    createTimer      :: (KnownDom dom, HdlType dat, Num dat, Num (Sig dom dat))
                     => String -> Sig dom Bool -> m (PeriphToken proto Timer dom dat (TimerPhys dom))
    createRamp       :: KnownDom dom
                     => String -> Sig dom Bool -> m (PeriphToken proto Ramp dom (Unsigned 8) ())
    createRam        :: (Num dat, Num (Sig dom dat))
                     => Int -> [Integer] -> String -> m (PeriphToken proto RAM dom dat ())
    createRom        :: (Num dat, Num (Sig dom dat))
                     => Int -> RomImage dat -> String -> m (PeriphToken proto ROM dom dat ())
    loadFile         :: FilePath -> m String
    loadFileBytes    :: FilePath -> m [Word8]
    sysInput         :: forall a dom. (KnownDom dom, HdlType a) => String -> m (Sig dom a)
    sysOutput        :: forall a dom. (KnownDom dom, HdlType a) => String -> Sig dom a -> m ()

instance SysDSL SysNet where
    createBus        = createBusImpl
    createHarvardCPU = createHarvardCPUImpl
    createIrq        = createIrqImpl
    noIrq            = noIrqImpl
    createUart       = createUartImpl
    createGpio       = createGpioImpl
    createTimer      = createTimerImpl
    createRamp       = createRampImpl
    createRam        = createRamImpl
    createRom        = createRomImpl
    loadFile         = loadFileImpl
    loadFileBytes    = loadFileBytesImpl
    sysInput         = sysInputImpl
    sysOutput        = sysOutputImpl

-- | Run a system description, returning the user result, the flat 'NetNode'
-- list (for VHDL emission), and the system documentation.  Pure: no file
-- environment, so any 'loadFile' yields @""@.
runSystemDSL :: SysNet a -> (a, [NetNode], SysDoc)
runSystemDSL (SysNet st) = (a, nodes, sbDoc b)
  where
    ((a, b), nodes, _design) = runNetM (runStateT st emptyBuild)

-- ---------------------------------------------------------------------------
-- Deferred file loading
-- ---------------------------------------------------------------------------

-- | Request the contents of a file as text.  The read is __deferred__: a pure
-- run sees @""@, while the IO interpreter ('Isacle.System.Emit.writeSystem' /
-- 'Isacle.System.CLI.systemMain') performs a harvest pass to collect requested
-- paths, reads them, and re-runs with the contents available.
--
-- Because paths are gathered before contents are known, the /set/ of files a
-- system loads must not depend on any file's contents (paths are normally
-- literals — this is not a real restriction in practice).
loadFileImpl :: FilePath -> SysNet String
loadFileImpl path = SysNet $ do
    modify $ \b -> b { sbReqs = path : sbReqs b }
    b <- get
    pure (maybe "" BSC.unpack (Map.lookup path (sbEnv b)))

-- | 'loadFile' returning raw bytes — feed a ROM image with
-- 'Isacle.System.Rom.romFromBytes'.
loadFileBytesImpl :: FilePath -> SysNet [Word8]
loadFileBytesImpl path = SysNet $ do
    modify $ \b -> b { sbReqs = path : sbReqs b }
    b <- get
    pure (maybe [] BS.unpack (Map.lookup path (sbEnv b)))

-- ---------------------------------------------------------------------------
-- BusDSL monad
-- ---------------------------------------------------------------------------

-- | The slave-side bus port a peripheral entity exposes (input bundle): the
-- broadcast master request, which the peripheral decodes for itself.  A 'PortRef'
-- record, so 'bind'/'entity' name the sub-entity's ports from these fields.
data SlavePort dom dat = SlavePort
    { spReq   :: Sig dom Bool
    , spWe    :: Sig dom Bool
    , spAddr  :: Sig dom (Unsigned 32)
    , spWData :: Sig dom dat
    } deriving (Generic)

deriving instance (KnownDom dom, HdlType dat) => Named (SlavePort dom dat)

-- | Internal record for one peripheral slot inside a bus.  The peripheral is
-- instantiated as its own sub-entity (via 'bind'/'entity') at 'attachPeripheral'
-- time; the slot just records the slave response it drives for the bus read mux.
data PeriphSlot dom dat = PeriphSlot
    { psName :: String
    , psBase :: Word32
    , psSize :: Word32      -- address window in bytes
    , psSpec :: PeriphSpec
    , psResp :: SlaveResp Sig dom dat
    }

data BusDSLState dom dat = BusDSLState
    { bdsPeriph :: [PeriphEntry]
    , bdsSlots  :: [PeriphSlot dom dat]
    , bdsMaster :: MasterReq Sig dom (Unsigned 32) dat
      -- ^ The bus master's request, broadcast to every peripheral (each decodes
      -- its own address window via 'busPortIface').
    }

-- | Bus sub-monad; execute 'attachPeripheral' calls inside 'createBus'.  The
-- @proto@ phantom pins the signalling every peripheral in this bus is attached
-- over (unified with the master's protocol at 'createBus').
newtype BusDSL (proto :: Type) dom dat a = BusDSL (StateT (BusDSLState dom dat) NetM a)
    deriving newtype (Functor, Applicative, Monad)

-- | Attach a peripheral at @base@.
--
-- Synthesises the peripheral register file as a named sub-entity (its own
-- VHDL file), wires it to the enclosing bus, accumulates its read-data into
-- the bus mux, records it in the bus map, and returns the peripheral's
-- physical outputs.  The output type @a@ is determined by the token:
--
-- @
-- (tx, rxIrq, txIrq) <- attachPeripheral 0x100 uart0   -- UART token
-- (port, ddr)         <- attachPeripheral 0x300 gpio0   -- GPIO token
-- @
attachPeripheral
    :: forall proto p dom dat a.
       ( KnownDom dom, HdlType dat, Num dat, Num (Sig dom dat)
       , HdlPhys a, Named a )
    => Word32
    -> PeriphToken proto p dom dat a
    -> BusDSL proto dom dat a
attachPeripheral base token = BusDSL $ do
    st <- get
    let master = bdsMaster st       -- broadcast: the peripheral decodes its window
        nm     = ptName token
        -- Spec (pure, no emission): only the register-field metadata, which
        -- doesn't depend on the request values.
        specBus = busPortIface (mqReq master) (mqWe master)
                               (mqAddr master) (mqWData master) base
        (_, _, spec) = runPeriphDef hdlOps specBus (ptDef token)
        size  = case specSize spec of { 0 -> ptAddrSize token; n -> n }
        entry = PeriphEntry { peName = nm, peBase = base, peSpec = spec }
        -- Top-level names for the physical outputs: @<instance>_<port>@.
        physNames = [ nm ++ "_" ++ portName ps | ps <- portSpecs (Proxy @a) ]
        -- The peripheral's behaviour as a sub-entity body: decode the bus port,
        -- return its read-data signal and physical-output bundle.
        body :: SlavePort dom dat -> NetM (Sig dom dat, a)
        body sp =
            let busP = busPortIface (spReq sp) (spWe sp) (spAddr sp) (spWData sp) base
                (phys, rd, _) = runPeriphDef hdlOps busP (ptDef token)
            in pure (rd, phys)

    -- Instantiate the peripheral as its own named sub-entity (e.g. "gpio0" →
    -- gpio0.vhd) with the entity tooling; the broadcast master drives its ports.
    (rd, phys) <- lift $ instanceOf nm
        (entity nm body :: EntityDef (SlavePort dom dat) (Sig dom dat, a))
        SlavePort { spReq   = mqReq   master, spWe    = mqWe    master
                  , spAddr  = mqAddr  master, spWData = mqWData master }
    -- Promote the peripheral's physical outputs to top-level SoC ports.
    lift $ emitPhysOuts physNames domInfo datW phys

    let slot = PeriphSlot
            { psName = nm, psBase = base, psSize = size, psSpec = spec
            , psResp = SlaveResp { srRData = rd, srStall = sigFalse }
            }
    put st { bdsPeriph = bdsPeriph st ++ [entry]
           , bdsSlots  = bdsSlots  st ++ [slot]
           }
    pure phys

  where
    domInfo  = domId   (Proxy @dom)
    datW     = fromIntegral (natVal (Proxy @(Width dat)))

-- ---------------------------------------------------------------------------
-- createBus
-- ---------------------------------------------------------------------------

-- | Build a named bus interconnect, driven by the master wires in @bh@.
--
-- Wire the 'BusDSL' peripherals to the bus and connect the aggregated
-- read-data and stall signals back into the 'BusHandle'.  Obtain @bh@ from
-- 'createHarvardCPU' (or 'orphanBusMaster' for test-only systems).
--
-- @
-- bh <- createHarvardCPU \@16 \@16 \@16 "cpu" myCPUDef myISA romWords
-- (tx, port) <- createBus "databus" bh $ do
--     tx'   <- attachPeripheral 0x100 uartTok
--     port' <- attachPeripheral 0x300 gpioTok
--     return (tx', port')
-- @
createBusImpl
    :: forall proto dom dat a.
       (BusArch proto, KnownDom dom, HdlType dat, Num dat, Num (Sig dom dat))
    => String
    -> BusDSL proto dom dat a
    -> SysNet (Bus proto dom (Unsigned 32) dat, a)
createBusImpl busName (BusDSL busSt) = SysNet $ do
    -- Root request placeholders: forward-declared 'freshSig' wires the driving
    -- master (a CPU, a cache, …) fills in later via 'driveBus'.  The peripherals
    -- are instantiated /now/, wired to these placeholders — so the bus is a
    -- complete node the moment 'createBus' returns; only its root is unconnected.
    reqSink <- lift $ do
        rq <- freshSig; we <- freshSig; ad <- freshSig; wd <- freshSig
        pure MasterReq { mqReq = rq, mqWe = we, mqAddr = ad, mqWData = wd }

    -- Single pass.  Each peripheral is instantiated as its own sub-entity by
    -- 'attachPeripheral' (broadcast request in, response out); the bus read mux
    -- selects the addressed child's read data by window.
    (userA, finalSt) <- lift $ runStateT busSt
        BusDSLState { bdsPeriph = [], bdsSlots = [], bdsMaster = reqSink }
    let children :: [BusChild Sig dom dat]
        children   = [ (toInteger (psBase s), toInteger (psSize s), psResp s)
                     | s <- bdsSlots finalSt ]
        masterResp = fst $ synthBus (busArch :: proto) reqSink children
        periph     = bdsPeriph finalSt

    modify $ \b -> b { sbDoc = let doc = sbDoc b
                               in doc { sdBuses = sdBuses doc ++ [BusSection busName periph] } }
    pure ( Bus { busName = busName, busReq = reqSink, busResp = masterResp }
         , userA )

-- ---------------------------------------------------------------------------
-- CPU
-- ---------------------------------------------------------------------------

-- | The Harvard CPU's input ports (a 'PortRef' record so 'bind'/'entity' name
-- the sub-entity's ports).  Field types carry widths via @Unsigned cw/caw@ and
-- @dat@; the ISA compiler runs type-erased and is re-tagged at the boundary.
data HCpuIn dom dat cw caw = HCpuIn
    { hciCodeRd  :: Sig dom (Unsigned cw)    -- ^ single code read port data (opcode, then operands)
    , hciDmemRd  :: Sig dom dat              -- ^ data read result (from the bus)
    , hciStall   :: Sig dom Bool
    , hciIrqPend :: Sig dom Bool
    , hciIrqVec  :: Sig dom (Unsigned caw)
    } deriving (Generic)

-- | The Harvard CPU's output ports.
data HCpuOut dom dat aw caw = HCpuOut
    { hcoCodeAddr   :: Sig dom (Unsigned caw)  -- ^ single code read address (PC, then PC+1, PC+2, …)
    , hcoDataRdAddr :: Sig dom (Unsigned aw)
    , hcoDataWrEn   :: Sig dom Bool
    , hcoDataWrAddr :: Sig dom (Unsigned aw)
    , hcoDataWrData :: Sig dom dat
    , hcoIrqAck     :: Sig dom Bool            -- ^ 1 the cycle the handler takes the IRQ
    } deriving (Generic)

deriving instance (KnownDom dom, HdlType dat, KnownNat cw, KnownNat caw)
    => Named (HCpuIn dom dat cw caw)
deriving instance (KnownDom dom, HdlType dat, KnownNat aw, KnownNat caw)
    => Named (HCpuOut dom dat aw caw)

-- | Re-tag a signal's phantom representation type — a no-op rewrap (the second
-- 'Sig' parameter is phantom).  Bridges the ISA compiler's type-erased
-- @Sig dom ()@ and the width-carrying port types at an entity boundary.
retagSig :: Sig dom a -> Sig dom b
retagSig (SWire w) = SWire w
retagSig (SExpr m) = SExpr m

-- | The cached von Neumann CPU's input ports (driven by the L1 cache + IRQ).
data VnCpuIn dom dat aw = VnCpuIn
    { vciInstr   :: Sig dom dat            -- ^ instruction word from the cache
    , vciDmemRd  :: Sig dom dat            -- ^ data read result from the cache
    , vciStall   :: Sig dom Bool
    , vciIrqPend :: Sig dom Bool
    , vciIrqVec  :: Sig dom (Unsigned aw)
    } deriving (Generic)

-- | The cached von Neumann CPU's output ports (→ the L1 cache).
data VnCpuOut dom dat aw = VnCpuOut
    { vcoFetchAddr  :: Sig dom (Unsigned aw)
    , vcoDataRdAddr :: Sig dom (Unsigned aw)
    , vcoDataWrEn   :: Sig dom Bool
    , vcoDataWrAddr :: Sig dom (Unsigned aw)
    , vcoDataWrData :: Sig dom dat
    } deriving (Generic)

deriving instance (KnownDom dom, HdlType dat, KnownNat aw)
    => Named (VnCpuIn dom dat aw)
deriving instance (KnownDom dom, HdlType dat, KnownNat aw)
    => Named (VnCpuOut dom dat aw)

-- | Synthesise a Harvard CPU into the system, connect it to a data bus, and
-- instantiate an internal code ROM.
--
-- * @addrW@, @codeWordW@, @codeAddrW@ — type-level widths via TypeApplications.
-- * @dataBus@   — handle returned by 'createBus'; the CPU's load/store
--   signals are wired directly to the bus master wires.
-- * @romWords@  — initial instruction words for the code ROM (padded with 0).
--
-- The CPU address width (@addrW@) may differ from the bus address width
-- (always 32-bit); a zero-extend resize is inserted automatically.
createHarvardCPUImpl :: forall core addrW codeWordW codeAddrW proto cproto (dom :: Type) dat alu.
              ( KnownDom dom
              , KnownNat addrW, KnownNat codeWordW, KnownNat codeAddrW
              , addrW <= 32, codeAddrW <= 32
              , HdlType dat
              , HdlType core, Generic core, GFields (Rep core)
              , BusMaster proto, BusMaster cproto
              )
           => String
           -> Chip core alu (Width dat) addrW codeWordW codeAddrW
           -> Bus cproto dom (Unsigned 32) (Unsigned codeWordW)  -- ^ code bus (data = code word)
           -> Bus proto  dom (Unsigned 32) dat                   -- ^ data bus
           -> IrqDriver dom codeAddrW                            -- ^ interrupt driver ('noIrq' if dormant)
           -> SysNet ()
createHarvardCPUImpl instName (Chip cpuDef isaDef) codeBus dataBus irqDriver = SysNet $ do
    let codeWordB    = fromIntegral (natVal (Proxy @codeWordW))  :: Int
    let addrBits     = fromIntegral (natVal (Proxy @addrW))       :: Int
    let codeAddrBits = fromIntegral (natVal (Proxy @codeAddrW))   :: Int

    -- Pre-allocate the CPU's input signals.  They are driven below — ROM →
    -- instr/2nd-code-word, data bus → read-data/stall (via createBus), IRQ →
    -- tied off.  'freshSig' + later 'connectSig'/ROM form the fetch/bus feedback
    -- without a recursive knot (the CPU outputs feed the ROM, not its inputs).
    codeRdSig  <- lift freshSig
    dmemRdSig  <- lift freshSig
    stallSig   <- lift freshSig
    irqPendSig <- lift freshSig
    irqVecSig  <- lift freshSig

    -- CPU sub-entity body: the abstract, NetM-free 'synthHarvardCPU'' run at
    -- @s = Sig@, @m = NetM@.  Port types carry widths; re-tag to the compiler's
    -- erased @Sig dom ()@ on the way in, and back to the port types on the way out.
    let cpuBody :: HCpuIn dom dat codeWordW codeAddrW
                -> NetM (HCpuOut dom dat addrW codeAddrW)
        cpuBody hci = do
            cmi <- synthHarvardCPU' @core @Sig @NetM @dom @(Width dat) @addrW @codeWordW @codeAddrW
                       cpuDef isaDef
                       (retagSig (hciCodeRd  hci)) (retagSig (hciDmemRd  hci))
                       (retagSig (hciStall   hci))
                       (retagSig (hciIrqPend hci)) (retagSig (hciIrqVec  hci))
            pure HCpuOut
                { hcoCodeAddr   = retagSig (cmiCodeRdAddr cmi)
                , hcoDataRdAddr = retagSig (cmiDataRdAddr cmi)
                , hcoDataWrEn   =           cmiDataWrEn   cmi
                , hcoDataWrAddr = retagSig (cmiDataWrAddr cmi)
                , hcoDataWrData = retagSig (cmiDataWrData cmi)
                , hcoIrqAck     =           cmiIrqAck     cmi
                }

    -- Instantiate the CPU as a named sub-entity via the entity tooling.
    out <- lift $ instanceOf instName
        (entity instName cpuBody
            :: EntityDef (HCpuIn dom dat codeWordW codeAddrW)
                      (HCpuOut dom dat addrW codeAddrW))
        HCpuIn { hciCodeRd  = codeRdSig, hciDmemRd = dmemRdSig
               , hciStall   = stallSig,  hciIrqPend = irqPendSig, hciIrqVec = irqVecSig }

    -- Code bus: a SINGLE-read-port memory, handed in as a peripheral handle (a ROM
    -- assembled by 'createBus').  The CPU drives one code address (PC on the
    -- opcode-fetch cycle, then PC+1, PC+2, … as it sequences operand words) as a
    -- read-only master; the bus returns that one code word.  'driveBus' picks the
    -- code bus's protocol master logic — same mechanism as the data bus.
    codeAddrR <- lift $ materialize (hcoCodeAddr out) >>= resizeTo codeAddrBits 32
    let codeReq = MasterReq
            { mqReq   = sigTrue
            , mqWe    = sigFalse
            , mqAddr  = SWire codeAddrR                :: Sig dom (Unsigned 32)
            , mqWData = sigLitW 0 codeWordB            :: Sig dom (Unsigned codeWordW)
            }
    codeResp <- lift $ driveBus codeBus codeReq
    lift $ connectSig codeRdSig (srRData codeResp)

    -- Drive the CPU's interrupt inputs from the supplied 'IrqDriver' ('noIrq'
    -- ties them to 0).  The driver's sources are wired at the system level; @mdo@
    -- lets a source come from a bus this CPU drives.
    lift $ connectSig irqPendSig (irqPending irqDriver)
    lift $ connectSig irqVecSig  (irqVector  irqDriver)
    -- Acknowledge back to the driver: 1 the cycle the handler takes the IRQ, so a
    -- latching controller clears its held request and re-arms.
    lift $ connectSig (irqAck irqDriver) (hcoIrqAck out)

    -- Resize CPU address outputs to the 32-bit bus width.
    wrAddrR <- lift $ materialize (hcoDataWrAddr out) >>= resizeTo addrBits 32
    rdAddrR <- lift $ materialize (hcoDataRdAddr out) >>= resizeTo addrBits 32

    -- Hand the CPU's single-channel request to the data bus.  'driveBus' is the
    -- bus protocol's master logic (keyed on the peripheral handle's @proto@): for
    -- a 'SimpleBus' it wires combinationally; a Wishbone bus would generate a
    -- handshake here — same call site, master chosen by the bus's type.  The
    -- response feeds the CPU's pre-allocated read-data / stall inputs.
    let cpuReq = MasterReq
            { mqReq   = sigTrue
            , mqWe    = hcoDataWrEn out
            , mqAddr  = mux (hcoDataWrEn out) (SWire wrAddrR) (SWire rdAddrR)
                          :: Sig dom (Unsigned 32)
            , mqWData = hcoDataWrData out
            }
    resp <- lift $ driveBus dataBus cpuReq
    lift $ connectSig dmemRdSig (srRData resp)
    lift $ connectSig stallSig  (srStall resp)
    modify $ \b -> b { sbDoc = let doc = sbDoc b
                               in doc { sdCPUs = sdCPUs doc ++ [instName] } }

-- | Synthesise an L1 cache that bridges the CPU two-port interface to the
-- system bus.
--
-- Returns a 'CacheHandle' that carries the pre-allocated CPU-facing wire IDs.
-- Pass the handle to 'createCachedCPU' to attach the CPU.
--
-- The cache acts as the sole master of @dataBus@: it forwards CPU addresses
-- to the bus and returns bus read data to the CPU.  In the current stub
-- implementation, stall is always 0 (pass-through, no actual caching).
createL1Cache
    :: forall proto dom wordW addrW dat.
       ( KnownDom dom, KnownNat wordW, KnownNat addrW, HdlType dat, BusMaster proto )
    => CacheConfig
    -> Bus proto dom (Unsigned 32) dat
    -> SysNet CacheHandle
createL1Cache cfg busNode = SysNet $ lift $
    synthL1Cache @proto @dom @wordW @addrW cfg busNode

-- | Synthesise a cached von Neumann CPU and connect it to a 'CacheHandle'.
--
-- The CPU's instruction fetch and data access both go through the L1 cache.
-- ROM, RAM, and peripherals all reside in the same flat address space on the
-- system bus; the CPU does not address them directly.
--
-- For a non-cached design (e.g. tightly-coupled SRAM), call 'createL1Cache'
-- with the stub implementation — the stall wire is permanently 0 and the
-- cache is a transparent pass-through.
createCachedCPU
    :: forall core addrW (dom :: Type) dat alu.
       ( KnownDom dom, KnownNat addrW, HdlType dat
       , HdlType core, Generic core, GFields (Rep core) )
    => String
    -> Chip core alu (Width dat) addrW (Width dat) addrW
    -> CacheHandle
    -> SysNet ()
createCachedCPU instName (Chip cpuDef isaDef) cacheH = SysNet $ do
    let addrBits  = fromIntegral (natVal (Proxy @addrW)) :: Int

    -- CPU sub-entity body: the abstract, NetM-free 'synthVonNeumannCPU'' at
    -- @s = Sig@, @m = NetM@.  Re-tag between port types and the compiler's erased
    -- @Sig dom ()@.
    let vnBody :: VnCpuIn dom dat addrW -> NetM (VnCpuOut dom dat addrW)
        vnBody vci = do
            vmi <- synthVonNeumannCPU' @core @Sig @NetM @dom @(Width dat) @addrW
                       cpuDef isaDef
                       (retagSig (vciInstr  vci)) (retagSig (vciDmemRd vci))
                       (retagSig (vciStall  vci)) (retagSig (vciIrqPend vci))
                       (retagSig (vciIrqVec vci))
            pure VnCpuOut
                { vcoFetchAddr  = retagSig (vniFetchAddr  vmi)
                , vcoDataRdAddr = retagSig (vniDataRdAddr vmi)
                , vcoDataWrEn   =           vniDataWrEn   vmi
                , vcoDataWrAddr = retagSig (vniDataWrAddr vmi)
                , vcoDataWrData = retagSig (vniDataWrData vmi)
                }

    -- Instantiate the CPU as a named sub-entity; the cache drives its inputs.
    out <- lift $ instanceOf instName
        (entity instName vnBody
            :: EntityDef (VnCpuIn dom dat addrW) (VnCpuOut dom dat addrW))
        VnCpuIn { vciInstr   = SWire (chInstrWord  cacheH)
                , vciDmemRd  = SWire (chDataRdData cacheH)
                , vciStall   = SWire (chStall      cacheH)
                , vciIrqPend = sigLitW 0 1                  -- no IRQ controller
                , vciIrqVec  = sigLitW 0 addrBits
                }

    -- Wire CPU outputs → cache inputs.
    lift $ do
        materialize (vcoFetchAddr  out) >>= alias (chFetchAddr  cacheH)
        materialize (vcoDataRdAddr out) >>= alias (chDataRdAddr cacheH)
        materialize (vcoDataWrEn   out) >>= alias (chDataWrEn   cacheH)
        materialize (vcoDataWrAddr out) >>= alias (chDataWrAddr cacheH)
        materialize (vcoDataWrData out) >>= alias (chDataWrData cacheH)

    modify $ \b -> b { sbDoc = let doc = sbDoc b
                               in doc { sdCPUs = sdCPUs doc ++ [instName] } }

-- | Emit a zero-extending resize node (identity when widths match).
resizeTo :: Int -> Int -> WireId -> NetM WireId
resizeTo srcW dstW src
    | srcW == dstW = return src
    | otherwise    = do { w <- freshWire; emit $ NComb w (PResize dstW) [src]; return w }

-- | Drive @dst@ from @src@ via a single-input OR (identity in synthesis).
alias :: WireId -> WireId -> NetM ()
alias dst src = emit $ NComb dst POr [src, src]

-- | Run a system description, returning the full 'Design' (top entity plus
-- any sub-entities such as RAM blocks) ready for 'emitVhdlDesignFiles'.
-- Apply @dom@ and @dat@ as visible type arguments to disambiguate:
-- @execSystemDSL \@Sys \@(Unsigned 8) "top" mySystem@
execSystemDSL :: forall a. String -> SysNet a -> Design
execSystemDSL name (SysNet st) =
    execDesign name (fmap fst (runStateT st emptyBuild))

-- | Run a system description once, returning the user result, the full 'Design'
-- (top entity plus sub-entities, for VHDL emission), and the 'SysDoc' (for the
-- memory map / C header / linker artifacts).  A single monad pass, so callers
-- that need every output — e.g. 'Isacle.System.Emit.writeSystem' — do not run
-- the description twice.
runSystemDesign :: forall a. String -> SysNet a -> (a, Design, SysDoc)
runSystemDesign name sys = (a, design, doc)
  where (a, design, doc, _reqs) = runSystemDesignWith Map.empty name sys

-- | 'runSystemDesign' with a resolved file environment (from the IO
-- interpreter's harvest pass) and, additionally, the list of paths the system
-- requested via 'loadFile' / 'loadFileBytes'.  Used to drive the two-pass
-- deferred-load resolution in 'Isacle.System.Emit'.
runSystemDesignWith
    :: forall a. FileEnv -> String -> SysNet a -> (a, Design, SysDoc, [FilePath])
runSystemDesignWith env name (SysNet st) = (a, design, sbDoc b, reverse (sbReqs b))
  where
    ((a, b), design) = runDesign name (runStateT st (emptyBuildWith env))

-- ---------------------------------------------------------------------------
-- Peripheral constructors
-- ---------------------------------------------------------------------------

-- | Create a UART peripheral token.
-- Register interface (UDR, USR, UBRR) is fully wired.
-- Physical outputs (TX, rxIrq, txIrq) are stubs until a serial-FSM
-- sub-component is implemented.
createUartImpl
    :: (KnownDom dom, HdlType dat, Num dat, Num (Sig dom dat))
    => String
    -> Sig dom Bool                  -- ^ RX serial line
    -> SysNet (PeriphToken proto UART dom dat (UartPhys dom))
createUartImpl name rxPin = pure $ PeriphToken
    { ptName     = name
    , ptDef      = do
          (txLine, rxIrq, txIrq) <- uartDefWithFSM rxPin
          return UartPhys { uartTxLine = txLine, uartRxIrq = rxIrq, uartTxIrq = txIrq }
    , ptAddrSize = 0
    }

-- | Create a GPIO peripheral token.
-- Fully implemented: DDR, PORT, and PIN registers all synthesize correctly.
createGpioImpl
    :: (Num dat, Num (Sig dom dat))
    => String
    -> Sig dom dat                   -- ^ input pin bus
    -> SysNet (PeriphToken proto GPIO dom dat (GpioPhys dom dat))
createGpioImpl name pins = pure $ PeriphToken
    { ptName     = name
    , ptDef      = gpioDef pins >>= \(port, ddr) ->
                       return GpioPhys { gpioPort = port, gpioDdr = ddr }
    , ptAddrSize = 0
    }

-- | Create a Timer peripheral token.
-- Physical outputs (overflow IRQ, compare-match IRQ) are stubs until a
-- counter-FSM sub-component is implemented.
createTimerImpl
    :: (KnownDom dom, HdlType dat, Num dat, Num (Sig dom dat))
    => String
    -> Sig dom Bool                  -- ^ tick / count enable
    -> SysNet (PeriphToken proto Timer dom dat (TimerPhys dom))
createTimerImpl name tick = pure $ PeriphToken
    { ptName     = name
    , ptDef      = do
          (ovf, cmp) <- timerDefWithFSM tick
          return TimerPhys { timerOvfIrq = ovf, timerCmpIrq = cmp }
    , ptAddrSize = 0
    }

-- | Create a signed-ramp peripheral token (PLAN_TYPED_HDL #3 demonstrator).
-- The bus carries @Unsigned 8@ but the internal datapath is @Signed 8@; the
-- ramp moves its CURRENT value toward SETPOINT by STEP each tick.  Register
-- map: 0 SETPOINT (RW), 1 STEP (RW), 2 CURRENT (RO).
createRampImpl
    :: KnownDom dom
    => String
    -> Sig dom Bool                  -- ^ tick / advance enable
    -> SysNet (PeriphToken proto Ramp dom (Unsigned 8) ())
createRampImpl name tick = pure $ PeriphToken
    { ptName     = name
    , ptDef      = rampDefWithFSM tick
    , ptAddrSize = 0
    }

-- | Create a synchronous block RAM peripheral token.
-- Attach with @attachPeripheral base ram0@; the RAM occupies @size@ entries
-- starting at @base@.
createRamImpl
    :: (Num dat, Num (Sig dom dat))
    => Int          -- ^ number of addressable entries
    -> [Integer]    -- ^ initial contents (padded to @size@ with 0)
    -> String       -- ^ instance name
    -> SysNet (PeriphToken proto RAM dom dat ())
createRamImpl size initVals name = pure $ PeriphToken
    { ptName     = name
    , ptDef      = blockRamDef size initVals
    , ptAddrSize = fromIntegral size
    }

-- | A typed ROM image: the initial words tagged with their word type @dat@, so a
-- 16-bit image is @RomImage (Unsigned 16)@ — not a bare @[Integer]@ whose word
-- width is a mystery.  The ROM's width is determined by this type.
newtype RomImage dat = RomImage [Integer]

-- | Create a read-only ROM peripheral token.  The read is __combinational__
-- (0-cycle: address → word the same cycle), so it serves equally as a CPU
-- instruction memory or a data-bus lookup table.  The word width comes from the
-- 'RomImage' type.  Attach with @attachPeripheral base rom0@; the ROM occupies
-- @size@ words starting at @base@.
createRomImpl
    :: (Num dat, Num (Sig dom dat))
    => Int                     -- ^ number of words
    -> RomImage dat            -- ^ typed image (word type = @dat@)
    -> String                  -- ^ instance name
    -> SysNet (PeriphToken proto ROM dom dat ())
createRomImpl size (RomImage ws) name = pure $ PeriphToken
    { ptName     = name
    , ptDef      = romCombDef size ws
    , ptAddrSize = fromIntegral size
    }

-- ---------------------------------------------------------------------------
-- Interrupts
-- ---------------------------------------------------------------------------

-- | The interrupt-control object handed to a CPU at construction: it drives the
-- CPU's @irq_pending@ / @irq_vector@ inputs.  A first-class value (not a name in
-- a registry) — build one with 'createIrq' and hand it to 'createHarvardCPU'
-- exactly as you hand it a bus.  @caw@ is the CPU's code-address (vector) width.
data IrqDriver dom caw = IrqDriver
    { irqPending :: Sig dom Bool               -- ^ → CPU irq_pending
    , irqVector  :: Sig dom (Unsigned caw)     -- ^ → CPU irq_vector
    , irqAck     :: Sig dom Bool               -- ^ ← CPU irq_ack (handler took the IRQ); clears the latch
    }

-- | An interrupt driver that never fires — the CPU's irq inputs sit at 0.  Use
-- for a CPU whose interrupts are dormant (e.g. an instruction-coverage SoC whose
-- programs enable interrupts but expect no source to vector them away).  The ack
-- input is accepted and ignored (the CPU drives it unconditionally).
noIrqImpl :: forall (dom :: Type) caw. (KnownDom dom, KnownNat caw, Num (Sig dom (Unsigned caw)))
      => SysNet (IrqDriver dom caw)
noIrqImpl = SysNet $ lift $ do
    ack <- freshSig
    pure IrqDriver { irqPending = sigFalse, irqVector = fromInteger 0, irqAck = ack }

-- | Build a __latching__ interrupt driver from a priority-ordered list of
-- @(request, vector)@ sources and a global enable.  Index 0 is highest priority.
--
-- Each source is latched (an SR flip-flop, the 'register' primitive) and __held__
-- until the CPU acknowledges taking the IRQ (@irq_ack@ clears it).  This is what
-- makes a one-cycle source (a timer overflow pulse) actually vector the CPU: the
-- request survives across the handler's multi-cycle commit, then clears so the
-- ISR runs and the controller re-arms.  A degenerate controller that ignores ack
-- would simply never clear — supported, but this one is re-armable.
--
-- @request@ signals are supplied directly — typically peripheral irq outputs
-- (e.g. @timerOvfIrq@ / @uartRxIrq@ off a 'createBus').  Vectors are the ISA's
-- interrupt addresses (chosen at the system level, not in the ISA package).
createIrqImpl
    :: forall (dom :: Type) caw. (KnownDom dom, KnownNat caw, Num (Sig dom (Unsigned caw)))
    => Sig dom Bool                 -- ^ global interrupt enable
    -> [(Sig dom Bool, Integer)]    -- ^ (request, vector), index 0 = highest priority
    -> SysNet (IrqDriver dom caw)
createIrqImpl enable srcs = SysNet $ lift $ do
    ack <- freshSig
    -- latch each source, cleared when the CPU acknowledges (shared ack: correct
    -- for one-at-a-time servicing, which the priority winner guarantees).
    latched <- forM srcs $ \(req, v) -> do
        rec p <- Hdl.register False ((p .||. req) .&&. sigNot ack)
        pure (p, fromInteger v :: Sig dom (Unsigned caw))
    let (pending, vector) = interruptArbiter latched enable
    pure IrqDriver { irqPending = pending, irqVector = vector, irqAck = ack }

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Emit a top-level output port from within 'SysNet'.
-- The signal type @a@ may differ from the bus data type @dat@
-- (e.g. expose a 'Bool' TX pin alongside 8-bit GPIO).
sysOutputImpl :: forall a dom.
             (KnownDom dom, HdlType a)
          => String -> Sig dom a -> SysNet ()
sysOutputImpl name sig = SysNet $ lift $ outputS @dom @a name sig

-- | Declare a top-level __input__ port and return its signal — the dual of
-- 'sysOutput'.  Wire the result into a peripheral constructor (e.g. GPIO pin
-- inputs, UART RX) so the SoC actually has data inputs rather than tying them to
-- constants:
--
-- @
-- gpioIn <- sysInput \"gpio_a_in\"
-- gpioA  <- createGpio \"gpio_a\" gpioIn
-- @
sysInputImpl :: forall a dom.
            (KnownDom dom, HdlType a)
         => String -> SysNet (Sig dom a)
sysInputImpl name = SysNet $ lift $ inputS @dom @a name

-- | Constant-false Bool signal (1-bit zero literal).
sigFalse :: Sig dom Bool
sigFalse = SExpr $ do
    out <- freshWire
    emit $ NComb out (PLit 0 1) []
    pure out

-- | Constant-true Bool signal (1-bit one literal).
sigTrue :: Sig dom Bool
sigTrue = SExpr $ do
    out <- freshWire
    emit $ NComb out (PLit 1 1) []
    pure out
