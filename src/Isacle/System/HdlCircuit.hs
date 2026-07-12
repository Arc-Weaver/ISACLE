-- | Bridge between the PeriphDef DSL and the isacle-hdl backend.
--
-- Provides 'hdlOps' and 'hdlBusIface' so any existing 'PeriphDef' can be
-- driven by the isacle-hdl NetNode IR.  Register-mapped peripherals
-- (GPIO, Timer, UART, …) work without modification.
module Isacle.System.HdlCircuit
    ( hdlOps
    , runPeriphNet
    , hdlBusIface
    , busPortIface
      -- * Named physical-output bundles
    , GpioPhys(..)
    , UartPhys(..)
    , TimerPhys(..)
      -- * Hierarchical output materialisation
    , HdlPhys(..)
    ) where

import Prelude
import Control.Monad (zipWithM_)
import Data.Proxy (Proxy(..))
import Data.Word (Word32)
import GHC.Generics (Generic)
import GHC.TypeLits (natVal)

import Hdl.Net
import Hdl.Sig
import Hdl.Types (Named(..))
import Hdl.Class (regEnS, ram, rom, named)
import Hdl.Prim (Unsigned)
import Isacle.System.Periph (PeriphOps(..), BusIface(..), PeriphDef, PeriphSpec, runPeriphDef)

-- ---------------------------------------------------------------------------
-- HDL ops
-- ---------------------------------------------------------------------------

-- | 'PeriphOps' for the isacle-hdl backend.  'sigReg' produces a deferred
-- 'NReg' node; duplicate emissions from 'SExpr' re-materialisation are
-- removed by the CSE pass in the VHDL emitter before output.
-- | The isacle-hdl (netlist) backend realisation of 'PeriphOps' — the stateful
-- ops are the real 'Hdl' 'regS' / 'ram' / 'rom' / 'named' primitives (no
-- 'SExpr'); combinational ops are the pure 'Signal' operators.  This is the
-- concrete @m = NetM@ instance the peripheral definitions run through.
hdlOps :: forall dom dat.
          (KnownDom dom, HdlType dat, Num dat)
       => PeriphOps (Sig dom) NetM dat
hdlOps = PeriphOps
    { sigReg      = regEnS
    , sigBlockMem = \size initVals wrEn wrAddr wrData rdAddr ->
                        ram size initVals rdAddr wrAddr wrData wrEn
    , sigRom      = \size initVals rdAddr -> rom size initVals rdAddr
    , sigAddrLt   = \addr lim -> addr .<. fromIntegral lim
    , sigZero     = fromInteger 0
    , sigAnd      = (.&&.)
    , sigMux      = mux
    , sigHint     = named
    , sigReinterp = sigReinterpret
    }

-- | Run a peripheral definition in an __isolated__ 'NetM' and return the pure
-- triple, discarding the emitted nodes.  Used for the metadata/spec pass (only
-- the 'PeriphSpec' is kept) and by the legacy standalone @*Unit@ wrappers.  The
-- real hardware pass instead runs 'runPeriphDef' in the ambient 'NetM'.
runPeriphNet :: PeriphOps (Sig dom) NetM dat -> BusIface (Sig dom) dat
             -> PeriphDef p (Sig dom) NetM dat a -> (a, Sig dom dat, PeriphSpec)
runPeriphNet ops bus def = let (r, _, _) = runNetM (runPeriphDef ops bus def) in r

-- ---------------------------------------------------------------------------
-- Named physical-output bundles
-- ---------------------------------------------------------------------------

data GpioPhys dom dat = GpioPhys
    { gpioPort :: Sig dom dat
    , gpioDdr  :: Sig dom dat
    } deriving Generic

deriving instance (KnownDom dom, HdlType dat) => Named (GpioPhys dom dat)

data UartPhys dom = UartPhys
    { uartTxLine :: Sig dom Bool
    , uartRxIrq  :: Sig dom Bool
    , uartTxIrq  :: Sig dom Bool
    } deriving Generic

deriving instance KnownDom dom => Named (UartPhys dom)

data TimerPhys dom = TimerPhys
    { timerOvfIrq :: Sig dom Bool
    , timerCmpIrq :: Sig dom Bool
    } deriving Generic

deriving instance KnownDom dom => Named (TimerPhys dom)

instance (KnownDom dom, HdlType dat) => HdlPhys (GpioPhys dom dat) where
    emitPhysOuts names _dom _datW phys = do
        wires <- toWireIds phys
        let specs = portSpecs (Proxy @(GpioPhys dom dat))
        sequence_ [ emit $ NOutput w nm (portWidth ps) (portDom ps)
                  | (nm, ps, w) <- zip3 names specs wires ]
    fromPhysWires = fromWireIds
    physOutCount _ = portCount (Proxy @(GpioPhys dom dat))

instance KnownDom dom => HdlPhys (UartPhys dom) where
    emitPhysOuts names _dom _datW phys = do
        wires <- toWireIds phys
        let specs = portSpecs (Proxy @(UartPhys dom))
        sequence_ [ emit $ NOutput w nm (portWidth ps) (portDom ps)
                  | (nm, ps, w) <- zip3 names specs wires ]
    fromPhysWires = fromWireIds
    physOutCount _ = portCount (Proxy @(UartPhys dom))

instance KnownDom dom => HdlPhys (TimerPhys dom) where
    emitPhysOuts names _dom _datW phys = do
        wires <- toWireIds phys
        let specs = portSpecs (Proxy @(TimerPhys dom))
        sequence_ [ emit $ NOutput w nm (portWidth ps) (portDom ps)
                  | (nm, ps, w) <- zip3 names specs wires ]
    fromPhysWires = fromWireIds
    physOutCount _ = portCount (Proxy @(TimerPhys dom))

-- ---------------------------------------------------------------------------
-- HDL bus interface
-- ---------------------------------------------------------------------------

-- | Build a 'BusIface' from four bus signals, with @base@ added to each
-- offset so the peripheral sees its own register addresses.
hdlBusIface :: forall dom dat.
               (KnownDom dom, HdlType dat, Num dat)
            => Sig dom (Unsigned 32)   -- ^ write address (bus-absolute)
            -> Sig dom dat             -- ^ write data
            -> Sig dom Bool            -- ^ write enable
            -> Sig dom (Unsigned 32)   -- ^ read address (bus-absolute)
            -> Word32                  -- ^ peripheral base address
            -> BusIface (Sig dom) dat
hdlBusIface wrAddr wrData wrEn rdAddr base = BusIface
    { biWrData    = wrData
    , biWrEqAddr  = \off -> wrEn .&&. (wrAddr .==. fromInteger (fromIntegral base + fromIntegral off))
    , biRdEqAddr  = \off -> rdAddr .==. fromInteger (fromIntegral base + fromIntegral off)
    , biWrEn      = wrEn
    , biRelWrAddr = wrAddr - fromIntegral base
    , biRelRdAddr = rdAddr - fromIntegral base
    }

-- | Build a 'BusIface' from a protocol-agnostic single-channel slave port
-- (@req@, @we@, @addr@, @wdata@) — the peripheral-facing side of a
-- 'Isacle.System.BusArch.BusPort'.
--
-- Unlike 'hdlBusIface' (which hardcodes SimpleBus's separate read/write
-- address+enable wires into the peripheral entity), this presents the unified
-- master/slave transaction shape, so the peripheral entity carries no bus
-- protocol — the protocol lives only in the bus's 'synthBus'.  A write targets
-- offset @off@ when @req && we && addr == base+off@; reads are decoded
-- combinationally by address (the interconnect gates the response by chip
-- select).
busPortIface :: forall dom dat.
                (KnownDom dom, HdlType dat, Num dat)
             => Sig dom Bool            -- ^ req   (transaction valid)
             -> Sig dom Bool            -- ^ we    (1 = write)
             -> Sig dom (Unsigned 32)   -- ^ addr  (bus-absolute)
             -> Sig dom dat             -- ^ wdata (write data)
             -> Word32                  -- ^ peripheral base address
             -> BusIface (Sig dom) dat
busPortIface req we addr wdata base = BusIface
    { biWrData    = wdata
    , biWrEqAddr  = \off -> req .&&. we .&&. (addr .==. fromInteger (fromIntegral base + fromIntegral off))
    , biRdEqAddr  = \off -> addr .==. fromInteger (fromIntegral base + fromIntegral off)
    , biWrEn      = req .&&. we
    , biRelWrAddr = addr - fromIntegral base
    , biRelRdAddr = addr - fromIntegral base
    }

-- ---------------------------------------------------------------------------
-- HdlPhys: materialise typed physical outputs across inBlock boundaries
-- ---------------------------------------------------------------------------

-- | Typeclass for the physical-output type @a@ of a peripheral.
--
-- When a peripheral is synthesised inside an 'inBlock' body, its physical
-- outputs (TX pin, GPIO PORT/DDR, overflow IRQ, …) must be:
--
--   1. Materialised and exposed as 'NOutput' ports inside the body
--      ('emitPhysOuts').
--   2. Reconstructed as 'Sig' values in the parent context from the
--      wire IDs returned by 'inBlock' ('fromPhysWires').
--
-- @datW@ is passed explicitly because 'Sig' type parameters are phantoms.
-- @physOutCount@ gives the number of physical outputs without running the body.
class HdlPhys a where
    -- | Promote each signal component to a top-level 'NOutput', named by the
    -- supplied list (one name per component, in bundle order).
    emitPhysOuts  :: [String] -> DomId -> Int -> a -> NetM ()
    -- | Reconstruct @a@ from parent output-port wire IDs (positional).
    fromPhysWires :: [WireId] -> a
    -- | Number of physical output ports (without running the body).
    physOutCount  :: proxy a -> Int

instance HdlPhys () where
    emitPhysOuts _ _ _ () = return ()
    fromPhysWires _     = ()
    physOutCount  _     = 0

instance HdlPhys (Sig dom Bool, Sig dom Bool, Sig dom Bool) where
    emitPhysOuts (n0:n1:n2:_) dom _ (s0, s1, s2) = do
        w0 <- materialize s0; emit $ NOutput w0 n0 1 dom
        w1 <- materialize s1; emit $ NOutput w1 n1 1 dom
        w2 <- materialize s2; emit $ NOutput w2 n2 1 dom
    emitPhysOuts ns _ _ _ = errNames "Bool,Bool,Bool" ns
    fromPhysWires (w0:w1:w2:_) = (SWire w0, SWire w1, SWire w2)
    fromPhysWires ws            =
        error $ "HdlPhys (Bool,Bool,Bool): expected ≥3 wires, got " ++ show (length ws)
    physOutCount _ = 3

instance HdlPhys (Sig dom Bool, Sig dom Bool) where
    emitPhysOuts (n0:n1:_) dom _ (s0, s1) = do
        w0 <- materialize s0; emit $ NOutput w0 n0 1 dom
        w1 <- materialize s1; emit $ NOutput w1 n1 1 dom
    emitPhysOuts ns _ _ _ = errNames "Bool,Bool" ns
    fromPhysWires (w0:w1:_) = (SWire w0, SWire w1)
    fromPhysWires ws         =
        error $ "HdlPhys (Bool,Bool): expected ≥2 wires, got " ++ show (length ws)
    physOutCount _ = 2

-- | Instance for two dat-width outputs (e.g. GPIO PORT + DDR).
-- Marked OVERLAPPABLE so the (Bool,Bool) instance takes priority when dat=Bool.
instance {-# OVERLAPPABLE #-} HdlPhys (Sig dom dat, Sig dom dat) where
    emitPhysOuts (n0:n1:_) dom datW (s0, s1) = do
        w0 <- materialize s0; emit $ NOutput w0 n0 datW dom
        w1 <- materialize s1; emit $ NOutput w1 n1 datW dom
    emitPhysOuts ns _ _ _ = errNames "dat,dat" ns
    fromPhysWires (w0:w1:_) = (SWire w0, SWire w1)
    fromPhysWires ws         =
        error $ "HdlPhys (dat,dat): expected ≥2 wires, got " ++ show (length ws)
    physOutCount _ = 2

-- | Instance for one Bool output followed by two dat-width outputs
-- (e.g. UART TX + GPIO PORT + DDR when mixed on a single bus).
-- Marked OVERLAPPABLE so the (Bool,Bool,Bool) instance wins when dat=Bool.
instance {-# OVERLAPPABLE #-} HdlPhys (Sig dom Bool, Sig dom dat, Sig dom dat) where
    emitPhysOuts (n0:n1:n2:_) dom datW (s0, s1, s2) = do
        w0 <- materialize s0; emit $ NOutput w0 n0 1    dom
        w1 <- materialize s1; emit $ NOutput w1 n1 datW dom
        w2 <- materialize s2; emit $ NOutput w2 n2 datW dom
    emitPhysOuts ns _ _ _ = errNames "Bool,dat,dat" ns
    fromPhysWires (w0:w1:w2:_) = (SWire w0, SWire w1, SWire w2)
    fromPhysWires ws            =
        error $ "HdlPhys (Bool,dat,dat): expected ≥3 wires, got " ++ show (length ws)
    physOutCount _ = 3

-- | Shared error for an under-length name list passed to 'emitPhysOuts'.
errNames :: String -> [String] -> a
errNames who ns =
    error $ "HdlPhys (" ++ who ++ "): too few phys-output names: " ++ show ns
