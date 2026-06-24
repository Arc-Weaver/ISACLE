-- | Bridge between the PeriphDef DSL and the isacle-hdl backend.
--
-- Provides 'hdlOps' and 'hdlBusIface' so any existing 'PeriphDef' can be
-- driven by the isacle-hdl NetNode IR.  Register-mapped peripherals
-- (GPIO, Timer, UART, …) work without modification.
module Isacle.System.HdlCircuit
    ( hdlOps
    , hdlBusIface
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
import Hdl.Types
import Hdl.Entity (PortRef(..))
import Hdl.Prim (Unsigned)
import Isacle.System.Periph (PeriphOps(..), BusIface(..))

-- ---------------------------------------------------------------------------
-- HDL ops
-- ---------------------------------------------------------------------------

-- | 'PeriphOps' for the isacle-hdl backend.  'sigReg' produces a deferred
-- 'NReg' node; duplicate emissions from 'SExpr' re-materialisation are
-- removed by the CSE pass in the VHDL emitter before output.
hdlOps :: forall dom dat.
          (KnownDom dom, HdlType dat, Num dat)
       => PeriphOps (Sig dom) dat
hdlOps = PeriphOps
    { sigReg      = hdlSigReg
    , sigBlockMem = hdlSigBlockMem
    , sigAddrLt   = \addr lim -> addr .<. fromIntegral lim
    , sigZero     = litSig 0
    , sigAnd      = (.&&.)
    , sigMux      = mux
    , sigHint     = \name s -> SExpr $ do
                        w <- materialize s
                        hintWire w name
                        pure w
    }
  where
    litSig :: Integer -> Sig dom dat
    litSig v = SExpr $ do
        out <- freshWire
        emit $ NComb out (PLit v w) []
        pure out
      where w = fromIntegral (natVal (Proxy @(Width dat)))

    hdlSigReg :: dat -> Sig dom Bool -> Sig dom dat -> Sig dom dat
    hdlSigReg initVal en inp = SExpr $ do
        outWid <- freshWire
        let bitWidth = fromIntegral (natVal (Proxy @(Width dat)))
            initBits = SomeBits (toBits initVal) bitWidth
            domInfo  = domId (Proxy @dom)
        defer $ do
            enWid  <- materialize en
            inWid  <- materialize inp
            emit $ NReg outWid inWid (Just enWid) initBits domInfo
        pure outWid

    hdlSigBlockMem :: Int -> [Integer]
                   -> Sig dom Bool -> Sig dom (Unsigned 32) -> Sig dom dat -> Sig dom (Unsigned 32)
                   -> Sig dom dat
    hdlSigBlockMem size initVals wrEn wrAddr wrData rdAddr = SExpr $ do
        out <- freshWire
        let datW   = fromIntegral (natVal (Proxy @(Width dat)))
            domInf = domId (Proxy @dom)
        defer $ do
            enW  <- materialize wrEn
            waW  <- materialize wrAddr
            wdW  <- materialize wrData
            raW  <- materialize rdAddr
            emit $ NMem out raW waW wdW enW size datW initVals domInf
        pure out

-- ---------------------------------------------------------------------------
-- Named physical-output bundles
-- ---------------------------------------------------------------------------

data GpioPhys dom dat = GpioPhys
    { gpioPort :: Sig dom dat
    , gpioDdr  :: Sig dom dat
    } deriving Generic

deriving instance (KnownDom dom, HdlType dat) => HdlPorts (GpioPhys dom dat)
deriving instance (KnownDom dom, HdlType dat) => PortRef  (GpioPhys dom dat)

data UartPhys dom = UartPhys
    { uartTxLine :: Sig dom Bool
    , uartRxIrq  :: Sig dom Bool
    , uartTxIrq  :: Sig dom Bool
    } deriving Generic

deriving instance KnownDom dom => HdlPorts (UartPhys dom)
deriving instance KnownDom dom => PortRef  (UartPhys dom)

data TimerPhys dom = TimerPhys
    { timerOvfIrq :: Sig dom Bool
    , timerCmpIrq :: Sig dom Bool
    } deriving Generic

deriving instance KnownDom dom => HdlPorts (TimerPhys dom)
deriving instance KnownDom dom => PortRef  (TimerPhys dom)

instance (KnownDom dom, HdlType dat) => HdlPhys (GpioPhys dom dat) where
    emitPhysOuts _dom _datW phys = do
        wires <- toWireIds phys
        let specs = portSpecs (Proxy @(GpioPhys dom dat))
        zipWithM_ (\ps w -> emit $ NOutput w (portName ps) (portWidth ps) (portDom ps)) specs wires
    fromPhysWires = fromWireIds
    physOutCount _ = portCount (Proxy @(GpioPhys dom dat))

instance KnownDom dom => HdlPhys (UartPhys dom) where
    emitPhysOuts _dom _datW phys = do
        wires <- toWireIds phys
        let specs = portSpecs (Proxy @(UartPhys dom))
        zipWithM_ (\ps w -> emit $ NOutput w (portName ps) (portWidth ps) (portDom ps)) specs wires
    fromPhysWires = fromWireIds
    physOutCount _ = portCount (Proxy @(UartPhys dom))

instance KnownDom dom => HdlPhys (TimerPhys dom) where
    emitPhysOuts _dom _datW phys = do
        wires <- toWireIds phys
        let specs = portSpecs (Proxy @(TimerPhys dom))
        zipWithM_ (\ps w -> emit $ NOutput w (portName ps) (portWidth ps) (portDom ps)) specs wires
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
    -- | Emit one 'NOutput' per signal component inside a sub-entity body.
    emitPhysOuts  :: DomId -> Int -> a -> NetM ()
    -- | Reconstruct @a@ from parent output-port wire IDs (positional).
    fromPhysWires :: [WireId] -> a
    -- | Number of physical output ports (without running the body).
    physOutCount  :: proxy a -> Int

instance HdlPhys () where
    emitPhysOuts _ _ () = return ()
    fromPhysWires _     = ()
    physOutCount  _     = 0

instance HdlPhys (Sig dom Bool, Sig dom Bool, Sig dom Bool) where
    emitPhysOuts dom _ (s0, s1, s2) = do
        w0 <- materialize s0; emit $ NOutput w0 "p0" 1 dom
        w1 <- materialize s1; emit $ NOutput w1 "p1" 1 dom
        w2 <- materialize s2; emit $ NOutput w2 "p2" 1 dom
    fromPhysWires (w0:w1:w2:_) = (SWire w0, SWire w1, SWire w2)
    fromPhysWires ws            =
        error $ "HdlPhys (Bool,Bool,Bool): expected ≥3 wires, got " ++ show (length ws)
    physOutCount _ = 3

instance HdlPhys (Sig dom Bool, Sig dom Bool) where
    emitPhysOuts dom _ (s0, s1) = do
        w0 <- materialize s0; emit $ NOutput w0 "p0" 1 dom
        w1 <- materialize s1; emit $ NOutput w1 "p1" 1 dom
    fromPhysWires (w0:w1:_) = (SWire w0, SWire w1)
    fromPhysWires ws         =
        error $ "HdlPhys (Bool,Bool): expected ≥2 wires, got " ++ show (length ws)
    physOutCount _ = 2

-- | Instance for two dat-width outputs (e.g. GPIO PORT + DDR).
-- Marked OVERLAPPABLE so the (Bool,Bool) instance takes priority when dat=Bool.
instance {-# OVERLAPPABLE #-} HdlPhys (Sig dom dat, Sig dom dat) where
    emitPhysOuts dom datW (s0, s1) = do
        w0 <- materialize s0; emit $ NOutput w0 "p0" datW dom
        w1 <- materialize s1; emit $ NOutput w1 "p1" datW dom
    fromPhysWires (w0:w1:_) = (SWire w0, SWire w1)
    fromPhysWires ws         =
        error $ "HdlPhys (dat,dat): expected ≥2 wires, got " ++ show (length ws)
    physOutCount _ = 2

-- | Instance for one Bool output followed by two dat-width outputs
-- (e.g. UART TX + GPIO PORT + DDR when mixed on a single bus).
-- Marked OVERLAPPABLE so the (Bool,Bool,Bool) instance wins when dat=Bool.
instance {-# OVERLAPPABLE #-} HdlPhys (Sig dom Bool, Sig dom dat, Sig dom dat) where
    emitPhysOuts dom datW (s0, s1, s2) = do
        w0 <- materialize s0; emit $ NOutput w0 "p0" 1    dom
        w1 <- materialize s1; emit $ NOutput w1 "p1" datW dom
        w2 <- materialize s2; emit $ NOutput w2 "p2" datW dom
    fromPhysWires (w0:w1:w2:_) = (SWire w0, SWire w1, SWire w2)
    fromPhysWires ws            =
        error $ "HdlPhys (Bool,dat,dat): expected ≥3 wires, got " ++ show (length ws)
    physOutCount _ = 3
