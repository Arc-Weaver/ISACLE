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
    ) where

import Prelude
import Data.Word (Word32)
import GHC.Generics (Generic)

import Hdl.Net
import Hdl.Sig
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

