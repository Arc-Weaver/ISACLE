-- | Bridge between the PeriphDef DSL and the isacle-hdl backend.
--
-- Provides 'hdlOps' and 'hdlBusIface' so any existing 'PeriphDef' can be
-- driven by the isacle-hdl NetNode IR.  Register-mapped peripherals
-- (GPIO, Timer, UART, …) work without modification.
module Isacle.System.HdlCircuit
    ( hdlOps
    , hdlBusIface
    ) where

import Prelude
import Data.Word (Word32)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (natVal)

import Isacle.Hdl.Net
import Isacle.Hdl.Types
import Isacle.Hdl.Prim (Unsigned)
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
