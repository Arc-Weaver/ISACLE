module Isacle.Periph.Timer
    ( -- * Peripheral kind tag
      Timer
      -- * PeriphDef description (single source of truth)
    , timerDef
      -- * Counter state machine
    , counterFSM
      -- * Standalone circuit wrapper
    , timerUnit
    ) where

import Prelude
import Data.Word (Word32)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (natVal)

import Hdl.Net
import Hdl.Types
import Hdl.Prim (Unsigned)
import Isacle.System.Periph
import Isacle.System.HdlCircuit (hdlOps, hdlBusIface)

-- ---------------------------------------------------------------------------
-- Peripheral kind tag
-- ---------------------------------------------------------------------------

data Timer

-- ---------------------------------------------------------------------------
-- Register map — single source of truth
-- ---------------------------------------------------------------------------

-- | Timer register map.
--
--   offset 0  TCCR  read/write  control (bit 0 = CTC mode)
--   offset 1  TCNT  read/write  counter value (reads current counter)
--   offset 2  OCR   read/write  output compare register
--
-- @cntSig@ is the current counter value driven by the counter state machine.
-- Writing TCNT presets the counter.
--
-- Returns @(tccr, ocr, tcntPreset, tcntWritten)@.
timerDef
    :: (Num dat)
    => sig dat                     -- ^ current counter value (from counter FSM)
    -> PeriphDef Timer sig dat (sig dat, sig dat, sig dat, sig Bool)
timerDef cntSig = do
    register RW8 0 "TCCR" "Timer control"
        [ bitF ReadWrite 0 "CTC" "CTC mode: reset counter on compare match" ]
    tccr <- onWrite "tccr" 0 0
    onRead 0 tccr

    field8 ReadWrite 1 "TCNT" "Counter value (write to preset)"
    (tcntPreset, tcntWritten) <- onWriteStrobe "tcnt" 1 0
    onRead 1 cntSig

    field8 ReadWrite 2 "OCR" "Output compare register"
    ocr <- onWrite "ocr" 2 0
    onRead 2 ocr

    return (tccr, ocr, tcntPreset, tcntWritten)

-- ---------------------------------------------------------------------------
-- Counter state machine
-- ---------------------------------------------------------------------------

-- | Clocked counter implementing the timer behaviour described by 'timerDef'.
--
--   Logic summary:
--     * Normal mode: increment on tick; wrap 0xFF→0x00 and raise ovf.
--     * CTC mode (bit 0 of tccr set): reset to 0 and raise cmp when cnt == ocr.
--     * Writing TCNT presets the counter immediately (synchronous preset).
--
--   The counter register is implemented with an 'NReg' node using deferred
--   emission so the feedback signal (cnt feeding timerDef feeding counterFSM
--   feeding cnt) is safe.
counterFSM
    :: forall dom dat.
       (KnownDom dom, HdlType dat, Num dat, Num (Sig dom dat))
    => Sig dom Bool           -- ^ tick / count enable
    -> Sig dom dat            -- ^ TCCR (from timerDef)
    -> Sig dom dat            -- ^ OCR  (from timerDef)
    -> Sig dom dat            -- ^ TCNT preset value (from timerDef onWriteStrobe)
    -> Sig dom Bool           -- ^ TCNT write strobe (from timerDef)
    -> (Sig dom dat, Sig dom Bool, Sig dom Bool)  -- ^ (cnt, ovfIrq, cmpIrq)
counterFSM tick tccr ocr tcntPreset tcntWritten = (cnt, ovf, cmp)
  where
    w       = fromIntegral (natVal (Proxy @(Width dat)))
    domInfo = domId (Proxy @dom)
    maxVal  = fromInteger (2^w - 1) :: Sig dom dat

    -- Counter register with deferred feedback.
    cnt = SExpr $ do
        outWid <- freshWire
        let cntSig     = SWire outWid :: Sig dom dat
            cntCtcMode = sigBit 0 tccr
            cntAtTop   = cntSig .==. ocr
            cntAtMax   = cntSig .==. maxVal
            cntInc     = cntSig + 1
            cntTick    = mux (cntCtcMode .&&. cntAtTop)
                             0
                             (mux (sigNot cntCtcMode .&&. cntAtMax)
                                  0
                                  cntInc)
            cntNext    = mux tcntWritten tcntPreset (mux tick cntTick cntSig)

        defer $ do
            nextWid <- materialize cntNext
            let initBits = SomeBits 0 w
            emit $ NReg outWid nextWid Nothing initBits domInfo
        pure outWid

    ctcMode = sigBit 0 tccr
    atTop   = cnt .==. ocr
    atMax   = cnt .==. maxVal

    ovf = tick .&&. sigNot ctcMode .&&. atMax  .&&. sigNot tcntWritten
    cmp = tick .&&. ctcMode        .&&. atTop  .&&. sigNot tcntWritten

-- ---------------------------------------------------------------------------
-- Standalone circuit wrapper
-- ---------------------------------------------------------------------------

-- | Memory-mapped timer built from 'timerDef' + 'counterFSM'.
--
--   Register layout:
--     base + 0  TCCR  control
--     base + 1  TCNT  counter (write = preset, read = current value)
--     base + 2  OCR   output compare
timerUnit
    :: (KnownDom dom, HdlType dat, Num dat, Num (Sig dom dat))
    => Word32                          -- ^ peripheral base address
    -> Sig dom Bool                    -- ^ tick / count enable
    -> Sig dom (Unsigned 32)           -- ^ bus write address
    -> Sig dom dat                     -- ^ bus write data
    -> Sig dom Bool                    -- ^ bus write enable
    -> Sig dom (Unsigned 32)           -- ^ bus read address
    -> (Sig dom dat, Sig dom Bool, Sig dom Bool)  -- ^ (rdData, ovfIrq, cmpIrq)
timerUnit base tick wrAddr wrData wrEn rdAddr = (rdData, ovfIrq, cmpIrq)
  where
    bus = hdlBusIface wrAddr wrData wrEn rdAddr base
    ((tccr, ocr, tcntPreset, tcntWritten), rdData, _spec) =
        runPeriphDef hdlOps bus (timerDef cnt)
    (cnt, ovfIrq, cmpIrq) =
        counterFSM tick tccr ocr tcntPreset tcntWritten
