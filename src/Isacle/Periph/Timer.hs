{-# LANGUAGE RecursiveDo #-}
module Isacle.Periph.Timer
    ( -- * Peripheral kind tag
      Timer
      -- * PeriphDef description (single source of truth)
    , timerDef
      -- * Counter state machine
    , counterFSM
      -- * Combined PeriphDef with FSM wired in
    , timerDefWithFSM
    ) where

import Prelude
import Control.Monad.Fix (MonadFix)
import Data.Kind (Type)

import Hdl.Monad (Hdl, registerEn)
import Hdl.Sig
import Hdl.Prim (Unsigned)
import Isacle.System.Periph

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
    :: (Num dat, MonadFix m)
    => sig dat                     -- ^ current counter value (from counter FSM)
    -> PeriphDef Timer sig m dat (sig dat, sig dat, sig dat, sig Bool)
timerDef cntSig = do
    -- TCCR: has the CTC bit-field, so it keeps the explicit register/bitF
    -- declaration (the read-back is the written value).
    register RW8 0 "TCCR" "Timer control"
        [ bitF ReadWrite 0 "CTC" "CTC mode: reset counter on compare match" ]
    tccr <- onWrite "tccr" 0 0
    onRead 0 tccr

    -- TCNT: typed metadata, but the read returns the FSM counter and the write
    -- presets via a strobe — explicit logic kept.
    fieldOf @(Unsigned 8) ReadWrite 1 "TCNT" "Counter value (write to preset)"
    (tcntPreset, tcntWritten) <- onWriteStrobe "tcnt" 1 0
    onRead 1 cntSig

    -- OCR: a plain read/write register — the fused typed PE2 combinator fits.
    ocr <- regField @(Unsigned 8) 2 "OCR" "Output compare register" 0

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
    :: forall (dom :: Type) dat m.
       (Hdl Sig m, KnownDom dom, HdlType dat, Num dat)
    => Sig dom Bool           -- ^ tick / count enable
    -> Sig dom dat            -- ^ TCCR (from timerDef)
    -> Sig dom dat            -- ^ OCR  (from timerDef)
    -> Sig dom dat            -- ^ TCNT preset value (from timerDef onWriteStrobe)
    -> Sig dom Bool           -- ^ TCNT write strobe (from timerDef)
    -> m (Sig dom dat, Sig dom Bool, Sig dom Bool)  -- ^ (cnt, ovfIrq, cmpIrq)
counterFSM tick tccr ocr tcntPreset tcntWritten = mdo
    -- The counter: advance on a tick, load on a TCNT write, hold otherwise.
    cnt <- registerEn 0 advance nextCnt
    let -- Decode the current state.
        ctcMode     = sigBit 0 tccr           -- TCCR bit 0: clear-timer-on-compare
        incremented = cnt + 1                 -- wraps to 0 automatically at 2^w
        reachedOcr  = cnt .==. ocr            -- compare match (CTC's top)
        reachedMax  = incremented .==. 0      -- free-running wrap (next tick is zero)

        -- Next count: CTC clears at OCR; normal mode wraps naturally.  A TCNT
        -- write preempts either and loads the preset.
        counted = mux (ctcMode .&&. reachedOcr) 0 incremented
        nextCnt = mux tcntWritten tcntPreset counted
        advance = tick .||. tcntWritten

        -- Interrupts fire only on a real tick (a software preset is not a tick).
        ticking = tick .&&. sigNot tcntWritten
        ovfIrq  = ticking .&&. sigNot ctcMode .&&. reachedMax
        cmpIrq  = ticking .&&. ctcMode        .&&. reachedOcr
    pure (cnt, ovfIrq, cmpIrq)

-- ---------------------------------------------------------------------------
-- Combined PeriphDef with counter FSM
-- ---------------------------------------------------------------------------

-- | 'timerDef' with 'counterFSM' wired in via a recursive binding.
-- Use this as the @ptDef@ in a 'PeriphToken' so that 'attachPeripheral'
-- gets real overflow and compare-match outputs instead of stubs.
timerDefWithFSM
    :: forall (dom :: Type) dat m.
       (Hdl Sig m, KnownDom dom, HdlType dat, Num dat)
    => Sig dom Bool              -- ^ tick / count enable
    -> PeriphDef Timer (Sig dom) m dat (Sig dom Bool, Sig dom Bool)
                                 -- ^ (ovfIrq, cmpIrq)
timerDefWithFSM tick = mdo
    (tccr, ocr, tcntPreset, tcntWritten) <- timerDef cnt
    (cnt, ovf, cmp) <- liftHdl (counterFSM tick tccr ocr tcntPreset tcntWritten)
    return (ovf, cmp)

-- ---------------------------------------------------------------------------
-- Standalone circuit wrapper
-- ---------------------------------------------------------------------------

-- (The legacy standalone @timerUnit@ wrapper was removed — use 'timerDefWithFSM'
--  as a 'PeriphToken' def via 'createTimer'.)
