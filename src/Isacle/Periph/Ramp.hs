{-# LANGUAGE RecursiveDo #-}
-- | A small signed-arithmetic peripheral: a value that ramps toward a
-- programmable setpoint by a programmable step each tick.  It is the integrated
-- demonstrator for the typed-HDL signed datapath (PLAN_TYPED_HDL #3): the bus
-- carries @Unsigned 8@, but the internal datapath is @Signed 8@, obtained by
-- /reinterpreting/ the same bits with 'asSigned' ('withRepr' . 'coerce').  Once
-- a wire is tagged 'RSigned' the emitter declares it @signed(..)@ and
-- @numeric_std@ overloading gives signed compare / +/- for free.
--
--   offset 0  SETPOINT  RW  target value (signed)
--   offset 1  STEP      RW  step magnitude per tick (signed)
--   offset 2  CURRENT   RO  current ramp value (signed)
--
-- Each tick the current value moves toward the setpoint by @step@, clamping so
-- the final step lands exactly on the setpoint (no oscillation around it).
module Isacle.Periph.Ramp
    ( -- * Peripheral kind tag
      Ramp
      -- * Signed/unsigned reinterpretation at the bus seam
    , asSigned
    , asUnsigned
      -- * PeriphDef description (single source of truth)
    , rampDef
      -- * Ramp state machine
    , rampFSM
      -- * Combined PeriphDef with FSM wired in
    , rampDefWithFSM
    ) where

import Prelude
import Data.Kind (Type)

import Hdl.Monad (Hdl, registerEn)
import Hdl.Sig
import Hdl.Bits (Signed, Unsigned, KnownNat)
import Isacle.System.Periph

-- ---------------------------------------------------------------------------
-- Peripheral kind tag
-- ---------------------------------------------------------------------------

data Ramp

-- ---------------------------------------------------------------------------
-- Signed/unsigned reinterpretation
-- ---------------------------------------------------------------------------

-- | Reinterpret an unsigned bus signal as signed (same bits), so the datapath
-- downstream is signed.  This is the only seam where representation changes.
asSigned :: KnownNat n => Sig dom (Unsigned n) -> Sig dom (Signed n)
asSigned = sigReinterpret

-- | Reinterpret a signed datapath signal back as unsigned for the bus read mux.
asUnsigned :: KnownNat n => Sig dom (Signed n) -> Sig dom (Unsigned n)
asUnsigned = sigReinterpret

-- ---------------------------------------------------------------------------
-- Register map — single source of truth
-- ---------------------------------------------------------------------------

-- | Ramp register map.  @curU@ is the current value (unsigned-encoded) driven
-- by 'rampFSM'.  Returns the signed @(setpoint, step)@ control signals for the
-- FSM to consume.
rampDef
    :: Monad m => Sig dom (Unsigned 8)        -- ^ current value (from 'rampFSM')
    -> PeriphDef Ramp (Sig dom) m (Unsigned 8)
                 (Sig dom (Signed 8), Sig dom (Signed 8))
rampDef curU = do
    -- Fused typed PE2 combinators — and a signed-register exercise (regField/
    -- roField @(Signed 8) carry the RSigned representation into the metadata).
    spU <- regField @(Signed 8) 0 "SETPOINT" "Target value (signed)" 0
    stU <- regField @(Signed 8) 1 "STEP" "Step magnitude per tick (signed)" 0
    roField @(Signed 8) 2 "CURRENT" "Current ramp value (signed, read-only)" curU
    return (asSigned spU, asSigned stU)

-- ---------------------------------------------------------------------------
-- Ramp state machine
-- ---------------------------------------------------------------------------

-- | Clocked signed datapath implementing the ramp behaviour.
--
--   * below setpoint → add @step@, clamped so it never overshoots;
--   * above setpoint → subtract @step@, clamped likewise;
--   * at setpoint    → hold.
--
-- The @current@ register is an 'NReg' tagged 'RSigned', emitted with deferred
-- feedback (current feeds rampDef feeds rampFSM feeds current).  Returns the
-- current value unsigned-encoded, ready for the bus read mux.
rampFSM
    :: forall (dom :: Type) m. (Hdl Sig m, KnownDom dom)
    => Sig dom Bool            -- ^ tick / advance enable
    -> Sig dom (Signed 8)      -- ^ setpoint (from rampDef)
    -> Sig dom (Signed 8)      -- ^ step     (from rampDef)
    -> m (Sig dom (Unsigned 8))  -- ^ current value (unsigned-encoded for the bus)
rampFSM tick setpoint step = mdo
    -- The signed 'current' register: advance on @tick@, hold otherwise.
    -- 'withRepr' carries the signed representation onto the wire; @mdo@ ties @cur@
    -- to its next value.
    cur0 <- registerEn (0 :: Signed 8) tick moved
    let cur    = withRepr cur0 :: Sig dom (Signed 8)
        below  = cur .<. setpoint
        above  = setpoint .<. cur
        up     = cur + step
        down   = cur - step
        upN    = mux (setpoint .<. up)   setpoint up
        downN  = mux (down .<. setpoint) setpoint down
        moved  = mux below upN (mux above downN cur)
    pure (asUnsigned cur)

-- ---------------------------------------------------------------------------
-- Combined PeriphDef with ramp FSM
-- ---------------------------------------------------------------------------

-- | 'rampDef' with 'rampFSM' wired in via a recursive binding.  Use as the
-- @ptDef@ in a 'PeriphToken'.  The ramp has no IRQ outputs.
rampDefWithFSM
    :: forall (dom :: Type) m. (Hdl Sig m, KnownDom dom)
    => Sig dom Bool                 -- ^ tick / advance enable
    -> PeriphDef Ramp (Sig dom) m (Unsigned 8) ()
rampDefWithFSM tick = mdo
    (setpoint, step) <- rampDef curU
    curU <- liftHdl (rampFSM tick setpoint step)
    return ()

-- ---------------------------------------------------------------------------
-- Standalone circuit wrapper
-- ---------------------------------------------------------------------------

-- (The legacy standalone @rampUnit@ wrapper was removed — use 'rampDefWithFSM'
--  as a 'PeriphToken' def via 'createRamp'.)
