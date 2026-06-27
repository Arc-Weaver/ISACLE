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
      -- * Standalone circuit wrapper
    , rampUnit
    ) where

import Prelude
import Data.Proxy (Proxy(..))
import Data.Word (Word32)

import Hdl.Net
import Hdl.Types
import Hdl.Bits (Signed, Unsigned, KnownNat)
import Isacle.System.Periph
import Isacle.System.HdlCircuit (hdlOps, hdlBusIface)

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
    :: Sig dom (Unsigned 8)        -- ^ current value (from 'rampFSM')
    -> PeriphDef Ramp (Sig dom) (Unsigned 8)
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
    :: forall dom. KnownDom dom
    => Sig dom Bool            -- ^ tick / advance enable
    -> Sig dom (Signed 8)      -- ^ setpoint (from rampDef)
    -> Sig dom (Signed 8)      -- ^ step     (from rampDef)
    -> Sig dom (Unsigned 8)    -- ^ current value (unsigned-encoded for the bus)
rampFSM tick setpoint step = asUnsigned cur
  where
    domInfo = domId (Proxy @dom)

    cur :: Sig dom (Signed 8)
    cur = SExpr $ do
        outWid <- freshWire
        reprWire outWid RSigned
        let curSig = SWire outWid :: Sig dom (Signed 8)
            below  = curSig .<. setpoint
            above  = setpoint .<. curSig
            up     = curSig + step
            down   = curSig - step
            -- clamp the final step onto the setpoint
            upN    = mux (setpoint .<. up)   setpoint up
            downN  = mux (down .<. setpoint) setpoint down
            moved  = mux below upN (mux above downN curSig)
            next   = mux tick moved curSig
        defer $ do
            nextWid <- materialize next
            emit $ NReg outWid nextWid Nothing (SomeBits 0 8) domInfo
        pure outWid

-- ---------------------------------------------------------------------------
-- Combined PeriphDef with ramp FSM
-- ---------------------------------------------------------------------------

-- | 'rampDef' with 'rampFSM' wired in via a recursive binding.  Use as the
-- @ptDef@ in a 'PeriphToken'.  The ramp has no IRQ outputs.
rampDefWithFSM
    :: KnownDom dom
    => Sig dom Bool                 -- ^ tick / advance enable
    -> PeriphDef Ramp (Sig dom) (Unsigned 8) ()
rampDefWithFSM tick = mdo
    (setpoint, step) <- rampDef curU
    let curU = rampFSM tick setpoint step
    return ()

-- ---------------------------------------------------------------------------
-- Standalone circuit wrapper
-- ---------------------------------------------------------------------------

-- | Memory-mapped ramp built from 'rampDef' + 'rampFSM'.
--
--   base + 0  SETPOINT
--   base + 1  STEP
--   base + 2  CURRENT (read = current value)
rampUnit
    :: KnownDom dom
    => Word32                          -- ^ peripheral base address
    -> Sig dom Bool                    -- ^ tick / advance enable
    -> Sig dom (Unsigned 32)           -- ^ bus write address
    -> Sig dom (Unsigned 8)            -- ^ bus write data
    -> Sig dom Bool                    -- ^ bus write enable
    -> Sig dom (Unsigned 32)           -- ^ bus read address
    -> Sig dom (Unsigned 8)            -- ^ read data
rampUnit base tick wrAddr wrData wrEn rdAddr = rdData
  where
    bus = hdlBusIface wrAddr wrData wrEn rdAddr base
    ((setpoint, step), rdData, _spec) =
        runPeriphDef hdlOps bus (rampDef curU)
    curU = rampFSM tick setpoint step
