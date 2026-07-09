-- | Two compatible interrupt control units (system-level, ISA-agnostic).
--
-- Both take a compile-time number @N@ of interrupt sources (index 0 = highest
-- priority), each paired 1:1 with a vector; they priority-select the active
-- source, drive the CPU's @irq_pending@ / @irq_vector@, and expose a
-- memory-mapped /status register/ reading the active source (0 = none).
--
--   * 'staticIrqDef'  — vectors are fixed at compile time; the bus face is just
--                       the status register.  This is the base for fixed-vector
--                       ISAs (see 'mcs51IrqVectors').
--   * 'progIrqDef'    — vectors live in a software-writable table (N registers,
--                       combinationally read — single-clock access), plus the
--                       status register.
--
-- The two are bus-compatible: same CPU-side outputs, same status register.  The
-- CPU stays ISA-agnostic (flexible vector input); an ISA's vector map is picked
-- here, at the system level, not in the ISA package.
module Isacle.Periph.InterruptController
    ( -- * Peripheral kind tag
      IrqCtrl
      -- * Units
    , staticIrqDef        -- ^ Unit 1: compile-time vectors
    , progIrqDef          -- ^ Unit 2: programmable vector table
      -- * Priority helper
    , activeSource
      -- * ISA vector tables (system-level data)
    , mcs51IrqVectors
    ) where

import Prelude
import Data.Word (Word8)
import Control.Monad (forM)
import GHC.TypeLits (KnownNat)

import Hdl.Prim  (Unsigned)
import Hdl.Types (Sig, HdlType, Width, mux, sigResize)
import Isacle.System.Periph
import Isacle.Periph.Interrupt (interruptArbiter)

-- ---------------------------------------------------------------------------
-- Peripheral kind tag
-- ---------------------------------------------------------------------------

data IrqCtrl

-- ---------------------------------------------------------------------------
-- Priority index (shared by both units → the status register)
-- ---------------------------------------------------------------------------

-- | 1-based index of the highest-priority active source (0 = none).  Sources are
-- in priority order: index 0 wins over index 1, etc.  This is what the status
-- register reads back so an ISR can dispatch on the source.
activeSource
    :: (HdlType dat, Num (Sig dom dat))
    => [Sig dom Bool] -> Sig dom dat
activeSource sources =
    foldr (\(i, req) acc -> mux req (fromInteger (i + 1)) acc)
          (fromInteger 0)
          (zip [(0 :: Integer) ..] sources)

-- ---------------------------------------------------------------------------
-- Unit 1 — static (compile-time) vectors
-- ---------------------------------------------------------------------------

-- | Fixed-vector interrupt controller.  The vectors are compile-time constants,
-- so the only bus-visible register is the status register (read-only) at
-- @statusOff@.  Returns the CPU-side @(irq_pending, irq_vector)@.
staticIrqDef
    :: forall dom dat vecW.
       ( HdlType dat, KnownNat (Width dat), Num (Sig dom dat)
       , KnownNat vecW, Num (Sig dom (Unsigned vecW)) )
    => Word8              -- ^ status register byte offset
    -> [Integer]          -- ^ vectors, index 0 = highest priority (length N)
    -> Sig dom Bool       -- ^ global enable
    -> [Sig dom Bool]     -- ^ N source requests (index 0 = highest priority)
    -> PeriphDef IrqCtrl (Sig dom) dat (Sig dom Bool, Sig dom (Unsigned vecW))
staticIrqDef statusOff vectors enable sources = do
    roField @dat statusOff "ISR" "Active interrupt source (0 = none)"
        (activeSource sources)
    let pairs             = zip sources (map fromInteger vectors)
        (pending, vector) = interruptArbiter pairs enable
    pure (pending, vector)

-- ---------------------------------------------------------------------------
-- Unit 2 — programmable vector table
-- ---------------------------------------------------------------------------

-- | Programmable-vector interrupt controller.  Adds a software-writable vector
-- table — @N@ individual registers at offsets @0..N-1@ (Approach A: plain
-- registers, so the arbiter reads them combinationally in one clock) — followed
-- by the status register (read-only) at offset @N@.  Each vector register is
-- bus-data wide and resized to the CPU's vector width.
progIrqDef
    :: forall dom dat vecW.
       ( HdlType dat, KnownNat (Width dat), Num dat, Num (Sig dom dat)
       , KnownNat vecW, Num (Sig dom (Unsigned vecW)) )
    => Int                -- ^ N (sources = vectors)
    -> Sig dom Bool       -- ^ global enable
    -> [Sig dom Bool]     -- ^ N source requests (index 0 = highest priority)
    -> PeriphDef IrqCtrl (Sig dom) dat (Sig dom Bool, Sig dom (Unsigned vecW))
progIrqDef n enable sources = do
    vregs <- forM [0 .. n - 1] $ \i ->
        regField @dat (fromIntegral i) ("VEC" ++ show i)
                 ("Interrupt vector " ++ show i) 0
    roField @dat (fromIntegral n) "ISR" "Active interrupt source (0 = none)"
        (activeSource sources)
    let vecs              = map (sigResize @vecW) vregs
        pairs             = zip sources vecs
        (pending, vector) = interruptArbiter pairs enable
    pure (pending, vector)

-- ---------------------------------------------------------------------------
-- ISA vector tables — system-level data (NOT in the ISA package)
-- ---------------------------------------------------------------------------

-- | The MCS-51 fixed interrupt vectors, in priority order: external 0, timer 0,
-- external 1, timer 1, serial.  Instantiate an 8051 controller with
-- @'staticIrqDef' statusOff 'mcs51IrqVectors' …@.
mcs51IrqVectors :: [Integer]
mcs51IrqVectors = [0x0003, 0x000B, 0x0013, 0x001B, 0x0023]
