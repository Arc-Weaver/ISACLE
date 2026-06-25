{-# LANGUAGE AllowAmbiguousTypes #-}
module Isacle.ISA.VonNeumann where

import Prelude
import GHC.TypeLits (Nat)

import Isacle.Harvard.ISA (ALU(..), FlushEvent(..), StallEvent(..))

-- ---------------------------------------------------------------------------
-- Von Neumann ISA interface
-- ---------------------------------------------------------------------------

-- | Pipeline-visible qualities for a von Neumann-architecture ISA.
--   Extends 'ALU' with a unified address space: the program counter lives in
--   the same @RamAddr@ domain as data.  There is no separate code bus, so
--   @readCode@ is not available — a VN ISA cannot be wired to a Harvard
--   synthesis function (compile-time error).
--
--   @FetchWord state@ — the unit delivered by the unified bus each cycle.
--     - 'BitVector' 32 for RV32I
--     - 'BitVector' 32 for SPARC V8
--
--   @MaxFetch state@ — static upper bound on instruction width in FetchWords.
--     - 1 for fixed-width 32-bit ISAs (RV32I, SPARC V8)
class ALU state => VonNeumannISA state where
    -- | Code-bus unit type (same bus as data on a VN machine).
    type FetchWord state

    -- | Static upper bound on instruction width in 'FetchWord' units.
    type MaxFetch  state :: Nat

    -- | Where the PC goes next.  Nothing = sequential (PC + instrFetch).
    --   Returns a data-address because the program counter lives in the
    --   unified address space.
    move :: Instr state -> state -> Maybe (RamAddr state)

    -- | Pipeline stages this instruction occupies. 1 = single-cycle.
    latency :: Instr state -> Int
    latency _ = 1

    -- | How many 'FetchWord' units this instruction occupies.  Default: 1.
    instrFetch :: Instr state -> Int
    instrFetch _ = 1

    -- | True when the CPU can accept an interrupt at the next instruction
    --   boundary.
    interruptible :: state -> Bool

    -- | Accept a pending interrupt.  The backend handles return-address save.
    acceptIrq :: state -> RamAddr state -> state

-- ---------------------------------------------------------------------------
-- Flush for Von Neumann pipelines
-- ---------------------------------------------------------------------------

-- | Flush and stall condition detection for von Neumann pipelines.
class (VonNeumannISA state, Eq (RamAddr state)) => HasFlushVN state where

    -- | Default: flush iff @move@ is taken.
    flushCondition
        :: Instr state
        -> state
        -> Maybe (FlushEvent (RamAddr state))
    flushCondition instr s = FlushBranch <$> move instr s

    -- | Default: stall iff write address equals read address.
    stallCondition
        :: RamAddr state
        -> RamAddr state
        -> Maybe (StallEvent (RamAddr state))
    stallCondition wa ra
        | wa == ra  = Just (StallReadAfterWrite ra)
        | otherwise = Nothing

-- ---------------------------------------------------------------------------
-- Optional: delay slot (SPARC V8 style)
-- ---------------------------------------------------------------------------

-- | ISAs that execute one instruction in the delay slot after every taken
--   branch before the redirect takes effect.  SPARC V8 is the canonical
--   example; the pipeline must advance one extra instruction before acting
--   on a flush from @flushCondition@.
class VonNeumannISA state => HasDelaySlot state
