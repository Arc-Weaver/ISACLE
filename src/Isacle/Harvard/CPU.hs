module Isacle.Harvard.CPU where

import Prelude
import Data.Maybe (fromMaybe)
import Isacle.Harvard.ISA

-- | Execute one decoded instruction using a 'HarvardISA', advancing the program counter.
--
--   The write address is computed from the PRE-compute state so that
--   post-increment/pre-decrement pointer updates in 'compute' do not
--   corrupt the write destination.
--
--   Returns @(new_state, maybe_write, next_pc)@.
runInstruction
    :: forall state
     . (HarvardISA state, Num (RomAddr state))
    => (state -> RomAddr state)             -- getPC
    -> (state -> RomAddr state -> state)    -- setPC
    -> (Instr state -> RomAddr state)       -- instruction size in fetch words
    -> Instr state
    -> Maybe (Val state)
    -> state
    -> (state, Maybe (RamAddr state, Val state), RomAddr state)
runInstruction getPC setPC instrSize instr mval s =
    let writeSpec = write instr s
        s'        = compute instr mval s
        nextPC    = fromMaybe (getPC s + instrSize instr) (move instr s')
    in (setPC s' nextPC, writeSpec, nextPC)
