module Isacle.ALU where

import Prelude (Maybe)

-- | Generic record-based CPU ALU interface.
--
--   This is a lightweight composition helper for cores that want to pass an
--   ALU implementation around as data. The Harvard pipeline uses the
--   typeclass-based interface in 'Isacle.Harvard.ISA'; both abstractions are
--   kept here because they support different styles of ISA-independent core
--   assembly.
--
--   Each field is a pure function from instruction + state to effect:
--     read    — which data RAM address to read before compute (if any)
--     compute — how the instruction transforms the CPU state
--     write   — which data RAM address/value to write after compute (if any)
--     jump    — where the program counter goes next (Nothing = sequential)
--
--   The four-way split matches the structural stages of most load-store CPUs
--   and is the key composition point: swap any field independently to build a
--   different CPU variant without touching the pipeline.
data ALU instr state ramaddr romaddr val = ALU
    { read    :: instr -> state -> Maybe ramaddr
    , compute :: instr -> Maybe val -> state -> state
    , write   :: instr -> state -> Maybe (ramaddr, val)
    , jump    :: instr -> state -> Maybe romaddr
    }
