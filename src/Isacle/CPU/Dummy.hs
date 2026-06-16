module Isacle.CPU.Dummy
    ( DummyCore(..)
    ) where

import Prelude

-- | A phantom CPU type for documentation in 'SystemDSL' SoC descriptions.
--   Pass the string "DummyCore" as the CPU type to 'createSimpleHarvard'
--   when no real ISA core is needed for an elaboration or test run.
--
--   ISA-specific packages (clavr for AVR, cl51 for MCS-51) define concrete
--   CPU implementations that wire their bus master signals into the netlist.
data DummyCore = DummyCore
