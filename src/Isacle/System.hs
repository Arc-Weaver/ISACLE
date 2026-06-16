-- | ISACLE SoC composition DSL.
--
-- Layers:
--
--   1. "Isacle.System.Periph"    — peripheral register definitions ('PeriphDef')
--   2. "Isacle.System.SystemDSL" — bus building and peripheral wiring
--      ('SystemDSL', 'createBus', 'attachPeripheral', 'createUart', etc.)
--   3. "Isacle.System.BusDef"    — address-space layout ('BusDef', 'attach')
--   4. "Isacle.System.Generate"  — artifact generators (C header, linker script)
module Isacle.System
    ( module Isacle.System.Spec
    , module Isacle.System.Periph
    , module Isacle.System.BusDef
    , module Isacle.System.BusArch
    , module Isacle.System.SystemDSL
    , module Isacle.System.Generate
    ) where

import Isacle.System.Spec
import Isacle.System.Periph
import Isacle.System.BusDef
import Isacle.System.BusArch
import Isacle.System.SystemDSL
import Isacle.System.Generate
