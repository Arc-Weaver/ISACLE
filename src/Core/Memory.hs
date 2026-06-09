module Core.Memory where

import Clash.Explicit.Prelude

-- | Synchronous single-port data RAM: read address in, write pair in, data out.
type RamUnit dom addr a = Signal dom addr -> Signal dom (Maybe (addr, a)) -> Signal dom a

-- | Synchronous read-only code ROM: address in, data out.
type RomUnit dom addr a = Signal dom addr -> Signal dom a
