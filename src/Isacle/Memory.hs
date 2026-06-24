module Isacle.Memory where

import Prelude (Bool)
import Hdl.Types (Sig)

-- | Synchronous single-port data RAM: read address in, write signals in, data out.
type RamUnit dom addr a =
       Sig dom addr
    -> Sig dom Bool             -- ^ write enable
    -> Sig dom addr             -- ^ write address
    -> Sig dom a                -- ^ write data
    -> Sig dom a                -- ^ read data out

-- | Read-only code ROM: address in, data out (combinational).
type RomUnit dom addr a = Sig dom addr -> Sig dom a
