module Isacle.Bus
    ( busReadMux
    , busMasterMux
    ) where

import Prelude
import Isacle.Hdl.Types (Sig, mux)
import Isacle.Hdl.Prim (Unsigned)

-- | Combine read-data outputs from N memory-mapped peripherals by sum.
--   Valid when address ranges are non-overlapping: each peripheral returns 0
--   for addresses outside its range, so at most one non-zero response exists
--   per cycle and addition correctly selects it.
busReadMux :: (Num dat, Num (Sig dom dat)) => [Sig dom dat] -> Sig dom dat
busReadMux = sum

-- | Select between two bus masters (e.g. CPU and DMA) driving the same
--   write-address/write-data/write-enable/read-address bus signals.
--   When @busy@ is True the DMA signals are routed to the bus;
--   otherwise the CPU signals are routed.
busMasterMux
    :: Sig dom Bool
    -> (Sig dom (Unsigned 32), Sig dom dat, Sig dom Bool, Sig dom (Unsigned 32))  -- ^ CPU (wrAddr, wrData, wrEn, rdAddr)
    -> (Sig dom (Unsigned 32), Sig dom dat, Sig dom Bool, Sig dom (Unsigned 32))  -- ^ DMA (wrAddr, wrData, wrEn, rdAddr)
    -> (Sig dom (Unsigned 32), Sig dom dat, Sig dom Bool, Sig dom (Unsigned 32))
busMasterMux busy (cpuWrA, cpuWrD, cpuWrE, cpuRdA) (dmaWrA, dmaWrD, dmaWrE, dmaRdA) =
    ( mux busy dmaWrA cpuWrA
    , mux busy dmaWrD cpuWrD
    , mux busy dmaWrE cpuWrE
    , mux busy dmaRdA cpuRdA
    )
