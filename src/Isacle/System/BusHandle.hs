-- | Bus master interface handle, shared between the system DSL and the cache.
module Isacle.System.BusHandle
    ( BusHandle(..)
    ) where

import Prelude
import Hdl.Net (WireId)

-- | Wire handles for one bus.
--
-- The master (CPU in a Harvard design, L1 cache in a VN design) drives the
-- write/read address and data wires; the aggregated peripheral read data comes
-- back on @bhRdData@.
data BusHandle = BusHandle
    { bhWrAddr :: WireId  -- ^ driven by bus master: write address
    , bhWrData :: WireId  -- ^ driven by bus master: write data
    , bhWrEn   :: WireId  -- ^ driven by bus master: write enable
    , bhRdAddr :: WireId  -- ^ driven by bus master: read address
    , bhRdData :: WireId  -- ^ driven by bus fabric: aggregated read data
    , bhStall  :: WireId  -- ^ driven by bus fabric: 1 while the current data
                          --   transaction is outstanding (read data not yet
                          --   valid / write not yet accepted).  The CPU holds
                          --   all architectural state while this is high.  The
                          --   physical origin of the stall (cache miss, bus
                          --   backpressure, read latency) is the bus type's
                          --   concern; the CPU only sees this abstract signal.
    , bhAddrW  :: Int     -- ^ address width in bits
    , bhDataW  :: Int     -- ^ data width in bits
    }
