-- | Bus master interface handle, shared between the system DSL and the cache.
module Isacle.System.BusHandle
    ( BusHandle(..)
    ) where

import Prelude
import Hdl.Types (Sig)

-- | The master interface of one bus, as typed signals at the HDL layer (never
-- raw @WireId@s).  The master (CPU in a Harvard design, L1 cache in a VN design)
-- drives the write/read address and data; the aggregated peripheral read data
-- and the stall come back from the fabric.
--
-- Widths and representation now live in the signal /types/ (@addr@/@dat@), not in
-- runtime @Int@ fields — a master whose widths don't match the bus is a compile
-- error.  @bhStall@ is held high while the data transaction is outstanding (read
-- data not valid / write not accepted); the CPU holds all architectural state
-- while it is high, regardless of the physical cause (cache miss, latency, …).
data BusHandle dom addr dat = BusHandle
    { bhWrAddr :: Sig dom addr   -- ^ master → fabric: write address
    , bhWrData :: Sig dom dat    -- ^ master → fabric: write data
    , bhWrEn   :: Sig dom Bool   -- ^ master → fabric: write enable
    , bhRdAddr :: Sig dom addr   -- ^ master → fabric: read address
    , bhRdData :: Sig dom dat    -- ^ fabric → master: aggregated read data
    , bhStall  :: Sig dom Bool   -- ^ fabric → master: 1 while the txn is outstanding
    }
