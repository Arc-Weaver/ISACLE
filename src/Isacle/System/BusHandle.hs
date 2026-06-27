-- | Bus master interface handle, shared between the system DSL and the cache.
module Isacle.System.BusHandle
    ( BusHandle(..)
    ) where

import Prelude
import GHC.TypeLits (Nat)
import Hdl.Net (WireId)

-- | Wire handles for one bus.
--
-- The master (CPU in a Harvard design, L1 cache in a VN design) drives the
-- write/read address and data wires; the aggregated peripheral read data comes
-- back on @bhRdData@.
--
-- A bus is /just wires/ — raw @std_logic_vector@ bits with no interpretation.
-- Signed-vs-unsigned (and any richer meaning) is imposed by the /peripherals/
-- that read those bits, not by the bus, so the phantom parameters carry only
-- wire counts, never a representation:
--
--   * @addrW@ — bus address width (bits), as a type-level 'Nat'.  A master
--     whose address width exceeds this is a compile error (@addrW <= busAddrW@).
--   * @dataW@ — bus data width (bits).  A master/peripheral whose data width
--     does not match the bus is a compile error.
--
-- The @Int@ fields below mirror these type-level widths as runtime values for
-- the @WireId@-based interconnect core; 'Isacle.System.SystemDSL.createBus'
-- populates them from the type, so they are guaranteed consistent.
data BusHandle (addrW :: Nat) (dataW :: Nat) = BusHandle
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
