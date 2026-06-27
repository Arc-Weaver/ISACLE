-- | L1 cache synthesis and CPU↔cache interface handle.
--
-- = Design
--
-- The L1 cache sits between the CPU (two-port view: separate I-fetch and
-- D-access) and the unified system bus (single-port).  On a miss it stalls
-- the CPU via 'VnMemIface.vniStall' and issues a burst refill on the bus.
--
-- = Implementation status
--
-- This module currently provides:
--
--   * The 'CacheHandle' type (returned by 'createL1Cache' in the SystemDSL)
--   * A stub 'synthL1Cache' emitting a pass-through circuit (no actual tags
--     or data arrays).  The pass-through connects CPU fetch/data ports directly
--     to the system bus and holds stall=0 permanently.
--
-- A full set-associative implementation with tag/data SRAM arrays, miss queue,
-- and burst refill engine will replace the stub once the VN synthesis path is
-- validated end-to-end.
module Isacle.Cache.L1
    ( CacheHandle(..)
    , synthL1Cache
    ) where

import Prelude
import Data.Proxy (Proxy(..))
import GHC.TypeLits (natVal, KnownNat)

import Hdl.Net
import qualified Hdl.Net as N
import Hdl.Types (KnownDom(..))
import Hdl.Prim (Unsigned)
import Isacle.Cache.Config (CacheConfig(..))
import Isacle.ISA.Backend.SynthVnCPU (VnMemIface(..))
import Isacle.System.BusHandle (BusHandle(..))

-- | Opaque handle returned by 'synthL1Cache' / 'createL1Cache'.
--
-- Pass to 'createCachedCPU' to connect the CPU's two-port interface to the
-- cache output.  The wire IDs embedded here are valid in the 'NetM' context
-- in which 'synthL1Cache' was called.
newtype CacheHandle = CacheHandle
    { chVnIface :: VnMemIface
      -- ^ Wire IDs for the CPU ↔ cache signals.  The CPU drives the address
      --   outputs; the cache drives the data inputs and the stall signal.
    }

-- | Synthesise an L1 cache that bridges a CPU two-port interface to a
-- single-port system bus.
--
-- __Stub implementation__: the cache is a pass-through.  Every access is
-- treated as a miss that is immediately resolved in the same cycle — i.e.
-- the cache just forwards addresses to the bus and returns bus read data
-- directly to the CPU without buffering.  @stall@ is permanently 0.
--
-- This is synthesis-correct: the resulting VHDL compiles and simulates but
-- has no caching effect.  Replace the body of this function with a real
-- tag/data-array implementation to add actual caching.
synthL1Cache
    :: forall dom wordW addrW busAddrW dat.
       ( KnownDom dom, KnownNat wordW, KnownNat addrW )
    => CacheConfig
    -> BusHandle busAddrW dat  -- ^ unified system bus (cache acts as sole bus master)
    -> NetM CacheHandle
synthL1Cache _cfg busH = do
    let domInfo  = domId (Proxy @dom)
    let wordBits = fromIntegral (natVal (Proxy @wordW)) :: Int
    let addrBits = fromIntegral (natVal (Proxy @addrW)) :: Int

    -- Pre-allocate input wires that the CPU will drive.
    fetchAddrW  <- freshWire
    dataRdAddrW <- freshWire
    dataWrEnW   <- freshWire
    dataWrAddrW <- freshWire
    dataWrDataW <- freshWire

    -- Pass-through: forward CPU write signals to the system bus.
    emit $ NComb (bhWrAddr busH) N.POr [fetchAddrW, fetchAddrW]  -- instruction fetches use fetch addr
    emit $ NComb (bhWrEn   busH) N.POr [dataWrEnW,  dataWrEnW]
    emit $ NComb (bhWrAddr busH) N.POr [dataWrAddrW, dataWrAddrW]
    emit $ NComb (bhWrData busH) N.POr [dataWrDataW, dataWrDataW]
    emit $ NComb (bhRdAddr busH) N.POr [dataRdAddrW, dataRdAddrW]

    -- Pass-through read data from bus to CPU.
    instrWordW  <- freshWire
    dataRdDataW <- freshWire
    emit $ NComb instrWordW  N.POr [bhRdData busH, bhRdData busH]
    emit $ NComb dataRdDataW N.POr [bhRdData busH, bhRdData busH]

    -- stall = 0 permanently (pass-through never stalls).
    stallW <- freshWire
    emit $ NComb stallW (N.PLit 0 1) []

    let _ = (domInfo, wordBits, addrBits)  -- suppress unused warnings

    return $ CacheHandle $ VnMemIface
        { vniFetchAddr  = fetchAddrW
        , vniDataRdAddr = dataRdAddrW
        , vniDataWrEn   = dataWrEnW
        , vniDataWrAddr = dataWrAddrW
        , vniDataWrData = dataWrDataW
        , vniInstrWord  = instrWordW
        , vniDataRdData = dataRdDataW
        , vniStall      = stallW
        , vniAddrW      = addrBits
        , vniWordW      = wordBits
        }
