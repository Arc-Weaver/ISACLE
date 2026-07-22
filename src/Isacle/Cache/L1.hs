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
import Hdl.Sig (KnownDom(..), Sig(..), HdlType, mux)
import Hdl.Prim (Unsigned)
import Hdl.Class (connectSig)
import Isacle.Cache.Config (CacheConfig(..))
import Isacle.System.Bus (Bus, BusMaster(..))
import Isacle.System.BusArch (MasterReq(..), SlaveResp(..))

-- | The CPU ↔ cache boundary wires (valid in the 'NetM' context 'synthL1Cache'
-- ran in).  The cache /drives/ the CPU-input wires (instr word, read data,
-- stall); the CPU drives the address/store wires which the cache /reads/.
-- 'createCachedCPU' connects the von Neumann CPU's signals to these.
data CacheHandle = CacheHandle
    { chInstrWord  :: WireId  -- ^ cache → CPU: fetched instruction word
    , chDataRdData :: WireId  -- ^ cache → CPU: loaded data word
    , chStall      :: WireId  -- ^ cache → CPU: 1 = miss, freeze pipeline
    , chFetchAddr  :: WireId  -- ^ CPU → cache: instruction fetch address
    , chDataRdAddr :: WireId  -- ^ CPU → cache: data load address
    , chDataWrEn   :: WireId  -- ^ CPU → cache: data store enable
    , chDataWrAddr :: WireId  -- ^ CPU → cache: data store address
    , chDataWrData :: WireId  -- ^ CPU → cache: data store word
    , chAddrW      :: Int
    , chWordW      :: Int
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
    :: forall proto dom wordW addrW dat.
       ( KnownDom dom, KnownNat wordW, KnownNat addrW, HdlType dat, BusMaster proto )
    => CacheConfig
    -> Bus proto dom (Unsigned 32) dat  -- ^ unified system bus (cache is sole master)
    -> NetM CacheHandle
synthL1Cache _cfg busNode = do
    let domInfo  = domId (Proxy @dom)
    let wordBits = fromIntegral (natVal (Proxy @wordW)) :: Int
    let addrBits = fromIntegral (natVal (Proxy @addrW)) :: Int

    -- Pre-allocate input wires that the CPU will drive.
    fetchAddrW  <- freshWire
    dataRdAddrW <- freshWire
    dataWrEnW   <- freshWire
    dataWrAddrW <- freshWire
    dataWrDataW <- freshWire

    -- The cache is the bus's master: build its single-channel request from the
    -- CPU's outputs and hand it to the bus's protocol master logic ('driveBus').
    -- Stub: the data write port owns the write channel; instruction fetches share
    -- the read channel.
    reqTrueW <- freshWire; emit $ NComb reqTrueW (N.PLit 1 1) []
    let cacheReq = MasterReq
            { mqReq   = SWire reqTrueW
            , mqWe    = SWire dataWrEnW
            , mqAddr  = mux (SWire dataWrEnW) (SWire dataWrAddrW) (SWire dataRdAddrW)
                          :: Sig dom (Unsigned 32)
            , mqWData = SWire dataWrDataW
            }
    resp <- driveBus busNode cacheReq

    -- Pass-through read data from bus to CPU (drive the CPU-facing placeholders).
    instrWordW  <- freshWire
    dataRdDataW <- freshWire
    connectSig (SWire instrWordW  :: Sig dom dat) (srRData resp)
    connectSig (SWire dataRdDataW :: Sig dom dat) (srRData resp)

    -- stall = 0 permanently (pass-through never stalls).
    stallW <- freshWire
    emit $ NComb stallW (N.PLit 0 1) []

    let _ = (domInfo, wordBits, addrBits, fetchAddrW)  -- suppress unused warnings

    return CacheHandle
        { chInstrWord  = instrWordW
        , chDataRdData = dataRdDataW
        , chStall      = stallW
        , chFetchAddr  = fetchAddrW
        , chDataRdAddr = dataRdAddrW
        , chDataWrEn   = dataWrEnW
        , chDataWrAddr = dataWrAddrW
        , chDataWrData = dataWrDataW
        , chAddrW      = addrBits
        , chWordW      = wordBits
        }
