{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- NB: NoImplicitPrelude is active from cabal common-options.
-- | Bus architectures (protocols): how a bus behaves, expressed entirely on the
-- 'Signal' class — no @NetM@.  Given the master's request and each child's
-- response, 'synthBus' is a /pure function/ producing the master's response and
-- each child's request: address decode is a 'Signal' compare, the read mux is
-- 'mux'.  The same layout can be realised under different protocols (SimpleBus,
-- Wishbone, …) by swapping the architecture.
module Isacle.System.BusArch
    ( -- * Architectures
      BusArch(..)
    , SimpleBus(..)
    , BurstBus(..)
      -- * The protocol-agnostic transaction signals
    , MasterReq(..)
    , SlaveResp(..)
    , BusChild
    ) where

import Prelude
import Data.Proxy (Proxy(..))
import GHC.TypeLits (natVal)
import Hdl.Types
    ( Signal, HdlType(..), HdlOrd, KnownDom
    , mux, (.<.), (.&&.), sigNot, sigLitW )
import Isacle.System.BusCap (Capability(..))

-- ---------------------------------------------------------------------------
-- Transaction signals — the master/slave halves of a single-outstanding bus
-- ---------------------------------------------------------------------------

-- | The request a master drives toward a slave (valid when @mqReq@ is high).
data MasterReq s dom addr dat = MasterReq
    { mqReq   :: s dom Bool   -- ^ a transaction is requested this cycle
    , mqWe    :: s dom Bool   -- ^ 1 = write, 0 = read
    , mqAddr  :: s dom addr   -- ^ transaction address
    , mqWData :: s dom dat    -- ^ write data (meaningful when @mqWe@)
    }

-- | The response a slave drives back to its master.
data SlaveResp s dom dat = SlaveResp
    { srRData :: s dom dat    -- ^ read data (valid when the transaction completes)
    , srStall :: s dom Bool   -- ^ 1 = not yet complete; the master must hold
    }

-- | A child placed on a bus: its base address, window size (bytes), and the
-- response it drives.  Base/size come from the layout ("Isacle.System.BusDef").
type BusChild s dom dat = (Integer, Integer, SlaveResp s dom dat)

-- ---------------------------------------------------------------------------
-- BusArch — the protocol
-- ---------------------------------------------------------------------------

-- | A bus architecture (protocol).  @synthBus arch master children@ is a pure
-- function: from the master's request and each child's response it produces the
-- master's response and each child's request.  No @NetM@ — just 'Signal' ops.
class BusArch arch where
    type Cap arch :: Capability
    type Cap arch = 'NonStalling

    -- | The protocol's canonical value, so a bus can realise @synthBus@ from just
    -- the /type/ @proto@ (pinned by the master's 'BusHandle').  Protocols are
    -- singletons (no fields), so this is the sole inhabitant.
    busArch :: arch

    synthBus :: (Signal s, HdlType addr, HdlOrd addr, HdlType dat, KnownDom dom)
             => arch
             -> MasterReq s dom addr dat
             -> [BusChild s dom dat]
             -> (SlaveResp s dom dat, [MasterReq s dom addr dat])
    synthBus _ _ _ = error "BusArch.synthBus: not implemented for this architecture"

-- ---------------------------------------------------------------------------
-- SimpleBus — combinational, single-master, no stall
-- ---------------------------------------------------------------------------

-- | A simple synchronous memory-mapped bus: single master, combinational
-- address decode, no stalling, no bursts.
data SimpleBus = SimpleBus

instance BusArch SimpleBus where
    busArch = SimpleBus
    synthBus _ master children =
        ( SlaveResp { srRData = readData, srStall = sigLitW 0 1 }
        , map childReq children )
      where
        -- each child sees a request only when its address window is selected
        selOf (base, size, _) = inWindow (mqAddr master) base size
        childReq c@(_, _, _) = master { mqReq = mqReq master .&&. selOf c }
        -- read data: the selected child's response, else zero
        readData = foldr (\c acc -> mux (selOf c) (rdataOf c) acc)
                         (sigLitW 0 (dataWidth master)) children
        rdataOf (_, _, resp) = srRData resp

-- | @addr@ in @[base, base+size)@ — as a 'Signal' Bool (combinational decode).
inWindow :: forall s dom addr. (Signal s, HdlType addr, HdlOrd addr)
         => s dom addr -> Integer -> Integer -> s dom Bool
inWindow addr base size =
    sigNot (addr .<. lit base) .&&. (addr .<. lit (base + size))
  where
    w       = fromIntegral (natVal (Proxy @(Width addr)))
    lit v   = sigLitW v w :: s dom addr

-- | The bus data width, read off the master's write-data signal's type.
dataWidth :: forall s dom addr dat. HdlType dat => MasterReq s dom addr dat -> Int
dataWidth _ = fromIntegral (natVal (Proxy @(Width dat)))

-- ---------------------------------------------------------------------------
-- BurstBus — burst-capable system bus (interconnect not yet implemented)
-- ---------------------------------------------------------------------------

-- | A burst-capable synchronous bus for cache-line refill (stalling-capable).
data BurstBus = BurstBus

instance BusArch BurstBus where
    type Cap BurstBus = 'Stalling
    busArch = BurstBus
    -- synthBus uses the default (error) until the burst interconnect lands.
