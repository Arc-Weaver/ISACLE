{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | The one bus abstraction: a __peripheral handle__, which /is/ a bus.
--
-- There is no master-vs-slave handle split.  A leaf peripheral and a whole
-- assembled address map are the same thing — a 'Bus' node carrying its
-- signalling protocol @proto@ in its type.  'Isacle.System.SystemDSL.createBus'
-- lays sub-peripherals out into a wider 'Bus'; the CPU is simply /handed/ a 'Bus'
-- and drives it.
--
-- The protocol lives in the type, and the /master logic is generated from it/:
-- 'driveBus' is a class method keyed on @proto@, so handing a 'SimpleBus'
-- peripheral to a CPU emits combinational simple-master wiring, while handing a
-- (future) @Wishbone@ peripheral emits Wishbone-master handshake logic — same CPU
-- call site, different generated master, chosen by the peripheral's type.
module Isacle.System.Bus
    ( Bus(..)
    , BusMaster(..)
    ) where

import Prelude
import Data.Kind (Type)

import Hdl.Types (Sig, HdlType, KnownDom)
import Hdl.Net   (NetM)
import Hdl.Class (connectSig)
import Isacle.System.BusArch (BusArch, SimpleBus, MasterReq(..), SlaveResp(..))

-- | A bus = a peripheral handle.  It presents one face: a root request it
-- consumes (@busReq@, forward-declared placeholders the driving master fills in)
-- and the aggregated response it produces (@busResp@).  @proto@ is the signalling
-- protocol the whole node speaks.
data Bus (proto :: Type) dom addr dat = Bus
    { busName :: String
    , busReq  :: MasterReq Sig dom addr dat  -- ^ root request placeholders (driven by the master)
    , busResp :: SlaveResp Sig dom dat        -- ^ aggregated response (a function of 'busReq')
    }

-- | A master capable of driving a @proto@ bus.  The instance /is/ the protocol's
-- master logic: given the peripheral handle and the master's single-channel
-- request, it wires the handshake and yields the master-facing response.  A CPU
-- driving a bus does so through this method, so the generated master matches the
-- bus it was handed — the whole point of putting @proto@ in the peripheral type.
class BusArch proto => BusMaster proto where
    driveBus :: (KnownDom dom, HdlType addr, HdlType dat)
             => Bus proto dom addr dat
             -> MasterReq Sig dom addr dat        -- ^ the master's request
             -> NetM (SlaveResp Sig dom dat)      -- ^ master-facing response

-- | 'SimpleBus': combinational single master.  There is no handshake, so driving
-- it is just connecting the master's request straight into the node's root
-- placeholders and handing back the combinational response.
instance BusMaster SimpleBus where
    driveBus bus req = do
        connectSig (mqReq   (busReq bus)) (mqReq   req)
        connectSig (mqWe    (busReq bus)) (mqWe    req)
        connectSig (mqAddr  (busReq bus)) (mqAddr  req)
        connectSig (mqWData (busReq bus)) (mqWData req)
        pure (busResp bus)
