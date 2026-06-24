-- | System-level bus builder for the isacle-hdl backend.
--
-- Provides a monadic API for attaching peripherals to a bus at known base
-- addresses.  Each 'attachPeriph' call:
--
--   * runs the peripheral's 'PeriphDef' against the bus signals
--   * contributes its read-data signal to an accumulated bus read mux
--   * captures the 'PeriphSpec' for static bus-map analysis
--
-- Addresses are assigned eagerly and the full bus map is available as a
-- pure 'BusMap' value.
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Isacle.System.HdlSystem
    ( -- * Bus builder
      HdlSys
    , attachPeriph
    , buildHdlBus
      -- * Bus map
    , BusMap
    , BusEntry(..)
    ) where

import Prelude
import Data.Word (Word32)
import Control.Monad.Trans.State.Strict

import Hdl.Types (Sig, KnownDom, HdlType)
import Hdl.Prim (Unsigned)
import Isacle.System.Periph
import Isacle.System.HdlCircuit (hdlOps, hdlBusIface)

-- ---------------------------------------------------------------------------
-- Bus map — static result of bus construction
-- ---------------------------------------------------------------------------

-- | One entry in the assembled bus map.
data BusEntry = BusEntry
    { beBase :: Word32
    , beSpec :: PeriphSpec
    } deriving (Show)

-- | Ordered list of bus entries, suitable for documentation or validation.
type BusMap = [BusEntry]

-- ---------------------------------------------------------------------------
-- HdlSys monad
-- ---------------------------------------------------------------------------

data HdlSysState dom dat = HdlSysState
    { hssWrAddr :: Sig dom (Unsigned 32)
    , hssWrData :: Sig dom dat
    , hssWrEn   :: Sig dom Bool
    , hssRdAddr :: Sig dom (Unsigned 32)
    , hssRdData :: Sig dom dat          -- accumulated read mux (summed)
    , hssBusMap :: BusMap
    }

-- | Monadic context for building a bus from a collection of peripherals.
-- Carries the four bus signals through each 'attachPeriph' call and
-- accumulates the read-data mux and bus map.
newtype HdlSys dom dat a = HdlSys
    { _runHdlSys :: State (HdlSysState dom dat) a }
    deriving newtype (Functor, Applicative, Monad)

-- ---------------------------------------------------------------------------
-- attachPeriph
-- ---------------------------------------------------------------------------

-- | Attach a peripheral at @base@, returning its physical outputs and spec.
--
-- Internally, 'runPeriphDef' is called with 'hdlOps' and an 'hdlBusIface'
-- constructed from the bus signals and @base@.  The resulting read-data
-- signal is added to the bus accumulator via @(+)@ — valid because
-- non-overlapping peripherals output zero when not addressed.
attachPeriph
    :: forall p dom dat a.
       (KnownDom dom, HdlType dat, Num dat, Num (Sig dom dat))
    => Word32                            -- ^ peripheral base address
    -> PeriphDef p (Sig dom) dat a       -- ^ peripheral definition
    -> HdlSys dom dat (a, PeriphSpec)
attachPeriph base def = HdlSys $ do
    st <- get
    let bus              = hdlBusIface (hssWrAddr st) (hssWrData st) (hssWrEn st) (hssRdAddr st) base
        (physOut, rdData, spec) = runPeriphDef hdlOps bus def
        entry            = BusEntry { beBase = base, beSpec = spec }
    put st { hssRdData = hssRdData st + rdData
           , hssBusMap = hssBusMap st ++ [entry]
           }
    return (physOut, spec)

-- ---------------------------------------------------------------------------
-- buildHdlBus
-- ---------------------------------------------------------------------------

-- | Run an 'HdlSys' bus-builder action against four bus signals.
--
-- Returns the user result, the combined read-data signal, and the bus map.
-- The bus map can be inspected for address-range validation or documentation
-- without synthesis.
buildHdlBus
    :: (KnownDom dom, HdlType dat, Num dat, Num (Sig dom dat))
    => Sig dom (Unsigned 32)   -- ^ write address
    -> Sig dom dat             -- ^ write data
    -> Sig dom Bool            -- ^ write enable
    -> Sig dom (Unsigned 32)   -- ^ read address
    -> HdlSys dom dat a
    -> (a, Sig dom dat, BusMap)
buildHdlBus wrAddr wrData wrEn rdAddr (HdlSys sys) =
    let initSt = HdlSysState
            { hssWrAddr = wrAddr
            , hssWrData = wrData
            , hssWrEn   = wrEn
            , hssRdAddr = rdAddr
            , hssRdData = 0
            , hssBusMap = []
            }
        (a, finalSt) = runState sys initSt
    in (a, hssRdData finalSt, hssBusMap finalSt)
