-- | System-level DSL for the isacle-hdl backend.
--
-- Provides an API close to the ExampleAPI target:
--
-- @
-- mySystem (uart0Rx, gpio0In) = runSystemDSL $ do
--     uart0 <- createUart "uart0" uart0Rx
--     gpio0 <- createGpio "gpio0" gpio0In
--
--     ((uart0Tx, gpio0Port, gpio0Ddr), _rdData) <- createBus "databus" $ do
--         (tx, _rxIrq, _txIrq) <- attachPeripheral 0x100 uart0
--         (port, ddr)           <- attachPeripheral 0x300 gpio0
--         return (tx, port, ddr)
--
--     return (uart0Tx, gpio0Port, gpio0Ddr)
-- @
--
-- 'attachPeripheral' is generic: the output type is determined by the token
-- type, so GHC infers the right thing without any cast.
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Isacle.System.SystemDSL
    ( -- * System-level monad
      SysDSL
    , runSystemDSL
      -- * Peripheral tokens (opaque; carry name + PeriphDef)
    , PeriphToken
      -- * Bus sub-monad
    , BusDSL
    , attachPeripheral
      -- * System-level operations
    , createBus
    , createSimpleVectorIrq
    , createSimpleHarvard
      -- * Peripheral constructors
    , createUart
    , createGpio
    , createTimer
    , createRam
    , createRom
      -- * System documentation
    , SysDoc(..)
    , BusSection(..)
    , PeriphEntry(..)
    ) where

import Prelude
import Data.Word (Word32)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class (lift)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (natVal)

import Isacle.Hdl.Net
import Isacle.Hdl.Types
import Isacle.Hdl.Prim (Unsigned)
import Isacle.System.Periph
import Isacle.System.HdlCircuit (hdlOps, hdlBusIface)
import Isacle.Periph.GPIO  (gpioDef, GPIO)
import Isacle.Periph.UART  (uartDef, UART)
import Isacle.Periph.Timer (timerDef, Timer)

-- ---------------------------------------------------------------------------
-- System documentation types
-- ---------------------------------------------------------------------------

-- | One peripheral in a bus section.
data PeriphEntry = PeriphEntry
    { peName :: String
    , peBase :: Word32
    , peSpec :: PeriphSpec
    } deriving (Show)

-- | A named memory bus and its attached peripherals.
data BusSection = BusSection
    { bsName    :: String
    , bsEntries :: [PeriphEntry]
    } deriving (Show)

-- | Top-level system documentation produced by 'runSystemDSL'.
data SysDoc = SysDoc
    { sdBuses :: [BusSection]
    , sdCPUs  :: [String]
    } deriving (Show)

emptySysDoc :: SysDoc
emptySysDoc = SysDoc [] []

-- ---------------------------------------------------------------------------
-- PeriphToken
-- ---------------------------------------------------------------------------

-- | Opaque handle produced by 'createUart' / 'createGpio' / 'createTimer'.
--
-- @a@ is the concrete physical-output type specific to the peripheral:
--
--   * UART  → @(Sig dom Bool, Sig dom Bool, Sig dom Bool)@ (TX, rxIrq, txIrq)
--   * GPIO  → @(Sig dom dat, Sig dom dat)@                 (PORT, DDR)
--   * Timer → @(Sig dom Bool, Sig dom Bool)@               (ovf, cmp)
--
-- 'attachPeripheral' is generic over @a@, so the right output type is
-- inferred from the token without any explicit annotation.
data PeriphToken p dom dat a = PeriphToken
    { ptName :: String
    , ptDef  :: PeriphDef p (Sig dom) dat a
    }

-- ---------------------------------------------------------------------------
-- SysDSL monad
-- ---------------------------------------------------------------------------

-- | System-level monad.  Wraps 'NetM' with accumulated 'SysDoc'.
newtype SysDSL dom dat a = SysDSL (StateT SysDoc NetM a)
    deriving newtype (Functor, Applicative, Monad)

-- | Run a system description, returning the user result, the flat 'NetNode'
-- list (for VHDL emission), and the system documentation.
runSystemDSL :: SysDSL dom dat a -> (a, [NetNode], SysDoc)
runSystemDSL (SysDSL st) = (a, nodes, doc)
  where
    ((a, doc), nodes, _design) = runNetM (runStateT st emptySysDoc)

-- ---------------------------------------------------------------------------
-- BusDSL monad
-- ---------------------------------------------------------------------------

data BusDSLState dom dat = BusDSLState
    { bdsWrAddr  :: Sig dom (Unsigned 32)
    , bdsWrData  :: Sig dom dat
    , bdsWrEn    :: Sig dom Bool
    , bdsRdAddr  :: Sig dom (Unsigned 32)
    , bdsRdData  :: Sig dom dat
    , bdsPeriph  :: [PeriphEntry]
    }

-- | Bus sub-monad; execute 'attachPeripheral' calls inside 'createBus'.
newtype BusDSL dom dat a = BusDSL (StateT (BusDSLState dom dat) NetM a)
    deriving newtype (Functor, Applicative, Monad)

-- | Attach a peripheral at @base@.
--
-- Evaluates the peripheral's 'PeriphDef' against the enclosing bus signals,
-- contributes its read-data to the bus mux, records it in the bus map, and
-- returns the peripheral's physical outputs.  The output type @a@ is
-- determined by the token, so no annotation is needed:
--
-- @
-- (tx, rxIrq, txIrq) <- attachPeripheral 0x100 uart0   -- UART token
-- (port, ddr)         <- attachPeripheral 0x300 gpio0   -- GPIO token
-- @
attachPeripheral
    :: (KnownDom dom, HdlType dat, Num dat, Num (Sig dom dat))
    => Word32
    -> PeriphToken p dom dat a
    -> BusDSL dom dat a
attachPeripheral base token = BusDSL $ do
    st <- get
    let bus              = hdlBusIface (bdsWrAddr st) (bdsWrData st) (bdsWrEn st) (bdsRdAddr st) base
        (phys, rd, spec) = runPeriphDef hdlOps bus (ptDef token)
        entry            = PeriphEntry { peName = ptName token, peBase = base, peSpec = spec }
    put st { bdsRdData = bdsRdData st + rd
           , bdsPeriph = bdsPeriph st ++ [entry]
           }
    pure phys

-- ---------------------------------------------------------------------------
-- createBus
-- ---------------------------------------------------------------------------

-- | Build a named bus.
--
-- Allocates four input port wires (write address, write data, write enable,
-- read address) representing the bus master interface, threads them through
-- the 'BusDSL' sub-block, and emits the combined read-data as an output port.
-- Returns the user result and the read-data signal.
--
-- @
-- ((tx, port), rdData) <- createBus "databus" $ do
--     (tx, _, _) <- attachPeripheral 0x100 uartTok
--     (port, _)  <- attachPeripheral 0x300 gpioTok
--     return (tx, port)
-- @
createBus
    :: forall dom dat a.
       (KnownDom dom, HdlType dat, Num dat, Num (Sig dom dat))
    => String
    -> BusDSL dom dat a
    -> SysDSL dom dat (a, Sig dom dat)
createBus busName (BusDSL busSt) = SysDSL $ do
    (wrAddr, wrData, wrEn, rdAddr) <- lift allocBusPorts
    let initSt = BusDSLState
            { bdsWrAddr = SWire wrAddr
            , bdsWrData = SWire wrData
            , bdsWrEn   = SWire wrEn
            , bdsRdAddr = SWire rdAddr
            , bdsRdData = 0
            , bdsPeriph = []
            }
    (a, finalSt) <- lift $ runStateT busSt initSt
    lift $ emitRdOutput (bdsRdData finalSt)
    let section = BusSection { bsName = busName, bsEntries = bdsPeriph finalSt }
    modify $ \doc -> doc { sdBuses = sdBuses doc ++ [section] }
    pure (a, bdsRdData finalSt)
  where
    datW  = fromIntegral (natVal (Proxy @(Width dat)))
    addrW = fromIntegral (natVal (Proxy @(Width (Unsigned 32))))
    dom   = domId (Proxy @dom)

    allocBusPorts :: NetM (WireId, WireId, WireId, WireId)
    allocBusPorts = do
        wa <- freshWire
        emit $ NInput wa (busName ++ "_wr_addr") addrW dom
        wd <- freshWire
        emit $ NInput wd (busName ++ "_wr_data") datW dom
        we <- freshWire
        emit $ NInput we (busName ++ "_wr_en") 1 dom
        ra <- freshWire
        emit $ NInput ra (busName ++ "_rd_addr") addrW dom
        pure (wa, wd, we, ra)

    emitRdOutput :: Sig dom dat -> NetM ()
    emitRdOutput sig = do
        wid <- materialize sig
        emit $ NOutput wid (busName ++ "_rd_data") datW dom

-- ---------------------------------------------------------------------------
-- IRQ controller
-- ---------------------------------------------------------------------------

-- | Build a simple priority-encoder IRQ vector signal.
-- Sources are in priority order (head = highest priority).
createSimpleVectorIrq
    :: [(Sig dom Bool, Word32)]
    -> SysDSL dom dat (Sig dom (Maybe (Unsigned 32)))
createSimpleVectorIrq _sources = pure $
    -- Stub: permanent Nothing (no interrupt).  Full implementation
    -- requires an IRQ combiner expressed as a NetNode sub-circuit.
    SExpr $ do
        out <- freshWire
        emit $ NComb out (PLit 0 33) []
        pure out

-- ---------------------------------------------------------------------------
-- CPU
-- ---------------------------------------------------------------------------

-- | Record a Harvard CPU in the system documentation.
createSimpleHarvard
    :: String                            -- ^ CPU instance name
    -> String                            -- ^ CPU type / ISA name
    -> Sig dom (Maybe (Unsigned 32))     -- ^ IRQ vector signal (unused stub)
    -> (a, Sig dom dat)                  -- ^ code bus handle (from createBus)
    -> (b, Sig dom dat)                  -- ^ data bus handle (from createBus)
    -> SysDSL dom dat ()
createSimpleHarvard instName cpuType _irqs _codeBus _dataBus = SysDSL $
    modify $ \doc -> doc { sdCPUs = sdCPUs doc ++ [instName ++ " (" ++ cpuType ++ ")"] }

-- ---------------------------------------------------------------------------
-- Peripheral constructors
-- ---------------------------------------------------------------------------

-- | Create a UART peripheral token.
-- Register interface (UDR, USR, UBRR) is fully wired.
-- Physical outputs (TX, rxIrq, txIrq) are stubs until a serial-FSM
-- sub-component is implemented.
createUart
    :: (Num dat, Num (Sig dom dat))
    => String
    -> Sig dom Bool                  -- ^ RX serial line (reserved for FSM)
    -> SysDSL dom dat (PeriphToken UART dom dat (Sig dom Bool, Sig dom Bool, Sig dom Bool))
createUart name _rxPin = pure $ PeriphToken
    { ptName = name
    , ptDef  = uartDef 0 0 >> return (sigFalse, sigFalse, sigFalse)
    }

-- | Create a GPIO peripheral token.
-- Fully implemented: DDR, PORT, and PIN registers all synthesize correctly.
createGpio
    :: (Num dat, Num (Sig dom dat))
    => String
    -> Sig dom dat                   -- ^ input pin bus
    -> SysDSL dom dat (PeriphToken GPIO dom dat (Sig dom dat, Sig dom dat))
createGpio name pins = pure $ PeriphToken
    { ptName = name
    , ptDef  = gpioDef pins
    }

-- | Create a Timer peripheral token.
-- Physical outputs (overflow IRQ, compare-match IRQ) are stubs until a
-- counter-FSM sub-component is implemented.
createTimer
    :: (Num dat, Num (Sig dom dat))
    => String
    -> Sig dom Bool                  -- ^ tick / count-enable (reserved for FSM)
    -> SysDSL dom dat (PeriphToken Timer dom dat (Sig dom Bool, Sig dom Bool))
createTimer name _tick = pure $ PeriphToken
    { ptName = name
    , ptDef  = timerDef 0 >> return (sigFalse, sigFalse)
    }

-- | Create a synchronous block RAM peripheral token.
-- Attach with @attachPeripheral base ram0@; the RAM occupies @size@ entries
-- starting at @base@.
createRam
    :: (Num dat, Num (Sig dom dat))
    => Int          -- ^ number of addressable entries
    -> [Integer]    -- ^ initial contents (padded to @size@ with 0)
    -> String       -- ^ instance name
    -> SysDSL dom dat (PeriphToken RAM dom dat ())
createRam size initVals name = pure $ PeriphToken
    { ptName = name
    , ptDef  = blockRamDef size initVals
    }

-- | Create a read-only ROM peripheral token.
-- Attach with @attachPeripheral base rom0@; the ROM occupies @size@ entries
-- starting at @base@.
createRom
    :: (Num dat, Num (Sig dom dat))
    => Int
    -> [Integer]
    -> String
    -> SysDSL dom dat (PeriphToken ROM dom dat ())
createRom size initVals name = pure $ PeriphToken
    { ptName = name
    , ptDef  = blockRomDef size initVals
    }

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Constant-false Bool signal (1-bit zero literal).
sigFalse :: Sig dom Bool
sigFalse = SExpr $ do
    out <- freshWire
    emit $ NComb out (PLit 0 1) []
    pure out
