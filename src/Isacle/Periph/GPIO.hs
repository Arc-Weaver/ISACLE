module Isacle.Periph.GPIO
    ( -- * Peripheral kind tag
      GPIO
      -- * PeriphDef description (single source of truth)
    , gpioDef
      -- * Standalone circuit wrapper
    , gpioUnit
    ) where

import Prelude
import Data.Word (Word32)
import Hdl.Types (KnownDom, HdlType, Sig)
import Hdl.Prim  (Unsigned)
import Isacle.System.Periph
import Isacle.System.HdlCircuit (hdlOps, hdlBusIface)

-- ---------------------------------------------------------------------------
-- Peripheral kind tag
-- ---------------------------------------------------------------------------

data GPIO

-- ---------------------------------------------------------------------------
-- Register map description — single source of truth
-- ---------------------------------------------------------------------------

-- | GPIO register map.
--
--   offset 0  PIN   read-only   sampled physical inputs
--   offset 1  DDR   read/write  data direction (1 = output)
--   offset 2  PORT  read/write  output latch
--
-- @pinsIn@ is the current physical pin-input signal.
-- Returns @(PORT latch signal, DDR signal)@.
gpioDef
    :: (Num dat)
    => sig dat                                    -- ^ physical pin inputs
    -> PeriphDef GPIO sig dat (sig dat, sig dat)  -- ^ (PORT output, DDR output)
gpioDef pinsIn = do
    -- Typed PE2 combinators: each register's name, offset and type are
    -- single-sourced, and its read/write logic is wired in the same call.
    roField  @(Unsigned 8) 0 "PIN"  "Sampled physical inputs"      pinsIn
    ddr  <- regField @(Unsigned 8) 1 "DDR"  "Data direction (1 = output)" 0
    port <- regField @(Unsigned 8) 2 "PORT" "Output latch"               0
    return (port, ddr)

-- ---------------------------------------------------------------------------
-- Standalone circuit wrapper
-- ---------------------------------------------------------------------------

-- | Memory-mapped GPIO port built from 'gpioDef'.
--   Accepts raw bus signals and returns (rdData, PORT latch, DDR).
--   For use via 'attachPeripheral' in 'SysDSL' prefer 'createGpio'.
gpioUnit
    :: (KnownDom dom, HdlType dat, Num dat, Num (Sig dom dat))
    => Word32                          -- ^ peripheral base address
    -> Sig dom dat                     -- ^ physical pin inputs
    -> Sig dom (Unsigned 32)           -- ^ bus write address
    -> Sig dom dat                     -- ^ bus write data
    -> Sig dom Bool                    -- ^ bus write enable
    -> Sig dom (Unsigned 32)           -- ^ bus read address
    -> (Sig dom dat, Sig dom dat, Sig dom dat)  -- ^ (rdData, PORT, DDR)
gpioUnit base pinsIn wrAddr wrData wrEn rdAddr =
    let bus              = hdlBusIface wrAddr wrData wrEn rdAddr base
        ((port, ddr), rdData, _spec) = runPeriphDef hdlOps bus (gpioDef pinsIn)
    in (rdData, port, ddr)
