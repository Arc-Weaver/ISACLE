module Isacle.Periph.GPIO
    ( -- * Peripheral kind tag
      GPIO
      -- * PeriphDef description (single source of truth)
    , gpioDef
      -- * Standalone circuit wrapper
    , gpioUnit
    ) where

import Prelude hiding (read)
import Data.Word (Word32)
import Hdl.Sig (KnownDom, HdlType, Sig)
import Hdl.Prim  (Unsigned)
import Isacle.System.Periph
import Isacle.System.HdlCircuit (hdlOps, runPeriphNet, hdlBusIface)

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
    :: (Num dat, HdlType dat, Monad m)
    => sig dat                                    -- ^ physical pin inputs
    -> PeriphDef GPIO sig m dat (sig dat, sig dat)  -- ^ (PORT output, DDR output)
gpioDef pinsIn = do
    -- PIN: read-only, driven by the sampled physical inputs (offset 0).
    roField @(Unsigned 8) 0 "PIN" "Sampled physical inputs" pinsIn
    -- DDR / PORT: read-write registers built with the PE3 handle API — declare
    -- the register (its width/type come from @\@8@), then wire its write side and
    -- read-back as separate actions.  @writeAction@ hands back a typed
    -- @sig (Unsigned 8)@; the read-back here just echoes it (a plain RW register),
    -- but 'liftHdl' logic could sit between 'writeAction' and 'readAction'.
    ddr  <- declareRegVector @8 "DDR"
    ddrV <- write ddr
    read ddr ddrV
    port <- declareRegVector @8 "PORT"
    portV <- write port
    read port portV
    -- Reinterpret the typed register values back to the bus data width for the
    -- physical output bundle the system wires generically.
    (,) <$> toBusData portV <*> toBusData ddrV

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
        ((port, ddr), rdData, _spec) = runPeriphNet hdlOps bus (gpioDef pinsIn)
    in (rdData, port, ddr)
