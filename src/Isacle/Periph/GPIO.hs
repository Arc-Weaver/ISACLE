module Isacle.Periph.GPIO
    ( -- * Peripheral kind tag
      GPIO
      -- * PeriphDef description (single source of truth)
    , gpioDef
    ) where

import Prelude hiding (read)
import Hdl.Sig (HdlType)
import Hdl.Prim  (Unsigned)
import Isacle.System.Periph

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
