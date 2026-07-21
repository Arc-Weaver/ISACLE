-- | Minimal SoC example: Tiny Harvard CPU + code ROM + GPIO.
--
-- The whole file is one import and one 'systemMain' call — the system is built
-- inline and handed straight to the CLI, which parses argv and generates the
-- requested artifacts.
--
-- The program is loaded from a binary file at generation time via 'loadFileBytes'
-- (a deferred read the interpreter resolves) + 'romFromBytes'.  See TinySocTH.hs
-- for the compile-time '$(romBin8 ...)' variant.
--
-- Run it:
--
-- > cabal build isacle
-- > cabal exec runghc examples/TinySoc.hs -- --out build/tiny_soc
-- > cabal exec runghc examples/TinySoc.hs -- --print --vhdl-only
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveAnyClass      #-}
module Main where

import GHC.Generics (Generic)
import Isacle.System.CLI
import Isacle.System.HdlCircuit (GpioPhys(..))
import Isacle.ISA.Chip          (Chip(..))
import Isacle.ISA.Example.Tiny  (TinyCore, TinyAlu, tinyCPUDef, tinyISA)

-- The Tiny CPU wired to its ISA.
tinyChip :: Chip TinyCore TinyAlu 8 8 8 8
tinyChip = Chip tinyCPUDef tinyISA

main :: IO ()
main = systemMain "tiny_soc" tinySoc

-- The SoC's output pins: the GPIO PORT/DDR, named by the record fields.
data TinySocOut = TinySocOut
    { gpio_port :: Sig Sys (Unsigned 8)
    , gpio_ddr  :: Sig Sys (Unsigned 8)
    } deriving (Generic, Named)

-- The SoC is a top-level entity: its input pin @gpio_in@ is the argument bundle
-- and its output pins are the returned 'TinySocOut' — both bound by the entity
-- flow.  'attachPeripheral' returns the GPIO's physical outputs, which we thread
-- into the output record.
tinySoc :: Port "gpio_in" (Sig Sys (Unsigned 8)) -> SysNet TinySocOut
tinySoc (Port gpioIn) = do
    -- Deferred file read: the SysNet only *records* the request; the interpreter
    -- (systemMain) reads examples/prog.bin and re-runs with its bytes available.
    progBytes <- loadFileBytes "examples/prog.bin"
    gpio0   <- createGpio "gpio0" gpioIn
    coderom <- createRom 256 (romFromBytes progBytes :: RomImage (Unsigned 8)) "coderom0"
    (codeBus, ()) <- createBus @_ @SimpleBus "codebus" (attachPeripheral 0x0  coderom >> pure ())
    (dataBus, gpioOut) <- createBus @_ @SimpleBus "databus" (attachPeripheral 0x60 gpio0)
    dormant <- noIrq
    createHarvardCPU "cpu0" tinyChip codeBus dataBus dormant
    pure TinySocOut { gpio_port = gpioPort gpioOut, gpio_ddr = gpioDdr gpioOut }
