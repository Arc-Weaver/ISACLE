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
module Main where

import Isacle.System.CLI
import Isacle.ISA.Chip          (Chip(..))
import Isacle.ISA.Example.Tiny  (TinyCore, TinyAlu, tinyCPUDef, tinyISA)

-- The Tiny CPU wired to its ISA.
tinyChip :: Chip TinyCore TinyAlu 8 8 8 8
tinyChip = Chip tinyCPUDef tinyISA

main :: IO ()
main = systemMain "tiny_soc" $ do
    -- Deferred file read: the SysDSL only *records* the request; the interpreter
    -- (systemMain) reads examples/prog.bin and re-runs with its bytes available.
    progBytes <- loadFileBytes "examples/prog.bin"
    gpioIn  <- sysInput "gpio_in" :: SysDSL (Sig Sys (Unsigned 8))
    gpio    <- createGpio "gpio0" gpioIn
    coderom <- createRom 256 (romFromBytes progBytes :: RomImage (Unsigned 8)) "coderom0"
    (codeBus, ()) <- createBus @SimpleBus "codebus" (attachPeripheral 0x0  coderom >> pure ())
    (dataBus, ()) <- createBus @SimpleBus "databus" (attachPeripheral 0x60 gpio    >> pure ())
    dormant <- noIrq
    createHarvardCPU "cpu0" tinyChip codeBus dataBus dormant
