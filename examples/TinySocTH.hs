-- | Same SoC as TinySoc.hs, but the program is embedded at __compile time__ via
-- Template Haskell (@$(romBin8 ...)@) instead of a deferred runtime read.  The
-- file is registered as a build dependency, so editing prog.bin retriggers
-- compilation.
--
-- > cabal build isacle
-- > cabal exec runghc examples/TinySocTH.hs -- --out build/tiny_soc_th
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TemplateHaskell     #-}
module Main where

import Isacle.System.CLI
import Isacle.ISA.Chip          (Chip(..))
import Isacle.ISA.Example.Tiny  (TinyCore, TinyAlu, tinyCPUDef, tinyISA)

tinyChip :: Chip TinyCore TinyAlu 8 8 8 8
tinyChip = Chip tinyCPUDef tinyISA

main :: IO ()
main = systemMain "tiny_soc_th" $ do
    gpioIn  <- sysInput "gpio_in" :: SysDSL (Sig Sys (Unsigned 8))
    gpio    <- createGpio "gpio0" gpioIn
    coderom <- createRom 256 ($(romBin8 "examples/prog.bin") :: RomImage (Unsigned 8)) "coderom0"
    (codeBus, ()) <- createBus @SimpleBus "codebus" (attachPeripheral 0x0  coderom >> pure ())
    (dataBus, ()) <- createBus @SimpleBus "databus" (attachPeripheral 0x60 gpio    >> pure ())
    dormant <- noIrq
    createHarvardCPU "cpu0" tinyChip codeBus dataBus dormant
