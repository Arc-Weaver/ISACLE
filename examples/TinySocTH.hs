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
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveAnyClass      #-}
module Main where

import GHC.Generics (Generic)
import Isacle.System.CLI
import Isacle.System.HdlCircuit (GpioPhys(..))
import Isacle.ISA.Chip          (Chip(..))
import Isacle.ISA.Example.Tiny  (TinyCore, TinyAlu, tinyCPUDef, tinyISA)

tinyChip :: Chip TinyCore TinyAlu 8 8 8 8
tinyChip = Chip tinyCPUDef tinyISA

main :: IO ()
main = systemMain "tiny_soc_th" tinySoc

data TinySocOut = TinySocOut
    { gpio_port :: Sig Sys (Unsigned 8)
    , gpio_ddr  :: Sig Sys (Unsigned 8)
    } deriving (Generic, Named)

-- Single input pin @gpio_in@ in, GPIO PORT/DDR out — both bound by the entity flow.
tinySoc :: Port "gpio_in" (Sig Sys (Unsigned 8)) -> SysNet TinySocOut
tinySoc (Port gpioIn) = do
    gpio0   <- createGpio "gpio0" gpioIn
    coderom <- createRom 256 ($(romBin8 "examples/prog.bin") :: RomImage (Unsigned 8)) "coderom0"
    (codeBus, ()) <- createBus @_ @SimpleBus "codebus" (attachPeripheral 0x0  coderom >> pure ())
    (dataBus, gpioOut) <- createBus @_ @SimpleBus "databus" (attachPeripheral 0x60 gpio0)
    dormant <- noIrq
    createHarvardCPU "cpu0" tinyChip codeBus dataBus dormant
    pure TinySocOut { gpio_port = gpioPort gpioOut, gpio_ddr = gpioDdr gpioOut }
