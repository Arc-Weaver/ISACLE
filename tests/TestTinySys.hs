{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE BinaryLiterals   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Prelude
import qualified Data.Map.Strict as Map
import System.Directory (createDirectoryIfMissing)

import Hdl.Net   (DomId(..), ClockEdge(..), ResetPolarity(..))
import Hdl.Sig (KnownDom(..), Sig(..))
import Hdl.Prim  (Unsigned)
import Hdl.Emit.Vhdl (emitVhdlDesignFiles)
import Isacle.System.SystemDSL
import Isacle.ISA.Chip (Chip(..))
import Isacle.ISA.Example.Tiny

data Sys
instance KnownDom Sys where
    domId _ = DomId "sys" 50000000 Rising ActiveHigh "rst"

-- Program: build GPIO PORT address (0x62), write 5, loop.
-- GPIO base = 0x60; PORT register is at base+2 = 0x62.
-- LDI max immediate is 15, so we build 96 via repeated doubling.
tinyChip :: Chip TinyCore TinyAlu 8 8 8 8
tinyChip = Chip tinyCPUDef tinyISA

prog :: [Integer]
prog = [ 0x46   -- 0: LDI r0, 6    -- r0 = 6
       , 0x10   -- 1: ADD r0, r0   -- r0 = 12
       , 0x10   -- 2: ADD r0, r0   -- r0 = 24
       , 0x10   -- 3: ADD r0, r0   -- r0 = 48
       , 0x10   -- 4: ADD r0, r0   -- r0 = 96 = 0x60 (GPIO base)
       , 0x52   -- 5: LDI r1, 2    -- r1 = 2
       , 0x14   -- 6: ADD r0, r1   -- r0 = 98 = 0x62 (PORT register)
       , 0x55   -- 7: LDI r1, 5    -- r1 = 5 (value to write)
       , 0x34   -- 8: ST [r0], r1  -- mem[0x62] = 5  → GPIO PORT = 5
       , 0xC9   -- 9: JMP 9        -- halt (infinite self-loop)
       ]

mySystem :: SysDSL ()
mySystem = do
    gpio <- createGpio "gpio0" (0 :: Sig Sys (Unsigned 8))
    coderom <- createRom 256 (RomImage prog :: RomImage (Unsigned 8)) "coderom0"
    (codeBus, ()) <- createBus @SimpleBus "codebus" $ do
        _ <- attachPeripheral 0x0 coderom
        return ()
    (dataBus, ()) <- createBus @SimpleBus "databus" $ do
        _ <- attachPeripheral 0x60 gpio
        return ()
    dormant <- noIrq
    createHarvardCPU "cpu0" tinyChip codeBus dataBus dormant

assert :: String -> Bool -> IO ()
assert msg False = putStrLn ("FAIL: " ++ msg)
assert msg True  = putStrLn ("ok:   " ++ msg)

main :: IO ()
main = do
    let outDir = "build/tiny_sys"
    createDirectoryIfMissing True outDir

    let design = execSystemDSL "tiny_sys" mySystem
    emitVhdlDesignFiles outDir design

    putStrLn "=== TinySys synthesis ==="
    assert "design non-empty"        (not (Map.null design))
    assert "top entity present"      (Map.member "tiny_sys" design)
    assert "gpio sub-entity present" (any ("gpio0" `elem`) [Map.keys design])
    putStrLn $ "Entities: " ++ show (Map.keys design)
    putStrLn $ "VHDL written to " ++ outDir
