{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Main where

import Prelude
import System.Directory (createDirectoryIfMissing)

import Hdl.Types
import Hdl.Net (DomId(..), ClockEdge(..), ResetPolarity(..), execDesign)
import Hdl.Emit.Vhdl
import Isacle.ISA.Backend.SynthCPU (synthHarvardCPU)
import Isacle.ISA.Example.Tiny

data Sys

instance KnownDom Sys where
    domId _ = DomId "sys" 50000000 Rising ActiveHigh "rst"

main :: IO ()
main = do
    let outDir = "build/tiny_cpu"
    createDirectoryIfMissing True outDir
    let design = execDesign "tiny_cpu" $
            synthHarvardCPU @Sys @8 @8 @8 @8 tinyCPUDef tinyISA
    emitVhdlDesignFiles outDir design
    putStrLn $ "TinyCPU synthesis done — VHDL written to " ++ outDir
