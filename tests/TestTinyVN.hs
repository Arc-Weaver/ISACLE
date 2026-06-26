{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}
-- | Smoke test: synthesise the TinyVN CPU as a standalone unit and emit VHDL.
module Main where

import Prelude
import System.Exit (exitFailure)
import qualified Data.Map.Strict as Map
import Hdl.Net (DomId(..), ClockEdge(..), ResetPolarity(..), runNetM)
import Hdl.Types (KnownDom(..))
import Hdl.Emit.Vhdl (emitVhdl)
import Isacle.ISA.Backend.SynthVnCPU (synthVonNeumannCPU)
import Isacle.ISA.Example.TinyVN (tinyVnCPUDef, tinyVnISA)

data Sys

instance KnownDom Sys where
    domId _ = DomId "sys" 10000000 Rising ActiveHigh "rst"

main :: IO ()
main = do
    putStrLn "=== TinyVN synthesis smoke test ==="

    let ((), nodes, _) = runNetM
            (synthVonNeumannCPU @Sys @32 @32 tinyVnCPUDef tinyVnISA)

    putStrLn $ "Netlist: " ++ show (length nodes) ++ " nodes"
    if null nodes
        then do putStrLn "FAIL: empty netlist"; exitFailure
        else putStrLn "OK: netlist non-empty"

    let vhdl = emitVhdl Map.empty "tiny_vn" nodes
    putStrLn $ "VHDL: " ++ show (length vhdl) ++ " chars"
    if null vhdl
        then do putStrLn "FAIL: empty VHDL"; exitFailure
        else putStrLn "OK: VHDL non-empty"

    putStrLn "PASS"
