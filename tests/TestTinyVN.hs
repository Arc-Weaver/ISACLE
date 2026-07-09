{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Smoke test: synthesise the TinyVN CPU as a standalone unit and emit VHDL.
module Main where

import Prelude
import System.Exit (exitFailure)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (natVal)
import qualified Data.Map.Strict as Map
import Hdl.Net (DomId(..), ClockEdge(..), ResetPolarity(..), runNetM, NetM, freshWire, emit, NetNode(..))
import Hdl.Types (KnownDom(..), Sig(..), materialize)
import Hdl.Emit.Vhdl (emitVhdl)
import Isacle.ISA.Backend.SynthVnCPU (synthVonNeumannCPU', VnMemIface(..))
import Isacle.ISA.Example.TinyVN (tinyVnCPUDef, tinyVnISA, TinyVnCore)

data Sys

instance KnownDom Sys where
    domId _ = DomId "sys" 10000000 Rising ActiveHigh "rst"

-- | Standalone top-level entity: create the VN CPU's input\/output ports.
tinyVnTop :: NetM ()
tinyVnTop = do
    let domInfo = domId (Proxy @Sys)
        w = fromIntegral (natVal (Proxy @32)) :: Int
        inPort :: forall a. String -> Int -> NetM (Sig Sys a)
        inPort name bits = do { wid <- freshWire; emit (NInput wid name bits domInfo); pure (SWire wid) }
        outPort :: forall a. String -> Int -> Sig Sys a -> NetM ()
        outPort name bits sig = do { wid <- materialize sig; emit (NOutput wid name bits domInfo) }
    instrW <- inPort "instr_word"   w
    dmemRd <- inPort "data_rd_data" w
    stall  <- inPort "stall"        1
    irqP   <- inPort "irq_pending"  1
    irqV   <- inPort "irq_vector"   w
    vmi <- synthVonNeumannCPU' @TinyVnCore @Sig @NetM @Sys @32 @32
               tinyVnCPUDef tinyVnISA instrW dmemRd stall irqP irqV
    outPort "fetch_addr"   (vniAddrW vmi) (vniFetchAddr  vmi)
    outPort "data_rd_addr" (vniAddrW vmi) (vniDataRdAddr vmi)
    outPort "data_wr_en"   1              (vniDataWrEn   vmi)
    outPort "data_wr_addr" (vniAddrW vmi) (vniDataWrAddr vmi)
    outPort "data_wr_data" (vniWordW vmi) (vniDataWrData vmi)

main :: IO ()
main = do
    putStrLn "=== TinyVN synthesis smoke test ==="

    let ((), nodes, _) = runNetM tinyVnTop

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
