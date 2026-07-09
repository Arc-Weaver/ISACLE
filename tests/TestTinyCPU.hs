{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Prelude
import Data.Proxy (Proxy(..))
import GHC.TypeLits (natVal)
import System.Directory (createDirectoryIfMissing)

import Hdl.Bits (Unsigned)
import Hdl.Types
import Hdl.Net (DomId(..), ClockEdge(..), ResetPolarity(..), execDesign, NetM, freshWire, emit, NetNode(..))
import Hdl.Emit.Vhdl
import Isacle.ISA.Backend.SynthCPU (synthHarvardCPU', CpuMemIface(..))
import Isacle.ISA.Example.Tiny

data Sys

instance KnownDom Sys where
    domId _ = DomId "sys" 50000000 Rising ActiveHigh "rst"

-- | Standalone top-level entity for the abstract 'synthHarvardCPU'': create the
-- CPU's input\/output ports (a von-Neumann-free smoke test of the VHDL emitter).
tinyTop :: NetM ()
tinyTop = do
    let domInfo = domId (Proxy @Sys)
        w = fromIntegral (natVal (Proxy @8)) :: Int
        inPort :: forall a. String -> Int -> NetM (Sig Sys a)
        inPort name bits = do { wid <- freshWire; emit (NInput wid name bits domInfo); pure (SWire wid) }
        outPort :: forall a. String -> Int -> Sig Sys a -> NetM ()
        outPort name bits sig = do { wid <- materialize sig; emit (NOutput wid name bits domInfo) }
    codeD  <- inPort "code_rd_data" w   -- single code read port
    dmemRd <- inPort "data_rd_data" w
    stall  <- inPort "stall"        1
    irqP   <- inPort "irq_pending"  1
    irqV   <- inPort "irq_vector"   w
    cmi <- synthHarvardCPU' @TinyCore @Sig @NetM @Sys @8 @8 @8 @8
               tinyCPUDef tinyISA codeD dmemRd stall irqP irqV
    outPort "code_rd_addr" (cmiCodeAddrW cmi) (cmiCodeRdAddr cmi)
    outPort "data_rd_addr" (cmiDataAddrW cmi) (cmiDataRdAddr cmi)
    outPort "data_wr_en"   1                  (cmiDataWrEn   cmi)
    outPort "data_wr_addr" (cmiDataAddrW cmi) (cmiDataWrAddr cmi)
    outPort "data_wr_data" (cmiWordW     cmi) (cmiDataWrData cmi)

main :: IO ()
main = do
    let outDir = "build/tiny_cpu"
    createDirectoryIfMissing True outDir
    let design = execDesign "tiny_cpu" tinyTop
    emitVhdlDesignFiles outDir design
    putStrLn $ "TinyCPU synthesis done — VHDL written to " ++ outDir
