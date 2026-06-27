module Main where

import Prelude

import qualified Tests.Isacle.Harvard.ISA      as ISA
import qualified Tests.Isacle.ISA.WidthCheck   as WidthCheck
import qualified Tests.Isacle.Harvard.Pipeline as Pipeline
import qualified Tests.Isacle.GPIO             as GPIO
import qualified Tests.Isacle.Periph.Timer     as Timer
import qualified Tests.Isacle.Periph.UART      as UART
import qualified Tests.Isacle.Periph.DMA       as DMA
import qualified Tests.Isacle.System.Bus       as Bus
import qualified Tests.Isacle.Layout    as Layout
import qualified Tests.Isacle.Sim              as Sim

main :: IO ()
main = do
    putStrLn "=== ISACLE unit tests ==="
    putStrLn "\n=== Harvard ISA ==="
    ISA.runIsaTests
    putStrLn "\n=== Harvard Pipeline ==="
    Pipeline.runPipelineTests
    putStrLn "\n=== GPIO peripheral ==="
    GPIO.runGpioTests
    putStrLn "\n=== Timer peripheral ==="
    Timer.runTimerTests
    putStrLn "\n=== UART peripheral ==="
    UART.runUartTests
    putStrLn "\n=== DMA engine ==="
    DMA.runDmaTests
    putStrLn "\n=== System Bus DSL ==="
    Bus.runBusTests
    putStrLn "\n=== Address-mapping layout ==="
    Layout.runLayoutTests
    putStrLn "\n=== ISA width check ==="
    WidthCheck.runWidthCheckTests
    putStrLn "\n=== Simulation interpreter ==="
    Sim.runSimTests
    putStrLn "\n=== all tests passed ==="
