module Main where

import Prelude
import Numeric (showHex)

import Isacle.Hdl.Net   (DomId(..), ClockEdge(..), ResetPolarity(..))
import Isacle.Hdl.Types (KnownDom(..), Sig(..))
import Isacle.Hdl.Prim  (Unsigned)
import Isacle.System.SystemDSL
import Isacle.System.Generate (sysExtractMemoryMap, sysGenCHeader)

data Clk
instance KnownDom Clk where
    domId _ = DomId { domName = "clk", domFreqHz = 12000000
                    , domEdge = Rising, domReset = ActiveHigh }

mySystem :: (Sig Clk Bool, Sig Clk (Unsigned 8))
         -> SysDSL Clk (Unsigned 8)
                   (Sig Clk Bool, Sig Clk (Unsigned 8), Sig Clk (Unsigned 8))
mySystem (uart0Rx, gpio0In) = do
    uart0 <- createUart "uart0" uart0Rx
    gpio0 <- createGpio "gpio0" gpio0In

    ((uart0Tx, gpio0Port, gpio0Ddr), _rdData) <- createBus "databus" $ do
        (tx, _rxIrq, _txIrq) <- attachPeripheral 0x100 uart0
        (port, ddr)           <- attachPeripheral 0x300 gpio0
        return (tx, port, ddr)

    return (uart0Tx, gpio0Port, gpio0Ddr)

main :: IO ()
main = do
    let rxPin  = SExpr (pure 0) :: Sig Clk Bool
        gpioIn = SExpr (pure 0) :: Sig Clk (Unsigned 8)
        (_outputs, _nodes, doc) = runSystemDSL (mySystem (rxPin, gpioIn))

    putStrLn "=== Memory Map ==="
    putStr (sysExtractMemoryMap doc)

    putStrLn "=== C Header ==="
    putStr (sysGenCHeader "memmap" doc)

    putStrLn "=== Bus entries ==="
    mapM_ (\bs -> mapM_ (\pe -> putStrLn (bsName bs ++ "  0x" ++ showHex (peBase pe) "  " ++ peName pe))
                        (bsEntries bs))
          (sdBuses doc)

    let entryCount = sum (map (length . bsEntries) (sdBuses doc))
    putStrLn $ "\nTotal peripherals: " ++ show entryCount
    if entryCount == 2
        then putStrLn "PASS"
        else putStrLn "FAIL: expected 2 peripherals"
