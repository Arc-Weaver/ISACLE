{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Main where

import Prelude
import Numeric (showHex)
import System.Exit (exitFailure)

import Hdl.Net   (DomId(..), ClockEdge(..), ResetPolarity(..))
import Hdl.Sig (KnownDom(..), Sig(..))
import Hdl.Prim  (Unsigned)
import Isacle.System.SystemDSL
import Isacle.System.HdlCircuit (GpioPhys(..), UartPhys(..))
import Isacle.System.Generate (sysExtractMemoryMap, sysGenCHeader)

assert :: String -> Bool -> IO ()
assert msg False = putStrLn ("FAIL: " ++ msg) >> exitFailure
assert msg True  = putStrLn ("ok:   " ++ msg)

-- ---------------------------------------------------------------------------
-- Test clock domain
-- ---------------------------------------------------------------------------

data Clk
instance KnownDom Clk where
    domId _ = DomId "clk" 12000000 Rising ActiveHigh "rst"

-- ---------------------------------------------------------------------------
-- System description: UART at 0x0100, GPIO at 0x0300
-- ---------------------------------------------------------------------------

mySystem
    :: Sig Clk Bool
    -> Sig Clk (Unsigned 8)
    -> SysNet
              (Sig Clk Bool, Sig Clk (Unsigned 8), Sig Clk (Unsigned 8))
mySystem rxPin gpioIn = do
    uart <- createUart "uart" rxPin
    gpio <- createGpio "gpio" gpioIn

    bh <- orphanBusMaster @SimpleBus
    (tx, port, ddr) <- createBus "databus" bh $ do
        uart' <- attachPeripheral 0x0100 uart
        gpio' <- attachPeripheral 0x0300 gpio
        return (uartTxLine uart', gpioPort gpio', gpioDdr gpio')

    return (tx, port, ddr)

-- ---------------------------------------------------------------------------
-- Main: print artifacts and run sanity checks
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
    let rxSig  = SExpr (pure 0) :: Sig Clk Bool
        gpioSig = SExpr (pure 0) :: Sig Clk (Unsigned 8)
        (_, _, doc) = runSystemDSL (mySystem rxSig gpioSig)

    putStrLn "=== Memory Map ==="
    putStr (sysExtractMemoryMap doc)

    putStrLn "\n=== C Header ==="
    putStr (sysGenCHeader "memmap" doc)

    putStrLn "\n=== Bus entries ==="
    mapM_ (\bs -> mapM_ (\pe ->
                putStrLn (bsName bs ++ "  0x" ++ showHex (peBase pe) "  " ++ peName pe))
            (bsEntries bs))
        (sdBuses doc)

    let entryCount = sum (map (length . bsEntries) (sdBuses doc))
    putStrLn $ "\nTotal peripherals: " ++ show entryCount

    assert "two peripherals registered" (entryCount == 2)
    putStrLn "PASS"
