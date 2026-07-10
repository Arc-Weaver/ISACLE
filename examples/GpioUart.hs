-- | Minimal peripheral example: a UART and a GPIO on one data bus.
--
-- Demonstrates the inline 'systemMain' style with register-mapped peripherals
-- and no CPU.  The peripheral physical outputs (UART TX, GPIO PORT/DDR) are
-- promoted to top-level ports automatically by 'attachPeripheral'.
--
-- Run it:
--
-- > cabal build isacle
-- > cabal exec runghc examples/GpioUart.hs -- --out build/gpio_uart
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
module Main where

import Isacle.System.CLI

main :: IO ()
main = systemMain "gpio_uart" $ do
    uart0 <- createUart "uart0" sigFalse                    -- RX tied low
    gpio0 <- createGpio "gpio0" (0 :: Sig Sys (Unsigned 8)) -- input pins tied to 0
    (_dataBus, ()) <- createBus @SimpleBus "databus" $ do
        _ <- attachPeripheral 0x100 uart0
        _ <- attachPeripheral 0x300 gpio0
        pure ()
    pure ()
