-- | Minimal peripheral example: a UART and a GPIO on one data bus.
--
-- Demonstrates the inline 'systemMain' style with register-mapped peripherals
-- and no CPU.  Input pins live in the argument record and output pins in the
-- returned record; 'attachPeripheral' returns each peripheral's physical outputs,
-- which are threaded into the output bundle (no ad-hoc promotion).
--
-- Run it:
--
-- > cabal build isacle
-- > cabal exec runghc examples/GpioUart.hs -- --out build/gpio_uart
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveAnyClass      #-}
module Main where

import GHC.Generics (Generic)
import Isacle.System.CLI
import Isacle.System.HdlCircuit (UartPhys(..), GpioPhys(..))

main :: IO ()
main = systemMain "gpio_uart" gpioUart

data GpioUartIn = GpioUartIn
    { uart_rx :: Sig Sys Bool          -- RX serial line
    , gpio_in :: Sig Sys (Unsigned 8)  -- input pins
    } deriving (Generic, Named)

data GpioUartOut = GpioUartOut
    { uart_tx   :: Sig Sys Bool          -- TX serial line
    , gpio_port :: Sig Sys (Unsigned 8)  -- driven output value
    , gpio_ddr  :: Sig Sys (Unsigned 8)  -- direction
    } deriving (Generic, Named)

gpioUart :: GpioUartIn -> SysNet GpioUartOut
gpioUart GpioUartIn{ uart_rx = uartRx, gpio_in = gpioIn } = do
    uart0 <- createUart "uart0" uartRx
    gpio0 <- createGpio "gpio0" gpioIn
    (_dataBus, (uartOut, gpioOut)) <- createBus @_ @SimpleBus "databus" $ do
        u <- attachPeripheral 0x100 uart0
        g <- attachPeripheral 0x300 gpio0
        pure (u, g)
    pure GpioUartOut { uart_tx   = uartTxLine uartOut
                     , gpio_port = gpioPort   gpioOut
                     , gpio_ddr  = gpioDdr    gpioOut }
