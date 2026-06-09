module Core.Periph.GPIO
    ( GPIOState(..)
    , gpioUnit
    ) where

import Clash.Prelude

-- | Internal state of one GPIO port.
data GPIOState dat = GPIOState
    { gpioDdr  :: dat   -- data direction register (1 = output)
    , gpioPort :: dat   -- output latch
    } deriving (Generic, NFDataX, Show, Eq)

-- | Generic memory-mapped GPIO port with three consecutive registers:
--
--     base + 0  PIN   read-only  → sampled physical inputs
--     base + 1  DDR   read/write → data direction (1 = output)
--     base + 2  PORT  read/write → output latch
--
--   Parameterised over address (@addr@) and data (@dat@) types so the same
--   component works for 8-bit AVR buses, 32-bit RISC-V buses, etc.
--
--   Writes take effect on the rising edge; reads return the value written in
--   the same cycle, matching synchronous read-data latency.
--
--   Outputs:
--     rdData  – registered read result (feeds the CPU data-in bus)
--     portOut – PORT latch (connect to output-enabled pins)
--     ddrOut  – DDR register (1 = driven output, connect to tri-state enable)
gpioUnit
    :: forall dom addr dat
     . ( HiddenClockResetEnable dom
       , NFDataX dat, Num dat
       , Eq addr, Num addr
       )
    => addr                                      -- base address
    -> Signal dom dat                            -- physical pin inputs
    -> Signal dom (Maybe addr)                   -- data bus read address
    -> Signal dom (Maybe (addr, dat))            -- data bus write
    -> ( Signal dom dat                          -- read data (registered)
       , Signal dom dat                          -- PORT output latch
       , Signal dom dat                          -- DDR (output enable)
       )
gpioUnit base pinsIn rdAddr wr = (rdData, portOut, ddrOut)
  where
    step (GPIOState ddr port) (pins, mrd, mwr) =
        let (ddr', port') = case mwr of
                Just (a, v)
                    | a == base + 1 -> (v,   port)
                    | a == base + 2 -> (ddr, v   )
                _                   -> (ddr, port)
            rd = case mrd of
                Just a
                    | a == base     -> pins
                    | a == base + 1 -> ddr'
                    | a == base + 2 -> port'
                _                   -> 0
        in (GPIOState ddr' port', (rd, port', ddr'))

    out     = mealy step (GPIOState 0 0) (bundle (pinsIn, rdAddr, wr))
    rdData  = fmap (\(r, _, _) -> r) out
    portOut = fmap (\(_, p, _) -> p) out
    ddrOut  = fmap (\(_, _, d) -> d) out
