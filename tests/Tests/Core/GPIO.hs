module Tests.Core.GPIO where

import Prelude

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.Hedgehog
import qualified Hedgehog as H

import qualified Clash.Prelude as C

import Core.Periph.GPIO

-- ---------------------------------------------------------------------------
-- Types and helpers
-- ---------------------------------------------------------------------------

type GAddr = C.Unsigned 16
type GData = C.Unsigned 8

gpioBase :: GAddr
gpioBase = 0x60   -- PIN=0x60, DDR=0x61, PORT=0x62

-- | Run gpioUnit for n cycles with given per-cycle inputs.
--   Returns (rdData, portOut, ddrOut) samples.
runGpio
    :: Int
    -> [GData]                      -- physical pin inputs
    -> [Maybe GAddr]                -- read addresses
    -> [Maybe (GAddr, GData)]       -- writes
    -> ([GData], [GData], [GData])
runGpio n pins rds wrs =
    let pad xs = xs ++ repeat (last xs)
        (rdSig, portSig, ddrSig) =
            C.withClockResetEnable (C.clockGen @C.System) C.resetGen C.enableGen
                (gpioUnit gpioBase
                    (C.fromList (pad (pins ++ [0])))
                    (C.fromList (pad (rds  ++ [Nothing])))
                    (C.fromList (pad (wrs  ++ [Nothing]))))
    in ( C.sampleN n rdSig
       , C.sampleN n portSig
       , C.sampleN n ddrSig
       )

-- ---------------------------------------------------------------------------
-- DDR tests
-- ---------------------------------------------------------------------------

-- Writing to the DDR register (base+1) must appear on ddrOut immediately.
prop_gpio_ddr_write_sets_direction :: H.Property
prop_gpio_ddr_write_sets_direction = H.withTests 1 . H.property $ do
    let (_, _, ddrOut) = runGpio 3
            [0, 0, 0]
            [Nothing, Nothing, Nothing]
            [Just (0x61, 0xFF), Nothing, Nothing]
    H.assert (0xFF `elem` ddrOut)

-- DDR value must persist when no further write is issued.
-- Write happens in cycle 1 (after the 1-cycle synchronous reset), then the
-- remaining samples must all carry the written value.
prop_gpio_ddr_persists_after_write :: H.Property
prop_gpio_ddr_persists_after_write = H.withTests 1 . H.property $ do
    let (_, _, ddrOut) = runGpio 6
            (repeat 0)
            (repeat Nothing)
            (Nothing : Just (0x61, 0xAA) : repeat Nothing)
    H.assert (all (== 0xAA) (drop 2 ddrOut))

-- Writing DDR must not alter the PORT latch.
prop_gpio_ddr_write_does_not_affect_port :: H.Property
prop_gpio_ddr_write_does_not_affect_port = H.withTests 1 . H.property $ do
    let (_, portOut, _) = runGpio 3
            (repeat 0)
            (repeat Nothing)
            [Just (0x61, 0xFF), Nothing, Nothing]
    H.assert (all (== 0) portOut)

-- ---------------------------------------------------------------------------
-- PORT tests
-- ---------------------------------------------------------------------------

prop_gpio_port_write_sets_latch :: H.Property
prop_gpio_port_write_sets_latch = H.withTests 1 . H.property $ do
    let (_, portOut, _) = runGpio 3
            (repeat 0)
            (repeat Nothing)
            [Just (0x62, 0x55), Nothing, Nothing]
    H.assert (0x55 `elem` portOut)

prop_gpio_port_persists_after_write :: H.Property
prop_gpio_port_persists_after_write = H.withTests 1 . H.property $ do
    let (_, portOut, _) = runGpio 6
            (repeat 0)
            (repeat Nothing)
            (Nothing : Just (0x62, 0xBB) : repeat Nothing)
    H.assert (all (== 0xBB) (drop 2 portOut))

-- Writing PORT must not alter DDR.
prop_gpio_port_write_does_not_affect_ddr :: H.Property
prop_gpio_port_write_does_not_affect_ddr = H.withTests 1 . H.property $ do
    let (_, _, ddrOut) = runGpio 3
            (repeat 0)
            (repeat Nothing)
            [Just (0x62, 0xFF), Nothing, Nothing]
    H.assert (all (== 0) ddrOut)

-- ---------------------------------------------------------------------------
-- Read tests
-- ---------------------------------------------------------------------------

-- Reading the PIN register (base+0) returns the current physical pin values.
prop_gpio_pin_read_returns_input :: H.Property
prop_gpio_pin_read_returns_input = H.withTests 1 . H.property $ do
    let (rdOut, _, _) = runGpio 2
            [0xAB, 0xAB]
            [Just 0x60, Just 0x60]
            [Nothing,   Nothing]
    H.assert (0xAB `elem` rdOut)

-- Reading DDR reflects the last written value.
-- Write in cycle 1 (post-reset), read in cycle 2+.
prop_gpio_read_ddr_after_write :: H.Property
prop_gpio_read_ddr_after_write = H.withTests 1 . H.property $ do
    let (rdOut, _, _) = runGpio 4
            (repeat 0)
            [Nothing, Nothing, Just 0x61, Just 0x61]
            [Nothing, Just (0x61, 0xCC), Nothing, Nothing]
    H.assert (0xCC `elem` rdOut)

-- Reading PORT reflects the last written value.
prop_gpio_read_port_after_write :: H.Property
prop_gpio_read_port_after_write = H.withTests 1 . H.property $ do
    let (rdOut, _, _) = runGpio 4
            (repeat 0)
            [Nothing, Nothing, Just 0x62, Just 0x62]
            [Nothing, Just (0x62, 0x77), Nothing, Nothing]
    H.assert (0x77 `elem` rdOut)

-- A write and read to the same register in the same cycle: the read sees the
-- newly written value (write-before-read within a cycle).
prop_gpio_write_read_same_cycle :: H.Property
prop_gpio_write_read_same_cycle = H.withTests 1 . H.property $ do
    let (rdOut, _, _) = runGpio 2
            (repeat 0)
            [Just 0x61, Nothing]           -- read DDR in cycle 0
            [Just (0x61, 0xF0), Nothing]   -- write DDR in cycle 0
    H.assert (0xF0 `elem` rdOut)

-- Reading an address outside [base, base+2] returns 0.
prop_gpio_unmapped_read_returns_zero :: H.Property
prop_gpio_unmapped_read_returns_zero = H.withTests 1 . H.property $ do
    let (rdOut, _, _) = runGpio 2
            [0xFF, 0xFF]
            [Just 0x00, Just 0xFF]         -- neither is in [0x60, 0x62]
            (repeat Nothing)
    H.assert (all (== 0) rdOut)

-- ---------------------------------------------------------------------------
-- Combined write / read sequence
-- ---------------------------------------------------------------------------

-- Sequence: write DDR=0x0F (cycle 1), write PORT=0xF0 (cycle 2),
-- then read DDR and PORT back (cycles 3+).
prop_gpio_sequential_writes_and_reads :: H.Property
prop_gpio_sequential_writes_and_reads = H.withTests 1 . H.property $ do
    let (rdOut, portOut, ddrOut) = runGpio 7
            (repeat 0)
            [Nothing, Nothing, Nothing, Just 0x61, Just 0x62, Just 0x61, Just 0x62]
            [Nothing, Just (0x61, 0x0F), Just (0x62, 0xF0), Nothing, Nothing, Nothing, Nothing]
    H.assert (0x0F `elem` ddrOut)
    H.assert (0xF0 `elem` portOut)
    H.assert (0x0F `elem` rdOut)
    H.assert (0xF0 `elem` rdOut)

gpioTests :: TestTree
gpioTests = $(testGroupGenerator)
