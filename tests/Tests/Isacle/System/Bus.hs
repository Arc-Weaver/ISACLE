{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Tests.Isacle.System.Bus where

import Prelude
import Data.Word (Word32)
import Data.Proxy (Proxy(..))
import qualified Data.Map.Strict as M
import System.Exit (exitFailure)

import Hdl.Net   (DomId(..), ClockEdge(..), ResetPolarity(..))
import Hdl.Sim   (simulateSystem)
import Isacle.System.BusCap
    ( Capability(..), canDrive, canDriveWidth, BusAdapter(..), widthAdapter, stallAdapter )
import Hdl.Types (KnownDom(..), Sig(..))
import Hdl.Prim  (Unsigned)
import Isacle.System.SystemDSL
import Isacle.System.HdlCircuit (GpioPhys(..))
import Isacle.System.Generate (sysExtractMemoryMap)
import Isacle.System.Reduce

assert :: String -> Bool -> IO ()
assert msg False = putStrLn ("FAIL: " ++ msg) >> exitFailure
assert msg True  = putStrLn ("ok:   " ++ msg)

data Clk
instance KnownDom Clk where
    domId _ = DomId "clk" 12000000 Rising ActiveHigh "rst"

-- ---------------------------------------------------------------------------
-- Test system: single GPIO peripheral on a data bus
-- ---------------------------------------------------------------------------

gpioBase :: Word32
gpioBase = 0x60

gpioBusSys
    :: Sig Clk (Unsigned 8)
    -> SysDSL Clk (Unsigned 8) (Sig Clk (Unsigned 8), Sig Clk (Unsigned 8))
gpioBusSys gpioIn = do
    gpio <- createGpio "gpio" gpioIn
    bh <- orphanBusMaster @32 @8
    (port, ddr) <- createBus "databus" bh $ do
        gpio' <- attachPeripheral gpioBase gpio
        return (gpioPort gpio', gpioDdr gpio')
    return (port, ddr)

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

runBusTests :: IO ()
runBusTests = do
    putStrLn "\n-- system DSL bus spec --"
    let pinSig = SExpr (pure 0) :: Sig Clk (Unsigned 8)
        (_, _, doc) = runSystemDSL (gpioBusSys pinSig)
        memmap = sysExtractMemoryMap doc
    assert "gpio present in memory map" ("gpio" `isSubstr` memmap)
    assert "base address in memory map" ("0x60" `isSubstr` memmap || "0x00000060" `isSubstr` memmap)
    assert "at least one bus section"   (not (null (sdBuses doc)))
    let entryCount = sum (map (length . bsEntries) (sdBuses doc))
    assert "one peripheral registered"  (entryCount == 1)

    -- Bus capability hierarchy (BU6): the legal connections witness via
    -- 'canDrive'; the forbidden one (NonStalling master → Stalling child) has
    -- no 'Subsumes' instance and would not compile.
    putStrLn "\n-- bus capability hierarchy --"
    assert "stalling drives stalling"
        (canDrive (Proxy @'Stalling)    (Proxy @'Stalling)    == ())
    assert "stalling drives non-stalling"
        (canDrive (Proxy @'Stalling)    (Proxy @'NonStalling) == ())
    assert "non-stalling drives non-stalling"
        (canDrive (Proxy @'NonStalling) (Proxy @'NonStalling) == ())

    -- Width axis: a wider (or equal) master may drive a narrower slave; the
    -- forbidden narrow→wide case (8-bit master, 32-bit slave) has no @<=@ witness.
    assert "32-bit master drives 8-bit slave"
        (canDriveWidth (Proxy @32) (Proxy @8) == ())
    assert "8-bit master drives 8-bit slave"
        (canDriveWidth (Proxy @8) (Proxy @8) == ())

    -- Crossing adapters (BU7): widths recorded for introspection.
    assert "width adapter records 32->8"
        (let a = widthAdapter 32 8 :: BusAdapter 'Stalling 'Stalling
         in adMasterWidth a == 32 && adChildWidth a == 8 && not (adInsertsStall a))
    assert "stall adapter inserts a handshake"
        (adInsertsStall (stallAdapter 8))

    -- One description, many reductions (SY6/SY7/BU5): the same system reduces
    -- to an Hdl netlist AND to software-facing renders, all from one run.
    putStrLn "\n-- system reductions --"
    let red = reduceSystem "gpiosys" (gpioBusSys pinSig)
    assert "reduces to a non-empty Hdl netlist (SY7)"
        (not (null (srHdlNodes red)))
    assert "reduces to a memory map mentioning gpio (BU5/SY6)"
        ("gpio" `isSubstr` srMemoryMap red)
    assert "reduces to a C header with the guard (SY6)"
        ("GPIOSYS_H" `isSubstr` srCHeader red)
    assert "standalone reduceToHdl agrees with the bundle"
        (length (snd (reduceToHdl (gpioBusSys pinSig))) == length (srHdlNodes red))
    assert "reduceToDesign yields a non-empty hierarchical design"
        (not (null (reduceToDesign "gpiosys" (gpioBusSys pinSig))))
    assert "reduceToLinkerScript mentions MEMORY"
        ("MEMORY" `isSubstr` srLinkerScript red)
    assert "reduceToMemoryMap standalone agrees with bundle"
        (reduceToMemoryMap (gpioBusSys pinSig) == srMemoryMap red)

    -- Whole-SoC simulation: the gpio system has no CPU master, so its bus-master
    -- interface is undriven. Those wires tie off to 0 (rather than stalling the
    -- solver), so the SoC simulates and its gpio outputs resolve — at reset both
    -- the port and DDR registers read 0.
    putStrLn "\n-- whole-SoC simulation --"
    let design = execSystemDSL @Clk @(Unsigned 8) "top" (gpioBusSys pinSig)
        socOut = head (simulateSystem design "top" M.empty 1)
    assert "whole-SoC sim resolves gpio port output"
        (M.lookup "gpio_GpioPhys_gpioPort" socOut == Just 0)
    assert "whole-SoC sim resolves gpio ddr output"
        (M.lookup "gpio_GpioPhys_gpioDdr" socOut == Just 0)

    -- Typed peripheral → C header, end to end: the ramp's signed registers
    -- (declared via regField/roField @(Signed 8)) surface as int8_t.
    putStrLn "\n-- signed peripheral C header --"
    let rampSys :: SysDSL Clk (Unsigned 8) ()
        rampSys = do
            r  <- createRamp "ramp0" (SExpr (pure 1))
            bh <- orphanBusMaster @32 @8
            _  <- createBus "rbus" bh (attachPeripheral 0x40 r >> return ())
            return ()
        rampHdr = reduceToCHeader "ramphdr" rampSys
    assert "ramp SETPOINT is int8_t in the C header"
        (("RAMP0_SETPOINT" `isSubstr` rampHdr) && ("int8_t" `isSubstr` rampHdr))
  where
    isSubstr _ []              = False
    isSubstr [] _              = True
    isSubstr n@(x:xs) (y:ys)
        | x == y               = isSubstr xs ys || isSubstr n ys
        | otherwise            = isSubstr n ys
