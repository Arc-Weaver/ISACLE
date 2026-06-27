{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Tests.Isacle.System.Bus where

import Prelude
import Data.Word (Word32)
import Data.Proxy (Proxy(..))
import System.Exit (exitFailure)

import Hdl.Net   (DomId(..), ClockEdge(..), ResetPolarity(..))
import Isacle.System.BusCap
    ( Capability(..), canDrive, BusAdapter(..), widthAdapter, stallAdapter )
import Hdl.Types (KnownDom(..), Sig(..))
import Hdl.Prim  (Unsigned)
import Isacle.System.SystemDSL
import Isacle.System.HdlCircuit (GpioPhys(..))
import Isacle.System.Generate (sysExtractMemoryMap)

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
    ((port, ddr), _rdData) <- createBus "databus" $ do
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

    -- Crossing adapters (BU7): widths recorded for introspection.
    assert "width adapter records 32->8"
        (let a = widthAdapter 32 8 :: BusAdapter 'Stalling 'Stalling
         in adMasterWidth a == 32 && adChildWidth a == 8 && not (adInsertsStall a))
    assert "stall adapter inserts a handshake"
        (adInsertsStall (stallAdapter 8))
  where
    isSubstr _ []              = False
    isSubstr [] _              = True
    isSubstr n@(x:xs) (y:ys)
        | x == y               = isSubstr xs ys || isSubstr n ys
        | otherwise            = isSubstr n ys
