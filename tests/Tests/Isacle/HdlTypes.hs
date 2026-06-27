{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for 'HdlType' instances beyond the scalar/record cases — currently
-- the array instance ('Vec n a', H4): width, MSB-first packing, and round-trip.
module Tests.Isacle.HdlTypes (runHdlTypesTests) where

import Prelude
import Data.Proxy   (Proxy(..))
import GHC.TypeLits (natVal)
import System.Exit  (exitFailure)

import Hdl.Types (Width, toBits, fromBits)
import Hdl.Bits  (Vec(..))
import Hdl.Prim  (Unsigned)

assert :: String -> Bool -> IO ()
assert msg False = putStrLn ("FAIL: " ++ msg) >> exitFailure
assert msg True  = putStrLn ("ok:   " ++ msg)

runHdlTypesTests :: IO ()
runHdlTypesTests = do
    -- Width (Vec 4 (Unsigned 8)) = 4 * 8 = 32.
    assert "Vec 4 (Unsigned 8) width is 32"
        (natVal (Proxy @(Width (Vec 4 (Unsigned 8)))) == 32)

    -- MSB-first packing: element 0 occupies the highest byte.
    let v = Vec [1, 2, 3, 4] :: Vec 4 (Unsigned 8)
    assert "Vec packs MSB-first (0x01020304)"
        (toBits v == 0x01020304)

    -- Round-trip through bits.
    assert "Vec fromBits . toBits = id"
        (let (Vec xs) = (fromBits (toBits v) :: Vec 4 (Unsigned 8))
         in xs == [1, 2, 3, 4])

    -- A two-element vector of wider elements.
    let w = Vec [0xABCD, 0x1234] :: Vec 2 (Unsigned 16)
    assert "Vec 2 (Unsigned 16) packs to 0xABCD1234"
        (toBits w == 0xABCD1234)
