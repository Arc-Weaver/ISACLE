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
import Hdl.Bits  (Vec(..), Signed, Arith(..))
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

    -- Width-adapting arithmetic: result type holds the whole result.
    -- add grows by one bit above the wider operand — the top bit IS the carry.
    assert "add @(Unsigned 8) (Unsigned 8): Width result = 9"
        (natVal (Proxy @(Width (AddR (Unsigned 8) (Unsigned 8)))) == 9)
    assert "add 255 1 = 256 (carry set in bit 8)"
        (toBits (add (255 :: Unsigned 8) (1 :: Unsigned 8)) == 256)
    assert "add 200 100 = 300 (no wrap, unlike fixed-width +)"
        (toBits (add (200 :: Unsigned 8) (100 :: Unsigned 8)) == 300)
    -- Different widths: Max n m + 1.  Unsigned 8 + Unsigned 16 -> Unsigned 17.
    assert "add @(Unsigned 8) (Unsigned 16): Width result = 17"
        (natVal (Proxy @(Width (AddR (Unsigned 8) (Unsigned 16)))) == 17)
    assert "add 255 (65535) = 65790 (8-bit + 16-bit, no loss)"
        (toBits (add (255 :: Unsigned 8) (65535 :: Unsigned 16)) == 65790)
    -- mul: full product, width n + m.
    assert "mul @(Unsigned 8) (Unsigned 8): Width result = 16"
        (natVal (Proxy @(Width (MulR (Unsigned 8) (Unsigned 8)))) == 16)
    assert "mul 200 200 = 40000 (full 16-bit product)"
        (toBits (mul (200 :: Unsigned 8) (200 :: Unsigned 8)) == 40000)
    assert "mul @(Unsigned 8) (Unsigned 4): Width result = 12"
        (natVal (Proxy @(Width (MulR (Unsigned 8) (Unsigned 4)))) == 12)
    -- Signed is preserved and correct.
    assert "signed add (-1) (-1) = -2"
        (add (-1 :: Signed 8) (-1 :: Signed 8) == (-2 :: Signed 9))
    assert "signed mul (-8) 16 = -128"
        (mul (-8 :: Signed 8) (16 :: Signed 8) == (-128 :: Signed 16))
