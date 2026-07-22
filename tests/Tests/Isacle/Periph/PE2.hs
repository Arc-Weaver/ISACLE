{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

-- | Tests for the PE2 typed-field-plus-logic combinators ('regField'/'roField'):
-- declaring a register wires its logic AND records correct typed metadata
-- (offset, width, representation, access) from a single call.
module Tests.Isacle.Periph.PE2 (runPE2Tests) where

import Prelude
import System.Exit (exitFailure)
import Data.Functor.Identity (Identity, runIdentity)

import Hdl.Bits (Signed)
import Hdl.Prim (Unsigned)
import Hdl.Net  (Repr(..))
import Isacle.System.Periph
import Isacle.System.Spec (NullSig(..))

assert :: String -> Bool -> IO ()
assert msg False = putStrLn ("FAIL: " ++ msg) >> exitFailure
assert msg True  = putStrLn ("ok:   " ++ msg)

data Demo

-- A peripheral with one signed RW register and one unsigned RO status register.
demo :: PeriphDef Demo NullSig Identity (Unsigned 8) ()
demo = do
    _ <- regField @(Signed 8)   0 "SETPOINT" "target value" 0
    roField @(Unsigned 8) 1 "STATUS"   "status bits"  NullSig

runPE2Tests :: IO ()
runPE2Tests = do
    let (_, _, PeriphSpec fields) = runIdentity (runPeriphDef nullOps nullBusIface demo)
    assert "two fields declared" (length fields == 2)
    case fields of
        [sp, st] -> do
            assert "SETPOINT at offset 0"   (fieldOffset sp == 0)
            assert "SETPOINT is 8-bit RW"   (fieldWidth sp == RW8 && fieldAccess sp == ReadWrite)
            assert "SETPOINT is signed"     (fieldRepr sp == RSigned)
            assert "STATUS at offset 1"     (fieldOffset st == 1)
            assert "STATUS is read-only"    (fieldAccess st == ReadOnly)
            assert "STATUS is unsigned"     (fieldRepr st == RUnsigned)
        _ -> assert "expected exactly two fields" False
