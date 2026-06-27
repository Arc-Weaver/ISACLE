{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}

-- | Tests for the shared address-mapping helper ('Isacle.Layout'): that a
-- record 'HdlType''s bit positions are derived MSB-first, that explicit address
-- windows lay out as given, that 'placeAt' shifts a window's flat view to a base
-- (the same operation cores and buses both rely on), and that the core-side
-- 'flagRec' derives CPU flags from the very same record layout.
module Tests.Isacle.Layout (runLayoutTests) where

import Prelude
import Data.Proxy (Proxy(..))
import Data.List  (sortOn)
import GHC.Generics (Generic, Rep)
import System.Exit (exitFailure)

import Hdl.Types (HdlType(..), GWidth, genericToBits, genericFromBits)
import Hdl.Bits  (Bit)
import Isacle.Layout
import Isacle.ISA.CPUDef (flagRec, runCPUDef, CPUSchema(..))
import Isacle.ISA.Types  (CPUFlag(..))

assert :: String -> Bool -> IO ()
assert msg False = putStrLn ("FAIL: " ++ msg) >> exitFailure
assert msg True  = putStrLn ("ok:   " ++ msg)

-- An 8-bit status register as a bit-map record (declaration order = MSB-first).
data Sreg = Sreg
    { fI :: Bit, fT :: Bit, fH :: Bit, fS :: Bit
    , fV :: Bit, fN :: Bit, fZ :: Bit, fC :: Bit
    } deriving Generic

instance HdlType Sreg where
    type Width Sreg = GWidth (Rep Sreg)
    toBits   = genericToBits
    fromBits = genericFromBits

runLayoutTests :: IO ()
runLayoutTests = do
    let l = bitLayout (Proxy @Sreg)
    assert "Sreg layout is 8 bits wide" (layoutSize l == 8)
    -- fI is declared first → top bit (7); fC is declared last → bit 0.
    assert "fI at bit 7" (lookupPlacement "fI" l == Just (Placement "fI" 7 1))
    assert "fC at bit 0" (lookupPlacement "fC" l == Just (Placement "fC" 0 1))
    assert "fS at bit 4" ((plPos <$> lookupPlacement "fS" l) == Just 4)

    -- Explicit address window: three byte-wide registers at 0,1,2.
    let win = addrLayout 3 [("UDR",0,1),("USR",1,1),("UBRR",2,1)]
    assert "window size 3" (layoutSize win == 3)
    assert "USR at offset 1" ((plPos <$> lookupPlacement "USR" win) == Just 1)

    -- placeAt: the bus assigns base 0x100; offsets become absolute addresses.
    let placed = placeAt 0x100 win
    assert "UDR placed at 0x100"  ((plPos <$> find' "UDR"  placed) == Just 0x100)
    assert "UBRR placed at 0x102" ((plPos <$> find' "UBRR" placed) == Just 0x102)

    -- Core-side flagRec: a CPU status register declared from the same Sreg
    -- record derives its flags from the same layout the peripheral path uses.
    let ((_reg, flags), sch) = runCPUDef (flagRec @Sreg "SREG")
    assert "flagRec declares an 8-bit SREG"
        (schRegisters sch == [("SREG", 8)])
    assert "flagRec records flag names MSB-first"
        (schStatusRegs sch == [("SREG", 8, ["fI","fT","fH","fS","fV","fN","fZ","fC"])])
    -- Bit positions match the record layout: fI=7 … fC=0, all in SREG.
    assert "flagRec flag bits agree with bitLayout"
        (sortOn fst [ (cpuFlagBit f, cpuFlagReg f) | f <- flags ]
            == [ (plPos p, "SREG") | p <- sortOn plPos (layoutPlacements l) ])
    assert "flag fC is bit 0 of SREG"
        (any (\f -> cpuFlagBit f == 0 && cpuFlagReg f == "SREG") flags)
  where
    find' n = foldr (\p acc -> if plName p == n then Just p else acc) Nothing
