{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Isacle.ISA.Types where

import Prelude
import GHC.OverloadedLabels (IsLabel(..))
import GHC.TypeLits (KnownSymbol, symbolVal)
import Data.Proxy (Proxy(..))
import Hdl.Bits

-- | Lets encoding field names be written as overloaded labels at call sites:
--   @register gpr #dddd@, @immediate #kkkkkkkk@.
--   The label text matches the repeated character in the 'encoding' string.
--   Orphan is intentional: no other instance of IsLabel l String exists.
instance KnownSymbol l => IsLabel l String where
    fromLabel = symbolVal (Proxy @l)

-- ---------------------------------------------------------------------------
-- Width helper
-- ---------------------------------------------------------------------------

width :: forall n. KnownNat n => SNat n
width = SNat

-- Common aliases
w8  :: SNat 8;  w8  = width @8
w16 :: SNat 16; w16 = width @16
w32 :: SNat 32; w32 = width @32
w64 :: SNat 64; w64 = width @64

byte :: SNat 8
byte = width @8

-- ---------------------------------------------------------------------------
-- CPU state element references
-- These are opaque handles produced by CPUDef and consumed by MonadALU
-- ---------------------------------------------------------------------------

newtype CPUFlag                            = CPUFlag    String
newtype CPURegister (w    :: Nat)          = CPURegister String
newtype CPURegFile  (count :: Nat) (w :: Nat) = CPURegFile  String

-- ---------------------------------------------------------------------------
-- Endianness
-- ---------------------------------------------------------------------------

data Endianness = LittleEndian | BigEndian
    deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- ALU primitive operations
-- ---------------------------------------------------------------------------

data ALUPrim
    = PAdd | PSub | PAnd | POr | PXor | PNot
    | PShiftL | PShiftR | PArithShiftR
    | PMul | PMulSigned
    deriving (Show, Eq)

