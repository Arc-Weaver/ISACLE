{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Isacle.ISA.Types where

import Prelude
import Data.Kind (Type)
import Data.List (intercalate)
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

-- | A flag is a named bit within a status register.
-- The status register is the primary storage; flags are just bit-addressed views into it.
data CPUFlag = CPUFlag
    { cpuFlagReg :: String   -- ^ name of the containing status register
    , cpuFlagBit :: Int      -- ^ bit position within the register (0 = LSB)
    } deriving (Show, Eq)

-- | A register handle, typed by the register's /value type/ (e.g.
-- @CPURegister (Unsigned 16)@, @CPURegister Sreg@) — not a bare width. The bit
-- width is @'Width' t@; reading a register yields an @IExpr (Width t)@.
newtype CPURegister (t :: Type) = CPURegister String

-- | A register-file handle: @count@ registers each of value type @t@.
newtype CPURegFile  (count :: Nat) (t :: Type) = CPURegFile  String

-- | Spelling preferred at core-definition sites: @RegisterFile 32 (Unsigned 8)@.
type RegisterFile = CPURegFile

-- | Project a single bit of a register as a flag view: @sreg ! 0@ is bit 0 of
-- the @sreg@ register. Combine with 'Isacle.ISA.CPUDef.newFlag' to name it.
(!) :: CPURegister t -> Int -> CPUFlag
CPURegister name ! bit = CPUFlag name bit
infixl 9 !

-- | A /view/ register (e.g. AVR X = GPR[26]:GPR[27]) encodes its backing file,
-- the file's element width, and entry indices in its handle name:
-- @"&GPR:8:26,27"@ — entries low byte first.  'encodeRegView' builds it; the IR
-- builder decodes it with 'decodeRegView'.
encodeRegView :: String -> Int -> [Int] -> String
encodeRegView file elemW idxs =
    "&" ++ file ++ ":" ++ show elemW ++ ":" ++ intercalate "," (map show idxs)

-- | Decode a view-register handle into @(file, elementWidth, indices)@;
-- 'Nothing' for an ordinary register name.
decodeRegView :: String -> Maybe (String, Int, [Int])
decodeRegView ('&':rest) = case break (== ':') rest of
    (file, ':':rest') -> case break (== ':') rest' of
        (ewStr, ':':idxs) -> Just (file, read ewStr, map read (commaSplit idxs))
        _                 -> Nothing
    _ -> Nothing
decodeRegView _ = Nothing

commaSplit :: String -> [String]
commaSplit s = case break (== ',') s of
    (h, ',':t) -> h : commaSplit t
    (h, _)     -> [h]

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

