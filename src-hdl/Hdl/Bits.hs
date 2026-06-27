{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType        #-}
-- | Clash-free bit-vector types for ISA simulation and pure combinational
-- logic.  A single @import Hdl.Bits@ provides everything needed:
-- bit types, numeric instances, bit-vector operations, and the 'Bits'
-- typeclass operators.
module Hdl.Bits
    ( -- * Core bit types (re-exported from "Hdl.Prim")
      Unsigned(..)
    , Bit(..)
      -- * BitVector alias (Unsigned n under a familiar name)
    , BitVector
      -- * Signed two's-complement integer
    , Signed(..)
      -- * Fixed-length vector
    , Vec(..)
    , Index
    , repeat
    , replace
    , (Hdl.Bits.!!)
      -- * Singleton type-level naturals (for 'slice')
    , SNat(..)
    , d0, d1, d2, d3, d4, d5, d6, d7
    , d8, d9, d10, d11, d12, d13, d14, d15
    , d16, d17, d18, d19, d20, d21, d22, d23
      -- * Bit-vector operations
    , pack, unpack
    , slice
    , (++#)
    , msb, lsb
    , zeroExtend, signExtend, truncateB, resize
    , bitCoerce
    , boolToBit
    , bitToU1, u1ToBit
      -- * Re-exported: 'Bits' typeclass and operators
    , Bits(..), FiniteBits(..)
      -- * Re-exported: type-level naturals
    , KnownNat, Nat
    ) where

import Prelude hiding (repeat, (!!))
import qualified Prelude as P
import Data.Bits (Bits(..), FiniteBits(..))
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownNat, Nat, natVal, type (+), type (*))

import Hdl.Prim (Unsigned(..), Bit(..))
import Hdl.Types (HdlType(..))
import Hdl.Net (Repr(..))

-- ---------------------------------------------------------------------------
-- BitVector
-- ---------------------------------------------------------------------------

-- | n-bit bit vector — identical to 'Unsigned n'.
type BitVector (n :: Nat) = Unsigned n

-- ---------------------------------------------------------------------------
-- Signed n
-- ---------------------------------------------------------------------------

-- | n-bit two's-complement signed integer.
newtype Signed (n :: Nat) = Signed Integer
    deriving (Eq, Ord)

instance Show (Signed n) where
    show (Signed v) = show v

wrapS :: forall n. KnownNat n => Integer -> Signed n
wrapS v =
    let w    = fromIntegral (natVal (Proxy @n)) :: Int
        mask = (1 `shiftL` w) - 1
        v'   = v .&. mask
    in Signed (if v' `testBit` (w - 1) then v' - (1 `shiftL` w) else v')

instance KnownNat n => Enum (Signed n) where
    toEnum   = wrapS @n . fromIntegral
    fromEnum (Signed v) = fromEnum v

instance KnownNat n => Real (Signed n) where
    toRational (Signed v) = toRational v

instance KnownNat n => Integral (Signed n) where
    toInteger (Signed v) = v
    quotRem (Signed a) (Signed b) =
        let (q, r) = quotRem a b in (wrapS @n q, wrapS @n r)

instance KnownNat n => Num (Signed n) where
    Signed a + Signed b = wrapS @n (a + b)
    Signed a - Signed b = wrapS @n (a - b)
    Signed a * Signed b = wrapS @n (a * b)
    negate (Signed a)   = wrapS @n (negate a)
    abs (Signed a)      = wrapS @n (abs a)
    signum (Signed a)   = Signed (signum a)
    fromInteger         = wrapS @n

-- | 'Signed n' as an HDL signal type: its width is @n@ and it erases to its
-- @n@-bit two's-complement bit pattern (mirrors the 'Unsigned' instance).
instance KnownNat n => HdlType (Signed n) where
    type Width (Signed n) = n
    toBits (Signed v) = v `mod` (2 ^ natVal (Proxy @n))
    fromBits          = wrapS @n
    hdlRepr _         = RSigned

-- ---------------------------------------------------------------------------
-- Vec n a
-- ---------------------------------------------------------------------------

-- | A list whose length is tracked at the type level (phantom).
-- Index bounds are NOT checked at runtime.
newtype Vec (n :: Nat) a = Vec [a]
    deriving (Eq, Ord)

instance Show a => Show (Vec n a) where
    show (Vec xs) = show xs

-- | A 'Vec' of 'HdlType' elements is itself an 'HdlType' (H4): its width is the
-- element width times the count, and it packs MSB-first — element 0 occupies
-- the highest bits, mirroring the record packing ('genericToBits') so an array
-- field of a core/peripheral record behaves like any other field. The value
-- representation is flat (the elements' bits concatenated); structure-preserving
-- VHDL-array emission is a separate signal-layer concern.
instance (HdlType a, KnownNat n, KnownNat (n * Width a))
      => HdlType (Vec n a) where
    type Width (Vec n a) = n * Width a
    toBits (Vec xs) = foldl (\acc x -> (acc `shiftL` w) .|. toBits x) 0 xs
      where w = fromIntegral (natVal (Proxy @(Width a)))
    fromBits packed = Vec
        [ fromBits ((packed `shiftR` (w * (cnt - 1 - i))) .&. mask) | i <- [0 .. cnt - 1] ]
      where
        w    = fromIntegral (natVal (Proxy @(Width a)))
        cnt  = fromIntegral (natVal (Proxy @n))
        mask = (1 `shiftL` w) - 1

(!!) :: Vec n a -> Index n -> a
Vec xs !! i = xs P.!! i
infixl 9 !!

repeat :: forall n a. KnownNat n => a -> Vec n a
repeat x = Vec (P.replicate (fromIntegral (natVal (Proxy @n))) x)

replace :: Index n -> a -> Vec n a -> Vec n a
replace i val (Vec xs) = Vec (take i xs ++ [val] ++ drop (i + 1) xs)

-- | Runtime integer index into a 'Vec'.
type Index (n :: Nat) = Int

-- ---------------------------------------------------------------------------
-- Singleton naturals
-- ---------------------------------------------------------------------------

data SNat (n :: Nat) = SNat

d0  :: SNat 0;  d0  = SNat
d1  :: SNat 1;  d1  = SNat
d2  :: SNat 2;  d2  = SNat
d3  :: SNat 3;  d3  = SNat
d4  :: SNat 4;  d4  = SNat
d5  :: SNat 5;  d5  = SNat
d6  :: SNat 6;  d6  = SNat
d7  :: SNat 7;  d7  = SNat
d8  :: SNat 8;  d8  = SNat
d9  :: SNat 9;  d9  = SNat
d10 :: SNat 10; d10 = SNat
d11 :: SNat 11; d11 = SNat
d12 :: SNat 12; d12 = SNat
d13 :: SNat 13; d13 = SNat
d14 :: SNat 14; d14 = SNat
d15 :: SNat 15; d15 = SNat
d16 :: SNat 16; d16 = SNat
d17 :: SNat 17; d17 = SNat
d18 :: SNat 18; d18 = SNat
d19 :: SNat 19; d19 = SNat
d20 :: SNat 20; d20 = SNat
d21 :: SNat 21; d21 = SNat
d22 :: SNat 22; d22 = SNat
d23 :: SNat 23; d23 = SNat

-- ---------------------------------------------------------------------------
-- pack / unpack  (identity — for Clash API compatibility)
-- ---------------------------------------------------------------------------

pack :: Unsigned n -> BitVector n
pack = id

unpack :: BitVector n -> Unsigned n
unpack = id

-- ---------------------------------------------------------------------------
-- slice
-- ---------------------------------------------------------------------------

-- | Extract bits [hi:lo] inclusive. Result width must be known from context.
slice :: (KnownNat hi, KnownNat lo)
      => SNat hi -> SNat lo -> Unsigned n -> Unsigned m
slice shi slo (Unsigned v) =
    let hi   = fromIntegral (natVal shi) :: Int
        lo   = fromIntegral (natVal slo) :: Int
        mask = (1 `shiftL` (hi - lo + 1)) - 1
    in Unsigned ((v `shiftR` lo) .&. mask)

-- ---------------------------------------------------------------------------
-- (++#)  bit concatenation
-- ---------------------------------------------------------------------------

infixl 5 ++#
-- | Concatenate: left operand becomes MSBs. Width of right operand must be
-- statically known.
(++#) :: forall m n. KnownNat n => Unsigned m -> Unsigned n -> Unsigned (m + n)
Unsigned hi ++# Unsigned lo =
    let nBits = fromIntegral (natVal (Proxy @n)) :: Int
    in Unsigned ((hi `shiftL` nBits) .|. lo)

-- ---------------------------------------------------------------------------
-- msb / lsb
-- ---------------------------------------------------------------------------

msb :: forall n. KnownNat n => Unsigned n -> Bit
msb (Unsigned v) =
    let n = fromIntegral (natVal (Proxy @n)) :: Int
    in if v `testBit` (n - 1) then Hi else Lo

lsb :: Unsigned n -> Bit
lsb (Unsigned v) = if v `testBit` 0 then Hi else Lo

-- ---------------------------------------------------------------------------
-- Width coercions
-- ---------------------------------------------------------------------------

zeroExtend :: KnownNat m => Unsigned n -> Unsigned m
zeroExtend (Unsigned v) = fromInteger v

signExtend :: KnownNat m => Signed n -> Signed m
signExtend (Signed v) = fromInteger v

truncateB :: KnownNat m => Unsigned n -> Unsigned m
truncateB (Unsigned v) = fromInteger v

-- | Widen or narrow; direction determined by type context.
resize :: KnownNat m => Unsigned n -> Unsigned m
resize (Unsigned v) = fromInteger v

-- ---------------------------------------------------------------------------
-- bitCoerce
-- ---------------------------------------------------------------------------

-- | Reinterpret the bit pattern of a value as another type of the same width.
class BitCoerce a b where
    bitCoerce :: a -> b

instance KnownNat n => BitCoerce (Unsigned n) (Signed n) where
    bitCoerce (Unsigned v) = wrapS @n v

instance KnownNat n => BitCoerce (Signed n) (Unsigned n) where
    bitCoerce (Signed v) = fromInteger v

instance KnownNat n => BitCoerce (Unsigned n) (Unsigned n) where
    bitCoerce = id

instance KnownNat n => BitCoerce (Signed n) (Signed n) where
    bitCoerce = id

-- ---------------------------------------------------------------------------
-- Bit conversions
-- ---------------------------------------------------------------------------

boolToBit :: Bool -> Bit
boolToBit True  = Hi
boolToBit False = Lo

bitToU1 :: Bit -> Unsigned 1
bitToU1 Lo = 0
bitToU1 Hi = 1

u1ToBit :: Unsigned 1 -> Bit
u1ToBit 0 = Lo
u1ToBit _ = Hi
