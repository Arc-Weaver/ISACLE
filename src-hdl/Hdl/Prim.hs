-- | Core hardware primitive types.
--
-- 'Unsigned n' and 'Bit' are the foundational types for all synthesis-path
-- code.  Everything else (signals, netlists, VHDL emission) is built on top
-- of these.
module Hdl.Prim
    ( -- * n-bit unsigned integer
      Unsigned(..)
      -- * Bit (1-bit value, distinct from Bool)
    , Bit(..)
    ) where

import Prelude
import Data.Bits (Bits(..), FiniteBits(..))
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownNat, Nat, natVal)

import Hdl.Types (HdlType(..))

-- ---------------------------------------------------------------------------
-- Unsigned n
-- ---------------------------------------------------------------------------

-- | n-bit unsigned integer. Arithmetic wraps modulo 2^n.
newtype Unsigned (n :: Nat) = Unsigned Integer
    deriving (Eq, Ord)

instance Show (Unsigned n) where
    show (Unsigned v) = show v

wrapU :: forall n. KnownNat n => Integer -> Unsigned n
wrapU v = Unsigned (v `mod` (2 ^ natVal (Proxy @n)))

raw :: Unsigned n -> Integer
raw (Unsigned v) = v

instance KnownNat n => HdlType (Unsigned n) where
    type Width (Unsigned n) = n
    toBits (Unsigned v) = v `mod` (2 ^ natVal (Proxy @n))
    fromBits             = wrapU

instance KnownNat n => Num (Unsigned n) where
    a + b       = wrapU @n (raw a + raw b)
    a - b       = wrapU @n (raw a - raw b)
    a * b       = wrapU @n (raw a * raw b)
    negate a    = wrapU @n (negate (raw a))
    abs         = id
    signum (Unsigned 0) = Unsigned 0
    signum _            = Unsigned 1
    fromInteger = wrapU @n

instance KnownNat n => Enum (Unsigned n) where
    toEnum   = wrapU @n . fromIntegral
    fromEnum (Unsigned v) = fromEnum v

instance KnownNat n => Real (Unsigned n) where
    toRational (Unsigned v) = toRational v

instance KnownNat n => Integral (Unsigned n) where
    toInteger (Unsigned v) = v
    quotRem (Unsigned a) (Unsigned b) =
        let (q, r) = quotRem a b in (wrapU @n q, wrapU @n r)

instance KnownNat n => Bits (Unsigned n) where
    Unsigned a .&. Unsigned b = wrapU @n (a .&. b)
    Unsigned a .|. Unsigned b = wrapU @n (a .|. b)
    xor (Unsigned a) (Unsigned b) = wrapU @n (xor a b)
    complement (Unsigned a) =
        let w    = fromIntegral (natVal (Proxy @n)) :: Int
            mask = (1 `shiftL` w) - 1
        in Unsigned (complement a .&. mask)
    shiftL (Unsigned a) n = wrapU @n (a `shiftL` n)
    shiftR (Unsigned a) n = Unsigned (a `shiftR` n)
    rotateL x n =
        let w = fromIntegral (natVal (Proxy @n)) :: Int
            s = n `mod` w
        in shiftL x s .|. shiftR x (w - s)
    rotateR x n =
        let w = fromIntegral (natVal (Proxy @n)) :: Int
            s = n `mod` w
        in shiftR x s .|. shiftL x (w - s)
    bit i = wrapU @n (1 `shiftL` i)
    testBit (Unsigned a) = testBit a
    bitSize _ = fromIntegral (natVal (Proxy @n))
    bitSizeMaybe _ = Just (fromIntegral (natVal (Proxy @n)))
    isSigned _ = False
    popCount (Unsigned a) = popCount a
    zeroBits = Unsigned 0

instance KnownNat n => FiniteBits (Unsigned n) where
    finiteBitSize _ = fromIntegral (natVal (Proxy @n))

-- ---------------------------------------------------------------------------
-- Bit
-- ---------------------------------------------------------------------------

-- | A single bit. Distinct from 'Bool': no True/False, just 'Lo'/'Hi'.
-- 'Num Bit' uses GF(2) arithmetic: 'Hi' + 'Hi' = 'Lo'.
data Bit = Lo | Hi deriving (Show, Eq, Ord, Bounded, Enum)

instance Num Bit where
    fromInteger 0 = Lo
    fromInteger _ = Hi
    Lo + Lo = Lo
    Hi + Hi = Lo  -- GF(2): 1+1=0
    _  + _  = Hi
    Lo * _  = Lo
    _  * Lo = Lo
    Hi * Hi = Hi
    negate = id
    abs    = id
    signum Lo = Lo
    signum Hi = Hi

instance Bits Bit where
    Hi .&. Hi = Hi
    _  .&. _  = Lo
    Lo .|. Lo = Lo
    _  .|. _  = Hi
    xor Hi Lo = Hi
    xor Lo Hi = Hi
    xor _  _  = Lo
    complement Hi = Lo
    complement Lo = Hi
    shiftL x 0 = x; shiftL _ _ = Lo
    shiftR x 0 = x; shiftR _ _ = Lo
    rotateL x _ = x
    rotateR x _ = x
    bit 0 = Hi; bit _ = Lo
    testBit Hi 0 = True; testBit _ _ = False
    bitSize _ = 1
    bitSizeMaybe _ = Just 1
    isSigned _ = False
    popCount Hi = 1; popCount Lo = 0
    zeroBits = Lo

instance HdlType Bit where
    type Width Bit = 1
    toBits Lo = 0
    toBits Hi = 1
    fromBits 0 = Lo
    fromBits _ = Hi
