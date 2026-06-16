module Isacle.Hdl.Prim
    ( -- * n-bit unsigned integer
      Unsigned(..)
      -- * Bit (1-bit value, distinct from Bool)
    , Bit(..)
    , bitToSig
    ) where

import Prelude
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownNat, Nat, natVal)

import Isacle.Hdl.Types (HdlType(..), Sig(..))

-- ---------------------------------------------------------------------------
-- Unsigned n
-- ---------------------------------------------------------------------------

-- | n-bit unsigned integer. The bit-width is carried in the type.
-- Arithmetic wraps modulo 2^n (matching hardware behaviour).
newtype Unsigned (n :: Nat) = Unsigned Integer
    deriving (Eq, Ord)

instance Show (Unsigned n) where
    show (Unsigned v) = show v

wrapU :: forall n. KnownNat n => Integer -> Unsigned n
wrapU v = Unsigned (v `mod` (2 ^ natVal (Proxy @n)))

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

raw :: Unsigned n -> Integer
raw (Unsigned v) = v

-- ---------------------------------------------------------------------------
-- Bit
-- ---------------------------------------------------------------------------

-- | A single bit. Identical to Bool in meaning but with cleaner hardware
-- semantics (no True/False; just '1'/'0').
data Bit = Lo | Hi deriving (Show, Eq, Ord, Bounded, Enum)

instance HdlType Bit where
    type Width Bit = 1
    toBits Lo = 0
    toBits Hi = 1
    fromBits 0 = Lo
    fromBits _ = Hi

-- | Adapt a Bool signal to a Bit signal (same wire, just type annotation).
bitToSig :: Sig dom Bool -> Sig dom Bit
bitToSig (SWire w) = SWire w
bitToSig (SExpr m) = SExpr m
