{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
-- | Exact, width-growing hardware arithmetic on typed signals.
--
-- 'Num' on a 'Sig' is the fixed-width /modular/ adder: same width in and out,
-- carry dropped, may overflow.  The ops here are the /exact/ complement — they
-- never lose bits, so the result type carries the grown width: @add@\/@sub@ grow
-- to @MaxN wa wb + 1@ (the carry) and @mul@ to @wa + wb@.  The operand types also
-- fix the signedness, so each instance lowers to the right primitive.
--
-- Implementation note: each op resizes both operands /up/ to the result width
-- and then uses the modular 'Num' op — which can't overflow there, because the
-- result width is large enough by construction.  So the exact ops are built from
-- the safe public surface ('sigResize' + 'Num'); no loose @primSig2@ needed.
--
-- There is deliberately no mixed-signedness instance, so adding an unsigned to a
-- signed signal is a type error — and because the primitives only ever grow,
-- there is no hidden downsize.  To land a growing chain at a target width,
-- 'Hdl.Types.sigResize' it once at the very end.
module Hdl.Arith
    ( HdlArith(..)
    , MaxN
    , sConcat
    ) where

import Prelude
import Data.Kind (Type)
import Data.Type.Bool (If)
import GHC.TypeLits (Nat, KnownNat, type (+), type (<=?))

import Hdl.Types (Sig, sigResize, sigConcat, HdlType(..))
import Hdl.Prim  (Unsigned)
import Hdl.Bits  (Signed, BitVector)

-- | Type-level maximum of two naturals.
type family MaxN (m :: Nat) (n :: Nat) :: Nat where
    MaxN m n = If (m <=? n) n m

-- | Exact (non-wrapping) arithmetic between two typed signals whose widths — and
-- signedness — may differ.  The result type is derived from the operand types.
class (HdlType a, HdlType b) => HdlArith a b where
    type AddR a b :: Type   -- ^ exact add\/sub result (carry-grown)
    type MulR a b :: Type   -- ^ exact product

    -- | Exact addition: @MaxN wa wb + 1@ bits, no overflow.
    add :: Sig dom a -> Sig dom b -> Sig dom (AddR a b)
    -- | Exact subtraction: same growth as 'add'.
    sub :: Sig dom a -> Sig dom b -> Sig dom (AddR a b)
    -- | Exact multiplication: @wa + wb@ bits.
    mul :: Sig dom a -> Sig dom b -> Sig dom (MulR a b)

instance (KnownNat m, KnownNat n, KnownNat (MaxN m n + 1), KnownNat (m + n))
      => HdlArith (Unsigned m) (Unsigned n) where
    type AddR (Unsigned m) (Unsigned n) = Unsigned (MaxN m n + 1)
    type MulR (Unsigned m) (Unsigned n) = Unsigned (m + n)
    add a b = sigResize @(MaxN m n + 1) a + sigResize @(MaxN m n + 1) b
    sub a b = sigResize @(MaxN m n + 1) a - sigResize @(MaxN m n + 1) b
    mul a b = sigResize @(m + n)         a * sigResize @(m + n)         b

-- Signed: structurally identical to the unsigned instance — the signed
-- behaviour (sign-extending resize, signed +/*/comparison) comes entirely from
-- the emitter declaring the wires @signed(..)@ via their representation tag, so
-- numeric_std overloading does the rest.
instance (KnownNat m, KnownNat n, KnownNat (MaxN m n + 1), KnownNat (m + n))
      => HdlArith (Signed m) (Signed n) where
    type AddR (Signed m) (Signed n) = Signed (MaxN m n + 1)
    type MulR (Signed m) (Signed n) = Signed (m + n)
    add a b = sigResize @(MaxN m n + 1) a + sigResize @(MaxN m n + 1) b
    sub a b = sigResize @(MaxN m n + 1) a - sigResize @(MaxN m n + 1) b
    mul a b = sigResize @(m + n)         a * sigResize @(m + n)         b

-- | Width-typed concatenation: @a@ in the high bits, @b@ in the low bits; the
-- result is an unsigned bit vector of the summed width (concatenation is bit
-- juxtaposition, so it carries no signedness).
sConcat :: forall dom a b. (HdlType a, HdlType b, KnownNat (Width a + Width b))
        => Sig dom a -> Sig dom b -> Sig dom (BitVector (Width a + Width b))
sConcat = sigConcat
