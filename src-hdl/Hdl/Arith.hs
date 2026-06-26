{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
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
-- There is deliberately no mixed-signedness instance, so adding an unsigned to a
-- signed signal is a type error — and because the primitives only ever grow,
-- there is no hidden downsize.  To land a growing chain at a target width,
-- 'Hdl.Types.sigResize' it once at the very end.
module Hdl.Arith
    ( HdlArith(..)
    , MaxN
    ) where

import Data.Kind (Type)
import Data.Type.Bool (If)
import GHC.TypeLits (Nat, KnownNat, type (+), type (<=?))

import Hdl.Net   (PrimOp(..))
import Hdl.Types (Sig, primSig2, sigResize, HdlType)
import Hdl.Prim  (Unsigned)

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

-- Unsigned: grow by resizing (zero-extend) both operands to the result width,
-- then emit the unsigned primitive.  (Signed awaits the emitter's per-wire
-- signed/unsigned tag, after which its instance slots in here unchanged in shape.)
instance (KnownNat m, KnownNat n, KnownNat (MaxN m n + 1), KnownNat (m + n))
      => HdlArith (Unsigned m) (Unsigned n) where
    type AddR (Unsigned m) (Unsigned n) = Unsigned (MaxN m n + 1)
    type MulR (Unsigned m) (Unsigned n) = Unsigned (m + n)
    add a b = primSig2 PAdd (sigResize @(MaxN m n + 1) a) (sigResize @(MaxN m n + 1) b)
    sub a b = primSig2 PSub (sigResize @(MaxN m n + 1) a) (sigResize @(MaxN m n + 1) b)
    mul a b = primSig2 PMul (sigResize @(m + n)         a) (sigResize @(m + n)         b)
