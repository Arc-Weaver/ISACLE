{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Convenience helpers over signals: constant signals, reductions over
-- collections, and 'Monoid' wrappers so signals fold with 'mconcat' \/ 'foldMap'.
--
-- These live in their own opt-in module (rather than on the wholesale-imported
-- 'Hdl.Types' surface) so they don't collide with the @sigTrue@\/@sigFalse@ that
-- some peripherals still define locally.  They replace patterns the peripherals
-- open-code today — e.g. the interrupt arbiter's @foldr ('.||.') sigFalse@ is
-- exactly 'orAll'.
--
-- The sum reduction here is the /modular/ one (fixed width, wrapping).  For an
-- exact, width-growing reduction use 'Hdl.Arith.add' directly (its result type
-- grows) and 'Hdl.Types.sigResize' once at the end.
module Hdl.Reduce
    ( -- * Constant signals
      sigLit
    , sigTrue
    , sigFalse
      -- * Reductions
    , orAll
    , andAll
    , sumModular
      -- * Monoid wrappers
    , SigAny(..)
    , SigAll(..)
    , SigSum(..)
    ) where

import Prelude
import Data.Proxy (Proxy(..))
import GHC.TypeLits (natVal)
import Hdl.Net   (lookupOrEmit, PrimOp(PLit))
import Hdl.Sig   (Sig(..))
import Hdl.Types (HdlType(..), (.||.), (.&&.))

-- | A constant signal holding a compile-time value of a synthesizable type.
-- (Signed literals are still emitted unsigned-encoded for now.)
sigLit :: forall a dom. HdlType a => a -> Sig dom a
sigLit x = SExpr (lookupOrEmit (PLit (toBits x) w) [])
  where w = fromIntegral (natVal (Proxy @(Width a)))

-- | The constant high / low 1-bit signals.
sigTrue, sigFalse :: Sig dom Bool
sigTrue  = sigLit True
sigFalse = sigLit False

-- | True iff any input is true (@foldr ('.||.') 'sigFalse'@).  Empty ⇒ false.
orAll :: [Sig dom Bool] -> Sig dom Bool
orAll = foldr (.||.) sigFalse

-- | True iff every input is true.  Empty ⇒ true.
andAll :: [Sig dom Bool] -> Sig dom Bool
andAll = foldr (.&&.) sigTrue

-- | Fixed-width /modular/ sum of same-typed signals (wraps; uses the 'Num'
-- instance).  Empty ⇒ 0.  For an exact, non-overflowing sum use 'Hdl.Arith.add'.
sumModular :: Num (Sig dom a) => [Sig dom a] -> Sig dom a
sumModular = foldr (+) (fromInteger 0)

-- | 'Sig' 'Bool' under OR (identity false) — @getSigAny . foldMap (SigAny . f)@.
newtype SigAny dom = SigAny { getSigAny :: Sig dom Bool }
instance Semigroup (SigAny dom) where SigAny a <> SigAny b = SigAny (a .||. b)
instance Monoid    (SigAny dom) where mempty = SigAny sigFalse

-- | 'Sig' 'Bool' under AND (identity true).
newtype SigAll dom = SigAll { getSigAll :: Sig dom Bool }
instance Semigroup (SigAll dom) where SigAll a <> SigAll b = SigAll (a .&&. b)
instance Monoid    (SigAll dom) where mempty = SigAll sigTrue

-- | A signal under modular addition (identity 0).
newtype SigSum dom a = SigSum { getSigSum :: Sig dom a }
instance Num (Sig dom a) => Semigroup (SigSum dom a) where
    SigSum a <> SigSum b = SigSum (a + b)
instance Num (Sig dom a) => Monoid (SigSum dom a) where
    mempty = SigSum (fromInteger 0)
