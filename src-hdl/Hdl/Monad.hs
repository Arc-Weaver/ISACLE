{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE PolyKinds                  #-}
-- | The HDL monad layer: a true monad capturing clocked hardware, replacing the
-- (unused) arrow scaffolding in "Hdl.Class".  The fundamental surface is the
-- 'Hdl' typeclass over a monad — 'register' (the one state primitive),
-- 'forceConnect' (the cross-domain escape), and 'caseOf' (decoded assignment).
-- 'HDL' is the concrete synthesis instance (over 'NetM'); naming rides on the
-- value type via 'Named' so signals carry their name to the emitter.
module Hdl.Monad
    ( -- * Named values
      Named(..)
    , name
    , erase
      -- * The HDL monad
    , Hdl(..)
    , HDL(..)
    ) where

import Prelude
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Foldable (foldrM)
import GHC.TypeLits (natVal)
import Control.Monad.Fix (MonadFix)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Hdl.Net   (NetM, freshWire, emit, defer, hintWire,
                  NetNode(NReg), SomeBits(..))
import Hdl.Types (Sig(..), Signal(sigLitW), materialize, mux, (.==.),
                  HdlType(..), KnownDom(..))

-- ---------------------------------------------------------------------------
-- Named — a representation-identical marker on the value type
-- ---------------------------------------------------------------------------

-- | A name marker carried on a signal's /value/ type: @Sig dom (Named a)@ is the
-- same wire as @Sig dom a@ with a name attached (in the netlist), the type only
-- tracking that it is named.  Erases to identical bits, so zero hardware cost.
newtype Named a = Named { unNamed :: a }

instance HdlType a => HdlType (Named a) where
    type Width (Named a) = Width a
    toBits (Named x) = toBits x
    fromBits         = Named . fromBits
    hdlRepr _        = hdlRepr (Proxy @a)

-- | Attach a name to a signal (outputs): @plain -> named@, emitting the name
-- onto the underlying wire so the VHDL signal is named, not @wN@.
name :: KnownDom dom => String -> Sig dom a -> Sig dom (Named a)
name nm s = SExpr $ do
    w <- materialize s
    hintWire w nm
    pure w

-- | Erase a name (inputs): hand the body the plain signal within a continuation,
-- so the name can seed a scope for the derived logic.  The inverse of 'name'.
erase :: Hdl m => Sig dom (Named a) -> (Sig dom a -> m r) -> m r
erase s k = k (retype s)
  where
    retype (SWire w) = SWire w
    retype (SExpr m) = SExpr m

-- ---------------------------------------------------------------------------
-- Hdl — the fundamental monad-level hardware surface
-- ---------------------------------------------------------------------------

-- | The fundamental HDL operations, abstract over the monad @m@.  Backends
-- (synthesis, future sim/doc) are instances.  Combinational logic is /not/ here
-- — it stays pure 'Signal' ops; only state and cross-domain wiring need the
-- monad.  Feedback (@rec q <- register i (step q)@) relies on 'MonadFix'.
class (Monad m, MonadFix m) => Hdl m where
    -- | A clocked register: reset value, next-state signal → current value.
    register     :: (HdlType a, KnownDom dom) => a -> Sig dom a -> m (Sig dom a)
    -- | A register with a write-enable (the enable-high case is 'register').
    registerEn   :: (HdlType a, KnownDom dom) => a -> Sig dom Bool -> Sig dom a -> m (Sig dom a)
    -- | The cross-domain "just connect it" escape hatch — for CDC code only.
    forceConnect :: Sig d1 a -> m (Sig d2 a)
    -- | Decoded assignment: select a branch by exact match on the selector,
    -- else the default.  Branches are monadic (all elaborate; the case selects
    -- their outputs).  Lowers to a mux chain today; a real VHDL @case@ later.
    caseOf       :: (HdlType sel, HdlType a, KnownDom dom)
                 => Sig dom sel
                 -> m (Sig dom a)               -- ^ default (@when others@)
                 -> Map sel (m (Sig dom a))     -- ^ branches (label → result)
                 -> m (Sig dom a)

-- ---------------------------------------------------------------------------
-- HDL — the concrete synthesis instance (over NetM)
-- ---------------------------------------------------------------------------

-- | The synthesis HDL monad.  @i@/@o@ are the entity interface (phantom here;
-- the 'HdlIO' entity layer reads them).  A true monad over the netlist builder.
newtype HDL (i :: Type) (o :: Type) a = HDL { runHdl :: NetM a }
    deriving newtype (Functor, Applicative, Monad, MonadFix)

instance Hdl (HDL i o) where
    register   initVal       = HDL . regNet initVal Nothing
    registerEn initVal en inp = HDL (regNet initVal (Just en) inp)
    forceConnect s            = HDL (pure (retypeDom s))
    caseOf sel dflt branches  = do
        d <- dflt
        foldrM step d (Map.toList branches)
      where
        step (k, mb) acc = do
            b <- mb
            pure (mux (sel .==. litOf k) b acc)

-- | Reimplemented register primitive (deferred 'NReg' emission so @mdo@ feedback
-- is safe), independent of the legacy "Hdl.Class".
regNet :: forall dom a. (HdlType a, KnownDom dom)
       => a -> Maybe (Sig dom Bool) -> Sig dom a -> NetM (Sig dom a)
regNet initVal mEn inp = do
    outWid <- freshWire
    let domInfo  = domId (Proxy @dom)
        bitWidth = fromIntegral (natVal (Proxy @(Width a)))
        initBits = SomeBits (toBits initVal) bitWidth
    defer $ do
        inWid <- materialize inp
        mEnW  <- traverse materialize mEn
        emit $ NReg outWid inWid mEnW initBits domInfo
    pure (SWire outWid)

-- | A literal signal of a selector label.
litOf :: forall dom a. HdlType a => a -> Sig dom a
litOf k = sigLitW (toBits k) (fromIntegral (natVal (Proxy @(Width a))))

-- | Retype the (phantom) clock domain of a signal — the wire is unchanged.
retypeDom :: Sig d1 a -> Sig d2 a
retypeDom (SWire w) = SWire w
retypeDom (SExpr m) = SExpr m
