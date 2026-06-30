{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
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
      -- * The Hdl typeclass (netlist instance: Hdl Sig NetM)
    , Hdl(..)
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
                  NetNode(NReg, NRegFile, NRegFileRead), SomeBits(..))
import Hdl.Types (Sig(..), Signal(sigLitW), materialize, mux, (.==.), (.<.),
                  sigNot, (.&&.), HdlType(..), HdlOrd, KnownDom(..))

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
erase :: Sig dom (Named a) -> (Sig dom a -> m r) -> m r
erase s k = k (retype s)
  where
    retype (SWire w) = SWire w
    retype (SExpr m) = SExpr m

-- ---------------------------------------------------------------------------
-- Hdl — the fundamental monad-level hardware surface
-- ---------------------------------------------------------------------------

-- | The fundamental HDL operations, abstract over both the signal interpreter
-- @s@ ('Signal') and the monad @m@ (the fundep @m -> s@ pairs them per backend).
-- Backends — the synthesis netlist instance below, future vhdl/verilog/sim — are
-- instances; design code is written @(Signal s, Hdl s m) => …@ and runs through
-- whichever it is interpreted by.  Combinational logic is /not/ here — it stays
-- pure 'Signal' ops; only state and cross-domain wiring need the monad.  Feedback
-- (@rec q <- register i (step q)@) relies on 'MonadFix'.
class (Signal s, Monad m, MonadFix m) => Hdl (s :: Type -> Type -> Type) m | m -> s where
    -- | A clocked register: reset value, next-state signal → current value.
    register     :: (HdlType a, KnownDom dom) => a -> s dom a -> m (s dom a)
    -- | A register with a write-enable (the enable-high case is 'register').
    registerEn   :: (HdlType a, KnownDom dom) => a -> s dom Bool -> s dom a -> m (s dom a)
    -- | The cross-domain "just connect it" escape hatch — for CDC code only.
    forceConnect :: s d1 a -> m (s d2 a)
    -- | Decoded assignment: select a branch by exact match on the selector,
    -- else the default.  Branches are monadic (all elaborate; the case selects
    -- their outputs).  Lowers to a mux chain today; a real VHDL @case@ later.
    caseOf       :: (HdlType sel, HdlType a, KnownDom dom)
                 => s dom sel
                 -> m (s dom a)               -- ^ default (@when others@)
                 -> Map sel (m (s dom a))     -- ^ branches (label → result)
                 -> m (s dom a)
    -- | Like 'caseOf' but keyed by inclusive @(lo, hi)@ ranges (discrete/ordered
    -- selector).  Overlap between distinct ranges is the caller's responsibility
    -- (VHDL forbids overlapping choices); lowers to range-compare muxes today.
    caseRange    :: (HdlType sel, HdlOrd sel, HdlType a, KnownDom dom)
                 => s dom sel
                 -> m (s dom a)                   -- ^ default (@when others@)
                 -> Map (sel, sel) (m (s dom a))  -- ^ branches (inclusive lo..hi → result)
                 -> m (s dom a)
    -- | A register bank: an array-valued clocked register (one field of a named
    -- record group, e.g. @cpu_state.GPR@) with @count@ entries.  Each write port
    -- is an indexed, enabled assignment @bank(idx) <= data@; several ports may
    -- fire in one cycle (e.g. MUL writing R0 and R1).  A bank of flip-flops, not
    -- block RAM.  Reads are combinational ('regBankRead').
    regBank      :: (HdlType a, KnownDom dom)
                 => String                              -- ^ record group name
                 -> String                              -- ^ array field name
                 -> Int                                 -- ^ entry count
                 -> [(s dom idx, s dom a, s dom Bool)]  -- ^ (index, data, enable) ports
                 -> m ()
    -- | Combinational indexed read of a 'regBank' field: @bank(idx)@.
    regBankRead  :: (HdlType a, KnownDom dom)
                 => String -> String -> Int -> s dom idx -> m (s dom a)

-- ---------------------------------------------------------------------------
-- Netlist instance — NetM (builds a NetNode netlist) is the netlist backend's
-- concrete Hdl instance, paired with Sig (its concrete Signal).  vhdl/verilog/
-- sim are sibling instances.  There is no concrete "HDL" type.
-- ---------------------------------------------------------------------------

instance Hdl Sig NetM where
    register   initVal       = regNet initVal Nothing
    registerEn initVal en inp = regNet initVal (Just en) inp
    forceConnect s            = pure (retypeDom s)
    caseOf sel dflt branches  = do
        d <- dflt
        foldrM step d (Map.toList branches)
      where
        step (k, mb) acc = do
            b <- mb
            pure (mux (sel .==. litOf k) b acc)
    caseRange sel dflt branches = do
        d <- dflt
        foldrM step d (Map.toList branches)
      where
        step ((lo, hi), mb) acc = do
            b <- mb
            let inRange = sigNot (sel .<. litOf lo) .&&. sigNot (litOf hi .<. sel)
            pure (mux inRange b acc)
    regBank group field count ports = regBankNet group field count ports
    regBankRead group field count addr =
        pure $ SExpr $ do
            addrW <- materialize addr
            outW  <- freshWire
            emit $ NRegFileRead outW group field addrW count
            pure outW

-- | Register-bank emission (deferred 'NRegFile' so feedback is safe): each port
-- materialises its (index, data, enable) wires inside the deferred action.
regBankNet :: forall dom a idx. (HdlType a, KnownDom dom)
           => String -> String -> Int
           -> [(Sig dom idx, Sig dom a, Sig dom Bool)]
           -> NetM ()
regBankNet group field count ports = defer $ do
    wports <- mapM (\(i, d, e) -> (,,) <$> materialize i <*> materialize d <*> materialize e)
                   ports
    let width   = fromIntegral (natVal (Proxy @(Width a)))
        domInfo = domId (Proxy @dom)
    emit $ NRegFile group field count width wports domInfo

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
