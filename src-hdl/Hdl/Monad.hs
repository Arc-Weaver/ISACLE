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
{-# LANGUAGE RecursiveDo                #-}
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
      -- * State-machine combinators
    , mealy
    , moore
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
    -- | A write-enabled register over a /runtime/ width and raw init bits — the
    -- type-erased dual of 'registerEn' (as 'regBank' is to a register file).  For
    -- "just bits" state (CPU scalar registers, sequencer counters/latches) whose
    -- width is known only at value level.  Emission is deferred so @mdo@ feedback
    -- (output → arbiter → next) is safe.
    registerW    :: KnownDom dom
                 => Int          -- ^ width (bits)
                 -> Integer      -- ^ reset value (raw bits)
                 -> s dom Bool   -- ^ write enable
                 -> s dom a      -- ^ next value
                 -> m (s dom a)
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
    -- record group, e.g. @cpu_state.GPR@) with @count@ entries of @width@ bits.
    -- Each write port is an indexed, enabled assignment @bank(idx) <= data@;
    -- several ports may fire in one cycle (e.g. MUL writing R0 and R1).  A bank
    -- of flip-flops, not block RAM.  Reads are combinational ('regBankRead').
    -- The entry @width@ is a runtime 'Int' (the bank is structural, type-erased
    -- bits) — like 'sigLitW'.
    regBank      :: KnownDom dom
                 => String                              -- ^ record group name
                 -> String                              -- ^ array field name
                 -> Int                                 -- ^ entry count
                 -> Int                                 -- ^ entry width (bits)
                 -> [(s dom idx, s dom a, s dom Bool)]  -- ^ (index, data, enable) ports
                 -> m ()
    -- | Combinational indexed read of a 'regBank' field: @bank(idx)@.
    regBankRead  :: KnownDom dom
                 => String -> String -> Int -> s dom idx -> m (s dom a)
    -- | Capture a (combinational) signal under a name: the monadic point at
    -- which a fresh, named wire is bound, so the emitter declares it as a named
    -- signal instead of @wN@.  Naming a derived 'Signal' expression names the
    -- whole expression's result.  This is how design code in the 'Hdl' monad
    -- attaches readable names without reaching for the netlist's @hintWire@.
    named        :: String -> s dom a -> m (s dom a)

-- ---------------------------------------------------------------------------
-- Mealy / Moore — the register fixpoint packaged as the classic combinators
-- ---------------------------------------------------------------------------

-- | A Mealy machine: @mealy init step out inp@ clocks a state initialised to
-- @init@, advanced each cycle by @step input state@, with an output
-- @out input state@ that depends on /both/ the current input and state.  Built
-- on the one state primitive 'register'; the @mdo@ ties the current-state
-- feedback (@state -> next -> state@).
mealy :: forall s m dom st i o. (Hdl s m, HdlType st, KnownDom dom)
      => st                                  -- ^ reset state
      -> (s dom i -> s dom st -> s dom st)    -- ^ next-state: input, state → state'
      -> (s dom i -> s dom st -> s dom o)     -- ^ output:     input, state → out
      -> s dom i -> m (s dom o)
mealy initial step out inp = mdo
    st <- register initial (step inp st)
    pure (out inp st)

-- | A Moore machine: like 'mealy', but the output is a function of the state
-- /only/ (@out state@) — so it is one cycle behind the input.
moore :: forall s m dom st i o. (Hdl s m, HdlType st, KnownDom dom)
      => st                                  -- ^ reset state
      -> (s dom i -> s dom st -> s dom st)    -- ^ next-state: input, state → state'
      -> (s dom st -> s dom o)                -- ^ output:     state → out
      -> s dom i -> m (s dom o)
moore initial step out inp = mdo
    st <- register initial (step inp st)
    pure (out st)

-- ---------------------------------------------------------------------------
-- Netlist instance — NetM (builds a NetNode netlist) is the netlist backend's
-- concrete Hdl instance, paired with Sig (its concrete Signal).  vhdl/verilog/
-- sim are sibling instances.  There is no concrete "HDL" type.
-- ---------------------------------------------------------------------------

instance Hdl Sig NetM where
    register   initVal       = regNet initVal Nothing
    registerEn initVal en inp = regNet initVal (Just en) inp
    registerW                  = regNetW
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
    regBank = regBankNet
    -- Fresh wire per call (no SExpr memoisation): two distinct indexed reads must
    -- never be merged into one wire by 'materialize's CSE.  The output wire is
    -- allocated eagerly, but the index is materialised in a DEFERRED action (as
    -- 'regNetW' does for registers) so an @mdo@ feedback address — e.g. a read
    -- whose address is a value still being tied by the surrounding @mfix@ — does
    -- not force that value during elaboration.
    regBankRead group field count addr = do
        outW <- freshWire
        defer $ do
            addrW <- materialize addr
            emit $ NRegFileRead outW group field addrW count
        pure (SWire outW)
    -- Deferred: the hint rides inside the returned signal and fires when it is
    -- materialised, so 'named' does NOT force its argument.  (An eager
    -- materialise here would break @mdo@ feedback — naming a signal that
    -- transitively depends on a not-yet-tied value would loop.)
    named nm s = pure $ SExpr $ do
        w <- materialize s
        hintWire w nm
        pure w

-- | Register-bank emission (deferred 'NRegFile' so feedback is safe): each port
-- materialises its (index, data, enable) wires inside the deferred action.
regBankNet :: forall dom a idx. KnownDom dom
           => String -> String -> Int -> Int
           -> [(Sig dom idx, Sig dom a, Sig dom Bool)]
           -> NetM ()
regBankNet group field count width ports = defer $ do
    wports <- mapM (\(i, d, e) -> (,,) <$> materialize i <*> materialize d <*> materialize e)
                   ports
    let domInfo = domId (Proxy @dom)
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

-- | Runtime-width register (deferred 'NReg'): the type-erased dual of 'regNet'.
regNetW :: forall dom a. KnownDom dom
        => Int -> Integer -> Sig dom Bool -> Sig dom a -> NetM (Sig dom a)
regNetW width initBits en inp = do
    outWid <- freshWire
    let domInfo = domId (Proxy @dom)
    defer $ do
        inWid <- materialize inp
        enWid <- materialize en
        emit $ NReg outWid inWid (Just enWid) (SomeBits initBits width) domInfo
    pure (SWire outWid)

-- | A literal signal of a selector label.
litOf :: forall dom a. HdlType a => a -> Sig dom a
litOf k = sigLitW (toBits k) (fromIntegral (natVal (Proxy @(Width a))))

-- | Retype the (phantom) clock domain of a signal — the wire is unchanged.
retypeDom :: Sig d1 a -> Sig d2 a
retypeDom (SWire w) = SWire w
retypeDom (SExpr m) = SExpr m
