{-# LANGUAGE AllowAmbiguousTypes #-}
module Isacle.Hdl.Class
    ( -- * Circuit typeclass
      Circuit(..)
      -- * NetBuilder: the synthesis interpreter
    , NetBuilder(..)
      -- * Named instantiation
    , instComp
      -- * Primitive circuit operations
    , regS
    , regEnS
    , primS
    , inputS
    , outputS
      -- * Optional wire naming
    , named
    ) where

import Prelude
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (natVal)

import Isacle.Hdl.Net
import Isacle.Hdl.Types

-- ---------------------------------------------------------------------------
-- Circuit typeclass
-- ---------------------------------------------------------------------------

class Circuit (c :: Type -> Type -> Type) where
    reg :: forall dom a.
           (HdlType a, KnownDom dom)
        => a -> c (Sig dom a) (Sig dom a)

    regEn :: forall dom a.
             (HdlType a, KnownDom dom)
          => a -> c (Sig dom Bool, Sig dom a) (Sig dom a)

newtype NetBuilder i o = NetBuilder { runNetBuilder :: i -> NetM o }

instance Circuit NetBuilder where
    reg   initVal = NetBuilder $ regS   initVal
    regEn initVal = NetBuilder $ uncurry (regEnS initVal)

-- ---------------------------------------------------------------------------
-- Named hierarchical instantiation
-- ---------------------------------------------------------------------------

-- | Instantiate a component as a named sub-entity in the current design.
-- The component's 'portBody' is run in a fresh sub-entity context; its
-- NInput declarations are matched positionally to the wires in @i@,
-- and fresh parent wires are allocated for each NOutput.
instComp :: forall i o.
            (HdlPorts i, HdlPorts o)
         => HdlComponent i o
         -> String    -- ^ instance label (e.g. "u_alu")
         -> i         -- ^ parent-side input signals
         -> NetM o
instComp comp instLabel inputs = do
    inWids  <- toWireIds inputs
    outPorts <- inBlock instLabel (entityName comp) inWids (portBody comp)
    return (fromWireIds [ w | (_, w, _) <- outPorts ])

-- ---------------------------------------------------------------------------
-- NetM primitives
-- ---------------------------------------------------------------------------

inputS :: forall dom a.
          (HdlType a, KnownDom dom)
       => String -> NetM (Sig dom a)
inputS name = do
    wid <- freshWire
    let w = fromIntegral (natVal (Proxy @(Width a)))
    emit $ NInput wid name w (domId (Proxy @dom))
    pure (SWire wid)

outputS :: forall dom a.
           (HdlType a, KnownDom dom)
        => String -> Sig dom a -> NetM ()
outputS name sig = do
    wid <- materialize sig
    let w = fromIntegral (natVal (Proxy @(Width a)))
    emit $ NOutput wid name w (domId (Proxy @dom))

-- | Register with deferred NReg emission so mdo feedback is safe.
regS :: forall dom a.
        (HdlType a, KnownDom dom)
     => a -> Sig dom a -> NetM (Sig dom a)
regS initVal inp = do
    outWid <- freshWire
    let domInfo  = domId (Proxy @dom)
        bitWidth = fromIntegral (natVal (Proxy @(Width a)))
        initBits = SomeBits (toBits initVal) bitWidth
    defer $ do
        inWid <- materialize inp
        emit $ NReg outWid inWid Nothing initBits domInfo
    pure (SWire outWid)

-- | Register with write-enable; same deferred strategy.
regEnS :: forall dom a.
          (HdlType a, KnownDom dom)
       => a -> Sig dom Bool -> Sig dom a -> NetM (Sig dom a)
regEnS initVal en inp = do
    outWid <- freshWire
    let domInfo  = domId (Proxy @dom)
        bitWidth = fromIntegral (natVal (Proxy @(Width a)))
        initBits = SomeBits (toBits initVal) bitWidth
    defer $ do
        enWid <- materialize en
        inWid <- materialize inp
        emit $ NReg outWid inWid (Just enWid) initBits domInfo
    pure (SWire outWid)

-- | Attach a human-readable name hint to the wire produced by any NetM action.
-- Optional: without it the emitter falls back to @wN@.
named :: String -> NetM (Sig dom a) -> NetM (Sig dom a)
named hint m = do
    sig <- m
    wid <- materialize sig
    hintWire wid hint
    pure (SWire wid)

primS :: PrimOp -> [WireId] -> NetM WireId
primS op ins = do
    outWid <- freshWire
    emit $ NComb outWid op ins
    pure outWid
