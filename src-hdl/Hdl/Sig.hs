{-# LANGUAGE AllowAmbiguousTypes #-}
-- | The concrete synthesis signal 'Sig' and its interpreter instances — the
-- netlist backend's realisation of the abstract 'Signal' surface (from
-- "Hdl.Types").  'Sig' embeds the netlist builder ('NetM'); it is therefore a
-- __backend__ module, and logic code never names it — logic is written over the
-- abstract @'Signal' s@ / @'Hdl' s m@ typeclasses and only meets 'Sig' when a
-- design is elaborated for export.
--
-- Re-exports "Hdl.Types", so a module that needs the concrete signal switches
-- @import Hdl.Types@ → @import Hdl.Sig@ and keeps the rest of its names.
module Hdl.Sig
    ( Sig(..)
    , materialize
    , withRepr
    , sigReinterpret
    , module Hdl.Types
    ) where

import Prelude
import Data.Kind (Type)
import GHC.TypeLits (natVal)
import Data.Proxy (Proxy(..))
import System.Mem.StableName (makeStableName)
import System.IO.Unsafe (unsafePerformIO)

import Hdl.Net
import Hdl.Types

-- ---------------------------------------------------------------------------
-- Per-signal clock domain — the concrete synthesis signal
-- ---------------------------------------------------------------------------

data Sig (dom :: k) (a :: Type)
    = SWire WireId
    | SExpr (NetM WireId)

materialize :: Sig dom a -> NetM WireId
materialize (SWire wid) = pure wid
materialize (SExpr m)   = memoSExpr m sn
  where sn = unsafePerformIO (makeStableName m)

-- ---------------------------------------------------------------------------
-- Primitive application
-- ---------------------------------------------------------------------------

primSig2 :: PrimOp -> Sig dom a -> Sig dom b -> Sig dom c
primSig2 op a b = SExpr $ do
    wa <- materialize a
    wb <- materialize b
    lookupOrEmit op [wa, wb]

primSig1 :: PrimOp -> Sig dom a -> Sig dom b
primSig1 op a = SExpr $ do
    wa <- materialize a
    lookupOrEmit op [wa]

-- ---------------------------------------------------------------------------
-- Signal instance — Sig is the synthesis interpreter (→ NetNode graph)
-- ---------------------------------------------------------------------------

instance Signal Sig where
    sigPrim1 = primSig1
    sigPrim2 = primSig2
    sigPrim3 op a b c = SExpr $ do
        wa <- materialize a
        wb <- materialize b
        wc <- materialize c
        lookupOrEmit op [wa, wb, wc]
    sigLitW v w = SExpr (lookupOrEmit (PLit v w) [])
    sigRetype (SWire w) = SWire w
    sigRetype (SExpr m) = SExpr m

-- | Tag a typed signal's wire with its representation (from 'hdlRepr'), so the
-- emitter declares it with the right VHDL signal type.
withRepr :: forall dom a. HdlType a => Sig dom a -> Sig dom a
withRepr s = SExpr $ do
    w <- materialize s
    reprWire w (hdlRepr (Proxy @a))
    pure w

-- | Reinterpret a signal's bits as another representation of the /same width/.
sigReinterpret :: forall b dom a. HdlType b => Sig dom a -> Sig dom b
sigReinterpret s = SExpr $ do
    w <- materialize s
    lookupOrEmit (PReinterpret (hdlRepr (Proxy @b))) [w]

-- ---------------------------------------------------------------------------
-- Num — combinational arithmetic on the concrete signal
-- ---------------------------------------------------------------------------

instance (HdlType a, Num a) => Num (Sig dom a) where
    (+)    = primSig2 PAdd
    (-)    = primSig2 PSub
    (*)    = primSig2 PMul
    negate = primSig1 PNot
    abs    = id
    signum = const (SExpr $ lookupOrEmit (PLit 1 w) [])
      where w = fromIntegral (natVal (Proxy @(Width a)))
    fromInteger n = SExpr $ lookupOrEmit (PLit n w) []
      where w = fromIntegral (natVal (Proxy @(Width a)))

-- ---------------------------------------------------------------------------
-- HdlPorts — the concrete single-signal port bundle
-- ---------------------------------------------------------------------------

instance (HdlType a, KnownDom dom) => HdlPorts (Sig dom a) where
    portCount _ = 1
    portSpecs _ = [PortSpec
        { portName  = "sig"
        , portWidth = fromIntegral (natVal (Proxy :: Proxy (Width a)))
        , portDom   = domId (Proxy @dom) }]
    toWireIds sig     = (:[]) <$> materialize sig
    fromWireIds [w]   = SWire w
    fromWireIds _     = error "HdlPorts (Sig): fromWireIds: wrong wire count"
