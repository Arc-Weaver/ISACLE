module Isacle.Hdl.Types
    ( -- * Per-signal domain tags
      Sig(..)
    , materialize
    , wire
      -- * Combinational operations
    , (.==.)
    , (.<.)
    , (.&&.)
    , (.||.)
    , sigAnd
    , sigOr
    , sigNot
    , mux
    , sigShiftL
    , sigShiftR
    , sigShiftLDyn
    , sigShiftRDyn
    , sigBit
    , sigBitDyn
      -- * Synthesizability constraints
    , HdlType(..)
    , HdlPorts(..)
    , PortSpec(..)
      -- * Component type
    , HdlComponent(..)
    , block
      -- * Clock domain typeclass
    , KnownDom(..)
    ) where

import Prelude
import GHC.TypeLits (KnownNat, Nat, natVal)
import Data.Kind (Type)
import Data.Proxy (Proxy(..))

import Isacle.Hdl.Net

-- ---------------------------------------------------------------------------
-- Per-signal clock domain
-- ---------------------------------------------------------------------------

data Sig (dom :: k) a
    = SWire WireId
    | SExpr (NetM WireId)

materialize :: Sig dom a -> NetM WireId
materialize (SWire wid) = pure wid
materialize (SExpr m)   = m

-- | Bind a 'Sig' to a fresh named wire, enabling safe fanout.
wire :: Sig dom a -> NetM (Sig dom a)
wire s = SWire <$> materialize s

-- ---------------------------------------------------------------------------
-- Num instance — combinational arithmetic
-- ---------------------------------------------------------------------------

primSig2 :: PrimOp -> Sig dom a -> Sig dom b -> Sig dom c
primSig2 op a b = SExpr $ do
    wa <- materialize a
    wb <- materialize b
    out <- freshWire
    emit $ NComb out op [wa, wb]
    pure out

primSig1 :: PrimOp -> Sig dom a -> Sig dom b
primSig1 op a = SExpr $ do
    wa <- materialize a
    out <- freshWire
    emit $ NComb out op [wa]
    pure out

-- ---------------------------------------------------------------------------
-- Comparison and logical operations
-- ---------------------------------------------------------------------------

infix 4 .==.
(.==.) :: HdlType a => Sig dom a -> Sig dom a -> Sig dom Bool
(.==.) = primSig2 PEq

infix 4 .<.
(.<.) :: HdlType a => Sig dom a -> Sig dom a -> Sig dom Bool
(.<.) = primSig2 PLt

infixr 3 .&&.
(.&&.) :: Sig dom Bool -> Sig dom Bool -> Sig dom Bool
(.&&.) = primSig2 PAnd

infixr 2 .||.
(.||.) :: Sig dom Bool -> Sig dom Bool -> Sig dom Bool
(.||.) = primSig2 POr

sigAnd :: Sig dom Bool -> Sig dom Bool -> Sig dom Bool
sigAnd = primSig2 PAnd

sigOr :: Sig dom Bool -> Sig dom Bool -> Sig dom Bool
sigOr = primSig2 POr

sigNot :: Sig dom Bool -> Sig dom Bool
sigNot = primSig1 PNot

-- | Multiplexer: @mux sel t f@ chooses @t@ when @sel@ is high, @f@ otherwise.
mux :: Sig dom Bool -> Sig dom a -> Sig dom a -> Sig dom a
mux sel t f = SExpr $ do
    ws <- materialize sel
    wt <- materialize t
    wf <- materialize f
    out <- freshWire
    emit $ NComb out PMux [ws, wt, wf]
    pure out

-- | Logical shift left by a dynamic (runtime) amount signal.
sigShiftLDyn :: Sig dom a -> Sig dom b -> Sig dom a
sigShiftLDyn = primSig2 PShiftL

-- | Logical shift right by a dynamic (runtime) amount signal.
sigShiftRDyn :: Sig dom a -> Sig dom b -> Sig dom a
sigShiftRDyn = primSig2 PShiftR

-- | Extract a single bit at a dynamic (runtime) index.
sigBitDyn :: Sig dom a -> Sig dom b -> Sig dom Bool
sigBitDyn val idx = sigBit 0 (sigShiftRDyn val idx)

-- | Logical shift left by a compile-time constant.
sigShiftL :: Int -> Sig dom a -> Sig dom a
sigShiftL n s = SExpr $ do
    ws  <- materialize s
    wa  <- freshWire
    emit $ NComb wa (PLit (toInteger n) 8) []
    out <- freshWire
    emit $ NComb out PShiftL [ws, wa]
    pure out

-- | Logical shift right by a compile-time constant.
sigShiftR :: Int -> Sig dom a -> Sig dom a
sigShiftR n s = SExpr $ do
    ws  <- materialize s
    wa  <- freshWire
    emit $ NComb wa (PLit (toInteger n) 8) []
    out <- freshWire
    emit $ NComb out PShiftR [ws, wa]
    pure out

-- | Extract a single bit at position @n@ as a Bool signal.
sigBit :: Int -> Sig dom a -> Sig dom Bool
sigBit n s = SExpr $ do
    ws  <- materialize s
    out <- freshWire
    emit $ NComb out (PSlice n n) [ws]
    pure out

-- ---------------------------------------------------------------------------

instance (HdlType a, Num a) => Num (Sig dom a) where
    (+)    = primSig2 PAdd
    (-)    = primSig2 PSub
    (*)    = primSig2 PMul
    negate = primSig1 PNot
    abs    = id
    signum = const (SExpr $ do { out <- freshWire; emit $ NComb out (PLit 1 w) []; pure out })
      where w = fromIntegral (natVal (Proxy @(Width a)))
    fromInteger n = SExpr $ do
        out <- freshWire
        emit $ NComb out (PLit n w) []
        pure out
      where w = fromIntegral (natVal (Proxy @(Width a)))

-- ---------------------------------------------------------------------------
-- Synthesizability
-- ---------------------------------------------------------------------------

class KnownNat (Width a) => HdlType (a :: Type) where
    type Width a :: Nat
    toBits   :: a -> Integer
    fromBits :: Integer -> a

instance HdlType Bool where
    type Width Bool = 1
    toBits False = 0
    toBits True  = 1
    fromBits 0   = False
    fromBits _   = True

-- ---------------------------------------------------------------------------
-- Port bundle typeclass
-- ---------------------------------------------------------------------------

data PortSpec = PortSpec
    { portName  :: String
    , portWidth :: Int
    , portDom   :: DomId
    } deriving (Show, Eq)

-- | Types that describe a bundle of HDL ports.
-- 'portCount' and 'portSpecs' describe structure; 'toWireIds'/'fromWireIds'
-- convert between Haskell signal bundles and flat lists of wire identifiers.
class HdlPorts a where
    -- | Number of wires in this port bundle.
    portCount   :: Proxy a -> Int
    -- | Port metadata in bundle order.
    portSpecs   :: DomId -> Proxy a -> [PortSpec]
    -- | Materialize all signals in the bundle to a flat wire list.
    toWireIds   :: a -> NetM [WireId]
    -- | Wrap a flat list of wire IDs back into a bundle (output side).
    fromWireIds :: [WireId] -> a

instance HdlType a => HdlPorts (Sig dom a) where
    portCount _ = 1
    portSpecs d _ = [PortSpec
        { portName  = "sig"
        , portWidth = fromIntegral (natVal (Proxy :: Proxy (Width a)))
        , portDom   = d }]
    toWireIds sig     = (:[]) <$> materialize sig
    fromWireIds [w]   = SWire w
    fromWireIds _     = error "HdlPorts (Sig): fromWireIds: wrong wire count"

instance (HdlPorts a, HdlPorts b) => HdlPorts (a, b) where
    portCount _ = portCount (Proxy @a) + portCount (Proxy @b)
    portSpecs d _ = portSpecs d (Proxy @a) ++ portSpecs d (Proxy @b)
    toWireIds (a, b) = (++) <$> toWireIds a <*> toWireIds b
    fromWireIds ws =
        let n = portCount (Proxy @a)
            (wa, wb) = splitAt n ws
        in (fromWireIds wa, fromWireIds wb)

instance (HdlPorts a, HdlPorts b, HdlPorts c) => HdlPorts (a, b, c) where
    portCount _ = portCount (Proxy @a) + portCount (Proxy @b) + portCount (Proxy @c)
    portSpecs d _ = portSpecs d (Proxy @a)
                 ++ portSpecs d (Proxy @b)
                 ++ portSpecs d (Proxy @c)
    toWireIds (a, b, c) = (\x y z -> x ++ y ++ z)
        <$> toWireIds a <*> toWireIds b <*> toWireIds c
    fromWireIds ws =
        let na = portCount (Proxy @a)
            nb = portCount (Proxy @b)
            (wa, rest) = splitAt na ws
            (wb, wc)   = splitAt nb rest
        in (fromWireIds wa, fromWireIds wb, fromWireIds wc)

-- ---------------------------------------------------------------------------
-- KnownDom
-- ---------------------------------------------------------------------------

class KnownDom (dom :: k) where
    domId :: Proxy dom -> DomId

-- ---------------------------------------------------------------------------
-- HdlComponent
-- ---------------------------------------------------------------------------

-- | A named HDL entity.  'i' and 'o' are phantom types representing the
-- input and output port bundles; 'portBody' is the circuit description that
-- declares all ports via 'inputS'/'outputS'.
data HdlComponent i o = HdlComponent
    { entityName :: String
    , portBody   :: NetM ()
    }

-- | Construct a named component from a port-declaring circuit body.
-- The body must use 'inputS'/'outputS' for all ports; 'i' and 'o' are
-- phantom types that constrain callers via 'HdlPorts'.
block :: String -> NetM () -> HdlComponent i o
block = HdlComponent
