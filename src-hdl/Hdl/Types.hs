{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE UndecidableInstances #-}
module Hdl.Types
    ( -- * Per-signal domain tags
      Sig(..)
    , materialize
      -- * Representation tagging
    , withRepr
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
    , (!)
      -- * Bitwise operations on any signal type
    , sigBwAnd
    , sigBwOr
    , sigBwXor
    , sigBwNot
      -- * Concatenation and resize
    , sigConcat
    , sigResize
      -- * Synthesizability constraints
    , HdlType(..)
    , HdlPorts(..)
    , PortSpec(..)
      -- * Generic derivation support (satisfy derived-instance constraints)
    , PortLayout(..)
      -- * Clock domain typeclass
    , KnownDom(..)
    ) where

import Prelude
import GHC.TypeLits (KnownNat, Nat, natVal)
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Coerce (coerce)
import System.Mem.StableName (makeStableName, hashStableName)
import System.IO.Unsafe (unsafePerformIO)
import GHC.Generics
    ( Generic, Rep, from, to
    , M1(..), K1(..), U1(..)
    , (:*:)(..), (:+:)(..)
    , S, R, C
    , Selector, selName
    , Constructor, conName
    )

import Hdl.Net

-- ---------------------------------------------------------------------------
-- Per-signal clock domain
-- ---------------------------------------------------------------------------

data Sig (dom :: k) a
    = SWire WireId
    | SExpr (NetM WireId)

materialize :: Sig dom a -> NetM WireId
materialize (SWire wid) = pure wid
materialize (SExpr m)   = memoSExpr m key
  where key = unsafePerformIO (hashStableName <$> makeStableName m)

-- ---------------------------------------------------------------------------
-- Num instance — combinational arithmetic
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

-- | Tag a typed signal's wire with its representation (from 'hdlRepr'), so the
-- emitter declares it with the right VHDL signal type.  Apply where a typed
-- value originates (ports, registers) — the emitter then propagates the tag
-- through combinational ops, so intermediate results inherit it.
withRepr :: forall dom a. HdlType a => Sig dom a -> Sig dom a
withRepr s = SExpr $ do
    w <- materialize s
    reprWire w (hdlRepr (Proxy @a))
    pure w

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
    lookupOrEmit PMux [ws, wt, wf]

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
    ws <- materialize s
    wa <- lookupOrEmit (PLit (toInteger n) 8) []
    lookupOrEmit PShiftL [ws, wa]

-- | Logical shift right by a compile-time constant.
sigShiftR :: Int -> Sig dom a -> Sig dom a
sigShiftR n s = SExpr $ do
    ws <- materialize s
    wa <- lookupOrEmit (PLit (toInteger n) 8) []
    lookupOrEmit PShiftR [ws, wa]

-- | Extract a single bit at position @n@ as a Bool signal.
sigBit :: Int -> Sig dom a -> Sig dom Bool
sigBit n s = SExpr $ do
    ws <- materialize s
    lookupOrEmit (PSlice n n) [ws]

infixl 9 !
-- | Bit-index operator: @sig ! n@ extracts bit @n@ (0 = LSB).
(!) :: Sig dom a -> Int -> Sig dom Bool
(!) = flip sigBit

-- ---------------------------------------------------------------------------
-- Bitwise operations on arbitrary signal types
-- ---------------------------------------------------------------------------

-- | Bitwise AND — works on any HdlType, not just Bool.
sigBwAnd :: Sig dom a -> Sig dom a -> Sig dom a
sigBwAnd = primSig2 PAnd

-- | Bitwise OR.
sigBwOr :: Sig dom a -> Sig dom a -> Sig dom a
sigBwOr = primSig2 POr

-- | Bitwise XOR.
sigBwXor :: Sig dom a -> Sig dom a -> Sig dom a
sigBwXor = primSig2 PXor

-- | Bitwise NOT.
sigBwNot :: Sig dom a -> Sig dom a
sigBwNot = primSig1 PNot

-- ---------------------------------------------------------------------------
-- Concatenation and resize
-- ---------------------------------------------------------------------------

-- | Concatenate two signals: @sigConcat hi lo@ places @hi@ in the upper bits.
sigConcat :: Sig dom a -> Sig dom b -> Sig dom c
sigConcat = primSig2 PConcat

-- | Resize a signal to @m@ bits (zero-extend or truncate).
-- The output phantom type @b@ is unconstrained — caller picks it via context
-- or a type annotation.  Use @TypeApplications@ to supply @m@.
sigResize :: forall m dom (a :: Type) (b :: Type). KnownNat m => Sig dom a -> Sig dom b
sigResize s = coerce (result :: Sig dom a)
  where
    result = SExpr $ do
        ws <- materialize s
        lookupOrEmit (PResize (fromIntegral (natVal (Proxy @m)))) [ws]

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
-- Synthesizability
-- ---------------------------------------------------------------------------

class KnownNat (Width a) => HdlType (a :: Type) where
    type Width a :: Nat
    toBits   :: a -> Integer
    fromBits :: Integer -> a
    -- | The wire representation this type erases to.  Drives the emitter's
    -- VHDL signal type (and thus numeric_std overloading).  Defaults to
    -- 'RUnsigned'; signed types override it.
    hdlRepr  :: Proxy a -> Repr
    hdlRepr _ = RUnsigned

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
--
-- For any Haskell record whose fields are all 'Sig' values (or other
-- 'HdlPorts' bundles), the four methods can be derived automatically:
--
-- @
-- data MyPorts = MyPorts { foo :: Sig SysClk (Unsigned 8), bar :: Sig SysClk Bool }
--   deriving (Generic, HdlPorts)
-- @
class HdlPorts a where
    -- | Number of wires in this port bundle.
    portCount   :: Proxy a -> Int
    -- | Port metadata in bundle order.  Each element knows its own domain.
    portSpecs   :: Proxy a -> [PortSpec]
    -- | Materialize all signals in the bundle to a flat wire list.
    toWireIds   :: a -> NetM [WireId]
    -- | Wrap a flat list of wire IDs back into a bundle (output side).
    fromWireIds :: [WireId] -> a

    default portCount   :: (Generic a, PortLayout (Rep a)) => Proxy a -> Int
    portCount _ = length (layoutSpecs @(Rep a))

    default portSpecs   :: (Generic a, PortLayout (Rep a)) => Proxy a -> [PortSpec]
    portSpecs _ = layoutSpecs @(Rep a)

    default toWireIds   :: (Generic a, PortLayout (Rep a)) => a -> NetM [WireId]
    toWireIds = layoutEncode . from

    default fromWireIds :: (Generic a, PortLayout (Rep a)) => [WireId] -> a
    fromWireIds ws = to (fst (layoutDecode @(Rep a) ws))

instance HdlPorts () where
    portCount   _ = 0
    portSpecs   _ = []
    toWireIds   _ = return []
    fromWireIds _ = ()

instance (HdlType a, KnownDom dom) => HdlPorts (Sig dom a) where
    portCount _ = 1
    portSpecs _ = [PortSpec
        { portName  = "sig"
        , portWidth = fromIntegral (natVal (Proxy :: Proxy (Width a)))
        , portDom   = domId (Proxy @dom) }]
    toWireIds sig     = (:[]) <$> materialize sig
    fromWireIds [w]   = SWire w
    fromWireIds _     = error "HdlPorts (Sig): fromWireIds: wrong wire count"

instance (HdlPorts a, HdlPorts b) => HdlPorts (a, b) where
    portCount _ = portCount (Proxy @a) + portCount (Proxy @b)
    portSpecs _ = portSpecs (Proxy @a) ++ portSpecs (Proxy @b)
    toWireIds (a, b) = (++) <$> toWireIds a <*> toWireIds b
    fromWireIds ws =
        let n = portCount (Proxy @a)
            (wa, wb) = splitAt n ws
        in (fromWireIds wa, fromWireIds wb)

instance (HdlPorts a, HdlPorts b, HdlPorts c) => HdlPorts (a, b, c) where
    portCount _ = portCount (Proxy @a) + portCount (Proxy @b) + portCount (Proxy @c)
    portSpecs _ = portSpecs (Proxy @a)
               ++ portSpecs (Proxy @b)
               ++ portSpecs (Proxy @c)
    toWireIds (a, b, c) = (\x y z -> x ++ y ++ z)
        <$> toWireIds a <*> toWireIds b <*> toWireIds c
    fromWireIds ws =
        let na = portCount (Proxy @a)
            nb = portCount (Proxy @b)
            (wa, rest) = splitAt na ws
            (wb, wc)   = splitAt nb rest
        in (fromWireIds wa, fromWireIds wb, fromWireIds wc)

-- ---------------------------------------------------------------------------
-- Generic port bundle machinery
-- ---------------------------------------------------------------------------

-- | Generic traversal for 'HdlPorts' derivation.  Users never write instances
-- of this class directly; it is satisfied automatically by the structure of
-- any @deriving Generic@ data type.
class PortLayout (f :: Type -> Type) where
    layoutSpecs  :: [PortSpec]
    layoutEncode :: f p -> NetM [WireId]
    layoutDecode :: [WireId] -> (f p, [WireId])

instance PortLayout U1 where
    layoutSpecs  = []
    layoutEncode _ = pure []
    layoutDecode ws = (U1, ws)

-- Datatype and constructor wrappers: delegate straight through.
instance {-# OVERLAPPABLE #-} PortLayout f => PortLayout (M1 i m f) where
    layoutSpecs  = layoutSpecs @f
    layoutEncode (M1 x) = layoutEncode x
    layoutDecode ws = let (x, ws') = layoutDecode @f ws in (M1 x, ws')

-- Selector field: use the Haskell field name as the port name.
instance {-# OVERLAPPING #-} (Selector s, HdlPorts a)
      => PortLayout (M1 S s (K1 R a)) where
    layoutSpecs =
        let nm    = selName (undefined :: M1 S s (K1 R a) ())
            specs = portSpecs (Proxy @a)
        in map (\p -> p { portName = if null nm then portName p else nm }) specs
    layoutEncode (M1 (K1 a)) = toWireIds a
    layoutDecode ws =
        let n             = portCount (Proxy @a)
            (taken, rest) = splitAt n ws
        in (M1 (K1 (fromWireIds taken)), rest)

-- Product of two fields.
instance (PortLayout f, PortLayout g) => PortLayout (f :*: g) where
    layoutSpecs = layoutSpecs @f ++ layoutSpecs @g
    layoutEncode (x :*: y) = (++) <$> layoutEncode x <*> layoutEncode y
    layoutDecode ws =
        let (x, ws')  = layoutDecode @f ws
            (y, ws'') = layoutDecode @g ws'
        in (x :*: y, ws'')

-- Constructor wrapper: prefix every field name with the constructor name.
instance {-# OVERLAPPING #-} (Constructor c, PortLayout f) => PortLayout (M1 C c f) where
    layoutSpecs =
        let cname = conName (undefined :: M1 C c f ())
        in map (\p -> p { portName = cname ++ "_" ++ portName p }) (layoutSpecs @f)
    layoutEncode (M1 x) = layoutEncode x
    layoutDecode ws = let (x, ws') = layoutDecode @f ws in (M1 x, ws')

-- ---------------------------------------------------------------------------
-- Sum type layout
--
-- SumLayout traverses the constructor tree to collect all field specs and
-- compute flat constructor indices, enabling a single correctly-sized tag port.
-- PortLayout (f :+: g) delegates to SumLayout so nested :+: nodes do NOT each
-- contribute their own tag bit.
-- ---------------------------------------------------------------------------

class SumLayout (f :: Type -> Type) where
    sumFieldSpecs :: [PortSpec]
    sumNumCons    :: Int
    -- Returns (constructor_index, all_field_wires) — unused fields are zero.
    sumEncode     :: f p -> NetM (Int, [WireId])
    -- Decode: always materialises the first (L1-most) constructor.
    sumDecode     :: [WireId] -> f p

instance {-# OVERLAPPING #-} (Constructor c, PortLayout f) => SumLayout (M1 C c f) where
    sumFieldSpecs = layoutSpecs @(M1 C c f)
    sumNumCons    = 1
    sumEncode (M1 x) = (0,) <$> layoutEncode x
    sumDecode ws     = let (x, _) = layoutDecode @f ws in M1 x

instance (SumLayout f, SumLayout g) => SumLayout (f :+: g) where
    sumFieldSpecs = sumFieldSpecs @f ++ sumFieldSpecs @g
    sumNumCons    = sumNumCons @f + sumNumCons @g
    sumEncode (L1 x) = do
        (idx, xWs) <- sumEncode x
        gZeros <- mapM (\p -> lookupOrEmit (PLit 0 (portWidth p)) []) (sumFieldSpecs @g)
        return (idx, xWs ++ gZeros)
    sumEncode (R1 y) = do
        (idx, yWs) <- sumEncode y
        fZeros <- mapM (\p -> lookupOrEmit (PLit 0 (portWidth p)) []) (sumFieldSpecs @f)
        return (sumNumCons @f + idx, fZeros ++ yWs)
    sumDecode ws =
        let nf = length (sumFieldSpecs @f)
        in L1 (sumDecode @f (take nf ws))

-- Bits needed to encode n distinct values (minimum 1).
tagWidth :: Int -> Int
tagWidth n = max 1 (ceiling (logBase 2 (fromIntegral n) :: Double))

-- Sum type: single tag port of ceil(log2(N)) bits, followed by all fields
-- from all constructors (unused fields zero on encode).
-- fromWireIds always decodes to the first constructor; use the tag signal
-- (first wire) with sigBit to discriminate constructors in the HDL body.
instance (SumLayout f, SumLayout g) => PortLayout (f :+: g) where
    layoutSpecs =
        let flds = sumFieldSpecs @f ++ sumFieldSpecs @g
            n    = sumNumCons @f + sumNumCons @g
            w    = tagWidth n
            dom  = case flds of
                     (s : _) -> portDom s
                     []      -> error "PortLayout (:+:): sum type has no Sig fields"
        in PortSpec { portName = "tag", portWidth = w, portDom = dom } : flds
    layoutEncode x = do
        let n = sumNumCons @f + sumNumCons @g
            w = tagWidth n
        (idx, ws) <- sumEncode @(f :+: g) x
        tagW <- lookupOrEmit (PLit (fromIntegral idx) w) []
        return (tagW : ws)
    layoutDecode (_ : ws) =
        let nf          = length (sumFieldSpecs @f)
            ng          = length (sumFieldSpecs @g)
            (fWs, rest) = splitAt nf ws
            (_, rest')  = splitAt ng rest
        in (L1 (sumDecode @f fWs), rest')
    layoutDecode [] = error "PortLayout (:+:): empty wire list"

-- ---------------------------------------------------------------------------
-- KnownDom
-- ---------------------------------------------------------------------------

class KnownDom (dom :: k) where
    domId :: Proxy dom -> DomId

