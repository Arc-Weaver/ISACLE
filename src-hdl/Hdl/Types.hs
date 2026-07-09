{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE UndecidableInstances #-}
module Hdl.Types
    ( -- * Per-signal domain tags
      Sig(..)
    , Signal(..)
    , materialize
      -- * Representation tagging
    , withRepr
    , sigReinterpret
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
      -- * Operation classes on HdlType values (lifted to signals)
    , HdlEq(..)
    , HdlOrd(..)
    , HdlPorts(..)
    , PortSpec(..)
      -- * Generic derivation support (satisfy derived-instance constraints)
    , PortLayout(..)
      -- * Generic HdlType derivation (records → packed value, Width = Σ fields)
    , GHdlType(..)
    , GWidth
    , genericToBits
    , genericFromBits
      -- * Record field extraction (bit-maps)
    , GFields(..)
    , recordFields
    , recordFieldPos
    , projectField
    , updateField
      -- * Clock domain typeclass
    , KnownDom(..)
    ) where

import Prelude
import GHC.TypeLits (KnownNat, Nat, natVal, type (+))
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import System.Mem.StableName (makeStableName)
import System.IO.Unsafe (unsafePerformIO)
import GHC.Generics
    ( Generic, Rep, from, to
    , M1(..), K1(..), U1(..)
    , (:*:)(..), (:+:)(..)
    , D, S, R, C
    , Selector, selName
    , Constructor, conName
    )

import Hdl.Net

-- ---------------------------------------------------------------------------
-- Per-signal clock domain
-- ---------------------------------------------------------------------------

data Sig (dom :: k) (a :: Type)
    = SWire WireId
    | SExpr (NetM WireId)

materialize :: Sig dom a -> NetM WireId
materialize (SWire wid) = pure wid
materialize (SExpr m)   = memoSExpr m sn
  where sn = unsafePerformIO (makeStableName m)

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

-- ---------------------------------------------------------------------------
-- Signal — the combinational signal surface, abstract over the interpreter
-- ---------------------------------------------------------------------------

-- | The combinational signal value surface, abstract over the interpreter
-- (tagless-final, P1).  'Sig' is the synthesis interpreter (→ NetNode graph);
-- simulation / documentation interpreters are future instances.  The two
-- primitive-application methods are the core; the pure combinational operators
-- (comparison, logic, bitwise, …) are derived free functions over any 'Signal'.
--
-- Synth-specific operations — 'withRepr', 'sigReinterpret', 'materialize' — are
-- deliberately /not/ methods: they belong to the wire/synthesis model, not to a
-- backend-agnostic signal.
class Signal (sig :: k -> Type -> Type) where
    -- | Apply a unary primitive operation.  Every signal type must be an
    -- 'HdlType' — so a phantom-erased @sig dom ()@ (no 'HdlType' instance) is a
    -- compile error: a signal always carries a real representation and width.
    sigPrim1 :: (HdlType a, HdlType b) => PrimOp -> sig dom a -> sig dom b
    -- | Apply a binary primitive operation.
    sigPrim2 :: (HdlType a, HdlType b, HdlType c)
             => PrimOp -> sig dom a -> sig dom b -> sig dom c
    -- | Apply a ternary primitive operation (e.g. mux).
    sigPrim3 :: (HdlType a, HdlType b, HdlType c, HdlType d)
             => PrimOp -> sig dom a -> sig dom b -> sig dom c -> sig dom d
    -- | A raw bit-pattern literal of the given value and width.  The low-level
    -- escape (genuinely value-level widths only, e.g. inside the emitter); typed
    -- code uses 'sigLit', whose width comes from the type and so cannot disagree.
    sigLitW  :: Integer -> Int -> sig dom a
    -- | A typed literal: its width is @'Width' a@, taken from the type — the
    -- type-driven replacement for 'sigLitW' (a literal can't disagree with the
    -- representation it's used at).
    sigLit   :: HdlType a => Integer -> sig dom a
    default sigLit :: forall dom a. HdlType a => Integer -> sig dom a
    sigLit v = sigLitW v (fromIntegral (natVal (Proxy @(Width a))))
    -- | Re-tag a signal's phantom representation type (same wire) — the lowering
    -- bridge where a fixed backend signal is viewed at a caller-demanded type.
    -- Sound only where the widths coincide; used at a few instruction-decode
    -- seams (instruction word / bus wires viewed at the demanded width).
    sigRetype :: (HdlType a, HdlType b) => sig dom a -> sig dom b

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
-- emitter declares it with the right VHDL signal type.  Apply where a typed
-- value originates (ports, registers) — the emitter then propagates the tag
-- through combinational ops, so intermediate results inherit it.
withRepr :: forall dom a. HdlType a => Sig dom a -> Sig dom a
withRepr s = SExpr $ do
    w <- materialize s
    reprWire w (hdlRepr (Proxy @a))
    pure w

-- | Reinterpret a signal's bits as another representation of the /same width/
-- (e.g. @Unsigned n@ ↔ @Signed n@).  Unlike 'withRepr', which retags a leaf
-- wire in place, this emits a distinct cast wire (VHDL @signed(..)@\/@unsigned(..)@),
-- so the source wire keeps its own representation and may still be used under it.
-- This is the seam to cross between an unsigned bus and a signed datapath.
sigReinterpret :: forall b dom a. HdlType b => Sig dom a -> Sig dom b
sigReinterpret s = SExpr $ do
    w <- materialize s
    lookupOrEmit (PReinterpret (hdlRepr (Proxy @b))) [w]

-- ---------------------------------------------------------------------------
-- Comparison and logical operations
-- ---------------------------------------------------------------------------

infix 4 .==.
(.==.) :: (Signal sig, HdlEq a) => sig dom a -> sig dom a -> sig dom Bool
(.==.) = sigPrim2 PEq

infix 4 .<.
(.<.) :: (Signal sig, HdlOrd a) => sig dom a -> sig dom a -> sig dom Bool
(.<.) = sigPrim2 PLt

infixr 3 .&&.
(.&&.) :: Signal sig => sig dom Bool -> sig dom Bool -> sig dom Bool
(.&&.) = sigPrim2 PAnd

infixr 2 .||.
(.||.) :: Signal sig => sig dom Bool -> sig dom Bool -> sig dom Bool
(.||.) = sigPrim2 POr

sigAnd :: Signal sig => sig dom Bool -> sig dom Bool -> sig dom Bool
sigAnd = sigPrim2 PAnd

sigOr :: Signal sig => sig dom Bool -> sig dom Bool -> sig dom Bool
sigOr = sigPrim2 POr

sigNot :: Signal sig => sig dom Bool -> sig dom Bool
sigNot = sigPrim1 PNot

-- | Multiplexer: @mux sel t f@ chooses @t@ when @sel@ is high, @f@ otherwise.
mux :: (Signal sig, HdlType a) => sig dom Bool -> sig dom a -> sig dom a -> sig dom a
mux = sigPrim3 PMux

-- | Logical shift left by a dynamic (runtime) amount signal.
sigShiftLDyn :: (Signal sig, HdlType a, HdlType b) => sig dom a -> sig dom b -> sig dom a
sigShiftLDyn = sigPrim2 PShiftL

-- | Logical shift right by a dynamic (runtime) amount signal.
sigShiftRDyn :: (Signal sig, HdlType a, HdlType b) => sig dom a -> sig dom b -> sig dom a
sigShiftRDyn = sigPrim2 PShiftR

-- | Extract a single bit at a dynamic (runtime) index.
sigBitDyn :: (Signal sig, HdlType a, HdlType b) => sig dom a -> sig dom b -> sig dom Bool
sigBitDyn val idx = sigBit 0 (sigShiftRDyn val idx)

-- | Logical shift left by a compile-time constant.
sigShiftL :: forall sig dom a. (Signal sig, HdlType a) => Int -> sig dom a -> sig dom a
sigShiftL n s = sigPrim2 PShiftL s (sigLitW (toInteger n) 8 :: sig dom a)

-- | Logical shift right by a compile-time constant.
sigShiftR :: forall sig dom a. (Signal sig, HdlType a) => Int -> sig dom a -> sig dom a
sigShiftR n s = sigPrim2 PShiftR s (sigLitW (toInteger n) 8 :: sig dom a)

-- | Extract a single bit at position @n@ as a Bool signal.
sigBit :: (Signal sig, HdlType a) => Int -> sig dom a -> sig dom Bool
sigBit n = sigPrim1 (PSlice n n)

infixl 9 !
-- | Bit-index operator: @sig ! n@ extracts bit @n@ (0 = LSB).
(!) :: (Signal sig, HdlType a) => sig dom a -> Int -> sig dom Bool
(!) = flip sigBit

-- ---------------------------------------------------------------------------
-- Bitwise operations on arbitrary signal types
-- ---------------------------------------------------------------------------

-- | Bitwise AND — works on any HdlType, not just Bool.
sigBwAnd :: (Signal sig, HdlType a) => sig dom a -> sig dom a -> sig dom a
sigBwAnd = sigPrim2 PAnd

-- | Bitwise OR.
sigBwOr :: (Signal sig, HdlType a) => sig dom a -> sig dom a -> sig dom a
sigBwOr = sigPrim2 POr

-- | Bitwise XOR.
sigBwXor :: (Signal sig, HdlType a) => sig dom a -> sig dom a -> sig dom a
sigBwXor = sigPrim2 PXor

-- | Bitwise NOT.
sigBwNot :: (Signal sig, HdlType a) => sig dom a -> sig dom a
sigBwNot = sigPrim1 PNot

-- ---------------------------------------------------------------------------
-- Concatenation and resize
-- ---------------------------------------------------------------------------

-- | Concatenate two signals: @sigConcat hi lo@ places @hi@ in the upper bits.
sigConcat :: (Signal sig, HdlType a, HdlType b, HdlType c)
          => sig dom a -> sig dom b -> sig dom c
sigConcat = sigPrim2 PConcat

-- | Resize a signal to @m@ bits (zero-extend or truncate).
-- The output phantom type @b@ is unconstrained — caller picks it via context
-- or a type annotation.  Use @TypeApplications@ to supply @m@.
sigResize :: forall m sig dom (a :: Type) (b :: Type).
             (Signal sig, KnownNat m, HdlType a, HdlType b) => sig dom a -> sig dom b
sigResize = sigPrim1 (PResize (fromIntegral (natVal (Proxy @m))))

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
-- Generic HdlType derivation (records → a packed value; Width = Σ fields)
--
-- A record whose fields are all 'HdlType' becomes an 'HdlType' by deriving
-- 'Generic' and writing:
--
-- @
-- data Foo = Foo { a :: Unsigned 4, b :: Signed 4 } deriving Generic
-- instance HdlType Foo where
--   type Width Foo = GWidth (Rep Foo)
--   toBits   = genericToBits
--   fromBits = genericFromBits
-- @
--
-- Packing is MSB-first in field order (the first field occupies the high bits),
-- matching the flatten used for memory/bus.
-- ---------------------------------------------------------------------------

-- | Type-level sum of field widths over a 'Generic' representation.
type family GWidth (f :: Type -> Type) :: Nat where
    GWidth U1         = 0
    GWidth (M1 i c f) = GWidth f
    GWidth (K1 r a)   = Width a
    GWidth (f :*: g)  = GWidth f + GWidth g

-- | Value-level packing over a 'Generic' representation.
class GHdlType (f :: Type -> Type) where
    gWidth    :: Int                 -- ^ runtime field-bits width
    gToBits   :: f p -> Integer
    gFromBits :: Integer -> f p

instance GHdlType U1 where
    gWidth      = 0
    gToBits   _ = 0
    gFromBits _ = U1

instance GHdlType f => GHdlType (M1 i c f) where
    gWidth          = gWidth @f
    gToBits  (M1 x) = gToBits x
    gFromBits n     = M1 (gFromBits n)

instance HdlType a => GHdlType (K1 r a) where
    gWidth          = fromIntegral (natVal (Proxy @(Width a)))
    gToBits  (K1 a) = toBits a
    gFromBits n     = K1 (fromBits n)

instance (GHdlType f, GHdlType g) => GHdlType (f :*: g) where
    gWidth            = gWidth @f + gWidth @g
    gToBits (x :*: y) = (gToBits x `shiftL` gWidth @g) .|. gToBits y
    gFromBits n       = gFromBits (n `shiftR` wg) :*: gFromBits (n .&. ((1 `shiftL` wg) - 1))
      where wg = gWidth @g

-- | Derived 'toBits' for a record: pack fields MSB-first in field order.
genericToBits :: (Generic a, GHdlType (Rep a)) => a -> Integer
genericToBits = gToBits . from

-- | Derived 'fromBits' for a record: inverse of 'genericToBits'.
genericFromBits :: (Generic a, GHdlType (Rep a)) => Integer -> a
genericFromBits = to . gFromBits

-- ---------------------------------------------------------------------------
-- Operation classes on HdlType values — the per-type semantics, lifted to
-- signals (the lift is 'Signal'/'sigPrim').  Each method is the value-level
-- meaning of the operation, which also yields the simulation interpreter
-- directly.  Defaults cover every 'HdlType' (signed vs unsigned via 'hdlRepr'),
-- so the blanket instances below carry no per-type boilerplate.
-- ---------------------------------------------------------------------------

-- | Equality.  Lifted to signals as '(.==.)'.
class HdlType a => HdlEq a where
    hEq :: a -> a -> Bool
    hEq x y = toBits x == toBits y

-- | Ordering — signed or unsigned per the type's 'hdlRepr'.  Lifted to signals
-- as '(.<.)'.
class HdlEq a => HdlOrd a where
    hLt :: a -> a -> Bool
    hLt x y = case hdlRepr (Proxy @a) of
        RSigned -> asSigned (toBits x) < asSigned (toBits y)
        _       -> toBits x < toBits y
      where
        n :: Int
        n = fromIntegral (natVal (Proxy @(Width a)))
        asSigned v = if v >= (2 :: Integer) ^ (n - 1)
                       then v - (2 :: Integer) ^ n else v

instance HdlType a => HdlEq a
instance HdlType a => HdlOrd a

-- ---------------------------------------------------------------------------
-- Record field extraction (for bit-maps / documentation)
-- ---------------------------------------------------------------------------

-- | Generic extraction of a record's field names and bit widths, in
-- declaration order.  Used to derive a register's bit-fields from a record
-- 'HdlType' (so CPU flags and peripheral bit-fields share one mechanism).
class GFields (f :: Type -> Type) where
    gFields :: [(String, Int)]

instance GFields U1 where
    gFields = []

instance GFields f => GFields (M1 D c f) where
    gFields = gFields @f

instance GFields f => GFields (M1 C c f) where
    gFields = gFields @f

instance (Selector s, HdlType a) => GFields (M1 S s (K1 R a)) where
    gFields = [ ( selName (undefined :: M1 S s (K1 R a) ())
                , fromIntegral (natVal (Proxy @(Width a))) ) ]

instance (GFields f, GFields g) => GFields (f :*: g) where
    gFields = gFields @f ++ gFields @g

-- | A record 'HdlType'\'s @(fieldName, bitWidth)@ list, in declaration order.
recordFields :: forall a. (Generic a, GFields (Rep a)) => Proxy a -> [(String, Int)]
recordFields _ = gFields @(Rep a)

-- ---------------------------------------------------------------------------
-- Generic record-field projection (typed, over any 'Signal' backend)
--
-- The core-state record is one 'HdlType'; its fields are read and updated by
-- these plain helper functions over 'Signal' — no backend, no CPU concepts.
-- Packing is MSB-first in field order (see 'GHdlType'), so a field's LSB offset
-- is the sum of the widths of the fields declared after it.
-- ---------------------------------------------------------------------------

-- | @(lo-bit offset, width)@ of the named field within a record's packing.
recordFieldPos :: forall r. (Generic r, GFields (Rep r)) => Proxy r -> String -> (Int, Int)
recordFieldPos _ fld =
    case break ((== fld) . fst) (recordFields (Proxy @r)) of
        (_, (_, w) : after) -> (sum (map snd after), w)   -- lo = widths below the field
        (_, [])             -> error ("recordFieldPos: no field " ++ fld)

-- | Read the named field of a record-typed signal, at the field's type @a@.
projectField :: forall a r sig dom.
                (Signal sig, HdlType r, HdlType a, Generic r, GFields (Rep r))
             => String -> sig dom r -> sig dom a
projectField fld rec =
    let (lo, w) = recordFieldPos (Proxy @r) fld
    in sigPrim1 (PSlice (lo + w - 1) lo) rec

-- | Return the record with its named field replaced by @val@ (other fields kept).
updateField :: forall a r sig dom.
               (Signal sig, HdlType r, HdlType a, Generic r, GFields (Rep r))
            => String -> sig dom a -> sig dom r -> sig dom r
updateField fld val rec =
    let (lo, w)  = recordFieldPos (Proxy @r) fld
        total    = fromIntegral (natVal (Proxy @(Width r)))
        maskVal  = (2 ^ total - 1) - (2 ^ (lo + w) - 2 ^ lo)   -- 1s except [lo, lo+w-1]
        cleared  = sigPrim2 PAnd rec (sigLit maskVal :: sig dom r) :: sig dom r
        widened  = sigPrim1 (PResize total) val                    :: sig dom r
        shifted  = sigPrim2 PShiftL widened (sigLit (fromIntegral lo) :: sig dom r) :: sig dom r
    in sigPrim2 POr cleared shifted

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

