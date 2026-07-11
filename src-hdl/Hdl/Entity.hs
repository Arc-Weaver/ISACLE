{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE UndecidableInstances #-}
module Hdl.Entity
    ( -- * Port reference
      PortRef(..)
      -- * Generic derivation support
    , PortLabels(..)
      -- * Port mapping between bundles
    , PortMap
      -- * Behavioral description
    , hdl
      -- * Entity construction
    , Entity
    , entity
    , entityName
    , entityBody
      -- * Synthesis override
    , SynthTarget(..)
    , withSynth
      -- * Elaborated port declaration
    , Dir(..)
    , PortDecl(..)
      -- * Elaborated entity
    , ElabEntity(..)
      -- * Elaboration
    , elaborate
    , elaborateDesign
    ) where

import Prelude
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import qualified Data.Map.Strict as Map
import GHC.Generics
    ( Generic, Rep
    , M1(..), K1(..), U1(..)
    , (:*:)(..), (:+:)(..)
    , S, R, C
    , Selector, selName
    , Constructor, conName
    )

import Hdl.Net
import Hdl.Sig   (Sig)
import Hdl.Types

-- ---------------------------------------------------------------------------
-- PortRef
-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- PortLabels — Generic traversal for port name derivation
-- ---------------------------------------------------------------------------

-- | Generic traversal that extracts Haskell record field names as port names.
-- Users never write instances of this class; it is satisfied automatically by
-- @deriving Generic@.
class PortLabels (f :: Type -> Type) where
    portLabels :: [String]

instance PortLabels U1 where portLabels = []

instance {-# OVERLAPPABLE #-} PortLabels f => PortLabels (M1 i m f) where
    portLabels = portLabels @f

instance {-# OVERLAPPING #-} Selector s => PortLabels (M1 S s (K1 R a)) where
    portLabels = [selName (undefined :: M1 S s (K1 R a) ())]

instance (PortLabels f, PortLabels g) => PortLabels (f :*: g) where
    portLabels = portLabels @f ++ portLabels @g

instance {-# OVERLAPPING #-} (Constructor c, PortLabels f) => PortLabels (M1 C c f) where
    portLabels =
        let cname = conName (undefined :: M1 C c f ())
        in map (\n -> cname ++ "_" ++ n) (portLabels @f)

-- SumLabels mirrors SumLayout: collect all field labels without adding inner
-- tag entries, so that PortLabels (f :+: g) emits exactly one "tag".
class SumLabels (f :: Type -> Type) where
    sumLabels :: [String]

instance {-# OVERLAPPING #-} (Constructor c, PortLabels f) => SumLabels (M1 C c f) where
    sumLabels = portLabels @(M1 C c f)

instance (SumLabels f, SumLabels g) => SumLabels (f :+: g) where
    sumLabels = sumLabels @f ++ sumLabels @g

instance (SumLabels f, SumLabels g) => PortLabels (f :+: g) where
    portLabels = "tag" : sumLabels @f ++ sumLabels @g

-- ---------------------------------------------------------------------------
-- PortRef
-- ---------------------------------------------------------------------------

-- | A port bundle with declared port names.
-- Named record types can derive this automatically alongside 'HdlPorts':
--
-- @
-- data MyPorts = MyPorts { foo :: Sig SysClk Bool, bar :: Sig SysClk (Unsigned 8) }
--   deriving (Generic, HdlPorts, PortRef)
-- @
class HdlPorts a => PortRef a where
    portNames :: Proxy a -> [String]

    default portNames :: (Generic a, PortLabels (Rep a)) => Proxy a -> [String]
    portNames _ = portLabels @(Rep a)

instance PortRef () where
    portNames _ = []

instance (HdlType a, KnownDom dom) => PortRef (Sig dom a) where
    portNames _ = ["p"]

instance (PortRef a, PortRef b) => PortRef (a, b) where
    portNames _ = [ "p" ++ show i | i <- [0 .. portCount (Proxy @(a, b)) - 1] ]

instance (PortRef a, PortRef b, PortRef c) => PortRef (a, b, c) where
    portNames _ = [ "p" ++ show i | i <- [0 .. portCount (Proxy @(a, b, c)) - 1] ]

-- ---------------------------------------------------------------------------
-- PortMap
-- ---------------------------------------------------------------------------

-- | Structural compatibility between two port bundles for synthesis
-- substitution.  @PortMap i vi@ declares that our ports @i@ map onto
-- vendor ports @vi@.
class (PortRef i, PortRef vi) => PortMap i vi

-- ---------------------------------------------------------------------------
-- Port declarations
-- ---------------------------------------------------------------------------

data Dir = In | Out deriving (Show, Eq, Ord)

data PortDecl = PortDecl
    { pdName  :: String
    , pdDir   :: Dir
    , pdWidth :: Int
    , pdDom   :: DomId
    } deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- ElabEntity
-- ---------------------------------------------------------------------------

-- | An elaborated entity: port declarations and the flat node list.
-- This is the form consumed by the VHDL emitter.
data ElabEntity = ElabEntity
    { elabName  :: String
    , elabPorts :: [PortDecl]
    , elabNodes :: [NetNode]
    } deriving (Show)

-- ---------------------------------------------------------------------------
-- Entity
-- ---------------------------------------------------------------------------

-- | A first-class HDL entity.
--
-- 'entityBody' is the behavioral simulation model.
-- 'entitySynth', when present, substitutes a vendor primitive at synthesis;
-- the port mapping is expressed as type-safe functions between bundles.
data Entity i o = Entity
    { entityName  :: String
    , entityBody  :: i -> NetM o   -- ^ the netlist-backend body (pass 2: polymorphic @Hdl s m@)
    , entitySynth :: Maybe (SynthTarget i o)
    }

-- ---------------------------------------------------------------------------
-- SynthTarget
-- ---------------------------------------------------------------------------

data SynthTarget i o = forall vi vo.
    ( PortMap i vi
    , PortMap vo o
    ) => SynthTarget (Entity vi vo)

-- ---------------------------------------------------------------------------
-- Smart constructors
-- ---------------------------------------------------------------------------

-- | Identity for now (the body already is a @NetM@ computation); kept so call
-- sites read @bind "x" (hdl go)@.  Retires once bodies go polymorphic.
hdl :: (i -> NetM o) -> (i -> NetM o)
hdl = id

-- | Build an entity from a name and its behavioral description.
entity :: String -> (i -> NetM o) -> Entity i o
entity name body = Entity name body Nothing

-- | Attach a vendor synthesis target to an entity.
withSynth :: Entity i o -> SynthTarget i o -> Entity i o
withSynth e s = e { entitySynth = Just s }

-- ---------------------------------------------------------------------------
-- Elaboration
-- ---------------------------------------------------------------------------

-- | Elaborate an entity: allocate input wires, run the behavioral body,
-- emit 'NInput'/'NOutput' nodes, and return the fully described 'ElabEntity'.
elaborate :: forall i o. (PortRef i, PortRef o) => Entity i o -> ElabEntity
elaborate Entity{..} = ElabEntity entityName portDecls nodes
  where
    iSpecs    = zipWith setName (portNames (Proxy @i)) (portSpecs (Proxy @i))
    oSpecs    = zipWith setName (portNames (Proxy @o)) (portSpecs (Proxy @o))
    portDecls = map (toDecl In) iSpecs ++ map (toDecl Out) oSpecs
    nodes     = execNetM $ do
        inWires  <- mapM allocInput iSpecs
        outputs  <- entityBody (fromWireIds inWires)
        outWires <- toWireIds outputs
        mapM_ (uncurry emitOutput) (zip oSpecs outWires)

    setName n ps      = ps { portName = n }
    allocInput ps     = do
        wid <- freshWire
        emit $ NInput wid (portName ps) (portWidth ps) (portDom ps)
        return wid
    emitOutput ps wid = emit $ NOutput wid (portName ps) (portWidth ps) (portDom ps)
    toDecl dir ps     = PortDecl (portName ps) dir (portWidth ps) (portDom ps)

-- | Like 'elaborate', but also returns the full 'Design' — including any
-- sub-entities instantiated via 'instEntity' inside the body.
-- Use this for hierarchical designs.
elaborateDesign :: forall i o. (PortRef i, PortRef o) => Entity i o -> (ElabEntity, Design)
elaborateDesign Entity{..} = (ElabEntity entityName portDecls topNodes, fullDesign)
  where
    iSpecs    = zipWith setName (portNames (Proxy @i)) (portSpecs (Proxy @i))
    oSpecs    = zipWith setName (portNames (Proxy @o)) (portSpecs (Proxy @o))
    portDecls = map (toDecl In) iSpecs ++ map (toDecl Out) oSpecs
    (_, topNodes, subDesign) = runNetM $ do
        inWires  <- mapM allocInput iSpecs
        outputs  <- entityBody (fromWireIds inWires)
        outWires <- toWireIds outputs
        mapM_ (uncurry emitOutput) (zip oSpecs outWires)
    fullDesign = Map.insert entityName topNodes subDesign


    setName n ps      = ps { portName = n }
    allocInput ps     = do
        wid <- freshWire
        emit $ NInput wid (portName ps) (portWidth ps) (portDom ps)
        return wid
    emitOutput ps wid = emit $ NOutput wid (portName ps) (portWidth ps) (portDom ps)
    toDecl dir ps     = PortDecl (portName ps) dir (portWidth ps) (portDom ps)
