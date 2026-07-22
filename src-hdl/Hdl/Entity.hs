{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE UndecidableInstances #-}
module Hdl.Entity
    ( -- * Port mapping between bundles
      PortMap
      -- * Behavioral description
    , hdl
      -- * EntityDef construction
    , EntityDef
    , entityDef
    , entityName
    , entityBody
      -- * Synthesis override
    , SynthTarget(..)
    , withSynth
      -- * Elaborated port declaration
    , Dir(..)
    , PortDecl(..)
      -- * Elaborated entityDef
    , ElabEntity(..)
      -- * Elaboration
    , elaborate
    , elaborateDesign
    , elaborateTop
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
-- PortMap
-- ---------------------------------------------------------------------------

-- | Structural compatibility between two port bundles for synthesis
-- substitution.  @PortMap i vi@ declares that our ports @i@ map onto
-- vendor ports @vi@.  (Port names come from 'Named'.)
class (Named i, Named vi) => PortMap i vi

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

-- | An elaborated entityDef: port declarations and the flat node list.
-- This is the form consumed by the VHDL emitter.
data ElabEntity = ElabEntity
    { elabName  :: String
    , elabPorts :: [PortDecl]
    , elabNodes :: [NetNode]
    } deriving (Show)

-- ---------------------------------------------------------------------------
-- EntityDef
-- ---------------------------------------------------------------------------

-- | A first-class HDL entityDef.
--
-- 'entityBody' is the behavioral simulation model.
-- 'entitySynth', when present, substitutes a vendor primitive at synthesis;
-- the port mapping is expressed as type-safe functions between bundles.
data EntityDef i o = EntityDef
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
    ) => SynthTarget (EntityDef vi vo)

-- ---------------------------------------------------------------------------
-- Smart constructors
-- ---------------------------------------------------------------------------

-- | Identity for now (the body already is a @NetM@ computation); kept so call
-- sites read @bind "x" (hdl go)@.  Retires once bodies go polymorphic.
hdl :: (i -> NetM o) -> (i -> NetM o)
hdl = id

-- | Build an entityDef from a name and its behavioral description.
entityDef :: String -> (i -> NetM o) -> EntityDef i o
entityDef name body = EntityDef name body Nothing

-- | Attach a vendor synthesis target to an entityDef.
withSynth :: EntityDef i o -> SynthTarget i o -> EntityDef i o
withSynth e s = e { entitySynth = Just s }

-- ---------------------------------------------------------------------------
-- Elaboration
-- ---------------------------------------------------------------------------

-- | Elaborate an entityDef: allocate input wires, run the behavioral body,
-- emit 'NInput'/'NOutput' nodes, and return the fully described 'ElabEntity'.
elaborate :: forall i o. (Named i, Named o) => EntityDef i o -> ElabEntity
elaborate EntityDef{..} = ElabEntity entityName portDecls nodes
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

-- | Elaborate a __top-level__ design whose body runs in a monad layered over
-- 'NetM' (e.g. the system builder's @StateT SysBuild NetM@).  Top-level ports are
-- bound from the @Named@ input/output bundle __types__ using the __same__
-- port-from-types resolution 'elaborate'/'instEntity' use for every sub-entity —
-- an entity is an entity, so the top level needs no special-cased primitives.
--
-- @runBody@ receives the input bundle (built from freshly-allocated 'NInput'
-- wires) and must return the output bundle (from which 'NOutput' nodes are
-- emitted) paired with any extra result @r@ to thread out (e.g. accumulated build
-- state).  Returns the output bundle, that extra result, and the full 'Design'
-- (top entity under @name@ plus every sub-entity instantiated inside the body).
elaborateTop
    :: forall i o r. (Named i, Named o)
    => String
    -> (i -> NetM (o, r))
    -> (o, r, Design)
elaborateTop name runBody = (o, r, Map.insert name topNodes subDesign)
  where
    iSpecs = zipWith setName (portNames (Proxy @i)) (portSpecs (Proxy @i))
    oSpecs = zipWith setName (portNames (Proxy @o)) (portSpecs (Proxy @o))
    ((o, r), topNodes, subDesign) = runNetM $ do
        inWires  <- mapM allocInput iSpecs
        (o', r') <- runBody (fromWireIds inWires)
        outWires <- toWireIds o'
        mapM_ (uncurry emitOutput) (zip oSpecs outWires)
        pure (o', r')

    setName n ps      = ps { portName = n }
    allocInput ps     = do
        wid <- freshWire
        emit $ NInput wid (portName ps) (portWidth ps) (portDom ps)
        return wid
    emitOutput ps wid = emit $ NOutput wid (portName ps) (portWidth ps) (portDom ps)

-- | Like 'elaborate', but also returns the full 'Design' — including any
-- sub-entities instantiated via 'instEntity' inside the body.
-- Use this for hierarchical designs.
elaborateDesign :: forall i o. (Named i, Named o) => EntityDef i o -> (ElabEntity, Design)
elaborateDesign EntityDef{..} = (ElabEntity entityName portDecls topNodes, fullDesign)
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
