{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Isacle.Hdl.Net
    ( -- * Wire identifiers
      WireId
      -- * Clock domain metadata
    , DomId(..)
    , ClockEdge(..)
    , ResetPolarity(..)
      -- * Netlist IR
    , NetNode(..)
    , PrimOp(..)
    , SomeBits(..)
      -- * Wire name hints
    , hintWire
      -- * Multi-entity design
    , Design
      -- * Builder monad
    , NetM
    , NetSt(..)
    , runNetM
    , execNetM
    , runDesign
    , execDesign
    , freshWire
    , emit
    , defer
    , inBlock
    ) where

import Prelude
import Control.Monad (forM)
import Control.Monad.Fix (MonadFix)
import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map
import Numeric.Natural (Natural)

-- ---------------------------------------------------------------------------
-- Wire identifiers
-- ---------------------------------------------------------------------------

type WireId = Int

-- ---------------------------------------------------------------------------
-- Clock domain metadata
-- ---------------------------------------------------------------------------

data ClockEdge = Rising | Falling
    deriving (Show, Eq)

data ResetPolarity = ActiveHigh | ActiveLow
    deriving (Show, Eq)

data DomId = DomId
    { domName    :: String
    , domFreqHz  :: Natural
    , domEdge    :: ClockEdge
    , domReset   :: ResetPolarity
    } deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Primitive operations
-- ---------------------------------------------------------------------------

data PrimOp
    = PAdd | PSub | PMul
    | PAnd | POr  | PXor | PNot
    | PMux
    | PEq  | PLt
    | PSlice Int Int
    | PConcat
    | PResize Int
    | PLit Integer Int
    | PShiftL   -- ^ logical shift left  (a, amount)
    | PShiftR   -- ^ logical shift right (a, amount)
    deriving (Show, Eq, Ord)

-- ---------------------------------------------------------------------------
-- Bitvector literal carrier
-- ---------------------------------------------------------------------------

data SomeBits = SomeBits
    { sbValue :: Integer
    , sbWidth :: Int
    } deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Netlist IR nodes
-- ---------------------------------------------------------------------------

data NetNode
    = NReg
        { nOut  :: WireId
        , nIn   :: WireId
        , nEn   :: Maybe WireId
        , nInit :: SomeBits
        , nDom  :: DomId
        }
    | NComb
        { nOut :: WireId
        , nOp  :: PrimOp
        , nIns :: [WireId]
        }
    | NInput
        { nOut      :: WireId
        , nPortName :: String
        , nWidth    :: Int
        , nDom      :: DomId
        }
    | NOutput
        { nIn       :: WireId
        , nPortName :: String
        , nWidth    :: Int
        , nDom      :: DomId
        }
    | NSubInst
        { nInstName   :: String
        , nEntityName :: String
        -- Ports from the parent's perspective.
        -- nInPorts:  parent wire drives sub-entity input.
        -- nOutPorts: sub-entity output drives a fresh parent wire (width recorded).
        , nInPorts    :: [(String, WireId)]
        , nOutPorts   :: [(String, WireId, Int)]
        }
    -- | Synchronous block RAM.
    -- Write is registered (rising clock edge, guarded by nMemWrEn).
    -- Read is asynchronous (combinational) so single-cycle bus reads work.
    -- Synthesis tools infer BRAM or LUTRAM depending on size and target.
    | NMem
        { nOut      :: WireId     -- ^ read-data output wire
        , nMemRdA   :: WireId     -- ^ read address
        , nMemWrA   :: WireId     -- ^ write address
        , nMemWrD   :: WireId     -- ^ write data
        , nMemWrEn  :: WireId     -- ^ write enable (1-bit)
        , nMemSize  :: Int        -- ^ number of addressable entries
        , nMemDatW  :: Int        -- ^ data width in bits
        , nMemInit  :: [Integer]  -- ^ initial contents (padded to nMemSize with 0)
        , nDom      :: DomId
        }
    -- | Read-only ROM — purely combinational lookup.
    | NRom
        { nOut      :: WireId
        , nRomRdA   :: WireId
        , nRomSize  :: Int
        , nRomDatW  :: Int
        , nRomInit  :: [Integer]
        }
    | NHint
        { nHintWire :: WireId
        , nHintName :: String
        }
    deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Multi-entity design
-- ---------------------------------------------------------------------------

-- | A complete design: maps each entity name to its flat node list.
-- The top-level entity is included under its own name by 'runDesign'.
type Design = Map.Map String [NetNode]

-- ---------------------------------------------------------------------------
-- Builder monad
-- ---------------------------------------------------------------------------

data NetSt = NetSt
    { netNodes     :: [NetNode]
    , netWireCount :: WireId
    , netHier      :: [String]
    , netDeferred  :: [NetM ()]
    , netDesign    :: Design      -- sub-entity definitions accumulated here
    }

instance Show NetSt where
    show s = "NetSt { netNodes = " ++ show (netNodes s)
          ++ ", netWireCount = " ++ show (netWireCount s)
          ++ ", netHier = " ++ show (netHier s)
          ++ ", netDeferred = <" ++ show (length (netDeferred s)) ++ " actions>"
          ++ ", netDesign = <" ++ show (Map.size (netDesign s)) ++ " entities> }"

newtype NetM a = NetM { _unNetM :: State NetSt a }
    deriving newtype (Functor, Applicative, Monad, MonadFix)

initSt :: NetSt
initSt = NetSt
    { netNodes     = []
    , netWireCount = 0
    , netHier      = []
    , netDeferred  = []
    , netDesign    = Map.empty
    }

-- | Run a circuit, returning the top-level node list and accumulated sub-entity design.
runNetM :: NetM a -> (a, [NetNode], Design)
runNetM (NetM m) = (a, reverse (netNodes finalSt), netDesign finalSt)
  where
    (a, st0)    = runState m initSt
    deferred    = reverse (netDeferred st0)
    (_, finalSt) = runState (mapM_ _unNetM deferred) st0 { netDeferred = [] }

-- | Flat single-entity extraction (backward-compatible).
execNetM :: NetM a -> [NetNode]
execNetM m = nodes
  where (_, nodes, _) = runNetM m

-- | Run a circuit, placing all entities (including the top) into a 'Design'.
runDesign :: String -> NetM a -> (a, Design)
runDesign topName m = (a, Map.insert topName topNodes design)
  where (a, topNodes, design) = runNetM m

execDesign :: String -> NetM a -> Design
execDesign name = snd . runDesign name

-- ---------------------------------------------------------------------------
-- Primitives
-- ---------------------------------------------------------------------------

-- | Attach a name hint to a wire; used by 'named' in 'Isacle.Hdl.Class'.
-- The emitter uses hints to prefer human-readable signal names over wN.
hintWire :: WireId -> String -> NetM ()
hintWire wid n = emit (NHint wid n)

freshWire :: NetM WireId
freshWire = NetM $ do
    n <- gets netWireCount
    modify $ \s -> s { netWireCount = n + 1 }
    pure n

emit :: NetNode -> NetM ()
emit node = NetM $ modify $ \s -> s { netNodes = node : netNodes s }

-- | Schedule an action for after the main body (used by registers for
-- feedback-safe deferred NReg emission).
defer :: NetM () -> NetM ()
defer action = NetM $ modify $ \s -> s { netDeferred = action : netDeferred s }

-- ---------------------------------------------------------------------------
-- Hierarchical instantiation
-- ---------------------------------------------------------------------------

-- | Run a component body as a named sub-entity.
-- The body must declare its own ports via 'inputS'/'outputS'.
-- Input wires are matched positionally to NInput nodes in body-emission order.
-- Returns the output port connections: (portName, freshParentWire, width).
inBlock :: String      -- ^ instance label
        -> String      -- ^ entity name (used as the design key)
        -> [WireId]    -- ^ parent wires for input ports, positional
        -> NetM ()     -- ^ component body
        -> NetM [(String, WireId, Int)]
inBlock instName entityName inWireIds body = NetM $ do
    st0 <- get
    -- Run the body in a fresh sub-entity state (local wire counter from 0).
    let subSt0 = initSt { netDesign = netDesign st0 }
    let (_, subSt') = runState (_unNetM body) subSt0
    -- Drain deferred actions within the sub-entity.
    let deferred = reverse (netDeferred subSt')
    let (_, subSt) = runState (mapM_ _unNetM deferred) subSt' { netDeferred = [] }
    let subNodes = reverse (netNodes subSt)
    -- Build input port map by zipping discovered NInput names with caller's wires.
    let inputNodes = [ n | n@NInput{} <- subNodes ]
        inPorts    = zip (map nPortName inputNodes) inWireIds
    -- Collect output port info.
    let outputNodes = [ n | n@NOutput{} <- subNodes ]
    -- Allocate a fresh parent wire for each output port.
    let parentSt = st0 { netDesign = netDesign subSt }  -- propagate nested design
        (outPorts, parentSt') = runState
            (forM outputNodes $ \n -> do
                w <- _unNetM freshWire
                return (nPortName n, w, nWidth n))
            parentSt
    -- Record sub-entity in the design (merging any nested entities it defined).
    let design' = Map.insert entityName subNodes (netDesign parentSt')
    -- Emit the instance marker into the parent.
    let instNode = NSubInst instName entityName inPorts outPorts
    put parentSt'
        { netNodes  = instNode : netNodes parentSt'
        , netDesign = design'
        }
    return outPorts
