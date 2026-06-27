{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

-- | Bus capability hierarchy and crossing adapters (BU6 / BU7).
--
-- A master may only drive a child it is /at least as capable as/. The one rule
-- that matters here is stall capability: a stalling master can drive anything
-- (it simply never observes a stall from a combinational child), but a
-- non-stalling master driving a stalling child would silently drop the child's
-- stall — broken by construction. Expressed as a typeclass 'Subsumes' whose
-- forbidden combination has /no instance/, so the bad connection is a type
-- error rather than a comment (it previously lived only as a note in
-- 'Isacle.System.BusArch').
--
-- When the rule would forbid a connection you nonetheless need (a non-stalling
-- master reaching a stalling fabric, or two different widths), you cross the
-- seam with a 'BusAdapter' (BU7) — the one place protocol/width conversion is
-- expressed. An adapter is introspectable (its two faces are visible to a
-- runner) and reduces to Hdl like everything else.
module Isacle.System.BusCap
    ( -- * Capability lattice (BU6)
      Capability(..)
    , Subsumes
    , canDrive
      -- * Crossing adapters (BU7)
    , BusAdapter(..)
    , widthAdapter
    , stallAdapter
    ) where

import Prelude
import Data.Proxy (Proxy)

-- | A bus port's stall capability: whether it participates in a handshake.
data Capability = NonStalling | Stalling

-- | @Subsumes m c@ holds iff a master of capability @m@ may /directly/ drive a
-- child of capability @c@.
--
-- A stalling master subsumes both kinds of child; a non-stalling master may
-- only drive a non-stalling child. The forbidden case — a non-stalling master
-- driving a stalling child — has no instance, making such a connection fail to
-- compile. To cross it intentionally, insert a 'stallAdapter'.
class Subsumes (m :: Capability) (c :: Capability)

instance Subsumes 'Stalling    'Stalling
instance Subsumes 'Stalling    'NonStalling
instance Subsumes 'NonStalling 'NonStalling
-- (no instance Subsumes 'NonStalling 'Stalling — intentionally a type error)

-- | Witness that a master of capability @m@ may drive a child of capability @c@
-- (BU6). A pure type-level check: it type-checks exactly when the connection is
-- legal and is a compile error otherwise. Thread it through a nesting site to
-- make the rule enforced rather than merely documented.
canDrive :: Subsumes m c => Proxy m -> Proxy c -> ()
canDrive _ _ = ()

-- | A crossing between two bus faces (BU7). Records the master-side and
-- child-side capability and data width so a runner can introspect the
-- conversion (and so width/protocol mismatches are explicit, not implicit).
--
-- The phantom capabilities @mcap@/@ccap@ let an adapter present one capability
-- to the master and a different one to the child, which is precisely how it
-- legalises an otherwise-forbidden 'Subsumes'.
data BusAdapter (mcap :: Capability) (ccap :: Capability) = BusAdapter
    { adMasterWidth :: Int       -- ^ data width on the master face (bits)
    , adChildWidth  :: Int       -- ^ data width on the child face (bits)
    , adInsertsStall :: Bool     -- ^ does the adapter add a handshake?
    , adName        :: String    -- ^ for introspection / diagnostics
    } deriving (Eq, Show)

-- | A pure width converter at a fixed capability: same handshake behaviour on
-- both faces, only the data width changes (e.g. a 32→8 down-converter).
widthAdapter :: forall cap. Int -> Int -> BusAdapter cap cap
widthAdapter mw cw = BusAdapter mw cw False
    ("width " ++ show mw ++ "->" ++ show cw)

-- | A handshake-inserting bridge: presents a non-stalling face to the master
-- while driving a stalling child, by registering the request and holding until
-- the child's stall clears. This is the sanctioned way to let a non-stalling
-- master reach a stalling fabric — the adapter, not a dropped stall.
stallAdapter :: Int -> BusAdapter 'NonStalling 'Stalling
stallAdapter w = BusAdapter w w True "stall-bridge"
