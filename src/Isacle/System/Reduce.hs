{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

-- | Explicit system reductions (SY6 / SY7 / BU5).
--
-- A system /is not/ a netlist, a memory map, or a C header — it /reduces to/
-- each of them. That distinction is the whole point of the system layer: one
-- description, many interpreters. This module names those reductions instead of
-- leaving them implicit in @runSystemDSL@'s tuple, so the "reduces to" relation
-- is first-class and the multi-render targets are discoverable.
--
--   * 'reduceToHdl' — the Hdl netlist (SY7: a system reduces to Hdl I/O).
--   * 'reduceToDesign' — the full 'Design' (top entity + sub-entities) for VHDL.
--   * 'reduceToDoc' — the introspectable topology (BU5: bus/peripheral map).
--   * 'reduceToMemoryMap' / 'reduceToCHeader' / 'reduceToLinkerScript' — the
--     per-target software-facing renders (SY6).
--
-- 'reduceSystem' runs the description once and bundles every reduction, so they
-- are guaranteed to be reductions of the /same/ elaboration.
module Isacle.System.Reduce
    ( SysReductions(..)
    , reduceSystem
    , reduceToHdl
    , reduceToDesign
    , reduceToDoc
    , reduceToMemoryMap
    , reduceToCHeader
    , reduceToLinkerScript
    ) where

import Prelude

import Hdl.Net (NetNode, Design)
import Isacle.System.SystemDSL (SysDSL, SysDoc, runSystemDSL, execSystemDSL)
import Isacle.System.Generate
    (sysExtractMemoryMap, sysGenCHeader, sysGenLinkerScript)

-- | Every target one system description reduces to, computed from a single run.
-- A system is none of these on its own; each field is a distinct reduction.
data SysReductions a = SysReductions
    { srResult       :: a          -- ^ the system's exposed I/O (the user result)
    , srHdlNodes     :: [NetNode]  -- ^ the flat Hdl netlist (SY7)
    , srDoc          :: SysDoc     -- ^ the introspectable topology (BU5)
    , srMemoryMap    :: String     -- ^ memory-map text (SY6)
    , srCHeader      :: String     -- ^ C header, guarded by the system name (SY6)
    , srLinkerScript :: String     -- ^ GNU LD linker script (SY6)
    }

-- | Reduce a system to all of its targets at once. @name@ guards the C header.
reduceSystem :: forall dom dat a. String -> SysDSL dom dat a -> SysReductions a
reduceSystem name dsl =
    let (a, nodes, doc) = runSystemDSL dsl
    in SysReductions
        { srResult       = a
        , srHdlNodes     = nodes
        , srDoc          = doc
        , srMemoryMap    = sysExtractMemoryMap doc
        , srCHeader      = sysGenCHeader name doc
        , srLinkerScript = sysGenLinkerScript doc
        }

-- | A system reduces to Hdl I/O: its exposed result plus the flat netlist (SY7).
-- The result and the nodes are /not/ the system — they are what it reduces to.
reduceToHdl :: SysDSL dom dat a -> (a, [NetNode])
reduceToHdl dsl = let (a, nodes, _) = runSystemDSL dsl in (a, nodes)

-- | Reduce to the full 'Design' (top entity + sub-entities) for VHDL emission.
reduceToDesign :: forall dom dat a. String -> SysDSL dom dat a -> Design
reduceToDesign = execSystemDSL

-- | Reduce to the introspectable topology document (BU5).
reduceToDoc :: SysDSL dom dat a -> SysDoc
reduceToDoc dsl = let (_, _, doc) = runSystemDSL dsl in doc

-- | Reduce to memory-map text (SY6).
reduceToMemoryMap :: SysDSL dom dat a -> String
reduceToMemoryMap = sysExtractMemoryMap . reduceToDoc

-- | Reduce to a C header guarded by the given name (SY6).
reduceToCHeader :: String -> SysDSL dom dat a -> String
reduceToCHeader name = sysGenCHeader name . reduceToDoc

-- | Reduce to a GNU LD linker script (SY6).
reduceToLinkerScript :: SysDSL dom dat a -> String
reduceToLinkerScript = sysGenLinkerScript . reduceToDoc
