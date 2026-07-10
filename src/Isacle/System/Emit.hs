-- | One-call artifact generation for a 'SysDSL' system.
--
-- A system is a single @'SysDSL' a@ value from which every output derives.  This
-- module runs that description __once__ (via 'runSystemDesign') and renders any
-- subset of:
--
--   * the VHDL 'Design' (top entity + sub-entities),
--   * a human-readable memory map,
--   * a C header (register/base macros), and
--   * a GNU LD linker script.
--
-- 'writeSystem' collapses the whole \"build a dir, emit VHDL, emit the software
-- artifacts\" dance into one call; 'renderSystem' is its pure core for the REPL.
{-# LANGUAGE NamedFieldPuns #-}
module Isacle.System.Emit
    ( -- * Options
      GenOptions(..)
    , defaultGenOptions
      -- * Rendered artifacts (pure)
    , SystemArtifacts(..)
    , renderSystem
    , renderSystemWith
      -- * Deferred file loading
    , resolveFiles
      -- * Write to disk
    , writeSystem
    , writeSystemWith
    ) where

import Prelude
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS
import Control.Monad (when, forM)
import Data.List (nub)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import Hdl.Net (Design)
import Hdl.Emit.Vhdl (emitVhdlDesign)
import Isacle.System.SystemDSL
    (SysDSL, SysDoc, FileEnv, runSystemDesignWith)
import Isacle.System.Generate
    (sysExtractMemoryMap, sysGenCHeader, sysGenLinkerScript)
import Isacle.System.MemBanks
    ( MemBank, extractMemBanks, memFileName, coeFileName
    , renderMem, renderCoe, renderManifest )

-- ---------------------------------------------------------------------------
-- Options
-- ---------------------------------------------------------------------------

-- | Which artifacts to emit.  Flag-driven: flip individual fields to trim the
-- output.  'defaultGenOptions' emits everything.
data GenOptions = GenOptions
    { goVhdl     :: Bool   -- ^ emit @\<entity\>.vhd@ files
    , goMemMap   :: Bool   -- ^ emit @\<name\>.memmap@
    , goHeader   :: Bool   -- ^ emit @\<name\>.h@
    , goLinker   :: Bool   -- ^ emit @\<name\>.ld@
    , goMemFiles :: Bool   -- ^ emit per-bank @.mem@/@.coe@ + @\<name\>.membanks.json@
    , goGuard    :: String -- ^ C-header include-guard; @""@ ⇒ derive from name
    } deriving (Show)

-- | Emit every artifact; guard derived from the system name.
defaultGenOptions :: GenOptions
defaultGenOptions = GenOptions
    { goVhdl = True, goMemMap = True, goHeader = True, goLinker = True
    , goMemFiles = True, goGuard = "" }

-- ---------------------------------------------------------------------------
-- Rendered artifacts
-- ---------------------------------------------------------------------------

-- | Everything 'renderSystem' produced.  A field is 'Nothing' when its flag was
-- off; the VHDL map is empty when 'goVhdl' is off.  'saDesign' / 'saDoc' are the
-- raw intermediates, handy for further inspection in the REPL.
data SystemArtifacts = SystemArtifacts
    { saDesign :: Design                 -- ^ elaborated design (all entities)
    , saDoc    :: SysDoc                 -- ^ system documentation
    , saVhdl   :: Map.Map String String  -- ^ entity name → VHDL source
    , saMemMap :: Maybe String
    , saHeader :: Maybe String
    , saLinker :: Maybe String
    , saBanks  :: [MemBank]              -- ^ ROM/RAM banks (empty if 'goMemFiles' off)
    }

-- | Run a system once and render the artifacts its 'GenOptions' request.  Pure —
-- no disk access, so any 'Isacle.System.SystemDSL.loadFile' yields @""@.  Use
-- 'writeSystem' (or 'resolveFiles' + 'renderSystemWith') to resolve file loads.
renderSystem :: GenOptions -> String -> SysDSL a -> SystemArtifacts
renderSystem = renderSystemWith Map.empty

-- | 'renderSystem' with a resolved file environment (see 'resolveFiles'), so
-- deferred 'Isacle.System.SystemDSL.loadFile' / 'loadFileBytes' calls see real
-- contents.
renderSystemWith :: FileEnv -> GenOptions -> String -> SysDSL a -> SystemArtifacts
renderSystemWith env opts name sys = SystemArtifacts
    { saDesign = design
    , saDoc    = doc
    , saVhdl   = if goVhdl opts then emitVhdlDesign design else Map.empty
    , saMemMap = whenOpt (goMemMap opts) (sysExtractMemoryMap doc)
    , saHeader = whenOpt (goHeader opts) (sysGenCHeader guard doc)
    , saLinker = whenOpt (goLinker opts) (sysGenLinkerScript doc)
    , saBanks  = if goMemFiles opts then extractMemBanks doc design else []
    }
  where
    (_, design, doc, _reqs) = runSystemDesignWith env name sys
    guard = case goGuard opts of { "" -> name; g -> g }
    whenOpt b x = if b then Just x else Nothing

-- | Resolve a system's deferred file loads: a harvest pass collects every path
-- requested via 'Isacle.System.SystemDSL.loadFile' / 'loadFileBytes' (with an
-- empty environment), those files are read, and the resulting 'FileEnv' is
-- returned for a real render pass.  (The set of requested paths must not depend
-- on file contents — see 'Isacle.System.SystemDSL.loadFile'.)
resolveFiles :: String -> SysDSL a -> IO FileEnv
resolveFiles name sys = do
    let (_, _, _, reqs) = runSystemDesignWith Map.empty name sys
    entries <- forM (nub reqs) $ \p -> do
        bytes <- BS.readFile p
        pure (p, bytes)
    pure (Map.fromList entries)

-- ---------------------------------------------------------------------------
-- Write to disk
-- ---------------------------------------------------------------------------

-- | Render a system and write the requested artifacts under @dir@, printing a
-- one-line-per-file summary.  @name@ is the top-entity / file basename.  Emits
-- every artifact ('defaultGenOptions'); use 'writeSystemWith' to select.
writeSystem :: FilePath -> String -> SysDSL a -> IO ()
writeSystem = writeSystemWith defaultGenOptions

-- | 'writeSystem' with explicit 'GenOptions'.  Resolves deferred file loads
-- ('resolveFiles') before rendering, so 'Isacle.System.SystemDSL.loadFile' sees
-- real contents.
writeSystemWith :: GenOptions -> FilePath -> String -> SysDSL a -> IO ()
writeSystemWith opts dir name sys = do
    createDirectoryIfMissing True dir
    env <- resolveFiles name sys
    let SystemArtifacts { saVhdl, saMemMap, saHeader, saLinker, saBanks } =
            renderSystemWith env opts name sys
    -- One .vhd per entity (top + sub-entities), same convention as
    -- 'emitVhdlDesignFiles'.
    mapM_ (\(ent, vhdl) -> writeOut (dir </> ent ++ ".vhd") vhdl)
          (Map.toList saVhdl)
    maybe (pure ()) (writeOut (dir </> name ++ ".memmap")) saMemMap
    maybe (pure ()) (writeOut (dir </> name ++ ".h"))      saHeader
    maybe (pure ()) (writeOut (dir </> name ++ ".ld"))     saLinker
    -- Per-bank reloadable memory contents + a manifest tying bank → address →
    -- data file (for a post-synthesis updatemem/Tcl reload).
    mapM_ (\b -> do writeOut (dir </> memFileName b) (renderMem b)
                    writeOut (dir </> coeFileName b) (renderCoe b))
          saBanks
    when (not (null saBanks)) $
        writeOut (dir </> name ++ ".membanks.json") (renderManifest saBanks)
    when (Map.null saVhdl && all null [saMemMap, saHeader, saLinker] && null saBanks) $
        putStrLn "(nothing to write — all artifacts disabled)"
  where
    writeOut path contents = do
        writeFile path contents
        putStrLn ("wrote " ++ path)
