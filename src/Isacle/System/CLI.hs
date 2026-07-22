-- | Command-line entry point for a 'SysNet' system.
--
-- The minimal-boilerplate path: a whole system file is one import and one call,
-- with the system constructed inline and handed straight to 'systemMain'.
--
-- @
-- import Isacle.System.CLI
--
-- main :: IO ()
-- main = systemMain "tiny_soc" $ do
--     gpio <- createGpio "gpio0" (0 :: Sig Sys (Unsigned 8))
--     ...
-- @
--
-- Run it with @cabal exec runghc MySystem.hs -- --out build/x --vhdl-only@.
--
-- 'Sys' is a ready-made 50 MHz clock domain so the common case needs no
-- @KnownDom@ boilerplate; declare your own domain if you need different clocking.
module Isacle.System.CLI
    ( -- * Entry point
      systemMain
      -- * Default clock domain
    , Sys
      -- * Re-exports (so a system file needs only this one import)
    , module Isacle.System.SystemDSL
    , module Isacle.System.Emit
    , module Isacle.System.Rom
    , KnownDom(..)
    , DomId(..), ClockEdge(..), ResetPolarity(..)
    , Sig(..)
    , Unsigned, Signed
      -- * Top-level ports (for the entity-style @i -> SysNet o@ system body)
    , Named
    , Port(..)
    ) where

import Prelude
import Control.Monad (when)
import Data.List (isPrefixOf)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import qualified Data.Map.Strict as Map

import Hdl.Net   (DomId(..), ClockEdge(..), ResetPolarity(..))
import Hdl.Sig (KnownDom(..), Sig(..))
import Hdl.Types (Named, Port(..))
import Hdl.Prim  (Unsigned)
import Hdl.Bits  (Signed)
import Isacle.System.SystemDSL
import Isacle.System.Emit
import Isacle.System.Rom
import Isacle.System.MemBanks (memFileName, renderMem, renderManifest)

-- ---------------------------------------------------------------------------
-- Default clock domain
-- ---------------------------------------------------------------------------

-- | A ready-made 50 MHz, rising-edge, active-high-reset clock domain.  Annotate
-- signals with @Sig Sys a@ to use it without declaring a @KnownDom@ instance.
data Sys
instance KnownDom Sys where
    domId _ = DomId "sys" 50000000 Rising ActiveHigh "rst"

-- ---------------------------------------------------------------------------
-- Argument parsing
-- ---------------------------------------------------------------------------

-- | Accumulated CLI configuration.
data CliConf = CliConf
    { ccName  :: String        -- ^ top-entity / basename
    , ccOut   :: Maybe FilePath -- ^ output dir (default @build/<name>@)
    , ccOpts  :: GenOptions
    , ccPrint :: Bool          -- ^ render to stdout instead of writing files
    }

-- | Fold argv into a 'CliConf'.  Returns @Left err@ on an unknown/ill-formed flag.
parseArgs :: String -> [String] -> Either String CliConf
parseArgs name = go CliConf { ccName = name, ccOut = Nothing
                            , ccOpts = defaultGenOptions, ccPrint = False }
  where
    go conf [] = Right conf
    go conf (a:rest) = case a of
        "--out"      -> withArg rest $ \v r -> go conf { ccOut  = Just v } r
        "--name"     -> withArg rest $ \v r -> go conf { ccName = v }      r
        "--guard"    -> withArg rest $ \v r -> go conf { ccOpts = (ccOpts conf) { goGuard = v } } r
        "--vhdl-only"-> go (setOpts conf (\o -> o { goVhdl = True, goMemMap = False
                                                  , goHeader = False, goLinker = False
                                                  , goMemFiles = False })) rest
        "--mem-only" -> go (setOpts conf (\o -> o { goVhdl = False, goMemMap = False
                                                  , goHeader = False, goLinker = False
                                                  , goMemFiles = True })) rest
        "--no-vhdl"  -> go (setOpts conf (\o -> o { goVhdl     = False })) rest
        "--no-memmap"-> go (setOpts conf (\o -> o { goMemMap   = False })) rest
        "--no-header"-> go (setOpts conf (\o -> o { goHeader   = False })) rest
        "--no-linker"-> go (setOpts conf (\o -> o { goLinker   = False })) rest
        "--no-mem"   -> go (setOpts conf (\o -> o { goMemFiles = False })) rest
        "--print"    -> go conf { ccPrint = True } rest
        _ | a `elem` ["-h", "--help"] -> Left ""   -- "" ⇒ print usage, exit 0
          | "--" `isPrefixOf` a       -> Left ("unknown flag: " ++ a)
          | otherwise                 -> Left ("unexpected argument: " ++ a)

    withArg (v:r) k = k v r
    withArg []    _ = Left "flag expects a value"
    setOpts conf f = conf { ccOpts = f (ccOpts conf) }

usage :: String -> String
usage prog = unlines
    [ "usage: " ++ prog ++ " [flags]"
    , ""
    , "  --out DIR      output directory (default build/<name>)"
    , "  --name NAME    top-entity / file basename"
    , "  --guard NAME   C-header include guard (default: name)"
    , "  --vhdl-only    emit only VHDL"
    , "  --mem-only     emit only the reloadable memory files (.mem/.coe + manifest)"
    , "  --no-vhdl      skip VHDL"
    , "  --no-memmap    skip the memory map"
    , "  --no-header    skip the C header"
    , "  --no-linker    skip the linker script"
    , "  --no-mem       skip the .mem/.coe/manifest bank files"
    , "  --print        render to stdout instead of writing files"
    , "  -h, --help     this message"
    ]

-- ---------------------------------------------------------------------------
-- Entry point
-- ---------------------------------------------------------------------------

-- | Parse argv and generate the requested artifacts for @sys@.  @name@ is the
-- default top-entity name (overridable with @--name@); the default output
-- directory is @build/\<name\>@.
systemMain :: (Named i, Named o) => String -> (i -> SysNet o) -> IO ()
systemMain name sys = do
    args <- getArgs
    case parseArgs name args of
        Left "" -> getProgName >>= putStr . usage >> exitSuccess
        Left err -> do
            prog <- getProgName
            putStrLn ("error: " ++ err) >> putStr (usage prog) >> exitFailure
        Right conf
            | ccPrint conf -> do
                env <- resolveFiles (ccName conf) sys
                printArtifacts (renderSystemWith env (ccOpts conf) (ccName conf) sys)
            | otherwise    ->
                let dir = maybe ("build/" ++ ccName conf) id (ccOut conf)
                in writeSystemWith (ccOpts conf) dir (ccName conf) sys

-- | Dump every rendered artifact to stdout with section banners.
printArtifacts :: SystemArtifacts -> IO ()
printArtifacts a = do
    mapM_ (\(ent, vhdl) -> section (ent ++ ".vhd") vhdl) (Map.toList (saVhdl a))
    maybe (pure ()) (section "memory map")   (saMemMap a)
    maybe (pure ()) (section "C header")     (saHeader a)
    maybe (pure ()) (section "linker script") (saLinker a)
    mapM_ (\b -> section (memFileName b) (renderMem b)) (saBanks a)
    when (not (null (saBanks a))) $ section "membanks.json" (renderManifest (saBanks a))
  where
    section title body = do
        putStrLn ("===== " ++ title ++ " =====")
        putStr body
        putStrLn ""
