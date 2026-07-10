-- | Ways to build a 'RomImage' — the typed initial contents of a 'createRom'.
--
-- A ROM image is @[Integer]@ words tagged with the word type; 'createRom' pads
-- it to the ROM's declared @size@.  Sources:
--
--   * inline literal — @'RomImage' [0x46, 0xC9]@;
--   * a list you built — @'romFromList' xs@;
--   * bytes from a runtime 'Isacle.System.SystemDSL.loadFileBytes' —
--     @'romFromBytes' bytes@;
--   * a file read at __compile time__ — @$('romBin8' \"prog.bin\")@ (and the
--     little-endian 16-/32-bit variants).
--
-- Unlike the padding loaders in "Isacle.TH", these do __not__ pad to a power of
-- two: the only padding is 'createRom's @size@, so the two rules never disagree.
{-# LANGUAGE TemplateHaskell #-}
module Isacle.System.Rom
    ( -- * Pure constructors
      romFromList
    , romFromBytes
      -- * Compile-time file loaders (Template Haskell)
    , romBin8
    , romBin16LE
    , romBin32LE
    , romBinWith
    ) where

import Prelude
import Data.Word (Word8)
import Language.Haskell.TH (Q, Exp(..), Lit(..))
import Language.Haskell.TH.Syntax (addDependentFile, runIO)
import qualified Data.ByteString as BS

import Isacle.System.SystemDSL (RomImage(..))
import Isacle.TH (parseWords16LE, parseWords32LE)

-- | Wrap a word list as a ROM image (word width comes from the use site).
romFromList :: [Integer] -> RomImage dat
romFromList = RomImage

-- | Build a byte-wide ROM image from raw bytes — e.g. the result of
-- 'Isacle.System.SystemDSL.loadFileBytes'.
romFromBytes :: [Word8] -> RomImage dat
romFromBytes = RomImage . map fromIntegral

-- | Read a flat binary at compile time and splice it as a 'RomImage', using the
-- supplied byte→word parser.  Registers the file as a build dependency, so
-- editing it retriggers compilation.  No power-of-two padding.
romBinWith :: ([Word8] -> [Integer]) -> FilePath -> Q Exp
romBinWith parser path = do
    addDependentFile path
    content <- runIO (BS.readFile path)
    let ws = parser (BS.unpack content)
    pure (ConE 'RomImage `AppE` ListE (map (LitE . IntegerL) ws))

-- | Byte-addressed image: one word per byte.
romBin8 :: FilePath -> Q Exp
romBin8 = romBinWith (map fromIntegral)

-- | Little-endian 16-bit image: one word per byte pair.
romBin16LE :: FilePath -> Q Exp
romBin16LE = romBinWith parseWords16LE

-- | Little-endian 32-bit image: one word per four bytes.
romBin32LE :: FilePath -> Q Exp
romBin32LE = romBinWith parseWords32LE
