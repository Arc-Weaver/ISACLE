-- | Template Haskell helpers for loading binary images at compile time.
--
-- All loaders return @[Integer]@ list splices, matching the @[Integer]@
-- initial-contents parameter of 'Isacle.System.Periph.blockRomDef'.
module Isacle.TH
    ( loadBinWith
    , loadBin8
    , loadBin16LE
    , loadBin32LE
    , padToPow2
    , nextPow2
    ) where

import Prelude
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addDependentFile)
import qualified Data.ByteString as BS
import Data.Bits  (shiftL, (.|.), countLeadingZeros, finiteBitSize, bit)
import Data.Word  (Word8)

-- | Read a flat binary at compile time and splice it as @[Integer]@.
--   The list is zero-padded to the next power of two.
--   The file path is relative to the project root.
loadBinWith :: ([Word8] -> [Integer]) -> FilePath -> Q Exp
loadBinWith parser path = do
    addDependentFile path
    content <- runIO (BS.readFile path)
    let ws = padToPow2 (parser (BS.unpack content))
    return $ ListE (map (LitE . IntegerL) ws)

-- | Load a byte-addressed binary: each byte becomes one list element.
loadBin8 :: FilePath -> Q Exp
loadBin8 = loadBinWith (map fromIntegral)

-- | Load a little-endian 16-bit binary: each pair of bytes becomes one element.
loadBin16LE :: FilePath -> Q Exp
loadBin16LE = loadBinWith parseWords16LE

-- | Load a little-endian 32-bit binary: each group of four bytes becomes one element.
loadBin32LE :: FilePath -> Q Exp
loadBin32LE = loadBinWith parseWords32LE

parseWords16LE :: [Word8] -> [Integer]
parseWords16LE []           = []
parseWords16LE [_]          = []
parseWords16LE (lo:hi:rest) =
    ((fromIntegral hi `shiftL` 8) .|. fromIntegral lo) : parseWords16LE rest

parseWords32LE :: [Word8] -> [Integer]
parseWords32LE (b0:b1:b2:b3:rest) =
    ( fromIntegral b0
      .|. (fromIntegral b1 `shiftL`  8)
      .|. (fromIntegral b2 `shiftL` 16)
      .|. (fromIntegral b3 `shiftL` 24)
    ) : parseWords32LE rest
parseWords32LE _ = []

-- | Pad a list to the next power of two with zeros (minimum length 1).
padToPow2 :: [Integer] -> [Integer]
padToPow2 [] = [0]
padToPow2 xs =
    let n  = length xs
        n' = nextPow2 n
    in xs ++ replicate (n' - n) 0

-- | Smallest power of two >= n.
nextPow2 :: Int -> Int
nextPow2 n
    | n <= 1    = 1
    | otherwise = bit k
  where
    k = finiteBitSize (0 :: Int) - countLeadingZeros (n - 1)
