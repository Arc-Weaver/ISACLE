{-# LANGUAGE TemplateHaskell #-}
-- | Template Haskell helpers for loading binary images at compile time
-- and pattern-matching on bit vectors.
--
-- 'bitPattern' generates view-pattern matches on 'Unsigned n' values,
-- replacing the Clash @$(bitPattern …)@ quasiquoter without requiring
-- any Clash dependency.
--
-- Binary loaders return @[Integer]@ list splices, matching the @[Integer]@
-- initial-contents parameter of 'Isacle.System.Periph.blockRomDef'.
module Isacle.TH
    ( loadBinWith
    , loadBin8
    , loadBin16LE
    , loadBin32LE
    , parseWords16LE
    , parseWords32LE
    , padToPow2
    , nextPow2
    , bitPattern
    ) where

import Prelude
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addDependentFile)
import qualified Data.ByteString as BS
import Data.Bits  (shiftL, shiftR, (.|.), (.&.), countLeadingZeros,
                   finiteBitSize, bit)
import Data.List  (nub, sort, foldl')
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

-- ---------------------------------------------------------------------------
-- bitPattern
-- ---------------------------------------------------------------------------

-- | Generate a view-pattern splice for matching on 'Unsigned n' bit vectors.
--
-- @$(bitPattern "0001_11rd_dddd_rrrr_...._...._...._....") = Adc (unpack ddddd) (unpack rrrrr)@
--
-- Syntax:
--   * @0@, @1@  — literal bit values (checked at runtime)
--   * letter   — named bit; all occurrences of the same letter are
--                concatenated MSB-first to form a variable named by repeating
--                the letter (e.g. five @r@s → @rrrrr@)
--   * @.@       — don't care
--   * @_@, space — ignored separators
--
-- Variables are bound in alphabetical order by letter.
-- The input is treated as 'Unsigned n' (backed by 'Integer').
--
-- Requires @{-# LANGUAGE ViewPatterns #-}@ in the calling module.
bitPattern :: String -> Q Pat
bitPattern str = do
    let cleaned = filter (\c -> c /= '_' && c /= ' ') str
        n       = length cleaned
        iAt i   = n - 1 - i  -- bit position in Integer (0 = LSB)

        chars   = zip [0..] cleaned

        litBits = [(i, c) | (i, c) <- chars, c == '0' || c == '1']
        mask    = foldl' (\m (i, _) -> m .|. (1 `shiftL` iAt i)) (0::Integer) litBits
        expct   = foldl' (\e (i, c) -> if c == '1'
                                       then e .|. (1 `shiftL` iAt i)
                                       else e)   (0::Integer) litBits

        varBits = [(c, iAt i) | (i, c) <- chars, c /= '0', c /= '1', c /= '.']
        letters = nub (sort (map fst varBits))
        groups  = [(c, [p | (c', p) <- varBits, c' == c]) | c <- letters]

    vn    <- newName "bp_v"    -- name for the Unsigned argument
    rawVn <- newName "bp_raw"  -- name for the extracted Integer

    -- Build extraction expression for one variable group.
    -- Uses rawVn (an Integer) in scope.
    let extractE positions =
          let m = length positions - 1
              terms = [ [| ( $(varE rawVn)
                              `shiftR` $(litE (integerL (fromIntegral p)))
                              .&. 1 )
                            `shiftL` $(litE (integerL (fromIntegral (m - i)))) |]
                      | (i, p) <- zip [0..] positions ]
          in foldr1 (\a b -> [| $a .|. $b |]) terms

    -- Wrap each group's Integer extraction in the Unsigned constructor.
    let unsignedN = mkName "Unsigned"
    extractions <- mapM (\(_, ps) -> appE (conE unsignedN) (extractE ps)) groups

    -- Build: if rawVn .&. mask == expct then Just result else Nothing
    let checkE = [| $(varE rawVn) .&. $(litE (integerL mask))
                        == $(litE (integerL expct)) |]
    resultE <- case extractions of
                 [e] -> [| Just $(pure e) |]
                 _   -> [| Just $(tupE (map pure extractions)) |]
    bodyE   <- [| if $checkE then $(pure resultE) else Nothing |]

    -- Build the lambda using caseE so we can pattern-match Unsigned without
    -- a pattern splice (which is unsupported inside [| |]).
    caseBody <- caseE (varE vn)
                      [match (conP unsignedN [varP rawVn])
                             (normalB (pure bodyE))
                             []]
    viewFnExpr <- lamE [varP vn] (pure caseBody)

    -- Bound variable names: letter repeated count times, alphabetical order.
    let varNames = [mkName (replicate (length ps) c) | (c, ps) <- groups]
    innerPat <- case varNames of
                  [nm] -> conP 'Just [varP nm]
                  nms  -> conP 'Just [tupP (map varP nms)]

    return $ ViewP viewFnExpr innerPat
