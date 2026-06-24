module Isacle.ISA.Encoding
    ( -- * Parsed encoding descriptor
      EncodingInfo(..)
    , FieldName
    , parseEncoding
      -- * Field extraction
    , fieldKey
    , extractField
    , matchesWord
    ) where

import Prelude
import Data.Char (isAlpha)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

-- ---------------------------------------------------------------------------
-- Encoding string format
--
-- Characters (underscores are stripped before parsing):
--   '0' / '1'   fixed opcode bits
--   '.'         don't-care bit
--   alpha char  field bit; all occurrences of the same letter form one field,
--               left-to-right = MSB-to-LSB of the reconstructed value
--
-- Example:  "0111_kkkk_dddd_kkkk_...."
--   Stripped: "0111kkkkddddkkkk...."  (20 chars → bits 19..0)
--   Field k: positions [15,14,13,12, 7,6,5,4] (8 bits, non-contiguous)
--   Field d: positions [11,10,9,8]             (4 bits, contiguous)
--   Fixed 1: positions [19,18,17]
--   Fixed 0: position  [16]
-- ---------------------------------------------------------------------------

type FieldName = String  -- single-char string: "d", "k", "r", …

-- | Parsed encoding: field bit positions and opcode mask/value.
data EncodingInfo = EncodingInfo
    { encFields    :: Map FieldName [Int]  -- field key → bit positions (MSB first)
    , encMask      :: Integer              -- 1 where bit is fixed (0 or 1)
    , encValue     :: Integer              -- expected value of fixed bits
    , encTotalBits :: Int                  -- total bit width (after stripping '_')
    } deriving (Show)

-- | Normalise a field name from a 'register' / 'immediate' call to the
--   single-character key used in 'encFields'.
--   "ddddd" → "d",  "kkkkkkkk" → "k",  "r" → "r"
fieldKey :: String -> FieldName
fieldKey ""    = ""
fieldKey (c:_) = [c]

-- | Parse an encoding string into an 'EncodingInfo'.
parseEncoding :: String -> EncodingInfo
parseEncoding s =
    let stripped = filter (/= '_') s
        n        = length stripped
        -- (bit position from LSB, char), leftmost char → highest bit
        indexed  = zip [n-1, n-2 .. 0] stripped
        go acc (bp, c)
            | c == '0'  = acc { encMask  = encMask acc  .|. bit bp
                               }
            | c == '1'  = acc { encMask  = encMask acc  .|. bit bp
                               , encValue = encValue acc .|. bit bp
                               }
            | c == '.'  = acc
            | isAlpha c =
                let k   = [c]
                    cur = Map.findWithDefault [] k (encFields acc)
                in acc { encFields = Map.insert k (cur ++ [bp]) (encFields acc) }
            | otherwise = acc
        empty = EncodingInfo Map.empty 0 0 n
    in foldl go empty indexed
  where
    bit p = 1 `shiftL` p

-- | Extract a field value from a concrete instruction word.
--   Bit positions are in MSB-first order (left = highest bit of the field).
extractField :: [Int] -> Integer -> Integer
extractField bitPositions word =
    foldl (\acc bp -> (acc `shiftL` 1) .|. ((word `shiftR` bp) .&. 1))
          0 bitPositions

-- | True when @word@ matches the fixed-bit pattern described by the encoding.
matchesWord :: EncodingInfo -> Integer -> Bool
matchesWord enc word = (word .&. encMask enc) == encValue enc
