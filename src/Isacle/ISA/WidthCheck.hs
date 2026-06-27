-- | ISA ↔ memory-model width compatibility check (A2).
--
-- An instruction set is only realizable on a given memory model if its
-- encodings fit the code bus and its data values fit the data bus. The two
-- models differ in /how many/ checks that is:
--
--   * __Von Neumann__ unifies code and data on one bus, so there is a single
--     width and a single check: every instruction encoding and every data value
--     is measured against the one bus width.
--   * __Harvard__ has independent code and data buses, so there are two checks:
--     encodings against the /code/ width, values against the /data/ width.
--
-- This mirrors the bus capability rule ('Isacle.System.BusCap'): a structural
-- compatibility relation, here over widths instead of stall capability. It is a
-- value-level validator (the encoding widths are value-level), returning the
-- specific mismatches rather than a yes/no.
module Isacle.ISA.WidthCheck
    ( MemModel(..)
    , codeWidth
    , dataWidth
    , WidthError(..)
    , checkInstrWidths
    , checkDataWidths
    , checkMemModel
    ) where

import Prelude
import Isacle.ISA.Encoding (EncodingInfo(..))

-- | A memory model and its bus width(s), in bits.
data MemModel
    = VonNeumann Int        -- ^ one unified bus width
    | Harvard Int Int       -- ^ code-bus width, data-bus width
    deriving (Eq, Show)

-- | The code-bus width instructions are fetched over.
codeWidth :: MemModel -> Int
codeWidth (VonNeumann w) = w
codeWidth (Harvard c _)  = c

-- | The data-bus width loads/stores move over.
dataWidth :: MemModel -> Int
dataWidth (VonNeumann w) = w
dataWidth (Harvard _ d)  = d

-- | A specific width incompatibility.
data WidthError = WidthError
    { weItem   :: String   -- ^ instruction / value name
    , weBits   :: Int      -- ^ its width in bits
    , weReason :: String   -- ^ why it does not fit
    } deriving (Eq, Show)

-- | Check instruction encodings against the /code/ bus: each must be a whole
-- number of code words and fit within @maxFetch@ words. (The code-side check.)
checkInstrWidths :: Int          -- ^ code-bus width (bits)
                 -> Int          -- ^ MaxFetch (max instruction length in words)
                 -> [(String, EncodingInfo)]
                 -> [WidthError]
checkInstrWidths w maxFetch = concatMap check
  where
    check (name, enc)
        | w <= 0 =
            [WidthError name (encTotalBits enc) "non-positive code-bus width"]
        | encTotalBits enc `mod` w /= 0 =
            [WidthError name (encTotalBits enc)
                ("not a multiple of code-bus width " ++ show w)]
        | encTotalBits enc > maxFetch * w =
            [WidthError name (encTotalBits enc)
                ("exceeds MaxFetch (" ++ show maxFetch ++ ") * code width " ++ show w)]
        | otherwise = []

-- | Check declared data-value widths against the /data/ bus: each value must
-- fit in one data word. (The data-side check.)
checkDataWidths :: Int               -- ^ data-bus width (bits)
                -> [(String, Int)]   -- ^ named values and their widths
                -> [WidthError]
checkDataWidths w = concatMap check
  where
    check (name, bits)
        | w <= 0    = [WidthError name bits "non-positive data-bus width"]
        | bits > w  = [WidthError name bits
                          ("exceeds data-bus width " ++ show w)]
        | otherwise = []

-- | Validate an ISA against a memory model (A2). The model determines the shape
-- of the check: Von Neumann measures /both/ encodings and values against its
-- one width (one bus); Harvard measures encodings against the code width and
-- values against the data width (two buses). Returns all mismatches.
checkMemModel :: MemModel
              -> Int                      -- ^ MaxFetch
              -> [(String, EncodingInfo)] -- ^ instruction encodings
              -> [(String, Int)]          -- ^ data value widths
              -> [WidthError]
checkMemModel mm maxFetch instrs values = case mm of
    -- One bus, one width: a single check covers both code and data.
    VonNeumann w ->
        checkInstrWidths w maxFetch instrs ++ checkDataWidths w values
    -- Two buses: encodings vs code width, values vs data width.
    Harvard c d ->
        checkInstrWidths c maxFetch instrs ++ checkDataWidths d values
