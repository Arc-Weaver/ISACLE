-- | Memory banks — the ROM/RAM arrays in an elaborated 'Design', extracted as
-- addressable banks with stable names, so their /contents/ can be regenerated
-- and reloaded __without resynthesis__.
--
-- Each bank yields:
--
--   * a @.mem@ file — @$readmemh@-compatible hex, one word per line;
--   * a @.coe@ file — Xilinx BRAM IP initialization vector;
--   * a row in a JSON manifest — bank name, base address, depth×width, HDL signal
--     name, and the data-file names — the map a post-synthesis reload step
--     (Xilinx @updatemem@ / @data2mem@, or a Tcl @INIT@ reload) consumes.
--
-- The stable bank name is the peripheral instance / entity name (e.g.
-- @coderom0@), matching the @\<entity\>_rom@ array the VHDL emitter now names.
module Isacle.System.MemBanks
    ( BankKind(..)
    , MemBank(..)
    , extractMemBanks
    , memFileName
    , coeFileName
    , renderMem
    , renderCoe
    , renderManifest
    ) where

import Prelude
import Data.Char (toUpper)
import Data.List (find)
import Data.Word (Word32)
import qualified Data.Map.Strict as Map
import Numeric (showHex)

import Hdl.Net (Design, NetNode(..))
import Isacle.System.SystemDSL (SysDoc(..), BusSection(..), PeriphEntry(..))
import Isacle.System.Periph (specSize)

data BankKind = BankRom | BankRam
    deriving (Eq, Show)

-- | One memory bank in the design.
data MemBank = MemBank
    { mbName   :: String        -- ^ entity / peripheral instance name (stable id)
    , mbKind   :: BankKind
    , mbBase   :: Maybe Word32  -- ^ base address (from the memory map, if attached)
    , mbDepth  :: Int           -- ^ number of words
    , mbWidth  :: Int           -- ^ word width in bits
    , mbInit   :: [Integer]     -- ^ initial contents (padded to depth)
    , mbSignal :: String        -- ^ HDL array signal name inside the entity
    } deriving (Show)

-- | Extract every ROM/RAM bank from an elaborated design.  Depth/width/contents
-- come from the @NRom@/@NMem@ nodes; the base address is looked up by entity name
-- in the system's memory map.
extractMemBanks :: SysDoc -> Design -> [MemBank]
extractMemBanks doc design =
    concat [ banksIn ent nodes | (ent, nodes) <- Map.toList design ]
  where
    banksIn ent nodes =
        [ MemBank { mbName = ent, mbKind = BankRom, mbBase = baseOf ent
                  , mbDepth = nRomSize n, mbWidth = nRomDatW n
                  , mbInit = pad (nRomSize n) (nRomInit n)
                  , mbSignal = ent ++ "_rom" }
        | n@NRom{} <- nodes ]
        ++
        [ MemBank { mbName = ent, mbKind = BankRam, mbBase = baseOf ent
                  , mbDepth = nMemSize n, mbWidth = nMemDatW n
                  , mbInit = pad (nMemSize n) (nMemInit n)
                  , mbSignal = ent }
        | n@NMem{} <- nodes ]

    pad d vs = take d (vs ++ repeat 0)

    baseOf ent = peBase <$> find ((== ent) . peName)
                                 [ pe | bs <- sdBuses doc, pe <- bsEntries bs ]

-- | @.mem@ filename for a bank.
memFileName :: MemBank -> String
memFileName b = mbName b ++ ".mem"

-- | @.coe@ filename for a bank.
coeFileName :: MemBank -> String
coeFileName b = mbName b ++ ".coe"

-- | Hex digits needed for a word of the given bit width.
hexDigits :: Int -> Int
hexDigits w = (w + 3) `div` 4

hexWord :: Int -> Integer -> String
hexWord w v =
    let s = showHex (v `mod` (2 ^ w)) ""
        n = hexDigits w
    in replicate (n - length s) '0' ++ s

-- | Render a bank as @$readmemh@-compatible hex: one word per line, MSB word
-- first (ascending address), zero-padded to the word width.
renderMem :: MemBank -> String
renderMem b = unlines [ hexWord (mbWidth b) v | v <- mbInit b ]

-- | Render a bank as a Xilinx @.coe@ initialization vector (radix 16).
renderCoe :: MemBank -> String
renderCoe b = unlines $
    [ "memory_initialization_radix=16;"
    , "memory_initialization_vector=" ]
    ++ withSeps [ hexWord (mbWidth b) v | v <- mbInit b ]
  where
    -- comma-separated, final word terminated with ';'
    withSeps []     = [";"]
    withSeps [x]    = [x ++ ";"]
    withSeps (x:xs) = (x ++ ",") : withSeps xs

-- | Render a machine-readable JSON manifest of all banks: enough for an
-- @updatemem@ / Tcl reload script to find each bank and its data file.
renderManifest :: [MemBank] -> String
renderManifest banks = unlines $
    [ "{" , "  \"banks\": [" ]
    ++ commaJoin (map row banks)
    ++ [ "  ]" , "}" ]
  where
    commaJoin []     = []
    commaJoin [x]    = [x]
    commaJoin (x:xs) = (x ++ ",") : commaJoin xs

    row b = "    { " ++ intercalateFields
        [ ("name",   str (mbName b))
        , ("kind",   str (kindStr (mbKind b)))
        , ("base",   maybe "null" hex32 (mbBase b))
        , ("depth",  show (mbDepth b))
        , ("width",  show (mbWidth b))
        , ("signal", str (mbSignal b))
        , ("mem",    str (memFileName b))
        , ("coe",    str (coeFileName b))
        ] ++ " }"

    intercalateFields = go
      where go []           = ""
            go [(k,v)]      = field k v
            go ((k,v):rest) = field k v ++ ", " ++ go rest
    field k v = "\"" ++ k ++ "\": " ++ v
    str s     = "\"" ++ s ++ "\""
    hex32 w   = "\"0x" ++ map toUpper (showHex w "") ++ "\""
    kindStr BankRom = "rom"
    kindStr BankRam = "ram"
