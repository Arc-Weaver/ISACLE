{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The shared address-mapping helper (Part-II §3 / C5 / PE4).
--
-- One unifying idea ties together three things that look different but are the
-- same: register/flag bit positions, peripheral register offsets, and bus base
-- addresses. Each is a /flat view placed at a position within a containing
-- space/:
--
--   * a flag is a field placed at a bit position within its register;
--   * a register is a field placed at a byte offset within a peripheral window;
--   * a peripheral is a window placed at a base address within a bus space.
--
-- The same 'Placement'/'Layout' shape and the same 'placeAt' shift express all
-- three. The bit-position case ('bitLayout') is /derived for free/ from a record
-- 'HdlType' (C2): a flag's bit offset is simply its position in the record's
-- MSB-first flatten, so "flag = bit N of SREG" needs no separate declaration.
module Isacle.Layout
    ( Placement(..)
    , plHi
    , Layout(..)
    , bitLayout
    , addrLayout
    , placeAt
    , lookupPlacement
    ) where

import Prelude
import Data.Proxy (Proxy(..))
import Data.List  (find)
import GHC.Generics (Generic, Rep)

import Hdl.Types (GFields, recordFields)

-- | A named flat view occupying the index range @[plPos, plPos+plSpan)@ within
-- some containing space. The interpretation of an index — a bit, a byte — is the
-- space's, not the placement's; that is exactly what lets one shape serve flags,
-- registers, and peripherals alike.
data Placement = Placement
    { plName :: String   -- ^ the field/register/peripheral name
    , plPos  :: Int      -- ^ low index (LSB for bits, base offset for bytes)
    , plSpan :: Int      -- ^ number of indices occupied
    } deriving (Eq, Show)

-- | The high (inclusive) index of a placement.
plHi :: Placement -> Int
plHi p = plPos p + plSpan p - 1

-- | A set of placements within a containing space of a known size.
data Layout = Layout
    { layoutSize       :: Int          -- ^ total indices in the space
    , layoutPlacements :: [Placement]
    } deriving (Eq, Show)

-- | Bit-positions of a record 'HdlType''s fields (the C2 case). Each field is
-- placed at its position in the record's MSB-first flatten: the first declared
-- field occupies the most-significant bits. This is the layout 'fieldRec' uses
-- for a register's bit-fields and that flag projection reads — single-sourced
-- here so cores and peripherals agree by construction.
bitLayout :: forall a. (Generic a, GFields (Rep a)) => Proxy a -> Layout
bitLayout _ = Layout total (go (total - 1) fields)
  where
    fields = recordFields (Proxy @a)   -- [(name, width)] in declaration order
    total  = sum (map snd fields)
    go _  []            = []
    go hi ((n, w) : rest) =
        let lo = hi - w + 1
        in Placement n lo w : go (lo - 1) rest

-- | Explicit placements in an address window (the field → address / PE4 case):
-- each entry is @(name, lowOffset, span)@, base-relative. The bus later assigns
-- the base by 'placeAt'-ing the whole window.
addrLayout :: Int -> [(String, Int, Int)] -> Layout
addrLayout size = Layout size . map (\(n, off, w) -> Placement n off w)

-- | Place a flat view (its own layout) at a position within a larger space,
-- shifting every placement by the base. This is the one operation the whole
-- helper exists to provide: a peripheral's base-relative offsets become bus
-- addresses, a register's bit-map becomes a slice of a wider word, and so on.
placeAt :: Int -> Layout -> [Placement]
placeAt base = map (\p -> p { plPos = plPos p + base }) . layoutPlacements

-- | Find a placement by name.
lookupPlacement :: String -> Layout -> Maybe Placement
lookupPlacement n = find ((== n) . plName) . layoutPlacements
