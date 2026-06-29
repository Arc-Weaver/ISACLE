{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Isacle.ISA.CPUDef where

import Prelude
import Control.Monad.Writer
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (natVal)
import GHC.Generics (Generic, Rep)
import Hdl.Bits
import Hdl.Types (HdlType, Width, GFields, recordFields)
import Isacle.Layout (bitLayout, layoutSize, layoutPlacements, plName, plPos)
import Isacle.ISA.Types

-- ---------------------------------------------------------------------------
-- CPUDef monad
-- Declares the structure of a CPU's architectural state.
-- Run once to produce a CPUSchema that backends use for simulation,
-- synthesis, and documentation.
-- ---------------------------------------------------------------------------

newtype CPUDef a = CPUDef (Writer CPUSchema a)
    deriving newtype (Functor, Applicative, Monad)

data CPUSchema = CPUSchema
    { schEndianness :: Endianness
    , schRegFiles   :: [(String, Int, Int)]          -- name, count, element width
    , schRegisters  :: [(String, Int)]               -- name, width (includes status registers)
    , schStatusRegs :: [(String, Int, [String])]     -- name, width, flag names MSB-first
    , schAliasRegs  :: [(String, Integer)]           -- reg name, data address
    , schAliasFiles :: [(String, Integer)]           -- regfile name, data base address (entry i → base+i)
    , schFlags      :: [(String, Int, String)]       -- individual flags: reg, bit, flag name
    , schRegViews   :: [(String, String, [Int])]     -- view name, file, entry indices (low→high)
    }

instance Semigroup CPUSchema where
    a <> b = CPUSchema
        { schEndianness = schEndianness a
        , schRegFiles   = schRegFiles   a <> schRegFiles   b
        , schRegisters  = schRegisters  a <> schRegisters  b
        , schStatusRegs = schStatusRegs a <> schStatusRegs b
        , schAliasRegs  = schAliasRegs  a <> schAliasRegs  b
        , schAliasFiles = schAliasFiles a <> schAliasFiles b
        , schFlags      = schFlags      a <> schFlags      b
        , schRegViews   = schRegViews   a <> schRegViews   b
        }

instance Monoid CPUSchema where
    mempty = CPUSchema LittleEndian [] [] [] [] [] [] []

-- | Run a core definition, folding any individually-declared flags ('newFlag')
-- into the status-register bit maps ('schStatusRegs') so the synthesis backend
-- sees one uniform view however the flags were declared.
runCPUDef :: CPUDef a -> (a, CPUSchema)
runCPUDef (CPUDef w) =
    let (a, sch) = runWriter w
    in (a, sch { schStatusRegs = schStatusRegs sch ++ derivedStatusRegs sch })

-- | Group 'schFlags' by their register into @(reg, width, flagNamesMSBfirst)@.
derivedStatusRegs :: CPUSchema -> [(String, Int, [String])]
derivedStatusRegs sch =
    [ (rn, regWidthOf rn, namesMsbFirst rn)
    | rn <- regsWithFlags ]
  where
    regsWithFlags = nubOrd [ r | (r, _, _) <- schFlags sch ]
    regWidthOf rn = fromMaybe 8 (lookup rn (schRegisters sch))
    -- MSB-first: highest bit position first.
    namesMsbFirst rn =
        [ nm | (_, _, nm) <- sortByBitDesc [ f | f@(r,_,_) <- schFlags sch, r == rn ] ]
    sortByBitDesc = sortBy (\(_,b1,_) (_,b2,_) -> compare b2 b1)
    nubOrd = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

-- ---------------------------------------------------------------------------
-- CPUDef combinators
-- ---------------------------------------------------------------------------

endianness :: Endianness -> CPUDef ()
endianness e = CPUDef $ tell mempty { schEndianness = e }

-- | A CPU core definition: the monadic builder whose result is the core's
-- register record (e.g. @ISACoreDefinition (AVRALU pcW)@).
type ISACoreDefinition = CPUDef

-- | Declare a register file: @count@ registers of value type @t@. The count and
-- element width come from the result type — @newRegFile \"r\" :: _ (RegisterFile 32 (Unsigned 8))@.
newRegFile :: forall count t. (KnownNat count, HdlType t)
           => String -> CPUDef (CPURegFile count t)
newRegFile name = CPUDef $ do
    tell mempty { schRegFiles = [( name
                                 , fromIntegral (natVal (Proxy @count))
                                 , fromIntegral (natVal (Proxy @(Width t))) )] }
    pure (CPURegFile name)

-- | Declare a register of value type @t@; the bit width is @'Width' t@.
-- @newReg \"sp\" :: _ (CPURegister (Unsigned 16))@. Use @newReg \@t@ to pin the
-- value type explicitly when it cannot be inferred from the result.
newReg :: forall t. HdlType t => String -> CPUDef (CPURegister t)
newReg name = CPUDef $ do
    tell mempty { schRegisters = [(name, fromIntegral (natVal (Proxy @(Width t))))] }
    pure (CPURegister name)

-- | Declare a plain unsigned register by giving its width directly:
-- @reg \"PC\" (width \@22)@ → @CPURegister (Unsigned 22)@. A convenience over
-- @newReg \@(Unsigned w)@ for the common all-unsigned case.
reg :: forall w. KnownNat w => String -> SNat w -> CPUDef (CPURegister (Unsigned w))
reg name _ = newReg @(Unsigned w) name

-- | Name a flag: a bit-view of a register (@newFlag \"c\" (sreg ! 0)@). Records
-- the flag for the status-register bit map and returns the 'CPUFlag' view.
newFlag :: String -> CPUFlag -> CPUDef CPUFlag
newFlag fname f@(CPUFlag regName bitPos) = CPUDef $ do
    tell mempty { schFlags = [(regName, bitPos, fname)] }
    pure f

-- | Declare a status register: a single NReg that holds packed flag bits.
-- @flagNames@ is ordered MSB-first; the first name is the highest bit.
-- Returns the register reference and one CPUFlag per name, in the same order.
flagPack :: forall n. KnownNat n
         => String -> [String]
         -> CPUDef (CPURegister (Unsigned n), [CPUFlag])
flagPack regName flagNames = CPUDef $ do
    let w = fromIntegral (natVal (Proxy @n))
        flags = zipWith (\bitPos _ -> CPUFlag regName bitPos)
                        (reverse [0 .. length flagNames - 1])
                        flagNames
    tell mempty
        { schRegisters  = [(regName, w)]
        , schStatusRegs = [(regName, w, flagNames)]
        }
    pure (CPURegister regName, flags)

-- | Declare a status register from a record 'HdlType' — the core-side mirror of
-- the peripheral 'Isacle.System.Periph.fieldRec'. The register width and each
-- flag's bit position are /derived/ from the record's MSB-first layout through
-- the shared address-mapping helper ('Isacle.Layout.bitLayout'), so a CPU flag
-- and a peripheral bit-field share one mechanism and "flag = bit N" needs no
-- separate declaration (C2/C5). The register's type-level width is tied to the
-- record (@Width a@), giving length-by-default. Returns the register reference
-- and one 'CPUFlag' per field, in declaration (MSB-first) order.
flagRec :: forall a. (HdlType a, Generic a, GFields (Rep a))
        => String -> CPUDef (CPURegister a, [CPUFlag])
flagRec regName = CPUDef $ do
    let layout    = bitLayout (Proxy @a)
        w         = layoutSize layout
        places    = layoutPlacements layout           -- MSB-first
        flagNames = map plName places
        flags     = [ CPUFlag regName (plPos p) | p <- places ]
    tell mempty
        { schRegisters  = [(regName, w)]
        , schStatusRegs = [(regName, w, flagNames)]
        }
    pure (CPURegister regName, flags)

-- | Declare every field of a record 'HdlType' as a CPU register, single-sourcing
-- each register's name (the field selector) and width (the field's 'Width') from
-- the record — groundwork for the core-as-record reframe (C1/C3, step 2 of
-- @PLAN_CORE_REFRAME.md@). Adopting this in a core def replaces the hand-written
-- @reg "PC"@ / @reg "SP"@ calls: the schema's names and widths /are/ the record's
-- fields, so they cannot drift from the Haskell type. Status registers still use
-- 'flagRec' (for the bit-fields); this covers the plain scalar/array registers.
regsFromRecord :: forall a. (Generic a, GFields (Rep a)) => Proxy a -> CPUDef ()
regsFromRecord _ = CPUDef $ tell mempty { schRegisters = recordFields (Proxy @a) }

-- Declare that a register is readable/writable via a data space address.
-- The pipeline uses these to detect hazards across the register/memory boundary.
aliasReg :: CPURegister w -> Integer -> CPUDef ()
aliasReg (CPURegister name) addr = CPUDef $
    tell mempty { schAliasRegs = [(name, addr)] }

-- | Map a register file into the data address space at an explicit base
-- address: entry @i@ is at data address @base + i@ (the file is assumed
-- sequentially addressed).  The synthesis backend consumes this to route data
-- reads/writes in @[base, base+count)@ to the register file — so e.g. a read of
-- data address @base+5@ reaches @GPR[5]@.
aliasFile :: CPURegFile count w -> Integer -> CPUDef ()
aliasFile (CPURegFile name) base = CPUDef $
    tell mempty { schAliasFiles = [(name, base)] }

-- | A register that is a /view/ over consecutive register-file entries, low byte
-- first — the register-file analogue of 'newFlag' projecting a bit.  E.g.
-- @regView "X" gpr [26, 27]@ makes X the 16-bit pair @GPR[26]:GPR[27]@ (R27:R26),
-- with no storage of its own: reads concatenate the entries (first index = least
-- significant), writes split the value back across them.  The composite width
-- @w@ must equal @length idxs * elementWidth@.
regView :: forall w count t. (KnownNat w, HdlType t)
        => String -> CPURegFile count t -> [Int] -> CPUDef (CPURegister (Unsigned w))
regView name (CPURegFile fileName) idxs = CPUDef $ do
    tell mempty { schRegViews = [(name, fileName, idxs)] }
    pure (CPURegister (encodeRegView fileName elemW idxs))
  where
    elemW = fromIntegral (natVal (Proxy @(Width t)))
