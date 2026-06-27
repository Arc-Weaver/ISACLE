{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Isacle.ISA.CPUDef where

import Prelude
import Control.Monad.Writer
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
    , schAliasFiles :: [(String, String)]            -- regfile name, address function desc
    }

instance Semigroup CPUSchema where
    a <> b = CPUSchema
        { schEndianness = schEndianness a
        , schRegFiles   = schRegFiles   a <> schRegFiles   b
        , schRegisters  = schRegisters  a <> schRegisters  b
        , schStatusRegs = schStatusRegs a <> schStatusRegs b
        , schAliasRegs  = schAliasRegs  a <> schAliasRegs  b
        , schAliasFiles = schAliasFiles a <> schAliasFiles b
        }

instance Monoid CPUSchema where
    mempty = CPUSchema LittleEndian [] [] [] [] []

runCPUDef :: CPUDef a -> (a, CPUSchema)
runCPUDef (CPUDef w) = runWriter w

-- ---------------------------------------------------------------------------
-- CPUDef combinators
-- ---------------------------------------------------------------------------

endianness :: Endianness -> CPUDef ()
endianness e = CPUDef $ tell mempty { schEndianness = e }

regFile :: forall count w. (KnownNat count, KnownNat w)
        => String -> SNat count -> SNat w
        -> CPUDef (CPURegFile count w)
regFile name _sc _sw = CPUDef $ do
    tell mempty { schRegFiles = [(name, fromIntegral (natVal (Proxy @count)), fromIntegral (natVal (Proxy @w)))] }
    pure (CPURegFile name)

reg :: forall w. KnownNat w
    => String -> SNat w
    -> CPUDef (CPURegister w)
reg name _sw = CPUDef $ do
    tell mempty { schRegisters = [(name, fromIntegral (natVal (Proxy @w)))] }
    pure (CPURegister name)

-- | Declare a status register: a single NReg that holds packed flag bits.
-- @flagNames@ is ordered MSB-first; the first name is the highest bit.
-- Returns the register reference and one CPUFlag per name, in the same order.
flagPack :: forall n. KnownNat n
         => String -> [String]
         -> CPUDef (CPURegister n, [CPUFlag])
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
        => String -> CPUDef (CPURegister (Width a), [CPUFlag])
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

aliasFile :: CPURegFile count w -> String -> CPUDef ()
aliasFile (CPURegFile name) addrDesc = CPUDef $
    tell mempty { schAliasFiles = [(name, addrDesc)] }
