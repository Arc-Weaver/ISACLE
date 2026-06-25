{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Isacle.ISA.CPUDef where

import Prelude
import Control.Monad.Writer
import Data.Proxy (Proxy(..))
import GHC.TypeLits (natVal)
import Hdl.Bits
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

-- Declare that a register is readable/writable via a data space address.
-- The pipeline uses these to detect hazards across the register/memory boundary.
aliasReg :: CPURegister w -> Integer -> CPUDef ()
aliasReg (CPURegister name) addr = CPUDef $
    tell mempty { schAliasRegs = [(name, addr)] }

aliasFile :: CPURegFile count w -> String -> CPUDef ()
aliasFile (CPURegFile name) addrDesc = CPUDef $
    tell mempty { schAliasFiles = [(name, addrDesc)] }
