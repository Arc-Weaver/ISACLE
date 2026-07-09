{-# LANGUAGE KindSignatures #-}
-- | A CPU \"chip\": the storage-core definition bundled with the ISA that wires
-- it up.  'CPUDef' and 'ISADef' necessarily share the ALU record type, so
-- travelling together is the honest shape — and it removes the redundant
-- @\@core \@addrW \@cwW \@cawW@ type applications at every construction site
-- (they are pinned by the chip value's type instead).
module Isacle.ISA.Chip (Chip(..)) where

import Data.Kind (Type)
import GHC.TypeLits (Nat)

import Isacle.ISA.CPUDef (CPUDef)
import Isacle.ISA.Def    (ISADef)
import Isacle.ISA.Build  (ISABuild)

-- | A complete CPU definition.
--
--   * @core@  — the storage record (SynthCPU's state type; phantom here, it is
--     what the backend elaborates the register set into).
--   * @alu@   — the ALU record shared by 'CPUDef' and the ISA-build monad.
--   * @wordW@ — the CPU word width (= @Width dat@ of the bus it drives).
--   * @addrW@ \/ @cwW@ \/ @cawW@ — data-address, code-word, code-address widths.
data Chip (core :: Type) alu (wordW :: Nat) (addrW :: Nat) (cwW :: Nat) (cawW :: Nat) = Chip
    { chipCpu :: CPUDef alu
    , chipIsa :: ISADef (ISABuild alu wordW addrW cwW cawW)
    }
