{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
module Isacle.ISA.Def where

import Prelude
import Hdl.Bits
import Isacle.ISA.Types
import Isacle.ISA.ALU

-- ---------------------------------------------------------------------------
-- ResetDef monad
-- Declares the power-on state of each CPU state element.
-- Uses the same accessor style as MonadALU to avoid explicit destructuring.
-- ---------------------------------------------------------------------------

newtype ResetDef alu a = ResetDef { runResetDef :: alu -> [ResetEntry] }
    deriving Functor

data ResetEntry
    = forall w. ResetRegEntry  String (Unsigned w)
    | ResetFlagEntry String Int Bit   -- ^ status register name, bit position, value

instance Applicative (ResetDef alu) where
    pure _  = ResetDef (const [])
    f <*> x = ResetDef $ \alu -> runResetDef f alu <> runResetDef x alu

instance Monad (ResetDef alu) where
    return = pure
    m >>= f = ResetDef $ \alu ->
        runResetDef m alu <> runResetDef (f undefined) alu

resetReg :: (alu -> CPURegister w) -> Unsigned w -> ResetDef alu ()
resetReg sel val = ResetDef $ \alu ->
    let CPURegister name = sel alu
    in [ResetRegEntry name val]

resetFlag :: (alu -> CPUFlag) -> Bit -> ResetDef alu ()
resetFlag sel val = ResetDef $ \alu ->
    let CPUFlag { cpuFlagReg = rn, cpuFlagBit = bp } = sel alu
    in [ResetFlagEntry rn bp val]

-- ---------------------------------------------------------------------------
-- ISADef
-- Binds the architectural roles of named CPU state elements and declares
-- the instruction set. Runners use this to wire up fetch, interrupt
-- handling, context save/restore, and reset logic.
-- ---------------------------------------------------------------------------

data ISADef m = ISADef
    { -- | Which register drives instruction fetch
      isaPc            :: m SomeCPURegister

      -- | Interrupt service routine body, or Nothing for no IRQ support.
      -- Written using the same MonadALU DSL as regular instructions.
      -- Use irqGate to add an additional gate condition (e.g. the global IE flag);
      -- use irqVector to read the externally-supplied vector address.
    , isaInterruptBody :: Maybe (m ())

      -- | Power-on reset state for all declared state elements
    , isaReset         :: ResetDef (AluDef m) ()

      -- | The instruction definitions
    , isaInstrs        :: [m ()]
    }

-- Existential wrapper so isaPc / isaInterruptVec don't fix the width
-- in the ISADef record — the runner extracts width at elaboration time.
data SomeCPURegister = forall w. SomeCPURegister (CPURegister w)

-- ---------------------------------------------------------------------------
-- defineISA
-- ---------------------------------------------------------------------------

defineISA :: (MonadALU m, AluDef m ~ alu)
          => ISADef m -> ISADef m
defineISA = id
