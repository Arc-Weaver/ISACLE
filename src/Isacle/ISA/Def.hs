{-# LANGUAGE ExistentialQuantification #-}
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
      isaPc          :: m SomeCPURegister

      -- | Flag that gates interrupt acceptance
    , isaInterruptEn :: m CPUFlag

      -- | Register loaded with the interrupt vector on acceptance
    , isaInterruptVec :: m SomeCPURegister

      -- | State saved on interrupt / call, in push order.
      -- Each item must be exactly one data word wide; wider values
      -- must be packed first. Byte order follows CPUDef endianness.
    , isaContextSave :: [ContextItem m]

      -- | Optional privilege/supervisor mode flag.
      -- Nothing for architectures with no privilege levels.
    , isaSupervisor  :: Maybe (m CPUFlag)

      -- | Power-on reset state for all declared state elements
    , isaReset       :: ResetDef (AluDef m) ()

      -- | The instruction definitions
    , isaInstrs      :: [m ()]
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
