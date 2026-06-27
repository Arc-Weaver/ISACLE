{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Isacle.ISA.ALU where

import Prelude hiding (Word)
import Data.Kind (Type)
import GHC.TypeLits (KnownNat, Nat)
import Hdl.Bits (Bit(..))
import Hdl.Types (HdlType, Width)
import Isacle.ISA.Types
import Isacle.ISA.IR
import Isacle.ISA.EncodingDSL (Encoding, Field, runEncoding, fldKey, fieldVal)

-- ---------------------------------------------------------------------------
-- MonadALU
-- The core typeclass for instruction definitions.
-- Parameterised over the backend monad m; the associated types pin the
-- word and address widths for a specific CPU.
-- ---------------------------------------------------------------------------

class Monad m => MonadALU m where
    type AluDef   m :: Type
    type Word     m :: Type
    type DataAddr m :: Type

    -- ------------------------------------------------------------------
    -- ALU definition accessors
    -- Access named elements of the CPU's architectural state.
    -- ------------------------------------------------------------------

    -- | Access any element of the ALU definition by field selector
    cpu      :: (AluDef m -> a) -> m a

    -- | Get a register reference by indexing a register file with an
    -- instruction field. The field name must match a name in the encoding.
    register :: (AluDef m -> CPURegFile count t)
             -> String
             -> m (CPURegister t)

    -- | Like 'register', but adds a compile-time constant to the field value
    -- before forming the register index.  Used for instruction encodings that
    -- address a sub-range of a register file — e.g. AVR upper registers
    -- R16–R31 where the 4-bit 'd' field encodes (Rd − 16).
    -- Default: ignores offset (suitable for documentation backends).
    registerWithOffset :: (AluDef m -> CPURegFile count t)
                       -> String -> Int -> m (CPURegister t)
    registerWithOffset sel field _ = register sel field

    -- | Get a flag reference from the ALU definition
    cpuFlag  :: (AluDef m -> CPUFlag) -> m CPUFlag

    -- | Get a constant/immediate value from the instruction encoding
    immediate :: KnownNat n => String -> m (IExpr n)

    -- ------------------------------------------------------------------
    -- Instruction metadata
    -- Set within each instruction definition body.
    -- ------------------------------------------------------------------

    mnemonic :: String -> m ()
    doc      :: String -> m ()
    encoding :: String -> m ()

    -- ------------------------------------------------------------------
    -- Register operations
    -- ------------------------------------------------------------------

    readReg  :: HdlType t => CPURegister t -> m (IExpr (Width t))
    writeReg :: HdlType t => CPURegister t -> IExpr (Width t) -> m ()

    -- ------------------------------------------------------------------
    -- Data memory operations
    -- Endianness from CPUDef determines byte order for multi-word ops.
    -- ------------------------------------------------------------------

    readMem  :: DataAddr m -> m (Word m)
    writeMem :: DataAddr m -> Word m -> m ()

    -- ------------------------------------------------------------------
    -- Flag operations
    --
    -- Flags are bit-addressed views into status registers.
    -- The low-level primitives (getFlag / setFlag) take an explicit CPUFlag
    -- handle.  The higher-level readFlag / writeFlag take a selector in the
    -- same style as 'cpu', so call sites need no intermediate binding.
    --
    -- readFlag mcsCY   ≡   getFlag =<< cpuFlag mcsCY
    -- writeFlag mcsCY v ≡  cpuFlag mcsCY >>= \f -> setFlag f v
    -- ------------------------------------------------------------------

    getFlag :: CPUFlag -> m (IExpr 1)
    setFlag :: CPUFlag -> IExpr 1 -> m ()

    readFlag :: (AluDef m -> CPUFlag) -> m (IExpr 1)
    readFlag sel = getFlag =<< cpuFlag sel

    writeFlag :: (AluDef m -> CPUFlag) -> IExpr 1 -> m ()
    writeFlag sel v = cpuFlag sel >>= \f -> setFlag f v

    -- | Resize a k-bit value to n bits (zero-extend up, truncate down).
    resizeBits :: (KnownNat k, KnownNat n) => IExpr k -> m (IExpr n)
    resizeBits = pure . tResize

    -- | Sign-extend a k-bit value to n bits (2's complement).
    signExtendBits :: (KnownNat k, KnownNat n) => IExpr k -> m (IExpr n)
    signExtendBits = pure . tSignExt

    -- | Test whether an n-bit value is zero, returning a 1-bit result.
    isZero :: KnownNat n => IExpr n -> m (IExpr 1)
    isZero = pure . tIsZero

    -- | Conditional absolute jump: write target to pcReg only when cond == 1.
    absJumpIf :: HdlType t => CPURegister t -> IExpr 1 -> IExpr (Width t) -> m ()

    -- ------------------------------------------------------------------
    -- ALU operations
    -- ------------------------------------------------------------------

    aluOp :: KnownNat w => ALUPrim -> IExpr w -> IExpr w -> m (IExpr w)
    aluOp PNot a _ = pure (tUn PNot a)
    aluOp op   a b = pure (tBin op a b)

    -- | Produce a hardware constant.
    litC :: KnownNat n => Integer -> m (IExpr n)
    litC = pure . tLit

    -- ------------------------------------------------------------------
    -- Bit-extraction primitives
    -- Backends that do not override these return Lo (synthesis stub).
    -- ------------------------------------------------------------------

    -- | Return the most-significant bit of a word.
    wordMsb :: KnownNat w => IExpr w -> m Bit
    wordMsb _ = return Lo

    -- | Return Hi when the word is zero, Lo otherwise.
    wordIsZero :: KnownNat w => IExpr w -> m Bit
    wordIsZero _ = return Lo

    -- | Add two w-bit values with a carry-in; returns (result, carry-out,
    -- half-carry from bits 3→4). Default stubs the carry bits as Lo.
    addWithFlags :: KnownNat w
                 => IExpr w -> IExpr w -> Bit
                 -> m (IExpr w, Bit, Bit)
    addWithFlags a b _ = do r <- aluOp PAdd a b; return (r, Lo, Lo)

    -- | Subtract @b@ and borrow-in from @a@; returns (result, borrow-out,
    -- half-borrow from bits 3→4). Default stubs the borrow bits as Lo.
    subWithFlags :: KnownNat w
                 => IExpr w -> IExpr w -> Bit
                 -> m (IExpr w, Bit, Bit)
    subWithFlags a b _ = do r <- aluOp PSub a b; return (r, Lo, Lo)

-- ---------------------------------------------------------------------------
-- Register access by record-field projection (C1)
--
-- A scalar register is a real field of the CPU's architectural-state record, so
-- it is reached by /projecting that field/ — never by a string. 'readField' and
-- 'writeField' take the field selector itself and fuse the projection with the
-- read/write; the register's width comes from the field's type:
--
-- > sreg <- readField avrSREG     -- IExpr 8  (the SREG field is CPURegister 8)
-- > writeField avrZ ptr1          -- width = the Z field's width
--
-- (The register file is indexed, so it keeps 'register'/'registerWithOffset'.)
-- ---------------------------------------------------------------------------

-- | Read a register named by a field selector of the core state record.
readField :: (MonadALU m, HdlType t) => (AluDef m -> CPURegister t) -> m (IExpr (Width t))
readField sel = cpu sel >>= readReg

-- | Write a register named by a field selector of the core state record.
writeField :: (MonadALU m, HdlType t) => (AluDef m -> CPURegister t) -> IExpr (Width t) -> m ()
writeField sel e = cpu sel >>= \r -> writeReg r e

-- | Read a register-file slot: the file field selector + the encoding field that
-- indexes it. No handle is exposed to the instruction.
readRegFile :: (MonadALU m, HdlType t)
            => (AluDef m -> CPURegFile count t) -> String -> m (IExpr (Width t))
readRegFile sel field = register sel field >>= readReg

-- | Write a register-file slot (file selector + index field).
writeRegFile :: (MonadALU m, HdlType t)
             => (AluDef m -> CPURegFile count t) -> String -> IExpr (Width t) -> m ()
writeRegFile sel field e = register sel field >>= \r -> writeReg r e

-- | 'readRegFile' with a compile-time index offset (e.g. AVR R16–R31).
readRegFileOffset :: (MonadALU m, HdlType t)
                  => (AluDef m -> CPURegFile count t) -> String -> Int -> m (IExpr (Width t))
readRegFileOffset sel field off = registerWithOffset sel field off >>= readReg

-- | 'writeRegFile' with a compile-time index offset.
writeRegFileOffset :: (MonadALU m, HdlType t)
                   => (AluDef m -> CPURegFile count t) -> String -> Int -> IExpr (Width t) -> m ()
writeRegFileOffset sel field off e = registerWithOffset sel field off >>= \r -> writeReg r e

-- ---------------------------------------------------------------------------
-- Encoding-DSL front end (typed field placeholders)
-- ---------------------------------------------------------------------------

-- | Build an instruction's encoding from fixed bits and typed field
-- placeholders, lifting the result (the placeholders) into the instruction
-- monad and setting the encoding. See "Isacle.ISA.EncodingDSL".
defineInstruction :: MonadALU m => Encoding a -> m a
defineInstruction enc = let (a, str) = runEncoding enc in encoding str >> pure a

-- | Read a register-file slot indexed by a field /placeholder/ — no string
-- field name, no exposed handle. The placeholder carries the decoded index.
readRegFileF :: (MonadALU m, HdlType t)
             => (AluDef m -> CPURegFile count t) -> Field idx -> m (IExpr (Width t))
readRegFileF sel f = register sel [fldKey f] >>= readReg

-- | Write a register-file slot indexed by a field placeholder.
writeRegFileF :: (MonadALU m, HdlType t)
              => (AluDef m -> CPURegFile count t) -> Field idx -> IExpr (Width t) -> m ()
writeRegFileF sel f e = register sel [fldKey f] >>= \r -> writeReg r e

-- | 'readRegFileF' with a compile-time index offset (e.g. AVR upper regs R16–R31).
readRegFileFOffset :: (MonadALU m, HdlType t)
                   => (AluDef m -> CPURegFile count t) -> Field idx -> Int -> m (IExpr (Width t))
readRegFileFOffset sel f off = registerWithOffset sel [fldKey f] off >>= readReg

-- | 'writeRegFileF' with a compile-time index offset.
writeRegFileFOffset :: (MonadALU m, HdlType t)
                    => (AluDef m -> CPURegFile count t) -> Field idx -> Int -> IExpr (Width t) -> m ()
writeRegFileFOffset sel f off e = registerWithOffset sel [fldKey f] off >>= \r -> writeReg r e

-- | Read a field placeholder as a width-typed value (re-exported convenience).
immediateF :: KnownNat (Width t) => Field t -> IExpr (Width t)
immediateF = fieldVal

-- | Conditional absolute jump by register field selector (no handle exposed):
-- writes @target@ to the selected register only when @cond@ is 1.
absJumpIfF :: (MonadALU m, HdlType t)
           => (AluDef m -> CPURegister t) -> IExpr 1 -> IExpr (Width t) -> m ()
absJumpIfF sel cond target = cpu sel >>= \r -> absJumpIf r cond target

-- ---------------------------------------------------------------------------
-- MonadHarvardALU
-- Extends MonadALU with a separate read-only code address space.
-- The code word width and address width are independent of the data side.
-- ---------------------------------------------------------------------------

class MonadALU m => MonadHarvardALU m where
    type CodeAddr m :: Type
    type CodeWord m :: Type

    readCode :: CodeAddr m -> m (CodeWord m)

-- ---------------------------------------------------------------------------
-- MonadIRQ
-- Extends MonadALU with interrupt-vector access and interrupt gating.
-- Separate from MonadALU so ISAs without IRQ support carry no overhead.
-- ---------------------------------------------------------------------------

class MonadALU m => MonadIRQ m where
    -- | Bit width of the interrupt vector address.
    -- For Harvard architectures this equals the code-address width (pcW);
    -- for Von Neumann it equals the data-address width.
    -- Exposing the width as a Nat lets callers use 'resizeBits' to coerce the
    -- vector into the PC register width without an extra constraint.
    type IrqAddrW m :: Nat

    -- | Return the externally-supplied interrupt vector address.
    -- Wired to an @irq_vector@ input port in hardware so an interrupt
    -- controller outside the core can drive it.
    irqVector :: m (IExpr (IrqAddrW m))

    -- | Condition all subsequent writes in the current do-block on the given
    -- 1-bit signal, similar to Haskell's @guard@ but for hardware writes.
    -- In synthesis the condition is ANDed with the current match wire; in
    -- simulation the body is skipped when the condition evaluates to 0.
    irqGate :: m (IExpr 1) -> m ()

-- ---------------------------------------------------------------------------
-- Common helpers
-- Reusable building blocks for instruction definitions.
-- ---------------------------------------------------------------------------

-- | Relative jump: add a signed offset to the current PC
relJump :: (MonadALU m, HdlType t)
        => CPURegister t -> IExpr (Width t) -> m ()
relJump pcReg offset = do
    current <- readReg pcReg
    writeReg pcReg (current + offset)

-- | Absolute jump: load a new value directly into the PC
absJump :: (MonadALU m, HdlType t)
        => CPURegister t -> IExpr (Width t) -> m ()
absJump pcReg target = writeReg pcReg target

-- | Push a word onto the stack.
-- Reads the SP, writes to mem[SP], decrements SP.
-- Byte order for multi-word values determined by CPUDef endianness.
push :: (MonadALU m, HdlType t, DataAddr m ~ IExpr (Width t))
     => CPURegister t -> Word m -> m ()
push spReg val = do
    sp <- readReg spReg
    writeMem (bitCoerce sp) val
    writeReg spReg (sp - 1)

-- | Pop a word from the stack.
-- Increments SP, reads from mem[SP].
pop :: (MonadALU m, HdlType t, DataAddr m ~ IExpr (Width t))
    => CPURegister t -> m (Word m)
pop spReg = do
    sp <- readReg spReg
    let sp' = sp + 1
    writeReg spReg sp'
    readMem (bitCoerce sp')

-- ---------------------------------------------------------------------------
-- Indirect addressing mode helpers
-- ---------------------------------------------------------------------------

-- | Read via a register used as a data pointer
indirectRead :: (MonadALU m, HdlType t, DataAddr m ~ IExpr (Width t))
             => CPURegister t -> m (Word m)
indirectRead ptrReg = readReg ptrReg >>= readMem . bitCoerce

-- | Write via a register used as a data pointer
indirectWrite :: (MonadALU m, HdlType t, DataAddr m ~ IExpr (Width t))
              => CPURegister t -> Word m -> m ()
indirectWrite ptrReg val = do
    ptr <- readReg ptrReg
    writeMem (bitCoerce ptr) val

-- | Read with post-increment
indirectReadPostInc :: (MonadALU m, HdlType t, DataAddr m ~ IExpr (Width t))
                    => CPURegister t -> m (Word m)
indirectReadPostInc ptrReg = do
    ptr <- readReg ptrReg
    writeReg ptrReg (ptr + 1)
    readMem (bitCoerce ptr)

-- | Read with pre-decrement
indirectReadPreDec :: (MonadALU m, HdlType t, DataAddr m ~ IExpr (Width t))
                   => CPURegister t -> m (Word m)
indirectReadPreDec ptrReg = do
    ptr <- readReg ptrReg
    let ptr' = ptr - 1
    writeReg ptrReg ptr'
    readMem (bitCoerce ptr')

-- | Read with constant offset
indirectReadOffset :: (MonadALU m, HdlType t, DataAddr m ~ IExpr (Width t))
                   => CPURegister t -> IExpr (Width t) -> m (Word m)
indirectReadOffset ptrReg offset = do
    ptr <- readReg ptrReg
    readMem (bitCoerce (ptr + offset))

