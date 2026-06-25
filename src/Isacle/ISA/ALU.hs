{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Isacle.ISA.ALU where

import Prelude hiding (Word)
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (natVal)
import Hdl.Bits
import Isacle.ISA.Types

-- ---------------------------------------------------------------------------
-- ContextItem
-- Items saved to the stack on interrupt or subroutine call.
-- Any register width is allowed; the backend handles packing/splitting
-- into data-space words according to the CPU's endianness.
-- ---------------------------------------------------------------------------

data ContextItem m = forall w. KnownNat w => SaveWord (m (Unsigned w))

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
    register :: (AluDef m -> CPURegFile count w)
             -> String
             -> m (CPURegister w)

    -- | Like 'register', but adds a compile-time constant to the field value
    -- before forming the register index.  Used for instruction encodings that
    -- address a sub-range of a register file — e.g. AVR upper registers
    -- R16–R31 where the 4-bit 'd' field encodes (Rd − 16).
    -- Default: ignores offset (suitable for documentation backends).
    registerWithOffset :: (AluDef m -> CPURegFile count w)
                       -> String -> Int -> m (CPURegister w)
    registerWithOffset sel field _ = register sel field

    -- | Get a flag reference from the ALU definition
    cpuFlag  :: (AluDef m -> CPUFlag) -> m CPUFlag

    -- | Get a constant/immediate value from the instruction encoding
    immediate :: String -> m (Unsigned n)

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

    readReg  :: KnownNat w => CPURegister w -> m (Unsigned w)
    writeReg :: KnownNat w => CPURegister w -> Unsigned w -> m ()

    -- ------------------------------------------------------------------
    -- Data memory operations
    -- Endianness from CPUDef determines byte order for multi-word ops.
    -- ------------------------------------------------------------------

    readMem  :: DataAddr m -> m (Word m)
    writeMem :: DataAddr m -> Word m -> m ()

    -- ------------------------------------------------------------------
    -- Flag operations
    -- ------------------------------------------------------------------

    getFlag :: CPUFlag -> m (Unsigned 1)
    setFlag :: CPUFlag -> Unsigned 1 -> m ()

    -- | Sign-extend a k-bit value to n bits, interpreting the source as 2's
    -- complement signed.  Default uses Haskell signed wrapping (correct for
    -- simulation); the synthesis backend overrides this to emit PSignedResize.
    signExtendBits :: forall k n. (KnownNat k, KnownNat n) => Unsigned k -> m (Unsigned n)
    signExtendBits v =
        let w   = fromIntegral (natVal (Proxy @k)) :: Int
            raw = toInteger v
            sgn = raw >= (1 `shiftL` (w - 1))
            ext = if sgn then raw - (1 `shiftL` w) else raw
        in pure (fromInteger ext)

    -- | Test whether an n-bit value is zero, returning a 1-bit result.
    isZero :: KnownNat n => Unsigned n -> m (Unsigned 1)

    -- | Conditional absolute jump: write target to pcReg only when cond == 1.
    absJumpIf :: KnownNat w => CPURegister w -> Unsigned 1 -> Unsigned w -> m ()

    -- ------------------------------------------------------------------
    -- ALU operations
    -- ------------------------------------------------------------------

    aluOp :: KnownNat w => ALUPrim -> Unsigned w -> Unsigned w -> m (Unsigned w)

    -- | Produce a hardware constant.  The default is a pure 'fromInteger'
    -- (correct for simulation); the SynthM backend overrides this to emit a
    -- 'PLit' NetNode so the constant does not alias a live signal wire.
    litC :: KnownNat n => Integer -> m (Unsigned n)
    litC v = pure (fromInteger v)

    -- ------------------------------------------------------------------
    -- Bit-extraction primitives
    -- Backends that do not override these return Lo (synthesis stub).
    -- ------------------------------------------------------------------

    -- | Return the most-significant bit of a word.
    wordMsb :: KnownNat w => Unsigned w -> m Bit
    wordMsb _ = return Lo

    -- | Return Hi when the word is zero, Lo otherwise.
    wordIsZero :: KnownNat w => Unsigned w -> m Bit
    wordIsZero _ = return Lo

    -- | Add two w-bit values with a carry-in; returns (result, carry-out,
    -- half-carry from bits 3→4). Default stubs the carry bits as Lo.
    addWithFlags :: KnownNat w
                 => Unsigned w -> Unsigned w -> Bit
                 -> m (Unsigned w, Bit, Bit)
    addWithFlags a b _ = do r <- aluOp PAdd a b; return (r, Lo, Lo)

    -- | Subtract @b@ and borrow-in from @a@; returns (result, borrow-out,
    -- half-borrow from bits 3→4). Default stubs the borrow bits as Lo.
    subWithFlags :: KnownNat w
                 => Unsigned w -> Unsigned w -> Bit
                 -> m (Unsigned w, Bit, Bit)
    subWithFlags a b _ = do r <- aluOp PSub a b; return (r, Lo, Lo)

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
-- Common helpers
-- Reusable building blocks for instruction definitions.
-- ---------------------------------------------------------------------------

-- | Relative jump: add a signed offset to the current PC
relJump :: (MonadALU m, KnownNat w)
        => CPURegister w -> Signed w -> m ()
relJump pcReg offset = do
    current <- readReg pcReg
    writeReg pcReg (current + bitCoerce offset)

-- | Absolute jump: load a new value directly into the PC
absJump :: (MonadALU m, KnownNat w)
        => CPURegister w -> Unsigned w -> m ()
absJump pcReg target = writeReg pcReg target

-- | Push a word onto the stack.
-- Reads the SP, writes to mem[SP], decrements SP.
-- Byte order for multi-word values determined by CPUDef endianness.
push :: (MonadALU m, DataAddr m ~ Unsigned spWidth, KnownNat spWidth)
     => CPURegister spWidth -> Word m -> m ()
push spReg val = do
    sp <- readReg spReg
    writeMem (bitCoerce sp) val
    writeReg spReg (sp - 1)

-- | Pop a word from the stack.
-- Increments SP, reads from mem[SP].
pop :: (MonadALU m, DataAddr m ~ Unsigned spWidth, KnownNat spWidth)
    => CPURegister spWidth -> m (Word m)
pop spReg = do
    sp <- readReg spReg
    let sp' = sp + 1
    writeReg spReg sp'
    readMem (bitCoerce sp')

-- ---------------------------------------------------------------------------
-- Indirect addressing mode helpers
-- ---------------------------------------------------------------------------

-- | Read via a register used as a data pointer
indirectRead :: (MonadALU m, DataAddr m ~ Unsigned addrWidth, KnownNat addrWidth)
             => CPURegister addrWidth -> m (Word m)
indirectRead ptrReg = readReg ptrReg >>= readMem . bitCoerce

-- | Write via a register used as a data pointer
indirectWrite :: (MonadALU m, DataAddr m ~ Unsigned addrWidth, KnownNat addrWidth)
              => CPURegister addrWidth -> Word m -> m ()
indirectWrite ptrReg val = do
    ptr <- readReg ptrReg
    writeMem (bitCoerce ptr) val

-- | Read with post-increment
indirectReadPostInc :: (MonadALU m, DataAddr m ~ Unsigned addrWidth, KnownNat addrWidth)
                    => CPURegister addrWidth -> m (Word m)
indirectReadPostInc ptrReg = do
    ptr <- readReg ptrReg
    writeReg ptrReg (ptr + 1)
    readMem (bitCoerce ptr)

-- | Read with pre-decrement
indirectReadPreDec :: (MonadALU m, DataAddr m ~ Unsigned addrWidth, KnownNat addrWidth)
                   => CPURegister addrWidth -> m (Word m)
indirectReadPreDec ptrReg = do
    ptr <- readReg ptrReg
    let ptr' = ptr - 1
    writeReg ptrReg ptr'
    readMem (bitCoerce ptr')

-- | Read with constant offset
indirectReadOffset :: (MonadALU m, DataAddr m ~ Unsigned addrWidth, KnownNat addrWidth)
                   => CPURegister addrWidth -> Unsigned addrWidth -> m (Word m)
indirectReadOffset ptrReg offset = do
    ptr <- readReg ptrReg
    readMem (bitCoerce (ptr + offset))

-- ---------------------------------------------------------------------------
-- Context save helpers
-- Build ContextItem values for isaContextSave lists.
-- ---------------------------------------------------------------------------

-- | Save a register into the context. Any register width is valid;
-- the backend packs it into data-space words using the CPU's endianness.
saveWordReg :: (MonadALU m, KnownNat w) => (AluDef m -> CPURegister w) -> ContextItem m
saveWordReg sel = SaveWord (readReg =<< cpu sel)
