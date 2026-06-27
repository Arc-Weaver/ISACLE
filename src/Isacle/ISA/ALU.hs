{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Isacle.ISA.ALU where

import Prelude hiding (Word)
import Data.Char (toUpper)
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import GHC.Records (HasField)
import GHC.TypeLits (KnownNat, Nat, Symbol, KnownSymbol, symbolVal)
import Hdl.Bits (Bit(..))
import Hdl.Types (HdlType, Width)
import Isacle.ISA.Types
import Isacle.ISA.IR

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

    readReg  :: KnownNat w => CPURegister w -> m (IExpr w)
    writeReg :: KnownNat w => CPURegister w -> IExpr w -> m ()

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
    absJumpIf :: KnownNat w => CPURegister w -> IExpr 1 -> IExpr w -> m ()

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
-- Typed register access (C1)
--
-- A CPU's ALU /handle/ record (e.g. @AVRALU pcW@) stands for a typed
-- architectural-state record (e.g. @AvrState pcW@). Declaring
-- @type instance CoreState (AVRALU pcW) = AvrState pcW@ lets instruction bodies
-- read/write a scalar register by its state field name, with the width taken
-- from the field's type — no hand-written width, no loose string key:
--
-- > sreg <- readField @"sreg"     -- IExpr (Width Sreg)
-- > writeField @"pc" newPc        -- width = the pc field's width
--
-- The register key is the field name upper-cased, matching the schema the
-- 'Isacle.ISA.CPUDef.CPUDef' declares.
-- ---------------------------------------------------------------------------

-- | The typed architectural-state record an ALU handle record stands for.
type family CoreState (alu :: Type) :: Type

-- | A register key from a state field name: the name, upper-cased.
fieldKeyU :: forall (name :: Symbol). KnownSymbol name => String
fieldKeyU = map toUpper (symbolVal (Proxy @name))

-- | Read a scalar register by its 'CoreState' field name; width = the field's.
readField :: forall (name :: Symbol) a m.
    ( KnownSymbol name, MonadALU m
    , HasField name (CoreState (AluDef m)) a
    , HdlType a, KnownNat (Width a) )
    => m (IExpr (Width a))
readField = readReg (CPURegister (fieldKeyU @name) :: CPURegister (Width a))

-- | Write a scalar register by its 'CoreState' field name; width = the field's.
writeField :: forall (name :: Symbol) a m.
    ( KnownSymbol name, MonadALU m
    , HasField name (CoreState (AluDef m)) a
    , HdlType a, KnownNat (Width a) )
    => IExpr (Width a) -> m ()
writeField = writeReg (CPURegister (fieldKeyU @name) :: CPURegister (Width a))

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
relJump :: (MonadALU m, KnownNat w)
        => CPURegister w -> IExpr w -> m ()
relJump pcReg offset = do
    current <- readReg pcReg
    writeReg pcReg (current + offset)

-- | Absolute jump: load a new value directly into the PC
absJump :: (MonadALU m, KnownNat w)
        => CPURegister w -> IExpr w -> m ()
absJump pcReg target = writeReg pcReg target

-- | Push a word onto the stack.
-- Reads the SP, writes to mem[SP], decrements SP.
-- Byte order for multi-word values determined by CPUDef endianness.
push :: (MonadALU m, DataAddr m ~ IExpr spWidth, KnownNat spWidth)
     => CPURegister spWidth -> Word m -> m ()
push spReg val = do
    sp <- readReg spReg
    writeMem (bitCoerce sp) val
    writeReg spReg (sp - 1)

-- | Pop a word from the stack.
-- Increments SP, reads from mem[SP].
pop :: (MonadALU m, DataAddr m ~ IExpr spWidth, KnownNat spWidth)
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
indirectRead :: (MonadALU m, DataAddr m ~ IExpr addrWidth, KnownNat addrWidth)
             => CPURegister addrWidth -> m (Word m)
indirectRead ptrReg = readReg ptrReg >>= readMem . bitCoerce

-- | Write via a register used as a data pointer
indirectWrite :: (MonadALU m, DataAddr m ~ IExpr addrWidth, KnownNat addrWidth)
              => CPURegister addrWidth -> Word m -> m ()
indirectWrite ptrReg val = do
    ptr <- readReg ptrReg
    writeMem (bitCoerce ptr) val

-- | Read with post-increment
indirectReadPostInc :: (MonadALU m, DataAddr m ~ IExpr addrWidth, KnownNat addrWidth)
                    => CPURegister addrWidth -> m (Word m)
indirectReadPostInc ptrReg = do
    ptr <- readReg ptrReg
    writeReg ptrReg (ptr + 1)
    readMem (bitCoerce ptr)

-- | Read with pre-decrement
indirectReadPreDec :: (MonadALU m, DataAddr m ~ IExpr addrWidth, KnownNat addrWidth)
                   => CPURegister addrWidth -> m (Word m)
indirectReadPreDec ptrReg = do
    ptr <- readReg ptrReg
    let ptr' = ptr - 1
    writeReg ptrReg ptr'
    readMem (bitCoerce ptr')

-- | Read with constant offset
indirectReadOffset :: (MonadALU m, DataAddr m ~ IExpr addrWidth, KnownNat addrWidth)
                   => CPURegister addrWidth -> IExpr addrWidth -> m (Word m)
indirectReadOffset ptrReg offset = do
    ptr <- readReg ptrReg
    readMem (bitCoerce (ptr + offset))

