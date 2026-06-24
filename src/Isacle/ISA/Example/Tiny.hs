{-# LANGUAGE TypeApplications #-}
-- | TinyCPU — a minimal 8-bit Harvard CPU for synthesis smoke-testing.
--
-- Architecture:
--   * 4 × 8-bit general-purpose registers (GPR[0..3])
--   * 8-bit program counter (PC)
--   * 1 flag: Z (zero)
--   * 8-bit instruction word, 8-bit code/data addresses
--
-- Instruction set (8-bit encoding, MSB first):
--
-- @
--   NOP           00000000          — no operation
--   ADD  rd, rs   0001 ss dd        — rd = rd + rs
--   MOV  rd, rs   0010 ss dd        — rd = rs
--   JMP  k        11 kk kkkk        — PC = k  (6-bit absolute target)
-- @
--
-- dd = 2-bit dest register index, ss = 2-bit src register index.
module Isacle.ISA.Example.Tiny
    ( TinyAlu(..)
    , tinyCPUDef
    , tinyISA
    ) where

import Prelude hiding (Word)
import Hdl.Bits
import Isacle.ISA

-- ---------------------------------------------------------------------------
-- ALU record
-- ---------------------------------------------------------------------------

data TinyAlu = TinyAlu
    { gpr  :: CPURegFile 4 8
    , pc   :: CPURegister 8
    , zero :: CPUFlag
    }

-- ---------------------------------------------------------------------------
-- CPU definition
-- ---------------------------------------------------------------------------

tinyCPUDef :: CPUDef TinyAlu
tinyCPUDef = do
    endianness LittleEndian
    g <- regFile "GPR" (width @4) byte
    p <- reg    "PC"  byte
    z <- flag   "Z"
    pure TinyAlu { gpr = g, pc = p, zero = z }

-- ---------------------------------------------------------------------------
-- Instruction bodies
-- ---------------------------------------------------------------------------

nopDef :: (MonadALU m, AluDef m ~ TinyAlu) => m ()
nopDef = do
    mnemonic "NOP"
    doc      "No operation"
    encoding "00000000"

addDef :: (MonadALU m, AluDef m ~ TinyAlu) => m ()
addDef = do
    mnemonic "ADD"
    doc      "Add two registers: rd = rd + rs"
    encoding "0001ssdd"
    rd <- register gpr "dd"
    rs <- register gpr "ss"
    zf <- cpuFlag zero
    a  <- readReg rd
    b  <- readReg rs
    r  <- aluOp PAdd a b
    writeReg rd r
    -- Signal-level zero detection requires a future typeclass extension;
    -- constant Lo is a synthesis placeholder.
    setFlag zf Lo

movDef :: (MonadALU m, AluDef m ~ TinyAlu) => m ()
movDef = do
    mnemonic "MOV"
    doc      "Copy register: rd = rs"
    encoding "0010ssdd"
    rd <- register gpr "dd"
    rs <- register gpr "ss"
    v  <- readReg rs
    writeReg rd v

-- | JMP encodes the target as a 6-bit immediate in bits [5:0].
-- Requires Word m ~ Unsigned 8 so the immediate can be passed to absJump.
jmpDef :: (MonadALU m, AluDef m ~ TinyAlu, Word m ~ Unsigned 8) => m ()
jmpDef = do
    mnemonic "JMP"
    doc      "Absolute jump: PC = k"
    encoding "11kkkkkk"
    p <- cpu pc
    k <- immediate "kkkkkk"
    absJump p k

-- ---------------------------------------------------------------------------
-- ISA definition
-- ---------------------------------------------------------------------------

tinyISA :: (MonadALU m, AluDef m ~ TinyAlu, Word m ~ Unsigned 8) => ISADef m
tinyISA = defineISA ISADef
    { isaPc           = SomeCPURegister <$> cpu pc
    , isaInterruptEn  = cpuFlag zero     -- no interrupts; Z used as placeholder
    , isaInterruptVec = SomeCPURegister <$> cpu pc
    , isaSupervisor   = Nothing
    , isaContextSave  = []
    , isaReset        = do
        resetReg  pc   0x00
        resetFlag zero Lo
    , isaInstrs       =
        [ nopDef
        , addDef
        , movDef
        , jmpDef
        ]
    }
