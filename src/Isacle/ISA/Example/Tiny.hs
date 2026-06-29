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
import Hdl.Bits hiding (zeroExtend, signExtend, truncateB, bitCoerce, add, mul)
import Isacle.ISA

-- ---------------------------------------------------------------------------
-- ALU record
-- ---------------------------------------------------------------------------

data TinyAlu = TinyAlu
    { gpr  :: CPURegFile 4 (Unsigned 8)
    , pc   :: CPURegister (Unsigned 8)
    , zero :: CPUFlag   -- bit 0 of the 1-bit "FLAGS" status register
    }

-- ---------------------------------------------------------------------------
-- CPU definition
-- ---------------------------------------------------------------------------

tinyCPUDef :: CPUDef TinyAlu
tinyCPUDef = do
    endianness LittleEndian
    g      <- newRegFile "GPR"
    p      <- newReg "PC"
    (_, zs)  <- flagPack @1 "FLAGS" ["Z"]
    let z    =  head zs
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
    let r = a + b
    writeReg rd r
    z  <- isZero r
    setFlag zf z

movDef :: (MonadALU m, AluDef m ~ TinyAlu) => m ()
movDef = do
    mnemonic "MOV"
    doc      "Copy register: rd = rs"
    encoding "0010ssdd"
    rd <- register gpr "dd"
    rs <- register gpr "ss"
    v  <- readReg rs
    writeReg rd v

-- | LDI rd, #n — load 4-bit immediate into register.
-- Encoding: 01rrnnnn  (rr = dest register bits[5:4], nnnn = value bits[3:0])
ldiDef :: (MonadALU m, AluDef m ~ TinyAlu, Word m ~ IExpr (Unsigned 8)) => m ()
ldiDef = do
    mnemonic "LDI"
    doc      "Load immediate: rd = #n (4-bit, 0–15)"
    encoding "01rrnnnn"
    rd <- register gpr "rr"
    n  <- immediate "nnnn"
    writeReg rd n

-- | ST [rd], rs — store register rs to data memory at address in rd.
-- Encoding: 0011ssdd  (ss = data register, dd = address register)
stDef :: (MonadALU m, AluDef m ~ TinyAlu, DataAddr m ~ IExpr (Unsigned 8), Word m ~ IExpr (Unsigned 8)) => m ()
stDef = do
    mnemonic "ST"
    doc      "Store to memory: mem[rd] = rs"
    encoding "0011ssdd"
    rd   <- register gpr "dd"
    rs   <- register gpr "ss"
    addr <- readReg rd
    val  <- readReg rs
    writeMem addr val

-- | BRZ k — branch if Z=1 (last ADD result was zero).
-- Encoding: 100kkkkk  (5-bit target, 0–31)
brzDef :: (MonadALU m, AluDef m ~ TinyAlu, Word m ~ IExpr (Unsigned 8)) => m ()
brzDef = do
    mnemonic "BRZ"
    doc      "Branch if zero flag set: if Z then PC = k"
    encoding "100kkkkk"
    p  <- cpu pc
    zf <- cpuFlag zero
    z  <- getFlag zf
    k  <- immediate "kkkkk"
    absJumpIf p z k

-- | BRNZ k — branch if Z=0 (last ADD result was nonzero).
-- Encoding: 101kkkkk  (5-bit target, 0–31)
brnzDef :: (MonadALU m, AluDef m ~ TinyAlu, Word m ~ IExpr (Unsigned 8)) => m ()
brnzDef = do
    mnemonic "BRNZ"
    doc      "Branch if zero flag clear: if not Z then PC = k"
    encoding "101kkkkk"
    p  <- cpu pc
    zf <- cpuFlag zero
    z  <- getFlag zf
    k  <- immediate "kkkkk"
    -- Take branch when z == 0 (zero flag is clear)
    nz <- isZero z
    absJumpIf p nz k

-- | JMP encodes the target as a 6-bit immediate in bits [5:0].
-- Requires Word m ~ IExpr (Unsigned 8) so the immediate can be passed to absJump.
jmpDef :: (MonadALU m, AluDef m ~ TinyAlu, Word m ~ IExpr (Unsigned 8)) => m ()
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

tinyISA :: (MonadALU m, AluDef m ~ TinyAlu, Word m ~ IExpr (Unsigned 8), DataAddr m ~ IExpr (Unsigned 8)) => ISADef m
tinyISA = defineISA ISADef
    { isaPc            = SomeCPURegister <$> cpu pc
    , isaInterruptBody = Nothing
    , isaReset         = do
        resetReg  pc   0x00
        resetFlag zero Lo
    , isaInstrs       =
        [ nopDef
        , addDef
        , movDef
        , stDef
        , ldiDef
        , brzDef
        , brnzDef
        , jmpDef
        ]
    }
