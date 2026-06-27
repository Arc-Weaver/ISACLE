{-# LANGUAGE TypeApplications #-}
-- | TinyVN — a minimal 32-bit von Neumann CPU for synthesis smoke-testing.
--
-- Architecture:
--   * 8 × 32-bit general-purpose registers (r0–r7)
--   * 32-bit program counter (PC), word-addressed
--   * 1 flag: Z (zero from last ALU result)
--   * 32-bit instruction word, 32-bit unified address space
--
-- Instruction set (32-bit encoding):
--
-- @
--   NOP               0x0000_0000
--   ADD  rd, rs       0x0100_0000 | (rs << 16) | (rd << 8)
--   LD   rd, ra       0x0200_0000 | (ra << 16) | (rd << 8)   -- rd = mem[ra]
--   ST   ra, rs       0x0300_0000 | (rs << 16) | (ra << 8)   -- mem[ra] = rs
--   ADDI rd, #imm8    0x0400_0000 | (rd << 16) | (imm8)      -- rd = rd + imm8
--   BEQ  k            0x0500_0000 | k[23:0]                   -- if Z: PC = k
--   JMP  k            0x0600_0000 | k[23:0]                   -- PC = k
-- @
--
-- This ISA uses only 'MonadALU' — no code bus, no Harvard-specific extensions.
-- It is synthesised with 'synthVonNeumannCPU'' and tested with
-- 'execSystemDSL' via 'createCachedCPU'.
module Isacle.ISA.Example.TinyVN
    ( TinyVnAlu(..)
    , tinyVnCPUDef
    , tinyVnISA
    ) where

import Prelude hiding (Word)
import Hdl.Bits hiding (zeroExtend, signExtend, truncateB, bitCoerce)
import Isacle.ISA

-- ---------------------------------------------------------------------------
-- ALU record
-- ---------------------------------------------------------------------------

data TinyVnAlu = TinyVnAlu
    { tvGpr  :: CPURegFile 8 (Unsigned 32)
    , tvPc   :: CPURegister (Unsigned 32)
    , tvZero :: CPUFlag
    }

-- ---------------------------------------------------------------------------
-- CPU definition
-- ---------------------------------------------------------------------------

tinyVnCPUDef :: CPUDef TinyVnAlu
tinyVnCPUDef = do
    endianness LittleEndian
    g          <- newRegFile "GPR"
    p          <- newReg "PC"
    (_, zs)    <- flagPack @1 "FLAGS" ["Z"]
    let z       = head zs
    pure TinyVnAlu { tvGpr = g, tvPc = p, tvZero = z }

-- ---------------------------------------------------------------------------
-- Instruction bodies
-- ---------------------------------------------------------------------------

-- | NOP: 0x00000000
tvNop :: (MonadALU m, AluDef m ~ TinyVnAlu) => m ()
tvNop = do
    mnemonic "NOP"
    doc      "No operation"
    encoding "00000000000000000000000000000000"

-- | ADD rd, rs: opcode=0x01, rs[2:0] in bits[18:16], rd[2:0] in bits[10:8]
tvAdd :: (MonadALU m, AluDef m ~ TinyVnAlu) => m ()
tvAdd = do
    mnemonic "ADD"
    doc      "Add registers: rd = rd + rs"
    encoding "00000001000000rrr000000ddd000000"
    rd <- register tvGpr "ddd"
    rs <- register tvGpr "rrr"
    zf <- cpuFlag tvZero
    a  <- readReg rd
    b  <- readReg rs
    r  <- aluOp PAdd a b
    writeReg rd r
    z  <- isZero r
    setFlag zf z

-- | LD rd, ra: load rd from mem[ra]
tvLd :: (MonadALU m, AluDef m ~ TinyVnAlu, DataAddr m ~ IExpr 32, Word m ~ IExpr 32) => m ()
tvLd = do
    mnemonic "LD"
    doc      "Load: rd = mem[ra]"
    encoding "00000010000000rrr000000ddd000000"
    rd <- register tvGpr "ddd"
    ra <- register tvGpr "rrr"
    addr <- readReg ra
    val  <- readMem addr
    writeReg rd val

-- | ST ra, rs: store rs to mem[ra]
tvSt :: (MonadALU m, AluDef m ~ TinyVnAlu, DataAddr m ~ IExpr 32, Word m ~ IExpr 32) => m ()
tvSt = do
    mnemonic "ST"
    doc      "Store: mem[ra] = rs"
    encoding "00000011000000rrr000000aaa000000"
    rs <- register tvGpr "rrr"
    ra <- register tvGpr "aaa"
    addr <- readReg ra
    val  <- readReg rs
    writeMem addr val

-- | ADDI rd, #imm8: rd = rd + zero-extended 8-bit immediate
tvAddi :: (MonadALU m, AluDef m ~ TinyVnAlu, Word m ~ IExpr 32) => m ()
tvAddi = do
    mnemonic "ADDI"
    doc      "Add immediate: rd = rd + imm8"
    encoding "00000100000000ddd00000000iiiiiiii"
    rd  <- register tvGpr "ddd"
    imm <- immediate "iiiiiiii"
    a   <- readReg rd
    r   <- aluOp PAdd a imm
    writeReg rd r

-- | BEQ k: if Z then PC = k (24-bit absolute target)
tvBeq :: (MonadALU m, AluDef m ~ TinyVnAlu, Word m ~ IExpr 32) => m ()
tvBeq = do
    mnemonic "BEQ"
    doc      "Branch if zero: if Z then PC = k"
    encoding "00000101kkkkkkkkkkkkkkkkkkkkkkkk"
    p  <- cpu tvPc
    zf <- cpuFlag tvZero
    z  <- getFlag zf
    k  <- immediate "kkkkkkkk"
    absJumpIf p z k

-- | JMP k: PC = k (24-bit absolute target)
tvJmp :: (MonadALU m, AluDef m ~ TinyVnAlu, Word m ~ IExpr 32) => m ()
tvJmp = do
    mnemonic "JMP"
    doc      "Absolute jump: PC = k"
    encoding "00000110kkkkkkkkkkkkkkkkkkkkkkkk"
    p <- cpu tvPc
    k <- immediate "kkkkkkkk"
    absJump p k

-- ---------------------------------------------------------------------------
-- ISA definition
-- ---------------------------------------------------------------------------

tinyVnISA :: ( MonadALU m
             , AluDef m ~ TinyVnAlu
             , Word m ~ IExpr 32
             , DataAddr m ~ IExpr 32
             ) => ISADef m
tinyVnISA = defineISA ISADef
    { isaPc            = SomeCPURegister <$> cpu tvPc
    , isaInterruptBody = Nothing
    , isaReset         = do
        resetReg  tvPc   0x00000000
        resetFlag tvZero Lo
    , isaInstrs =
        [ tvNop
        , tvAdd
        , tvLd
        , tvSt
        , tvAddi
        , tvBeq
        , tvJmp
        ]
    }
