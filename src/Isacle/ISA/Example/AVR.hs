-- | Skeleton AVR CPU definition — illustrates the ISA DSL.
-- Not a complete implementation.
{-# LANGUAGE TypeApplications #-}
module Isacle.ISA.Example.AVR where

import Prelude hiding (Word)
import Hdl.Bits hiding ((!!), zeroExtend, signExtend, truncateB, bitCoerce, slice)
import Isacle.ISA

-- ---------------------------------------------------------------------------
-- AVR ALU definition
-- ---------------------------------------------------------------------------

data AVRAlu = AVRAlu
    { gpr       :: CPURegFile 32 8
    , sp        :: CPURegister 16
    , pc        :: CPURegister 22
    , carry     :: CPUFlag
    , zero      :: CPUFlag
    , negative  :: CPUFlag
    , overflow  :: CPUFlag
    , sign      :: CPUFlag
    , halfCarry :: CPUFlag
    , bitCopy   :: CPUFlag
    , interrupt :: CPUFlag
    }

avrCPUDef :: CPUDef AVRAlu
avrCPUDef = do
    endianness LittleEndian
    g   <- regFile "GPR" (width @32) byte
    sp' <- reg    "SP"  w16
    pc' <- reg    "PC"  (width @22)
    (sreg, fs) <- flagPack @8 "SREG" ["I","T","H","S","V","N","Z","C"]
    let i = fs!!0; t = fs!!1; h = fs!!2; s = fs!!3
        v = fs!!4; n = fs!!5; z = fs!!6; c = fs!!7
    aliasFile g   "0x00 + regIndex"
    aliasReg  sp' 0x5D
    aliasReg  sreg 0x5F
    pure AVRAlu
        { gpr       = g
        , sp        = sp'
        , pc        = pc'
        , carry     = c
        , zero      = z
        , negative  = n
        , overflow  = v
        , sign      = s
        , halfCarry = h
        , bitCopy   = t
        , interrupt = i
        }

-- ---------------------------------------------------------------------------
-- Instruction definitions
-- ---------------------------------------------------------------------------

addDef :: (MonadALU m, AluDef m ~ AVRAlu) => m ()
addDef = do
    mnemonic "ADD"
    doc      "Add two registers without carry"
    encoding "0000_11rd_ddddd_rrrrr_...."
    rd <- register gpr "ddddd"
    rr <- register gpr "rrrrr"
    z  <- cpuFlag zero
    n  <- cpuFlag negative
    a  <- readReg rd
    b  <- readReg rr
    r  <- aluOp PAdd a b
    writeReg rd r
    zf <- isZero r
    setFlag z zf
    setFlag n (slice 7 7 r)

rjmpDef :: (MonadALU m, AluDef m ~ AVRAlu) => m ()
rjmpDef = do
    mnemonic "RJMP"
    doc      "Relative jump by signed 12-bit offset"
    encoding "1100_kkkkkkkkkkkk_...."
    k   <- immediate "kkkkkkkkkkkk"
    pc' <- cpu pc
    relJump pc' (signExtend (k :: IExpr 12) :: IExpr 22)

-- | LDS requires Word m ~ IExpr 8 and DataAddr m ~ IExpr 16
ldsDef :: ( MonadHarvardALU m, AluDef m ~ AVRAlu
          , Word m ~ IExpr 8, DataAddr m ~ IExpr 16
          ) => m ()
ldsDef = do
    mnemonic "LDS"
    doc      "Load direct from data space"
    encoding "1001_000d_ddddd_0000_kkkkkkkkkkkkkkkk_...."
    rd <- register gpr "ddddd"
    k  <- immediate "kkkkkkkkkkkkkkkk"
    v  <- readMem (k :: IExpr 16)
    writeReg rd v

-- | LPM reads a byte from program memory via the implicit Z register (r30:r31).
-- A complete implementation requires a MonadALU method for accessing register
-- file entries by a fixed index (not an encoding field); that is left as a
-- future DSL extension.
lpmDef :: ( MonadHarvardALU m, AluDef m ~ AVRAlu
          , Word m ~ IExpr 8
          , CodeAddr m ~ IExpr 16, CodeWord m ~ IExpr 16
          ) => m ()
lpmDef = do
    mnemonic "LPM"
    doc      "Load byte from program memory via Z register"
    encoding "1001_0101_1100_1000_...."

-- ---------------------------------------------------------------------------
-- ISA definition
-- ---------------------------------------------------------------------------

-- | AVR ISA definition. The data word is 8-bit (Word m ~ IExpr 8),
-- SP is 16-bit, and the PC is 22-bit.
avrISA :: ( MonadHarvardALU m, AluDef m ~ AVRAlu
          , Word m ~ IExpr 8, DataAddr m ~ IExpr 16
          ) => ISADef m
avrISA = defineISA ISADef
    { isaPc            = SomeCPURegister <$> cpu pc
    , isaInterruptBody = Nothing
    , isaReset         = do
        resetReg  pc 0x0000
        resetReg  sp 0x21FF
        resetFlag carry     Lo
        resetFlag zero      Lo
        resetFlag negative  Lo
        resetFlag overflow  Lo
        resetFlag sign      Lo
        resetFlag halfCarry Lo
        resetFlag bitCopy   Lo
        resetFlag interrupt Lo
    , isaInstrs =
        [ addDef
        , rjmpDef
        ]
    }
