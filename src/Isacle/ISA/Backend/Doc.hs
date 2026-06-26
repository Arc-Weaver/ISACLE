{-# LANGUAGE GADTs #-}
-- | Documentation renderer over the ISA IR.
--
-- An instruction body is built into an 'InstrIR' and its metadata (mnemonic,
-- doc string, encoding, operands) is read back out.  Operands are recovered
-- from the IR: register-file reads become 'OpRegister', decoded immediates
-- 'OpImmediate'.
module Isacle.ISA.Backend.Doc
    ( InstrSpec(..)
    , OperandSpec(..)
    , docInstr
    , docISA
    ) where

import Prelude hiding (Word)
import Data.List (nub)

import Isacle.ISA.Def
import Isacle.ISA.IR
import Isacle.ISA.Build (ISABuild, runISABuild)

-- | One operand referenced by an instruction body.
data OperandSpec
    = OpRegister String String   -- ^ (field key, register file name)
    | OpImmediate String         -- ^ field key
    deriving (Show, Eq)

-- | Collected documentation metadata for one instruction.
data InstrSpec = InstrSpec
    { specMnemonic :: String
    , specDoc      :: String
    , specEncoding :: String
    , specOperands :: [OperandSpec]
    } deriving (Show, Eq)

-- | Build one instruction body and read off its documentation spec.
docInstr :: alu -> ISABuild alu wordW addrW codeWordW codeAddrW () -> InstrSpec
docInstr alu body =
    let ir = runISABuild alu body
    in InstrSpec
        { specMnemonic = maybe "" id (iirMnemonic ir)
        , specDoc      = maybe "" id (iirDoc ir)
        , specEncoding = maybe "" id (iirEncoding ir)
        , specOperands = nub (concatMap operandsS (iirStmts ir))
        }

docISA :: alu -> ISADef (ISABuild alu wordW addrW codeWordW codeAddrW) -> [InstrSpec]
docISA alu idef = map (docInstr alu) (isaInstrs idef)

-- ---------------------------------------------------------------------------
-- Operand recovery
-- ---------------------------------------------------------------------------

operandsS :: IStmt -> [OperandSpec]
operandsS s = case s of
    SReadMem  _ a  -> operandsE a
    SReadCode _ a  -> operandsE a
    SWriteReg r e  -> regOperand r ++ operandsE e
    SWriteMem a d  -> operandsE a ++ operandsE d
    SWriteFlag _ e -> operandsE e
    SJumpIf r c t  -> regOperand r ++ operandsE c ++ operandsE t

regOperand :: RegRef w -> [OperandSpec]
regOperand (RegFile rf (FieldRef k) o) = [OpRegister (if null k then show o else k) rf]
regOperand (RegScalar _)               = []

operandsE :: IExpr w -> [OperandSpec]
operandsE e = case e of
    IField (FieldRef k)                -> [OpImmediate k]
    IReadReg (RegFile rf (FieldRef k) o) -> [OpRegister (if null k then show o else k) rf]
    IReadReg (RegScalar _)             -> []
    IReadRes _                         -> []
    IFlagRead _                        -> []
    IIrqVector                         -> []
    ILit _                             -> []
    IBin _ a b                         -> operandsE a ++ operandsE b
    IUn _ a                            -> operandsE a
    IResize a                          -> operandsE a
    ISignExt a                         -> operandsE a
    IZeroExt a                         -> operandsE a
    ITrunc a                           -> operandsE a
    IIsZero a                          -> operandsE a
    ISlice _ _ a                       -> operandsE a
    INamed _ a                         -> operandsE a
