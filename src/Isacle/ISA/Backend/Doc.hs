{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Isacle.ISA.Backend.Doc
    ( -- * Collected instruction metadata
      InstrSpec(..)
    , OperandSpec(..)
      -- * Doc monad
    , DocM
      -- * Runners
    , docInstr
    , docISA
    ) where

import Prelude hiding (Word)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Hdl.Bits
import Isacle.ISA.Types
import Isacle.ISA.ALU
import Isacle.ISA.Def

-- ---------------------------------------------------------------------------
-- Output types
-- ---------------------------------------------------------------------------

-- | One operand referenced by an instruction body.
data OperandSpec
    = OpRegister String String   -- ^ (encoding field name, register file name)
    | OpImmediate String         -- ^ encoding field name
    deriving (Show, Eq)

-- | Collected documentation metadata for one instruction.
data InstrSpec = InstrSpec
    { specMnemonic :: String
    , specDoc      :: String
    , specEncoding :: String
    , specOperands :: [OperandSpec]
    } deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Build accumulator (private)
-- ---------------------------------------------------------------------------

data InstrBuild = InstrBuild
    { ibMnemonic  :: String
    , ibDoc       :: String
    , ibEncoding  :: String
    , ibOperands  :: [OperandSpec]
    }

emptyBuild :: InstrBuild
emptyBuild = InstrBuild "" "" "" []

-- ---------------------------------------------------------------------------
-- DocM monad
-- Reader (alu) + State InstrBuild; no signals touched.
-- ---------------------------------------------------------------------------

newtype DocM alu a = DocM (ReaderT alu (State InstrBuild) a)
    deriving newtype (Functor, Applicative, Monad)

instance MonadALU (DocM alu) where
    type AluDef   (DocM alu) = alu
    type Word     (DocM alu) = Unsigned 8
    type DataAddr (DocM alu) = Unsigned 16

    cpu sel      = DocM (asks sel)
    cpuFlag sel  = DocM (asks sel)

    register sel field = DocM $ do
        alu <- ask
        let CPURegFile rfname = sel alu
        lift $ modify (\b -> b { ibOperands = ibOperands b ++ [OpRegister field rfname] })
        return (CPURegister field)

    immediate field = DocM $ do
        lift $ modify (\b -> b { ibOperands = ibOperands b ++ [OpImmediate field] })
        return (Unsigned 0)

    mnemonic s   = DocM . lift $ modify (\b -> b { ibMnemonic = s })
    doc s        = DocM . lift $ modify (\b -> b { ibDoc = s })
    encoding s   = DocM . lift $ modify (\b -> b { ibEncoding = s })

    readReg _    = return (Unsigned 0)
    writeReg _ _ = return ()
    readMem _    = return (Unsigned 0)
    writeMem _ _ = return ()
    getFlag _    = return 0
    setFlag _ _  = return ()
    isZero _        = return 0
    absJumpIf _ _ _ = return ()
    aluOp _ x _  = return x

instance MonadHarvardALU (DocM alu) where
    type CodeAddr (DocM alu) = Unsigned 16
    type CodeWord (DocM alu) = Unsigned 16
    readCode _ = return (Unsigned 0)

-- ---------------------------------------------------------------------------
-- Runners
-- ---------------------------------------------------------------------------

-- | Run one instruction body against an ALU definition record; collect its spec.
docInstr :: alu -> DocM alu () -> InstrSpec
docInstr alu m =
    let DocM inner = m
        build = execState (runReaderT inner alu) emptyBuild
    in InstrSpec
        { specMnemonic = ibMnemonic build
        , specDoc      = ibDoc      build
        , specEncoding = ibEncoding build
        , specOperands = ibOperands build
        }

-- | Run every instruction in an ISADef and return their specs in order.
docISA :: alu -> ISADef (DocM alu) -> [InstrSpec]
docISA alu idef = map (docInstr alu) (isaInstrs idef)
