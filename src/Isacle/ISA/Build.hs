{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies         #-}
-- | The IR-builder monad: running an instruction body in it produces the
-- 'InstrIR' source of truth.  This is the /single/ 'MonadALU' instance —
-- backends (@Synth@, @Sim@, @Doc@) are renderers over the resulting 'InstrIR'
-- rather than separate, leaky interpreters.
--
-- The value a body computes with is an 'IExpr', so @sp - 1@ builds
-- @IBin PSub@ — a value — and the synthesiser lowers it to a real subtractor.
-- No 'Hdl.Net.WireId' ever appears here.
module Isacle.ISA.Build
    ( ISABuild
    , runISABuild
    , evalISABuild
    ) where

import Prelude hiding (Word)
import Data.Char (isDigit)
import GHC.TypeLits (KnownNat)
import Control.Monad.Reader
import Control.Monad.State.Strict

import Isacle.ISA.Types
import Isacle.ISA.Encoding (fieldKey)
import Isacle.ISA.ALU
import Isacle.ISA.IR

-- ---------------------------------------------------------------------------
-- Builder state and monad
-- ---------------------------------------------------------------------------

data BuildSt = BuildSt
    { bsMnemonic :: Maybe String
    , bsDoc      :: Maybe String
    , bsEncoding :: Maybe String
    , bsGate     :: Maybe (IExpr 1)
    , bsStmts    :: [IStmt]   -- ^ reverse program order
    , bsReadCtr  :: Int
    }

initBuildSt :: BuildSt
initBuildSt = BuildSt Nothing Nothing Nothing Nothing [] 0

-- | Builds an 'InstrIR'.  Carries the ALU record (for 'cpu'/'register') in a
-- reader and accumulates statements in state.  The width parameters pin the
-- data/code word and address widths via the associated types.
newtype ISABuild alu (wordW :: k) (addrW :: k) (cwW :: k) (caW :: k) a
    = ISABuild (ReaderT alu (State BuildSt) a)
    deriving newtype (Functor, Applicative, Monad)

-- | Run an instruction body, producing its 'InstrIR'.
runISABuild :: alu -> ISABuild alu wordW addrW cwW caW () -> InstrIR
runISABuild alu (ISABuild m) =
    let st = execState (runReaderT m alu) initBuildSt
    in InstrIR (bsMnemonic st) (bsDoc st) (bsEncoding st)
               (bsGate st) (reverse (bsStmts st))

-- | Run a body for its /result/ only (ignoring the IR).  Used to read static
-- facts out of a body — e.g. the PC register name from @isaPc@.
evalISABuild :: alu -> ISABuild alu wordW addrW cwW caW a -> a
evalISABuild alu (ISABuild m) = evalState (runReaderT m alu) initBuildSt

emitStmt :: IStmt -> ISABuild alu wordW addrW cwW caW ()
emitStmt s = ISABuild $ modify $ \st -> st { bsStmts = s : bsStmts st }

freshRead :: ISABuild alu wordW addrW cwW caW ReadTok
freshRead = ISABuild $ do
    n <- gets bsReadCtr
    modify $ \st -> st { bsReadCtr = n + 1 }
    pure (ReadTok n)

-- | Parse a 'CPURegister' name into a 'RegRef'.
--
--   * @"rf:field"@        — register-file slot indexed by an instruction field.
--   * @"rf:field@off"@    — same, plus a constant @off@ added to the index
--                           (sub-range encodings, e.g. AVR R16–R31 → @+16@).
--   * @"rf:N"@ (N digits) — a /constant/ register-file index (e.g. @"GPR:0"@ for
--                           R0).  Represented as an empty field key with the
--                           index carried in the offset slot, so no field is
--                           extracted and the index lowers to a literal.
--   * anything else       — a scalar register.
toRegRef :: String -> RegRef w
toRegRef key = case break (== ':') key of
    (rf, ':':rest) -> case break (== '@') rest of
        (fk, '@':offStr)                    -> RegFile rf (FieldRef fk) (read offStr)
        (fk, _) | not (null fk), all isDigit fk
                                            -> RegFile rf (FieldRef "") (read fk)
                | otherwise                 -> RegFile rf (FieldRef fk) 0
    _ -> RegScalar key

-- ---------------------------------------------------------------------------
-- The one MonadALU instance
-- ---------------------------------------------------------------------------

instance (KnownNat wordW, KnownNat addrW)
      => MonadALU (ISABuild alu wordW addrW cwW caW) where
    type AluDef   (ISABuild alu wordW addrW cwW caW) = alu
    type Word     (ISABuild alu wordW addrW cwW caW) = IExpr wordW
    type DataAddr (ISABuild alu wordW addrW cwW caW) = IExpr addrW

    cpu sel     = ISABuild (asks sel)
    cpuFlag sel = ISABuild (asks sel)

    register sel field = ISABuild $ do
        alu <- ask
        let CPURegFile rfname = sel alu
        pure (CPURegister (rfname ++ ":" ++ fieldKey field))

    registerWithOffset sel field offset = ISABuild $ do
        alu <- ask
        let CPURegFile rfname = sel alu
        pure (CPURegister (rfname ++ ":" ++ fieldKey field ++ "@" ++ show offset))

    immediate field = pure (IField (FieldRef (fieldKey field)))

    mnemonic nm = ISABuild $ modify $ \s -> s { bsMnemonic = Just nm }
    doc      d  = ISABuild $ modify $ \s -> s { bsDoc      = Just d  }
    encoding e  = ISABuild $ modify $ \s -> s { bsEncoding = Just e  }

    readReg  (CPURegister key)   = pure (IReadReg (toRegRef key))
    writeReg (CPURegister key) e = emitStmt (SWriteReg (toRegRef key) e)

    readMem  addr = do { tok <- freshRead; emitStmt (SReadMem tok addr); pure (IReadRes tok) }
    writeMem addr val = emitStmt (SWriteMem addr val)

    getFlag flag   = pure (IFlagRead flag)
    setFlag flag v = emitStmt (SWriteFlag flag v)

    absJumpIf (CPURegister key) cond tgt =
        emitStmt (SJumpIf (toRegRef key) cond tgt)
    -- aluOp / litC / resizeBits / signExtendBits / isZero use the Term-based
    -- class defaults.

instance (KnownNat wordW, KnownNat addrW, KnownNat cwW, KnownNat caW)
      => MonadHarvardALU (ISABuild alu wordW addrW cwW caW) where
    type CodeAddr (ISABuild alu wordW addrW cwW caW) = IExpr caW
    type CodeWord (ISABuild alu wordW addrW cwW caW) = IExpr cwW

    readCode addr = do { tok <- freshRead; emitStmt (SReadCode tok addr); pure (IReadRes tok) }

instance (KnownNat wordW, KnownNat addrW, KnownNat caW)
      => MonadIRQ (ISABuild alu wordW addrW cwW caW) where
    type IrqAddrW (ISABuild alu wordW addrW cwW caW) = caW

    irqVector = pure IIrqVector

    irqGate condAction = do
        cond <- condAction
        ISABuild $ modify $ \s ->
            s { bsGate = Just (maybe cond (\g -> tBin PAnd g cond) (bsGate s)) }
