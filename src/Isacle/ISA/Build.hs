{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies         #-}
-- | The IR-builder monad: running an instruction body in it produces the
-- 'InstrIR' source of truth (annotations + ordered statements).  This is the
-- target the surface 'MonadALU' interface will build into — it accumulates
-- 'IStmt's in program order and mints 'ReadTok's for ordered reads.
--
-- The point of this module is that the value flowing through a body is an
-- 'IExpr' (via the 'Term' class), so @sp - 1@ constructs @'IBin' 'PSub'@ — a
-- value — rather than doing integer arithmetic on a smuggled wire id.  Nothing
-- here knows about 'Hdl.Net.WireId's; lowering to wires happens later, in the
-- synthesis renderer.
module Isacle.ISA.Build
    ( ISABuild
    , runISABuild
      -- * Builder operations (mirror the MonadALU surface, value type = IExpr)
    , setMnemonic
    , setDoc
    , setEncoding
    , readRegB
    , writeRegB
    , readMemB
    , writeMemB
    , readCodeB
    , writeFlagB
    , jumpIfB
    , immB
    , litB
    ) where

import Prelude
import Control.Monad.State.Strict
import GHC.TypeLits (KnownNat)

import Isacle.ISA.Types (CPUFlag)
import Isacle.ISA.IR

-- ---------------------------------------------------------------------------
-- Builder state
-- ---------------------------------------------------------------------------

data BuildSt = BuildSt
    { bsMnemonic :: Maybe String
    , bsDoc      :: Maybe String
    , bsEncoding :: Maybe String
    , bsStmts    :: [IStmt]   -- ^ accumulated in reverse (program order on finish)
    , bsReadCtr  :: Int       -- ^ next ReadTok id
    }

initBuildSt :: BuildSt
initBuildSt = BuildSt Nothing Nothing Nothing [] 0

newtype ISABuild a = ISABuild (State BuildSt a)
    deriving newtype (Functor, Applicative, Monad)

-- | Run a body, producing its 'InstrIR'.
runISABuild :: ISABuild a -> InstrIR
runISABuild (ISABuild m) =
    let st = execState m initBuildSt
    in InstrIR (bsMnemonic st) (bsDoc st) (bsEncoding st) (reverse (bsStmts st))

-- ---------------------------------------------------------------------------
-- Primitive operations
-- ---------------------------------------------------------------------------

emitStmt :: IStmt -> ISABuild ()
emitStmt s = ISABuild $ modify $ \st -> st { bsStmts = s : bsStmts st }

freshRead :: ISABuild ReadTok
freshRead = ISABuild $ do
    n <- gets bsReadCtr
    modify $ \st -> st { bsReadCtr = n + 1 }
    pure (ReadTok n)

setMnemonic :: String -> ISABuild ()
setMnemonic s = ISABuild $ modify $ \st -> st { bsMnemonic = Just s }

setDoc :: String -> ISABuild ()
setDoc s = ISABuild $ modify $ \st -> st { bsDoc = Just s }

setEncoding :: String -> ISABuild ()
setEncoding s = ISABuild $ modify $ \st -> st { bsEncoding = Just s }

-- | Register read is a pure value reference (no ordered effect).
readRegB :: KnownNat w => RegRef w -> ISABuild (IExpr w)
readRegB = pure . IReadReg

writeRegB :: KnownNat w => RegRef w -> IExpr w -> ISABuild ()
writeRegB ref e = emitStmt (SWriteReg ref e)

-- | Memory read is an ordered effect; its result is referred to via 'IReadRes'.
readMemB :: KnownNat w => IExpr aw -> ISABuild (IExpr w)
readMemB addr = do
    tok <- freshRead
    emitStmt (SReadMem tok addr)
    pure (IReadRes tok)

writeMemB :: IExpr aw -> IExpr ww -> ISABuild ()
writeMemB a d = emitStmt (SWriteMem a d)

-- | Code read (second instruction word, LPM) — also an ordered effect.
readCodeB :: KnownNat w => IExpr aw -> ISABuild (IExpr w)
readCodeB addr = do
    tok <- freshRead
    emitStmt (SReadCode tok addr)
    pure (IReadRes tok)

writeFlagB :: CPUFlag -> IExpr 1 -> ISABuild ()
writeFlagB f e = emitStmt (SWriteFlag f e)

jumpIfB :: KnownNat w => RegRef w -> IExpr 1 -> IExpr w -> ISABuild ()
jumpIfB pc cond tgt = emitStmt (SJumpIf pc cond tgt)

-- | A decoded instruction field as a value.
immB :: KnownNat w => String -> ISABuild (IExpr w)
immB k = pure (IField (FieldRef k))

-- | A literal value.
litB :: KnownNat w => Integer -> ISABuild (IExpr w)
litB = pure . ILit
