{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Simulation renderer over the ISA IR.
--
-- An instruction body is built into an 'InstrIR'
-- ('Isacle.ISA.Build.runISABuild') and then /interpreted/ here against a
-- 'SimState'.  Register reads observe the state at the start of the instruction
-- and writes are applied at the end (matching the synthesised core's
-- combinational-read / registered-write timing); memory reads are resolved in
-- program order so a later read can use an earlier read's result.
module Isacle.ISA.Backend.Sim
    ( -- * Simulation state
      SimState(..)
    , SimCPU(..)
    , emptySim
      -- * Runners
    , runInstr
    , execInstr
    , runIrq
    ) where

import Prelude hiding (Word)
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import Data.Bits

import Hdl.Types (HdlType)
import Hdl.Bits (Unsigned)
import Isacle.ISA.Types
import Isacle.ISA.Encoding
import Isacle.ISA.IR hiding ((.&.), (.|.), xor, shiftL, shiftR, arithShiftR, add, mul)
import Isacle.ISA.Build (ISABuild, runISABuild)

-- ---------------------------------------------------------------------------
-- Simulation state
-- ---------------------------------------------------------------------------

-- | CPU register and flag state, keyed by name (@"GPR:5"@, @"SP"@, …).
data SimCPU = SimCPU { scRegs :: Map String Integer }
    deriving (Show, Eq)

data SimState = SimState
    { ssCPU         :: SimCPU
    , ssDataMem     :: IntMap Integer
    , ssCodeMem     :: IntMap Integer
    , ssEncoding    :: Maybe EncodingInfo
    , ssIrqGateOpen :: Bool
    } deriving (Show)

emptySim :: SimState
emptySim = SimState (SimCPU Map.empty) IntMap.empty IntMap.empty Nothing True

-- ---------------------------------------------------------------------------
-- Expression evaluation
-- ---------------------------------------------------------------------------

data EvalEnv = EvalEnv
    { evRegs   :: Map String Integer
    , evInstr  :: Integer
    , evEnc    :: Maybe EncodingInfo
    , evIrqVec :: Maybe Integer
    , evToks   :: IntMap Integer
    }

widthOfE :: HdlType a => IExpr a -> Int
widthOfE = exprWidth

maskTo :: Int -> Integer -> Integer
maskTo w v = v .&. ((1 `shiftL` w) - 1)

evalE :: EvalEnv -> IExpr w -> Integer
evalE env = go
  where
    go :: forall k. IExpr k -> Integer
    go e0 = case e0 of
        ILit v                              -> maskTo (widthOfE e0) v
        IField (FieldRef k)                 -> field k
        IReadReg (RegScalar n)              -> maskTo (widthOfE e0) (reg n)
        IReadReg (RegFile rf (FieldRef k) o) -> maskTo (widthOfE e0) (reg (rf ++ ":" ++ show (field k + fromIntegral o)))
        -- A view register: concatenate its entries, low (first) entry least significant.
        IReadReg (RegEntries file ew idxs)  -> maskTo (widthOfE e0)
            (foldr (.|.) 0 [ reg (file ++ ":" ++ show idx) `shiftL` (p * ew)
                           | (p, idx) <- zip [0 ..] idxs ])
        IReadRes (ReadTok t)                -> IntMap.findWithDefault 0 t (evToks env)
        IFlagRead (CPUFlag rn bp)           -> (reg rn `shiftR` bp) .&. 1
        IIrqVector                          -> maybe 0 id (evIrqVec env)
        IBin op a b                         -> maskTo (widthOfE e0) (binOp op (go a) (go b) (widthOfE e0))
        IUn PNot a                          -> maskTo (widthOfE e0) (complement (go a))
        IUn _ a                             -> maskTo (widthOfE e0) (go a)
        IMux c t f                          -> maskTo (widthOfE e0) (if go c /= 0 then go t else go f)
        IResize a                           -> maskTo (widthOfE e0) (go a)
        IZeroExt a                          -> maskTo (widthOfE e0) (go a)
        ITrunc a                            -> maskTo (widthOfE e0) (go a)
        ISignExt a                          -> maskTo (widthOfE e0) (signExtend' (widthOfE a) (go a))
        -- Reinterpret keeps the bits; signedness is consumed by later sign-extend
        -- (widenTo) or by an explicitly signed op (PArithShiftR / PMulSigned).
        IReinterpret a                      -> maskTo (widthOfE e0) (go a)
        IIsZero a                           -> if go a == 0 then 1 else 0
        ISlice hi lo a                      -> (go a `shiftR` lo) .&. ((1 `shiftL` (hi - lo + 1)) - 1)
        INamed _ a                          -> go a
    reg n   = Map.findWithDefault 0 n (evRegs env)
    field k = case evEnc env of
        Just enc -> case Map.lookup k (encFields enc) of
            Just bps -> extractField bps (evInstr env)
            Nothing  -> 0
        Nothing -> 0

binOp :: ALUPrim -> Integer -> Integer -> Int -> Integer
binOp op a b w = case op of
    PAdd         -> a + b
    PSub         -> a - b
    PAnd         -> a .&. b
    POr          -> a .|. b
    PXor         -> xor a b
    PNot         -> complement a
    PShiftL      -> a `shiftL` fromIntegral b
    PShiftR      -> a `shiftR` fromIntegral b
    PArithShiftR -> arithShiftR w a (fromIntegral b)
    PMul         -> a * b
    PMulSigned   -> signedMul w a b

-- ---------------------------------------------------------------------------
-- Instruction interpretation
-- ---------------------------------------------------------------------------

-- | Interpret one 'InstrIR' against a state.
renderInstrSim :: Integer        -- ^ raw instruction word
               -> Maybe Integer  -- ^ IRQ vector (Just in an interrupt body)
               -> InstrIR
               -> SimState -> SimState
renderInstrSim instrWord mIrqVec ir st0 =
    -- Expose this instruction's parsed encoding on the result so callers can
    -- recover it (e.g. to build a decode table) after running a body.
    let st1 = st0 { ssEncoding = enc }
    in if not gateOpen then st1 else foldl' apply st1 (iirStmts ir)
  where
    enc   = fmap parseEncoding (iirEncoding ir)
    regs0 = scRegs (ssCPU st0)
    env t = EvalEnv regs0 instrWord enc mIrqVec t

    -- Resolve memory/code reads in program order, building the token map.
    toks = foldl' rd IntMap.empty (iirStmts ir)
    rd t (SReadMem  (ReadTok i) a) = IntMap.insert i (IntMap.findWithDefault 0 (fromIntegral (evalE (env t) a)) (ssDataMem st0)) t
    rd t (SReadCode (ReadTok i) a) = IntMap.insert i (IntMap.findWithDefault 0 (fromIntegral (evalE (env t) a)) (ssCodeMem st0)) t
    rd t _                         = t

    gateOpen = maybe True (\g -> evalE (env toks) g /= 0) (iirGate ir)
    ev :: IExpr w -> Integer
    ev e = evalE (env toks) e

    -- A view-register write fans out across its entries (low entry first).
    apply st (SWriteReg (RegEntries file ew idxs) e) =
        let v = ev e
        in foldl' (\s (p, idx) ->
                      putReg (file ++ ":" ++ show idx)
                             ((v `shiftR` (p * ew)) .&. ((1 `shiftL` ew) - 1)) s)
                  st (zip [0 ..] idxs)
    apply st (SWriteReg ref e)  = putReg (regRefKey ref) (ev e) st
    apply st (SWriteMem a d)    = st { ssDataMem = IntMap.insert (fromIntegral (ev a)) (ev d) (ssDataMem st) }
    apply st (SWriteFlag f e)   = putFlag f (ev e) st
    apply st (SJumpIf pc c t)   = if ev c /= 0 then putReg (regRefKey pc) (ev t) st else st
    apply st _                  = st  -- reads already resolved

    regRefKey :: RegRef w -> String
    regRefKey (RegScalar n)               = n
    regRefKey (RegEntries file _ idxs)    = file ++ ":" ++ show (head idxs)  -- views write via fan-out
    regRefKey (RegFile rf (FieldRef k) o) = rf ++ ":" ++ show (evalE (env toks) (IField (FieldRef k) :: IExpr (Unsigned 32)) + fromIntegral o)

    putReg n v st = st { ssCPU = (ssCPU st) { scRegs = Map.insert n v (scRegs (ssCPU st)) } }
    putFlag (CPUFlag rn bp) v st =
        let m   = 1 `shiftL` bp
            old = Map.findWithDefault 0 rn (scRegs (ssCPU st))
            new = (old .&. complement m) .|. (if v /= 0 then m else 0)
        in st { ssCPU = (ssCPU st) { scRegs = Map.insert rn new (scRegs (ssCPU st)) } }

-- ---------------------------------------------------------------------------
-- Runners
-- ---------------------------------------------------------------------------

-- | Run one instruction body against an ALU record and raw instruction word.
runInstr :: alu -> Integer
         -> ISABuild alu wordW addrW codeWordW codeAddrW ()
         -> SimState -> SimState
runInstr aluRec instrWord body st =
    renderInstrSim instrWord Nothing (runISABuild aluRec body) (st { ssIrqGateOpen = True })

-- | 'runInstr' from 'emptySim'.
execInstr :: alu -> Integer
          -> ISABuild alu wordW addrW codeWordW codeAddrW ()
          -> SimState
execInstr aluRec instrWord body = runInstr aluRec instrWord body emptySim

-- | Run an interrupt body with the supplied vector address.
runIrq :: alu -> Integer
       -> ISABuild alu wordW addrW codeWordW codeAddrW ()
       -> SimState -> SimState
runIrq aluRec irqVec body st =
    renderInstrSim 0 (Just irqVec) (runISABuild aluRec body) (st { ssIrqGateOpen = True })

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

arithShiftR :: Int -> Integer -> Int -> Integer
arithShiftR w a n =
    let sign     = a `shiftR` (w - 1)
        extended = if sign == 1 then a - (1 `shiftL` w) else a
    in (extended `shiftR` n) .&. ((1 `shiftL` w) - 1)

signedMul :: Int -> Integer -> Integer -> Integer
signedMul w a b =
    let half = 1 `shiftL` (w - 1)
        s x  = if x >= half then x - (1 `shiftL` w) else x
    in s a * s b

signExtend' :: Int -> Integer -> Integer
signExtend' srcW v =
    let half = 1 `shiftL` (srcW - 1)
    in if v >= half then v - (1 `shiftL` srcW) else v
