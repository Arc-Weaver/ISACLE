{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
-- | The synthesis renderer's expression lowering: 'IExpr' → a typed 'Signal'.
--
-- Written entirely against the 'Hdl'/'Signal' interface — no 'NetM', no
-- 'WireId'.  Combinational structure is pure 'Signal' composition; the monad
-- @m@ is used only to /bind named signals/ ('named'), so derived operations can
-- carry a readable name (the generated VHDL reads @sp_sub@, @ADD_d@, … instead
-- of @wN@).  Register reads, field extractions and read results are resolved
-- through a 'LowerCtx' supplied by the CPU synthesis pass.
module Isacle.ISA.Backend.Lower
    ( LowerCtx(..)
    , Named(..)
    , lowerExpr
    , lowerExpr_
      -- * Statement-level rendering
    , Rendered(..)
    , RegWrite(..)
    , Jump(..)
    , emptyRendered
    , renderInstr
    ) where

import Prelude
import Control.Monad (foldM)
import Data.List (intercalate)

import Hdl.Net (PrimOp)
import qualified Hdl.Net as N
import Hdl.Types (Signal(..))
import Hdl.Monad (Hdl, named)
import Isacle.ISA.Types (ALUPrim(..), CPUFlag)
import Isacle.ISA.IR

-- | Resolution callbacks for the leaves an 'IExpr' can reference.  Supplied by
-- the CPU synthesis pass (which owns the register file, the field decoder and
-- the per-cycle read-result signals).  All leaves resolve to a (combinational)
-- signal already; lowering only composes and names them.
data LowerCtx s dom = LowerCtx
    { lcReadReg   :: forall a. RegRef a -> s dom ()
    , lcField     :: FieldRef -> s dom ()
    , lcReadRes   :: ReadTok -> s dom ()
    , lcReadFlag  :: CPUFlag -> s dom ()
    , lcIrqVector :: s dom ()
    , lcMnemonic  :: Maybe String   -- ^ instruction mnemonic, for field naming
    }

-- | A lowered signal together with the name it carries, if any.  The name is
-- what lets derived operations be named after their inputs ("following").
data Named s dom = Named
    { nSig  :: s dom ()
    , nName :: Maybe String
    }

-- | Lower an expression, returning the signal and its propagated name.
lowerExpr :: forall s m dom a. (Hdl s m, Signal s)
          => LowerCtx s dom -> IExpr a -> m (Named s dom)
lowerExpr ctx = go
  where
    go :: forall k. IExpr k -> m (Named s dom)
    go (INamed nm e)   = do { Named s _ <- go e; nameAs (Just nm) s }
    go (IReadReg ref)  = nameAs (Just (regName ref))            (lcReadReg ctx ref)
    go (IField fr)     = nameAs (Just (mnem (frKey fr)))        (lcField ctx fr)
    go (IReadRes t@(ReadTok i)) = nameAs (Just ("rd" ++ show i)) (lcReadRes ctx t)
    go (IFlagRead f)   = pure (Named (lcReadFlag ctx f) Nothing)
    go IIrqVector      = nameAs (Just "irq_vector")            (lcIrqVector ctx)
    go e@(ILit v)      = pure (Named (sigLitW v (exprWidth e)) Nothing)  -- no name

    go (IBin op a b) = bin (sigPrim2 (toPrim op)) (followName (opTag op)) a b
    go (IUn op a)    = un  (sigPrim1 (toPrim op)) (tagWith (opTag op))   a
    go (IMux c t f)  = do
        Named sc _ <- go c
        bin (sigPrim3 N.PMux sc) (followName "sel") t f
    go (IIsZero a)   = do
        Named sa na <- go a
        nameAs (tagSuffix "_isZero" na) (sigPrim2 N.PEq sa (sigLitW 0 (exprWidth a)))

    go e@(IResize a)      = un (sigPrim1 (N.PResize (exprWidth e)))       keep a
    go e@(IZeroExt a)     = un (sigPrim1 (N.PResize (exprWidth e)))       keep a
    go e@(ITrunc a)       = un (sigPrim1 (N.PSlice (exprWidth e - 1) 0))  keep a
    go e@(ISignExt a)     = un (sigPrim1 (N.PSignedResize (exprWidth e))) keep a
    go e@(IReinterpret a) = un (sigPrim1 (N.PReinterpret (exprRepr e)))   keep a
    go (ISlice hi lo a)   = un (sigPrim1 (N.PSlice hi lo))                keep a

    -- A unary op carrying its child's name through @nameOf@.
    un :: forall x. (s dom () -> s dom ())
       -> (Maybe String -> Maybe String) -> IExpr x -> m (Named s dom)
    un f nameOf a = do { Named sa na <- go a; nameAs (nameOf na) (f sa) }
    -- A binary op named from both children via @nameOf@.
    bin :: forall x y. (s dom () -> s dom () -> s dom ())
        -> (Maybe String -> Maybe String -> Maybe String)
        -> IExpr x -> IExpr y -> m (Named s dom)
    bin f nameOf a b = do
        Named sa na <- go a; Named sb nb <- go b
        nameAs (nameOf na nb) (f sa sb)
    -- Bind a signal under an optional name and wrap as 'Named'.
    nameAs :: Maybe String -> s dom () -> m (Named s dom)
    nameAs (Just nm) s = do { s' <- named nm s; pure (Named s' (Just nm)) }
    nameAs Nothing   s = pure (Named s Nothing)

    keep na          = na                            -- propagate name unchanged
    tagWith tag      = fmap (\x -> tag ++ "_" ++ x)
    tagSuffix suf    = fmap (++ suf)
    mnem             = maybe id (\m x -> m ++ "_" ++ x) (lcMnemonic ctx)

-- | Lower an expression, discarding the propagated name.
lowerExpr_ :: (Hdl s m, Signal s) => LowerCtx s dom -> IExpr w -> m (s dom ())
lowerExpr_ ctx e = nSig <$> lowerExpr ctx e

-- ---------------------------------------------------------------------------
-- Naming helpers
-- ---------------------------------------------------------------------------

regName :: RegRef w -> String
regName (RegScalar n)              = n
regName (RegEntries f _ idxs)      = f ++ "_" ++ intercalate "_" (map show idxs)
regName (RegFile f (FieldRef k) s o)
    | null k    = f ++ "_r" ++ show o          -- constant index Rn
    | otherwise = f ++ "_" ++ k ++ sTag ++ oTag
  where
    sTag = if s /= 1 then "_x" ++ show s else ""
    oTag = if o /= 0 then "_p" ++ show o else ""

-- | Name a binary result after its operands, e.g. @sp \"sub\" => sp_sub@.
-- Only names when at least one operand carries a name, to avoid noise.
followName :: String -> Maybe String -> Maybe String -> Maybe String
followName tag (Just a) (Just b) = Just (a ++ "_" ++ tag ++ "_" ++ b)
followName tag (Just a) Nothing  = Just (a ++ "_" ++ tag)
followName tag Nothing  (Just b) = Just (tag ++ "_" ++ b)
followName _   Nothing  Nothing  = Nothing

opTag :: ALUPrim -> String
opTag PAdd         = "add"
opTag PSub         = "sub"
opTag PAnd         = "and"
opTag POr          = "or"
opTag PXor         = "xor"
opTag PNot         = "not"
opTag PShiftL      = "shl"
opTag PShiftR      = "shr"
opTag PArithShiftR = "asr"
opTag PMul         = "mul"
opTag PMulSigned   = "muls"

toPrim :: ALUPrim -> PrimOp
toPrim PAdd         = N.PAdd
toPrim PSub         = N.PSub
toPrim PAnd         = N.PAnd
toPrim POr          = N.POr
toPrim PXor         = N.PXor
toPrim PNot         = N.PNot
toPrim PShiftL      = N.PShiftL
toPrim PShiftR      = N.PShiftR
toPrim PArithShiftR = N.PShiftR
toPrim PMul         = N.PMul
toPrim PMulSigned   = N.PMul

-- ---------------------------------------------------------------------------
-- Statement-level rendering: InstrIR -> lowered write/read requests
-- ---------------------------------------------------------------------------

-- | A register write, lowered: the (annotated) destination plus the data signal.
data RegWrite s dom = forall w. RegWrite (RegRef w) (s dom ())

-- | A conditional jump, lowered: the PC register, condition and target signals.
data Jump s dom = forall w. Jump (RegRef w) (s dom ()) (s dom ())

-- | One instruction's effects, lowered to signals.  The CPU pass assembles
-- these into the register file, memory ports and the execution sequencer.
data Rendered s dom = Rendered
    { rRegWrites  :: [RegWrite s dom]
    , rMemWrites  :: [(s dom (), s dom ())]   -- ^ (address, data) in program order
    , rMemReads   :: [(ReadTok, s dom ())]    -- ^ (token, address) in program order
    , rCodeReads  :: [(ReadTok, s dom ())]
    , rFlagWrites :: [(CPUFlag, s dom ())]
    , rJumps      :: [Jump s dom]
    }

emptyRendered :: Rendered s dom
emptyRendered = Rendered [] [] [] [] [] []

-- | Lower every statement of an instruction's IR to signals, preserving program
-- order (which the sequencer relies on for memory transactions).
renderInstr :: forall s m dom. (Hdl s m, Signal s)
            => LowerCtx s dom -> InstrIR -> m (Rendered s dom)
renderInstr ctx ir = finish <$> foldM step emptyRendered (iirStmts ir)
  where
    -- accumulate in reverse, restore order at the end
    finish r = r
        { rRegWrites  = reverse (rRegWrites r)
        , rMemWrites  = reverse (rMemWrites r)
        , rMemReads   = reverse (rMemReads r)
        , rCodeReads  = reverse (rCodeReads r)
        , rFlagWrites = reverse (rFlagWrites r)
        , rJumps      = reverse (rJumps r)
        }

    step r (SWriteReg (RegEntries file ew idxs) e) = do
        -- A view-register write fans out to one register-file write per entry:
        -- entry p (low first) gets bits [p*ew .. p*ew+ew-1] of the value.
        w <- lowerExpr_ ctx e
        let ws = [ RegWrite (RegFile file (FieldRef "") 1 idx)
                            (sigPrim1 (N.PSlice ((p + 1) * ew - 1) (p * ew)) w)
                 | (p, idx) <- zip [0 :: Int ..] idxs ]
        pure r { rRegWrites = reverse ws ++ rRegWrites r }
    step r (SWriteReg ref e) = do
        w <- lowerExpr_ ctx e
        pure r { rRegWrites = RegWrite ref w : rRegWrites r }
    step r (SWriteMem a d) = do
        wa <- lowerExpr_ ctx a
        wd <- lowerExpr_ ctx d
        pure r { rMemWrites = (wa, wd) : rMemWrites r }
    step r (SReadMem tok a) = do
        wa <- lowerExpr_ ctx a
        pure r { rMemReads = (tok, wa) : rMemReads r }
    step r (SReadCode tok a) = do
        wa <- lowerExpr_ ctx a
        pure r { rCodeReads = (tok, wa) : rCodeReads r }
    step r (SWriteFlag f e) = do
        w <- lowerExpr_ ctx e
        pure r { rFlagWrites = (f, w) : rFlagWrites r }
    step r (SJumpIf pc cond tgt) = do
        wc <- lowerExpr_ ctx cond
        wt <- lowerExpr_ ctx tgt
        pure r { rJumps = Jump pc wc wt : rJumps r }
