{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeApplications    #-}
-- | The synthesis renderer's expression lowering: 'IExpr' → netlist wire.
--
-- This is where 'Hdl.Net.WireId's are born — never in the IR itself.  Because
-- every value carries its annotations, lowering can /inject/ a name onto each
-- materialised wire and /follow/ names through derived operations, so the
-- generated VHDL reads as @SP@, @ADD_d@, @sp_sub@ … instead of anonymous @wN@.
--
-- Register reads, field extractions and read results are resolved through a
-- 'LowerCtx' supplied by the CPU synthesis pass; this module owns only the
-- expression-tree lowering and the naming strategy.
{-# LANGUAGE ExistentialQuantification #-}
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
import Data.Proxy (Proxy(..))
import GHC.TypeLits (natVal, KnownNat)

import Hdl.Net (WireId, NetM, NetNode(..), PrimOp, freshWire, emit, hintWire)
import qualified Hdl.Net as N
import Isacle.ISA.Types (ALUPrim(..), CPUFlag)
import Isacle.ISA.IR

-- | Resolution callbacks for the leaves an 'IExpr' can reference.  Supplied by
-- the CPU synthesis pass (which owns the register file, the field decoder and
-- the per-cycle read-result wires).
data LowerCtx = LowerCtx
    { lcReadReg   :: forall w. RegRef w -> NetM WireId
    , lcField     :: FieldRef -> NetM WireId
    , lcReadRes   :: ReadTok -> NetM WireId
    , lcReadFlag  :: CPUFlag -> NetM WireId
    , lcIrqVector :: NetM WireId
    , lcMnemonic  :: Maybe String   -- ^ instruction mnemonic, for field-wire naming
    }

-- | A lowered wire together with the name it carries, if any.  The name is what
-- lets derived operations be named after their inputs ("following").
data Named = Named
    { nWire :: WireId
    , nName :: Maybe String
    }

-- | Lower an expression, returning the wire and its propagated name.
lowerExpr :: forall w. LowerCtx -> IExpr w -> NetM Named
lowerExpr ctx = go
  where
    go :: forall k. IExpr k -> NetM Named
    go (INamed nm e) = do
        Named w _ <- go e
        hintWire w nm
        pure (Named w (Just nm))

    go e@(ILit v) = do
        let bw = widthOf e
        w <- freshWire
        emit $ NComb w (N.PLit v bw) []
        pure (Named w Nothing)               -- literals carry no propagated name

    go (IReadReg ref) = do
        w <- lcReadReg ctx ref
        let nm = regName ref
        hintWire w nm
        pure (Named w (Just nm))

    go (IField fr) = do
        w <- lcField ctx fr
        let nm = maybe id (\m s -> m ++ "_" ++ s) (lcMnemonic ctx) (frKey fr)
        hintWire w nm
        pure (Named w (Just nm))

    go (IReadRes tok@(ReadTok i)) = do
        w <- lcReadRes ctx tok
        let nm = "rd" ++ show i
        hintWire w nm
        pure (Named w (Just nm))

    go (IBin op a b) = do
        Named wa na <- go a
        Named wb nb <- go b
        w <- freshWire
        emit $ NComb w (toPrim op) [wa, wb]
        let nm = followName (opTag op) na nb
        maybe (pure ()) (hintWire w) nm
        pure (Named w nm)

    go (IUn op a) = do
        Named wa na <- go a
        w <- freshWire
        emit $ NComb w (toPrim op) [wa]
        let nm = fmap (\s -> opTag op ++ "_" ++ s) na
        maybe (pure ()) (hintWire w) nm
        pure (Named w nm)

    go e@(IResize a)  = resizeLike (widthOf e) a
    go e@(IZeroExt a) = resizeLike (widthOf e) a
    go e@(ITrunc a)   = do
        let dst = widthOf e
        Named wa na <- go a
        w <- freshWire
        emit $ NComb w (N.PSlice (dst - 1) 0) [wa]
        propagate w na
    go e@(ISignExt a) = do
        let dst = widthOf e
        Named wa na <- go a
        w <- freshWire
        emit $ NComb w (N.PSignedResize dst) [wa]
        propagate w na
    go (IIsZero a) = do
        let bw = widthOf a
        Named wa na <- go a
        zw <- freshWire
        emit $ NComb zw (N.PLit 0 bw) []
        w  <- freshWire
        emit $ NComb w N.PEq [wa, zw]
        let nm = fmap (\s -> s ++ "_isZero") na
        maybe (pure ()) (hintWire w) nm
        pure (Named w nm)

    go (ISlice hi lo a) = do
        Named wa na <- go a
        w <- freshWire
        emit $ NComb w (N.PSlice hi lo) [wa]
        propagate w na

    go (IFlagRead flag) = do
        w <- lcReadFlag ctx flag
        pure (Named w Nothing)

    go IIrqVector = do
        w <- lcIrqVector ctx
        hintWire w "irq_vector"
        pure (Named w (Just "irq_vector"))

    resizeLike :: forall k. Int -> IExpr k -> NetM Named
    resizeLike dst a = do
        Named wa na <- go a
        w <- freshWire
        emit $ NComb w (N.PResize dst) [wa]
        propagate w na

    propagate w na = do
        maybe (pure ()) (hintWire w) na
        pure (Named w na)

-- | Lower an expression, discarding the propagated name.
lowerExpr_ :: LowerCtx -> IExpr w -> NetM WireId
lowerExpr_ ctx e = nWire <$> lowerExpr ctx e

-- ---------------------------------------------------------------------------
-- Naming helpers
-- ---------------------------------------------------------------------------

regName :: RegRef w -> String
regName (RegScalar n)            = n
regName (RegFile f (FieldRef k)) = f ++ "_" ++ k

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

-- | The bit width of an expression, recovered from the @KnownNat@ dictionary
-- each constructor carries.
widthOf :: forall w. KnownNat w => IExpr w -> Int
widthOf _ = fromIntegral (natVal (Proxy @w))

-- ---------------------------------------------------------------------------
-- Statement-level rendering: InstrIR -> lowered write/read requests
-- ---------------------------------------------------------------------------

-- | A register write, lowered: the (annotated) destination plus the data wire.
data RegWrite = forall w. RegWrite (RegRef w) WireId

-- | A conditional jump, lowered: the PC register, the condition wire, the
-- target wire.
data Jump = forall w. Jump (RegRef w) WireId WireId

-- | One instruction's effects, lowered to wires.  This is the synth-side
-- analogue of the old @SynthResult@, but produced from the 'InstrIR' source of
-- truth instead of a leaky interpreter — the CPU pass assembles these into the
-- register file, memory ports and the execution sequencer.
data Rendered = Rendered
    { rRegWrites  :: [RegWrite]
    , rMemWrites  :: [(WireId, WireId)]    -- ^ (address, data) in program order
    , rMemReads   :: [(ReadTok, WireId)]   -- ^ (token, address) in program order
    , rCodeReads  :: [(ReadTok, WireId)]
    , rFlagWrites :: [(CPUFlag, WireId)]
    , rJumps      :: [Jump]
    }

emptyRendered :: Rendered
emptyRendered = Rendered [] [] [] [] [] []

-- | Lower every statement of an instruction's IR to wires, preserving program
-- order (which the sequencer relies on for memory transactions).  Each
-- expression is lowered through 'lowerExpr', so annotation-driven naming
-- applies throughout.
renderInstr :: LowerCtx -> InstrIR -> NetM Rendered
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
