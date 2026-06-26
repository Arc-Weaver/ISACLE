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
module Isacle.ISA.Backend.Lower
    ( LowerCtx(..)
    , Named(..)
    , lowerExpr
    , lowerExpr_
    ) where

import Prelude
import Data.Proxy (Proxy(..))
import GHC.TypeLits (natVal, KnownNat)

import Hdl.Net (WireId, NetM, NetNode(..), PrimOp, freshWire, emit, hintWire)
import qualified Hdl.Net as N
import Isacle.ISA.Types (ALUPrim(..))
import Isacle.ISA.IR

-- | Resolution callbacks for the leaves an 'IExpr' can reference.  Supplied by
-- the CPU synthesis pass (which owns the register file, the field decoder and
-- the per-cycle read-result wires).
data LowerCtx = LowerCtx
    { lcReadReg  :: forall w. RegRef w -> NetM WireId
    , lcField    :: FieldRef -> NetM WireId
    , lcReadRes  :: ReadTok -> NetM WireId
    , lcMnemonic :: Maybe String   -- ^ instruction mnemonic, for field-wire naming
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
