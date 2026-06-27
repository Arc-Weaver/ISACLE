-- | A simulation interpreter for the 'Signal' typeclass: a second instance
-- alongside the synthesis 'Sig', realizing the tagless-final design — the same
-- combinational signal program is either /synthesised/ (→ NetNode graph, 'Sig')
-- or /computed/ (→ a value, 'SimSig').
--
-- 'SimSig' carries the bit-pattern value, its width, and its representation
-- ('Repr'), so the 'Signal' operations compute exactly what the emitter would
-- produce (signed-vs-unsigned comparison via the representation, etc.).
module Hdl.Sim
    ( SimSig(..)
    , simLit
    , simResult
    , evalSimOp
    ) where

import Prelude
import Data.Bits ((.&.), (.|.), xor, shiftL, shiftR)
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (natVal)

import Hdl.Net   (PrimOp(..), Repr(..))
import Hdl.Types (Signal(..), HdlType(..))

-- | A simulated combinational signal: @SimSig value width repr@.
data SimSig (dom :: k) (a :: Type) = SimSig !Integer !Int !Repr
    deriving (Eq, Show)

-- | Inject an 'HdlType' value: bit pattern from 'toBits', width from 'Width',
-- representation from 'hdlRepr'.
simLit :: forall a dom. HdlType a => a -> SimSig dom a
simLit x = SimSig (toBits x) (fromIntegral (natVal (Proxy @(Width a)))) (hdlRepr (Proxy @a))

-- | Read a simulated signal back as its 'HdlType' value.
simResult :: HdlType a => SimSig dom a -> a
simResult (SimSig v _ _) = fromBits v

instance Signal SimSig where
    sigPrim1 op (SimSig a wa ra)                    = mk (evalSimOp op [(a, wa, ra)])
    sigPrim2 op (SimSig a wa ra) (SimSig b wb rb)   = mk (evalSimOp op [(a, wa, ra), (b, wb, rb)])
    sigPrim3 op (SimSig a wa ra) (SimSig b wb rb) (SimSig c wc rc) =
        mk (evalSimOp op [(a, wa, ra), (b, wb, rb), (c, wc, rc)])
    sigLitW v w = SimSig (v .&. mask w) w RUnsigned

-- | Numeric literals/arithmetic in simulation (mirrors the 'Num (Sig dom a)'
-- synthesis instance, on actual values).
instance (HdlType a, Num a) => Num (SimSig dom a) where
    a + b         = sigPrim2 PAdd a b
    a - b         = sigPrim2 PSub a b
    a * b         = sigPrim2 PMul a b
    negate a      = sigPrim1 PNot a
    abs           = id
    signum _      = fromInteger 1
    fromInteger n = simLit (fromInteger n :: a)

mk :: (Integer, Int, Repr) -> SimSig dom a
mk (v, w, r) = SimSig v w r

mask :: Int -> Integer
mask w = (1 `shiftL` w) - 1

asSigned :: Integer -> Int -> Bool -> Integer
asSigned v w True  | v >= 1 `shiftL` (w - 1) = v - (1 `shiftL` w)
asSigned v _ _     = v

-- | Evaluate a 'PrimOp' on @(value, width, repr)@ operands, matching the VHDL
-- emitter's semantics.  Returns @(value, width, repr)@.
evalSimOp :: PrimOp -> [(Integer, Int, Repr)] -> (Integer, Int, Repr)
evalSimOp op ins = case (op, ins) of
    (PLit v w, [])                          -> (v .&. mask w, w, RUnsigned)
    (PAdd, [(a, wa, ra), (b, wb, _)])       -> let w = max wa wb in ((a + b) .&. mask w, w, ra)
    (PSub, [(a, wa, ra), (b, wb, _)])       -> let w = max wa wb in ((a - b) .&. mask w, w, ra)
    (PMul, [(a, wa, ra), (b, wb, _)])       -> let w = max wa wb in ((a * b) .&. mask w, w, ra)
    (PAnd, [(a, wa, ra), (b, wb, _)])       -> let w = max wa wb in (a .&. b, w, ra)
    (POr,  [(a, wa, ra), (b, wb, _)])       -> let w = max wa wb in (a .|. b, w, ra)
    (PXor, [(a, wa, ra), (b, wb, _)])       -> let w = max wa wb in (a `xor` b, w, ra)
    (PNot, [(a, wa, ra)])                   -> (mask wa `xor` a, wa, ra)
    (PMux, [(s, _, _), (t, wt, rt), (f, wf, _)])
                                            -> (if s /= 0 then t else f, max wt wf, rt)
    (PEq,  [(a, _, _), (b, _, _)])          -> (if a == b then 1 else 0, 1, RUnsigned)
    (PLt,  [(a, wa, ra), (b, wb, rb)])      ->
        let signed = ra == RSigned || rb == RSigned
        in (if asSigned a wa signed < asSigned b wb signed then 1 else 0, 1, RUnsigned)
    (PSlice hi lo, [(a, _, _)])             -> ((a `shiftR` lo) .&. mask (hi - lo + 1), hi - lo + 1, RUnsigned)
    (PConcat, [(a, _, _), (b, wb, _)])      -> ((a `shiftL` wb) .|. b, wb + msbW, RUnsigned)
      where msbW = case ins of { ((_, wa, _) : _) -> wa; _ -> 0 }
    (PResize w, [(a, wa, ra)])              -> (a .&. mask w, w, if w < wa then RUnsigned else ra)
    (PReinterpret r, [(a, wa, _)])          -> (a, wa, r)
    (PShiftL, [(a, wa, ra), (n, _, _)])     -> ((a `shiftL` fromInteger n) .&. mask wa, wa, ra)
    (PShiftR, [(a, wa, ra), (n, _, _)])     -> (a `shiftR` fromInteger n, wa, ra)
    _                                       -> error ("evalSimOp: unhandled " ++ show op)
