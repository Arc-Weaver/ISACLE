-- | Tiny combinator vocabulary over the netlist builder, so the ISA backend
-- reads as expressions instead of a wall of @freshWire; emit $ NComb …; pure@.
--
-- @comb op ins@ allocates a fresh wire, emits one 'NComb' node driving it, and
-- returns it; 'comb1'\/'comb2'\/'comb3' are the fixed-arity forms.  Everything
-- else (literals, identity buffers, reductions, mux trees) is built on top.
module Isacle.ISA.Backend.Wire
    ( comb, comb1, comb2, comb3
    , litW
    , named
    , identW
    , orReduce
    , priorityMux
    , andW, orW, notW, eqW, muxW, resizeW, sliceW
    ) where

import Prelude
import Data.List (foldl')
import Hdl.Net (WireId, NetM, NetNode(NComb), PrimOp, freshWire, emit, hintWire)
import qualified Hdl.Net as N

-- | Emit one combinational node and return its output wire.
comb :: PrimOp -> [WireId] -> NetM WireId
comb op ins = do { o <- freshWire; emit (NComb o op ins); pure o }

comb1 :: PrimOp -> WireId -> NetM WireId
comb1 op a = comb op [a]

comb2 :: PrimOp -> WireId -> WireId -> NetM WireId
comb2 op a b = comb op [a, b]

comb3 :: PrimOp -> WireId -> WireId -> WireId -> NetM WireId
comb3 op a b c = comb op [a, b, c]

-- | A literal constant wire.
litW :: Integer -> Int -> NetM WireId
litW v w = comb (N.PLit v w) []

-- | Attach a name hint to a wire and return it (so it threads through a @do@).
named :: String -> WireId -> NetM WireId
named nm w = hintWire w nm >> pure w

-- | Identity buffer: drive @dst@ from @src@ (@POr(x,x) = x@), a no-op when equal.
-- Used to forward a value onto a pre-allocated wire (register inputs, shared
-- read outputs).  The VHDL emitter renders this as a direct assignment.
identW :: WireId -> WireId -> NetM ()
identW dst src
    | dst == src = pure ()
    | otherwise  = emit (NComb dst N.POr [src, src])

-- Named binary/unary shorthands (the ops the backend reaches for most).
andW, orW, eqW :: WireId -> WireId -> NetM WireId
andW = comb2 N.PAnd
orW  = comb2 N.POr
eqW  = comb2 N.PEq

notW :: WireId -> NetM WireId
notW = comb1 N.PNot

muxW :: WireId -> WireId -> WireId -> NetM WireId
muxW = comb3 N.PMux

resizeW :: Int -> WireId -> NetM WireId
resizeW w a = comb1 (N.PResize w) a

sliceW :: Int -> Int -> WireId -> NetM WireId
sliceW hi lo a = comb1 (N.PSlice hi lo) a

-- | OR-reduce a list of 1-bit wires; constant-0 when empty.
orReduce :: [WireId] -> NetM WireId
orReduce []     = litW 0 1
orReduce (w:ws) = foldl' (\m x -> m >>= \a -> orW a x) (pure w) ws

-- | Right-to-left priority mux: the /first/ matching pair wins, else @def@.
priorityMux :: [(WireId, WireId)] -> WireId -> NetM WireId
priorityMux pairs def = foldr step (pure def) pairs
  where step (sel, v) acc = acc >>= muxW sel v
