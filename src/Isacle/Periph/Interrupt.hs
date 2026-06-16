module Isacle.Periph.Interrupt
    ( interruptArbiter
    ) where

import Prelude
import Isacle.Hdl.Net (freshWire, emit, NetNode(..), PrimOp(..))
import Isacle.Hdl.Types

-- | Combinational priority interrupt arbiter.
--
--   Sources are in priority order: index 0 = highest priority.  When multiple
--   sources are active simultaneously the lowest-index request wins.
--
--   The output is gated by @iEnabled@ (global interrupt enable flag).
--   Returns @(valid, address)@: @valid@ is True only when @iEnabled@ is True
--   and at least one source is active.
--
--   Parameterised over @addr@ so the same arbiter works for any ISA's
--   interrupt vector address type.  Pass interrupt vector addresses as
--   constant signals: @(reqSig, fromInteger 0x20)@.
interruptArbiter
    :: Num (Sig dom addr)
    => [(Sig dom Bool, Sig dom addr)]   -- ^ (request, vector address), index 0 = highest priority
    -> Sig dom Bool                     -- ^ global interrupt enable
    -> (Sig dom Bool, Sig dom addr)     -- ^ (valid, selected vector)
interruptArbiter sources iEnabled = (iEnabled .&&. anyActive, winner)
  where
    sigFalse = SExpr $ do
        out <- freshWire
        emit $ NComb out (PLit 0 1) []
        pure out

    anyActive = foldr (.||.) sigFalse (map fst sources)

    winner = foldr (\(req, vec) acc -> mux req vec acc) (fromInteger 0) sources
