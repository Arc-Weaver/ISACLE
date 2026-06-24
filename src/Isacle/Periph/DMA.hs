{-# LANGUAGE RecursiveDo #-}
module Isacle.Periph.DMA
    ( DMAState(..)
    , dmaEngine
    ) where

import Prelude
import Hdl.Net (freshWire, emit, NetNode(..), PrimOp(..), NetM)
import Hdl.Types
import Hdl.Class (regS)
import Hdl.Prim (Unsigned)

-- | DMA transfer engine state (used in simulation / pure testing).
--
--   In synthesis the state is flattened into individual registers by 'dmaEngine'.
data DMAState addr dat
    = DMAIdle
    | DMARead addr addr (Unsigned 16) Bool Bool
      -- src dst count incSrc incDst
    deriving (Show, Eq)

-- | Single-channel DMA transfer engine.
--
--   Performs autonomous block transfers between bus addresses without CPU
--   involvement.  The start signal is split into a valid flag and separate
--   field signals; a new transfer is accepted only when the DMA is idle and
--   @startVld@ is True and @startN@ is non-zero.
--
--   Transfer modes are controlled by @startIncSrc@ / @startIncDst@:
--     True  / True  — memory-to-memory  (both addresses increment each step)
--     True  / False — memory-to-peripheral (src increments, dst is fixed)
--     False / True  — peripheral-to-memory (src is fixed, dst increments)
--
--   Timing: a transfer of @n@ elements takes @n + 1@ clock cycles.
--     Cycle 0     : DMA issues first read; busy = True
--     Cycles 1…n-1: simultaneous write of previous data + read of next element
--     Cycle n     : final write committed; done fires; idle next cycle
--
--   Read and write outputs are valid/address (or valid/address/data) triples
--   rather than 'Maybe' wrappers, which cannot be synthesised directly.
dmaEngine
    :: forall dom addrW dat
     . ( KnownDom dom
       , HdlType (Unsigned addrW)
       , Num (Sig dom (Unsigned addrW))
       , HdlType (Unsigned 16)
       , Num (Sig dom (Unsigned 16))
       )
    => ( Sig dom Bool                -- ^ startVld
       , Sig dom (Unsigned addrW)    -- ^ startSrc
       , Sig dom (Unsigned addrW)    -- ^ startDst
       , Sig dom (Unsigned 16)       -- ^ startN   (element count, > 0)
       , Sig dom Bool                -- ^ startIncSrc
       , Sig dom Bool                -- ^ startIncDst
       )
    -> Sig dom dat                   -- ^ bus read response (one cycle after read address)
    -> NetM
        ( Sig dom Bool, Sig dom (Unsigned addrW)              -- dmaRd: (valid, addr)
        , Sig dom Bool, Sig dom (Unsigned addrW), Sig dom dat -- dmaWr: (valid, addr, dat)
        , Sig dom Bool                                         -- busy
        , Sig dom Bool                                         -- done
        )
dmaEngine (startVld, startSrc, startDst, startN, startIncSrc, startIncDst) rdData = mdo

    -- ── State registers ────────────────────────────────────────────────────────
    stVld    <- regS False stVldNext
    stSrc    <- regS 0     stSrcNext
    stDst    <- regS 0     stDstNext
    stCount  <- regS 0     stCountNext
    stIncSrc <- regS False stIncSrcNext
    stIncDst <- regS False stIncDstNext

    -- ── Combinational logic ────────────────────────────────────────────────────
    let sigTrue  = SExpr $ do { out <- freshWire; emit $ NComb out (PLit 1 1) []; pure out }
        sigFalse = SExpr $ do { out <- freshWire; emit $ NComb out (PLit 0 1) []; pure out }

        zero16 = fromInteger 0 :: Sig dom (Unsigned 16)
        one16  = fromInteger 1 :: Sig dom (Unsigned 16)

        -- Accept a new transfer when idle, valid start, and non-zero count
        idle    = sigNot stVld
        nonZero = sigNot (startN .==. zero16)
        accept  = idle .&&. startVld .&&. nonZero

        -- When in DMARead, is this the last element?
        isDone  = stVld .&&. (stCount .==. one16)

        -- Address advances for next read / next write destination
        nextSrc = mux stIncSrc (stSrc + 1) stSrc
        nextDst = mux stIncDst (stDst + 1) stDst

        -- ── Next-state ───────────────────────────────────────────────────────
        stVldNext    = mux accept sigTrue  (mux isDone sigFalse stVld)
        stSrcNext    = mux accept startSrc nextSrc
        stDstNext    = mux accept startDst nextDst
        stCountNext  = mux accept startN   (stCount - 1)
        stIncSrcNext = mux accept startIncSrc stIncSrc
        stIncDstNext = mux accept startIncDst stIncDst

        -- ── Outputs ──────────────────────────────────────────────────────────
        -- DMA read: issued on accept (first read) or when in DMARead and not done
        dmaRdVld  = accept .||. (stVld .&&. sigNot isDone)
        dmaRdAddr = mux accept startSrc nextSrc

        -- DMA write: data from previous cycle's read arrives while in DMARead
        dmaWrVld  = stVld
        dmaWrAddr = stDst
        dmaWrDat  = rdData

        busy = stVld .||. accept
        done = isDone

    pure (dmaRdVld, dmaRdAddr, dmaWrVld, dmaWrAddr, dmaWrDat, busy, done)
