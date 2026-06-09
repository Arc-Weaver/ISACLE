{-# LANGUAGE AllowAmbiguousTypes #-}
module Core.Harvard.Pipeline
    ( PipeState(..)
    , PipeInput(..)
    , PipeOutput(..)
    , emptyPipe
    , pipelineStep
    ) where

import Clash.Prelude hiding (read)
import Data.Maybe (fromMaybe)
import Core.Harvard.ISA

-- | Pipeline register state: n+1 slots plus a latency countdown.
--   Slot 0 is the execute (head) end; slot n is the fetch (tail) end.
data PipeState n instr stage = PipeState
    { psSlots   :: Vec (n+1) (Slot instr stage)
    , psLatency :: Unsigned 8
    } deriving (Generic, NFDataX)

-- | All-bubble initial state.
emptyPipe :: KnownNat n => PipeState n instr stage
emptyPipe = PipeState (repeat SEmpty) 0

-- | Inputs consumed each cycle.
data PipeInput state = PipeInput
    { pipeInstr   :: Maybe (Instr state)    -- decoded instruction from fetch
    , pipeMemResp :: Maybe (Val state)      -- data RAM read response
    , pipeIrqAddr :: Maybe (RomAddr state)  -- interrupt vector (Nothing = no IRQ)
    }

-- | Outputs produced each cycle.
data PipeOutput state = PipeOutput
    { pipeMemRead  :: Maybe (RamAddr state)              -- data RAM read request
    , pipeMemWrite :: Maybe (RamAddr state, Val state)   -- data RAM write
    , pipeFlush    :: Maybe (FlushEvent (RomAddr state)) -- PC redirect event
    , pipeStalled  :: Bool                               -- True = freeze fetch
    }

-- | Advance the pipeline by one clock cycle (pure; wrap in 'mealy' for hardware).
--
-- Instructions flow from slot n (fetch end) toward slot 0 (execute end) each
-- cycle. On a flush the entire pipeline is cleared and the fetch unit is
-- redirected via 'pipeFlush'. On a stall 'pipeStalled' is True and the fetch
-- unit must re-present the same instruction next cycle.
pipelineStep
    :: forall state n. (HasFlush state, KnownNat n)
    => PipeState n (Instr state) (IsaStage state)
    -> state
    -> PipeInput state
    -> ( PipeState n (Instr state) (IsaStage state)
       , state
       , PipeOutput state
       )
pipelineStep (PipeState slots lat) cpuState inp =
    let execSlot = head slots
        rest     = tail slots          -- Vec n
        newSlot  = maybe SEmpty SReady (pipeInstr inp)
        advance  = rest :< newSlot     -- Vec (n+1): shift toward head, admit new
        cleared  = repeat SEmpty       -- Vec (n+1): all bubbles
    in case execSlot of

      -- ── Bubble ────────────────────────────────────────────────────────────
      SEmpty ->
          case pipeIrqAddr inp of
              Just irqAddr | interruptible cpuState ->
                  let (cpu', mstage) = acceptIrq cpuState irqAddr
                      headSlot       = maybe SEmpty SIsa mstage
                      -- headSlot occupies slot 0; rest of pipeline is clear
                      slots'         = headSlot :> (repeat SEmpty :: Vec n (Slot (Instr state) (IsaStage state)))
                  in ( PipeState slots' 0
                     , cpu'
                     , PipeOutput Nothing Nothing (Just (FlushInterrupt irqAddr)) False
                     )
              _ ->
                  ( PipeState advance 0
                  , cpuState
                  , PipeOutput Nothing Nothing Nothing False
                  )

      -- ── ISA-specific multi-cycle stage ────────────────────────────────────
      -- Note: isaStageStep's ROM-word argument is not yet wired through the
      -- pipeline; ISA stages that need ROM data should carry it in their stage
      -- type.
      SIsa stage ->
          let memVal       = fromMaybe
                                 (errorX "Core.Harvard.Pipeline: ISA-specific stage requires a memory response, but none was provided")
                                       (pipeMemResp inp)
              (cpu', done) = isaStageStep stage
                                 (errorX "Core.Harvard.Pipeline: ISA-specific stage ROM feed is intentionally unimplemented; carry any required ROM data in the stage value", memVal)
                                 cpuState
          in case done of
              Left  stage' ->
                  ( PipeState (SIsa stage' :> rest) 0
                  , cpu'
                  , PipeOutput Nothing Nothing Nothing True
                  )
              Right () ->
                  ( PipeState advance 0
                  , cpu'
                  , PipeOutput Nothing Nothing Nothing False
                  )

      -- ── Waiting for data RAM response ─────────────────────────────────────
      SMemRead instr ->
          case pipeMemResp inp of
              Nothing ->
                  -- Hold everything until RAM responds.
                  ( PipeState slots lat
                  , cpuState
                  , PipeOutput Nothing Nothing Nothing True
                  )
              Just val ->
                  let cpu'   = compute instr (Just val) cpuState
                      mwrite = write instr cpu'
                      mflush = flushCondition instr cpu'
                      slots' = case mflush of
                                   Just _  -> cleared
                                   Nothing -> advance
                  in ( PipeState slots' 0
                     , cpu'
                     , PipeOutput Nothing mwrite mflush False
                     )

      -- ── Instruction ready to execute ──────────────────────────────────────
      SReady instr ->
          -- Count down any multi-cycle latency before executing.
          let initialLat   = fromIntegral (max 1 (latency @state instr) - 1) :: Unsigned 8
              effectiveLat = if lat == 0 then initialLat else lat - 1
          in if effectiveLat > 0
              then
                  ( PipeState slots effectiveLat
                  , cpuState
                  , PipeOutput Nothing Nothing Nothing True
                  )
              else case toIsaStage instr cpuState of
                  Just stage ->
                      -- Hand off to ISA-specific multi-cycle execution.
                      ( PipeState (SIsa stage :> rest) 0
                      , cpuState
                      , PipeOutput Nothing Nothing Nothing True
                      )
                  Nothing ->
                      case read instr cpuState of
                          Just addr ->
                              -- Issue data RAM read; slot becomes SMemRead.
                              ( PipeState (SMemRead instr :> rest) 0
                              , cpuState
                              , PipeOutput (Just addr) Nothing Nothing True
                              )
                          Nothing ->
                              -- Single-cycle execute.
                              let cpu'   = compute instr Nothing cpuState
                                  mwrite = write instr cpu'
                                  mflush = flushCondition instr cpu'
                                  slots' = case mflush of
                                               Just _  -> cleared
                                               Nothing -> advance
                              in ( PipeState slots' 0
                                 , cpu'
                                 , PipeOutput Nothing mwrite mflush False
                                 )
