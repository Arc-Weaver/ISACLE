{-# LANGUAGE AllowAmbiguousTypes #-}
module Isacle.Harvard.Pipeline
    ( PipeState(..)
    , PipeInput(..)
    , PipeOutput(..)
    , emptyPipe
    , pipelineStep
    ) where

import Prelude hiding (read)
import Data.Maybe (fromMaybe)
import Isacle.Harvard.ISA

-- | Pipeline register state: a list of slots (head = execute end, last = fetch end)
--   plus a latency countdown.
data PipeState instr stage = PipeState
    { psSlots   :: [Slot instr stage]
    , psLatency :: Int
    } deriving (Show, Eq)

-- | All-bubble initial state for a pipeline of @depth@ stages.
emptyPipe :: Int -> PipeState instr stage
emptyPipe depth = PipeState (replicate depth SEmpty) 0

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

-- | Advance the pipeline by one clock cycle (pure; wrap in a clocked register
--   for synthesis).
--
-- Instructions flow from the last slot (fetch end) toward the first slot
-- (execute end) each cycle. On a flush the entire pipeline is cleared and the
-- fetch unit is redirected via 'pipeFlush'. On a stall 'pipeStalled' is True
-- and the fetch unit must re-present the same instruction next cycle.
pipelineStep
    :: forall state. HasFlush state
    => PipeState (Instr state) (IsaStage state)
    -> state
    -> PipeInput state
    -> ( PipeState (Instr state) (IsaStage state)
       , state
       , PipeOutput state
       )
pipelineStep (PipeState slots lat) cpuState inp =
    let depth    = length slots
        execSlot = head slots
        rest     = tail slots
        newSlot  = maybe SEmpty SReady (pipeInstr inp)
        advance  = rest ++ [newSlot]            -- shift toward head, admit new
        cleared  = replicate depth SEmpty       -- all bubbles
    in case execSlot of

      -- ── Bubble ────────────────────────────────────────────────────────────
      SEmpty ->
          case pipeIrqAddr inp of
              Just irqAddr | interruptible cpuState ->
                  let (cpu', mstage) = acceptIrq cpuState irqAddr
                      headSlot       = maybe SEmpty SIsa mstage
                      slots'         = headSlot : replicate (depth - 1) SEmpty
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
      SIsa stage ->
          let memVal       = fromMaybe
                                 (error "Pipeline: ISA stage needs memory response")
                                 (pipeMemResp inp)
              (cpu', done) = isaStageStep stage
                                 (error "Pipeline: ISA stage ROM feed unimplemented; carry ROM data in stage value", memVal)
                                 cpuState
          in case done of
              Left  stage' ->
                  ( PipeState (SIsa stage' : rest) 0
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
          let initialLat   = max 1 (latency @state instr) - 1
              effectiveLat = if lat == 0 then initialLat else lat - 1
          in if effectiveLat > 0
              then
                  ( PipeState slots effectiveLat
                  , cpuState
                  , PipeOutput Nothing Nothing Nothing True
                  )
              else case toIsaStage instr cpuState of
                  Just stage ->
                      ( PipeState (SIsa stage : rest) 0
                      , cpuState
                      , PipeOutput Nothing Nothing Nothing True
                      )
                  Nothing ->
                      case read instr cpuState of
                          Just addr ->
                              ( PipeState (SMemRead instr : rest) 0
                              , cpuState
                              , PipeOutput (Just addr) Nothing Nothing True
                              )
                          Nothing ->
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
