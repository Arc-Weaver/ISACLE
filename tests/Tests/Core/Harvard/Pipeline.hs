module Tests.Core.Harvard.Pipeline where

import Prelude hiding (read, repeat, (!!))

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.Hedgehog
import qualified Hedgehog as H

import qualified Clash.Prelude as C

import Core.Harvard.ISA
import Core.Harvard.Pipeline

import Tests.Core.Harvard.ISA
    ( TState(..), TInstr(..), TIsaStage(..)
    , initState, withZero, setReg, getReg
    )

-- ---------------------------------------------------------------------------
-- 2-slot pipeline helpers
-- ---------------------------------------------------------------------------

type P1 = PipeState 1 TInstr TIsaStage

emptyP1 :: P1
emptyP1 = emptyPipe

step1 :: P1 -> TState -> PipeInput TState -> (P1, TState, PipeOutput TState)
step1 = pipelineStep

noInp :: PipeInput TState
noInp = PipeInput Nothing Nothing Nothing

withInstr :: TInstr -> PipeInput TState
withInstr i = PipeInput (Just i) Nothing Nothing

-- | Feed an instruction into the 2-slot pipeline and advance it to the
--   execute head (slot 0).  The result is ready for the actual execute step.
primeExec :: TInstr -> TState -> (P1, TState)
primeExec instr s =
    let (ps1, s1, _) = step1 emptyP1 s  (withInstr instr)
        (ps2, s2, _) = step1 ps1     s1 noInp
    in (ps2, s2)

-- ---------------------------------------------------------------------------
-- Bubble behaviour
-- ---------------------------------------------------------------------------

prop_empty_pipe_no_output :: H.Property
prop_empty_pipe_no_output = H.withTests 1 . H.property $ do
    let (_, _, out) = step1 emptyP1 initState noInp
    pipeMemRead  out H.=== Nothing
    pipeMemWrite out H.=== Nothing
    pipeFlush    out H.=== Nothing
    pipeStalled  out H.=== False

-- An instruction fed into an empty pipeline enters at the tail (slot 1), not
-- the head (slot 0) — it needs one more advance before execution.
prop_bubble_accepts_new_instruction :: H.Property
prop_bubble_accepts_new_instruction = H.withTests 1 . H.property $ do
    let (ps', _, _) = step1 emptyP1 initState (withInstr TNop)
    C.head (psSlots ps') H.=== SEmpty
    (psSlots ps' C.!! (1 :: C.Index 2)) H.=== SReady TNop

-- ---------------------------------------------------------------------------
-- Single-cycle execution
-- ---------------------------------------------------------------------------

prop_nop_executes_and_advances :: H.Property
prop_nop_executes_and_advances = H.withTests 1 . H.property $ do
    let (ps0, s0) = primeExec TNop initState    -- TNop now at slot 0
    let (ps1, s1, out) = step1 ps0 s0 noInp    -- execute
    s1           H.=== initState
    pipeFlush out H.=== Nothing
    pipeStalled out H.=== False
    C.head (psSlots ps1) H.=== SEmpty

prop_add_updates_register :: H.Property
prop_add_updates_register = H.withTests 1 . H.property $ do
    let s0 = setReg 0 3 (setReg 1 4 initState)
    let (ps0, s0') = primeExec (TAdd 0 1) s0
    let (_, s1, _) = step1 ps0 s0' noInp
    getReg 0 s1 H.=== 7

prop_jump_flushes_pipeline :: H.Property
prop_jump_flushes_pipeline = H.withTests 1 . H.property $ do
    let (ps0, s0) = primeExec (TJump 0x42) initState
    let (ps1, _, out) = step1 ps0 s0 noInp
    pipeFlush    out H.=== Just (FlushBranch 0x42)
    pipeStalled  out H.=== False
    psSlots ps1 H.=== C.repeat SEmpty

-- ---------------------------------------------------------------------------
-- Memory read (load)
-- ---------------------------------------------------------------------------

prop_load_issues_read_request :: H.Property
prop_load_issues_read_request = H.withTests 1 . H.property $ do
    let (ps0, s0)      = primeExec (TLoad 0 0x10) initState
    let (ps1, _, out)  = step1 ps0 s0 noInp
    pipeMemRead out H.=== Just 0x10
    pipeStalled out H.=== True
    C.head (psSlots ps1) H.=== SMemRead (TLoad 0 0x10)

prop_load_stalls_without_response :: H.Property
prop_load_stalls_without_response = H.withTests 1 . H.property $ do
    let (ps0, s0) = primeExec (TLoad 0 0x10) initState
    let (ps1, s1, _)  = step1 ps0 s0 noInp   -- issues read
    let (ps2, _, out) = step1 ps1 s1 noInp   -- no response yet
    pipeStalled out H.=== True
    C.head (psSlots ps2) H.=== SMemRead (TLoad 0 0x10)

prop_load_completes_with_response :: H.Property
prop_load_completes_with_response = H.withTests 1 . H.property $ do
    let (ps0, s0) = primeExec (TLoad 2 0x10) initState
    let (ps1, s1, _) = step1 ps0 s0 noInp                                  -- issue read
    let (_, s2, out) = step1 ps1 s1 (noInp { pipeMemResp = Just 0xAB })    -- response
    pipeStalled out H.=== False
    getReg 2 s2 H.=== 0xAB

-- ---------------------------------------------------------------------------
-- Memory write (store)
-- ---------------------------------------------------------------------------

prop_store_issues_write :: H.Property
prop_store_issues_write = H.withTests 1 . H.property $ do
    let s0 = setReg 1 0xBE initState
    let (ps0, s0') = primeExec (TStore 0x20 1) s0
    let (_, _, out) = step1 ps0 s0' noInp
    pipeMemWrite out H.=== Just (0x20, 0xBE)

-- ---------------------------------------------------------------------------
-- Multi-cycle latency (TMul latency = 2)
-- ---------------------------------------------------------------------------

prop_mul_stalls_one_extra_cycle :: H.Property
prop_mul_stalls_one_extra_cycle = H.withTests 1 . H.property $ do
    let s0 = setReg 0 3 (setReg 1 4 initState)
    let (ps0, s0') = primeExec (TMul 0 1) s0     -- TMul at execute head
    -- First execution attempt: latency=2, so counts down (stall).
    let (ps1, s1, o1) = step1 ps0 s0' noInp
    pipeStalled o1 H.=== True
    -- Second attempt: countdown expired; execute.
    let (_, s2, o2) = step1 ps1 s1 noInp
    pipeStalled o2 H.=== False
    getReg 0 s2 H.=== 12   -- 3 * 4

-- ---------------------------------------------------------------------------
-- Conditional branch
-- ---------------------------------------------------------------------------

prop_brz_no_flush_when_zero_clear :: H.Property
prop_brz_no_flush_when_zero_clear = H.withTests 1 . H.property $ do
    let s0 = withZero False initState
    let (ps0, s0') = primeExec (TBrZ 0x30) s0
    let (_, _, out) = step1 ps0 s0' noInp
    pipeFlush out H.=== Nothing

prop_brz_flushes_when_zero_set :: H.Property
prop_brz_flushes_when_zero_set = H.withTests 1 . H.property $ do
    let s0 = withZero True initState
    let (ps0, s0') = primeExec (TBrZ 0x30) s0
    let (_, _, out) = step1 ps0 s0' noInp
    pipeFlush out H.=== Just (FlushBranch 0x30)

-- ---------------------------------------------------------------------------
-- Interrupt acceptance
-- ---------------------------------------------------------------------------

prop_irq_accepted_at_bubble :: H.Property
prop_irq_accepted_at_bubble = H.withTests 1 . H.property $ do
    let inp = PipeInput Nothing Nothing (Just 0xFF)
    let (_, _, out) = step1 emptyP1 initState inp
    pipeFlush   out H.=== Just (FlushInterrupt 0xFF)
    pipeStalled out H.=== False

prop_irq_clears_pipeline :: H.Property
prop_irq_clears_pipeline = H.withTests 1 . H.property $ do
    -- TState.acceptIrq returns (s, Nothing), so after IRQ head is SEmpty.
    let inp = PipeInput (Just TNop) Nothing (Just 0xFF)
    let (ps', _, _) = step1 emptyP1 initState inp
    psSlots ps' H.=== C.repeat SEmpty

-- ---------------------------------------------------------------------------
-- Complex sequences
-- ---------------------------------------------------------------------------

-- Two independent adds pipelined back-to-back: second instruction enters
-- the pipeline while the first is advancing to the execute head.
prop_back_to_back_adds :: H.Property
prop_back_to_back_adds = H.withTests 1 . H.property $ do
    let s0 = setReg 0 1 (setReg 1 2 (setReg 2 3 initState))
    -- Cycle 1: TAdd 0 1 enters pipeline tail.
    let (ps1, s1, _) = step1 emptyP1 s0 (withInstr (TAdd 0 1))
    -- Cycle 2: TAdd 0 1 advances to head; TAdd 1 2 enters tail simultaneously.
    let (ps2, s2, _) = step1 ps1 s1 (withInstr (TAdd 1 2))
    -- At this point both instructions are in the pipeline.
    C.head (psSlots ps2) H.=== SReady (TAdd 0 1)
    (psSlots ps2 C.!! (1 :: C.Index 2)) H.=== SReady (TAdd 1 2)
    -- Cycle 3: TAdd 0 1 executes (r0 = 1+2 = 3).
    let (ps3, s3, _) = step1 ps2 s2 noInp
    getReg 0 s3 H.=== 3
    C.head (psSlots ps3) H.=== SReady (TAdd 1 2)
    -- Cycle 4: TAdd 1 2 executes (r1 = 2+3 = 5).
    let (_, s4, _) = step1 ps3 s3 noInp
    getReg 1 s4 H.=== 5
    getReg 0 s4 H.=== 3   -- first result preserved

-- An instruction sitting in slot 1 when a flush fires must be discarded.
prop_flush_discards_in_flight_instruction :: H.Property
prop_flush_discards_in_flight_instruction = H.withTests 1 . H.property $ do
    -- Cycle 1: TJump enters tail.
    let (ps1, s1, _) = step1 emptyP1 initState (withInstr (TJump 0x42))
    -- Cycle 2: TJump advances to head; TNop enters tail.
    let (ps2, s2, _) = step1 ps1 s1 (withInstr TNop)
    C.head (psSlots ps2) H.=== SReady (TJump 0x42)
    (psSlots ps2 C.!! (1 :: C.Index 2)) H.=== SReady TNop
    -- Cycle 3: TJump executes → flush.  TNop must be gone.
    let (ps3, _, out) = step1 ps2 s2 noInp
    pipeFlush    out H.=== Just (FlushBranch 0x42)
    psSlots ps3 H.=== C.repeat SEmpty

-- A load followed by an add that uses the loaded register: the add must see
-- the value written by the load.
prop_load_then_add_uses_loaded_value :: H.Property
prop_load_then_add_uses_loaded_value = H.withTests 1 . H.property $ do
    let s0 = setReg 1 5 initState   -- r1 = 5; r0 will be loaded from RAM
    -- Cycle 1: TLoad 0 0x10 enters tail.
    let (ps1, s1, _) = step1 emptyP1 s0 (withInstr (TLoad 0 0x10))
    -- Cycle 2: TLoad advances to head; TAdd 0 1 enters tail.
    let (ps2, s2, _) = step1 ps1 s1 (withInstr (TAdd 0 1))
    -- Cycle 3: TLoad issues RAM read, transitions to SMemRead; TAdd stalls.
    let (ps3, s3, o3) = step1 ps2 s2 noInp
    pipeMemRead o3 H.=== Just 0x10
    pipeStalled o3 H.=== True
    C.head (psSlots ps3) H.=== SMemRead (TLoad 0 0x10)
    -- TAdd must still be in the pipeline (slot 1), not lost.
    (psSlots ps3 C.!! (1 :: C.Index 2)) H.=== SReady (TAdd 0 1)
    -- Cycle 4: no RAM response yet → entire pipeline holds.
    let (ps4, s4, o4) = step1 ps3 s3 noInp
    pipeStalled o4 H.=== True
    C.head (psSlots ps4) H.=== SMemRead (TLoad 0 0x10)
    -- Cycle 5: RAM responds with 7 → TLoad completes, r0 = 7.
    let (ps5, s5, _) = step1 ps4 s4 (noInp { pipeMemResp = Just 7 })
    getReg 0 s5 H.=== 7
    -- TAdd has advanced to head after TLoad completed.
    C.head (psSlots ps5) H.=== SReady (TAdd 0 1)
    -- Cycle 6: TAdd executes → r0 = 7 + 5 = 12.
    let (_, s6, _) = step1 ps5 s5 noInp
    getReg 0 s6 H.=== 12

-- A taken branch followed by two NOPs in flight: both must be flushed.
prop_flush_clears_multiple_in_flight :: H.Property
prop_flush_clears_multiple_in_flight = H.withTests 1 . H.property $ do
    -- Fill the pipeline: TBrZ in slot 0, TNop in slot 1, more incoming.
    let s0 = withZero True initState  -- branch will be taken
    let (ps1, s1, _) = step1 emptyP1 s0 (withInstr (TBrZ 0x30))
    let (ps2, s2, _) = step1 ps1 s1 (withInstr TNop)
    -- Fire: branch executes while NOPs are in flight.
    let (ps3, _, out) = step1 ps2 s2 (withInstr TNop)
    pipeFlush out H.=== Just (FlushBranch 0x30)
    psSlots ps3 H.=== C.repeat SEmpty

-- After a flush the pipeline refills normally from the next instruction.
prop_refill_after_flush :: H.Property
prop_refill_after_flush = H.withTests 1 . H.property $ do
    -- Prime and execute a jump to flush.
    let (ps0, s0) = primeExec (TJump 0x50) initState
    let (ps1, s1, out1) = step1 ps0 s0 noInp
    pipeFlush out1 H.=== Just (FlushBranch 0x50)
    psSlots ps1 H.=== C.repeat SEmpty
    -- Feed a NOP into the cleared pipeline.
    let (ps2, _, _) = step1 ps1 s1 (withInstr TNop)
    -- NOP should appear at the tail (slot 1) of the refilling pipeline.
    (psSlots ps2 C.!! (1 :: C.Index 2)) H.=== SReady TNop

-- Interrupt accepted when head is a bubble; subsequent instructions are
-- discarded from the pipeline.
prop_irq_discards_in_flight_on_accept :: H.Property
prop_irq_discards_in_flight_on_accept = H.withTests 1 . H.property $ do
    -- Prime a NOP so it sits in slot 1 (head is still SEmpty).
    let (ps1, s1, _) = step1 emptyP1 initState (withInstr TNop)
    C.head (psSlots ps1) H.=== SEmpty
    (psSlots ps1 C.!! (1 :: C.Index 2)) H.=== SReady TNop
    -- Present IRQ while head is a bubble.
    let irqInp = PipeInput (Just TNop) Nothing (Just 0x80)
    let (ps2, _, out) = step1 ps1 s1 irqInp
    pipeFlush out H.=== Just (FlushInterrupt 0x80)
    -- All pipeline slots must be empty (TState.acceptIrq returns Nothing stage).
    psSlots ps2 H.=== C.repeat SEmpty

-- Two consecutive store instructions must each generate a distinct write.
prop_back_to_back_stores :: H.Property
prop_back_to_back_stores = H.withTests 1 . H.property $ do
    let s0 = setReg 0 0xAA (setReg 1 0xBB initState)
    -- Fill pipeline with two stores.
    let (ps1, s1, _) = step1 emptyP1 s0 (withInstr (TStore 0x10 0))
    let (ps2, s2, _) = step1 ps1 s1 (withInstr (TStore 0x20 1))
    -- Execute first store.
    let (ps3, s3, o3) = step1 ps2 s2 noInp
    pipeMemWrite o3 H.=== Just (0x10, 0xAA)
    -- Execute second store.
    let (_, _, o4) = step1 ps3 s3 noInp
    pipeMemWrite o4 H.=== Just (0x20, 0xBB)

pipelineTests :: TestTree
pipelineTests = $(testGroupGenerator)
