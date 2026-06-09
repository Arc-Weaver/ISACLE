module Tests.Core.Harvard.ISA where

import Prelude hiding (read, repeat, (!!))

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.Hedgehog
import qualified Hedgehog as H

import Clash.Prelude (Unsigned, Vec, Index, NFDataX, Generic, BitVector, repeat, replace, (!!))

import Core.Harvard.ISA

-- ---------------------------------------------------------------------------
-- Minimal test ISA
--
-- A tiny 4-register 8-bit machine: just enough to exercise every typeclass
-- method without importing any real ISA implementation.
-- ---------------------------------------------------------------------------

type TAddr = Unsigned 8
type TVal  = Unsigned 8
type TReg  = Index 4

data TState = TState
    { tRegs :: Vec 4 TVal
    , tPC   :: TAddr
    , tZero :: Bool
    } deriving (Show, Eq, Generic, NFDataX)

data TInstr
    = TNop
    | TAdd   TReg TReg    -- Rd += Rs; sets zero flag
    | TLoad  TReg TAddr   -- Rd = RAM[addr]
    | TStore TAddr TReg   -- RAM[addr] = Rs
    | TJump  TAddr        -- PC = addr (unconditional)
    | TBrZ   TAddr        -- PC = addr if zero flag set
    | TMul   TReg TReg    -- Rd *= Rs; latency 2
    deriving (Show, Eq, Generic, NFDataX)

-- Placeholder: this ISA has no multi-cycle stages beyond the generic ones.
data TIsaStage = TIsaStage
    deriving (Show, Eq, Generic, NFDataX)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

initState :: TState
initState = TState (repeat 0) 0 False

withZero :: Bool -> TState -> TState
withZero z s = s { tZero = z }

getReg :: TReg -> TState -> TVal
getReg r s = tRegs s !! r

setReg :: TReg -> TVal -> TState -> TState
setReg r v s = s { tRegs = replace r v (tRegs s) }

-- ---------------------------------------------------------------------------
-- ALU instance
-- ---------------------------------------------------------------------------

instance ALU TState where
    type Instr   TState = TInstr
    type RamAddr TState = TAddr
    type RomAddr TState = TAddr
    type Val     TState = TVal

    read (TLoad _ a) _ = Just a
    read _           _ = Nothing

    compute TNop         _     s = s
    compute (TAdd rd rs) _     s =
        let r  = getReg rd s + getReg rs s
            s' = setReg rd r s
        in s' { tZero = r == 0 }
    compute (TLoad rd _) mval  s = maybe s (\v -> setReg rd v s) mval
    compute (TStore _ _) _     s = s
    compute (TJump _)    _     s = s
    compute (TBrZ _)     _     s = s
    compute (TMul rd rs) _     s =
        let r = getReg rd s * getReg rs s
        in setReg rd r s

    write (TStore a rs) s = Just (a, getReg rs s)
    write _             _ = Nothing

    move (TJump a) _ = Just a
    move (TBrZ  a) s = if tZero s then Just a else Nothing
    move _         _ = Nothing

-- ---------------------------------------------------------------------------
-- ISA instance
-- ---------------------------------------------------------------------------

instance ISA TState where
    type IsaStage TState = TIsaStage
    -- FetchWord: 8-bit fetch units (not used by the current test pipeline,
    -- but fixes the Harvard code-bus type for this ISA).
    type FetchWord TState = Unsigned 8
    -- MaxFetch: all test instructions fit in a single fetch word.
    type MaxFetch  TState = 1

    latency (TMul _ _) = 2
    latency _          = 1

    toIsaStage _ _ = Nothing

    isaStageStep TIsaStage _ s = (s, Right ())

    interruptible _ = True

    acceptIrq s _ = (s, Nothing)

-- ---------------------------------------------------------------------------
-- HasFlush instance (all defaults)
-- ---------------------------------------------------------------------------

instance HasFlush TState

-- ---------------------------------------------------------------------------
-- Convenience: resolve the ambiguous HasFlush instance for stall tests
-- ---------------------------------------------------------------------------

stall :: TAddr -> TAddr -> Maybe (StallEvent TAddr)
stall = stallCondition @TState

-- ---------------------------------------------------------------------------
-- ALU.read tests
-- ---------------------------------------------------------------------------

prop_read_load_returns_address :: H.Property
prop_read_load_returns_address = H.withTests 1 . H.property $
    read (TLoad 0 0x42) initState H.=== Just 0x42

prop_read_nop_returns_nothing :: H.Property
prop_read_nop_returns_nothing = H.withTests 1 . H.property $
    read TNop initState H.=== Nothing

prop_read_store_returns_nothing :: H.Property
prop_read_store_returns_nothing = H.withTests 1 . H.property $
    read (TStore 0x10 0) initState H.=== Nothing

prop_read_jump_returns_nothing :: H.Property
prop_read_jump_returns_nothing = H.withTests 1 . H.property $
    read (TJump 0x20) initState H.=== Nothing

prop_read_add_returns_nothing :: H.Property
prop_read_add_returns_nothing = H.withTests 1 . H.property $
    read (TAdd 0 1) initState H.=== Nothing

-- ---------------------------------------------------------------------------
-- ALU.write tests
-- ---------------------------------------------------------------------------

prop_write_store_returns_addr_and_val :: H.Property
prop_write_store_returns_addr_and_val = H.withTests 1 . H.property $ do
    let s = setReg 1 0xAB initState
    write (TStore 0x10 1) s H.=== Just (0x10, 0xAB)

prop_write_nop_returns_nothing :: H.Property
prop_write_nop_returns_nothing = H.withTests 1 . H.property $
    write TNop initState H.=== Nothing

prop_write_load_returns_nothing :: H.Property
prop_write_load_returns_nothing = H.withTests 1 . H.property $
    write (TLoad 0 0x42) initState H.=== Nothing

prop_write_jump_returns_nothing :: H.Property
prop_write_jump_returns_nothing = H.withTests 1 . H.property $
    write (TJump 0x20) initState H.=== Nothing

-- ---------------------------------------------------------------------------
-- ALU.move tests
-- ---------------------------------------------------------------------------

prop_move_jump_always_taken :: H.Property
prop_move_jump_always_taken = H.withTests 1 . H.property $
    move (TJump 0x20) initState H.=== Just 0x20

prop_move_brz_taken_when_zero_set :: H.Property
prop_move_brz_taken_when_zero_set = H.withTests 1 . H.property $
    move (TBrZ 0x30) (withZero True initState) H.=== Just 0x30

prop_move_brz_not_taken_when_zero_clear :: H.Property
prop_move_brz_not_taken_when_zero_clear = H.withTests 1 . H.property $
    move (TBrZ 0x30) (withZero False initState) H.=== Nothing

prop_move_nop_returns_nothing :: H.Property
prop_move_nop_returns_nothing = H.withTests 1 . H.property $
    move TNop initState H.=== Nothing

prop_move_store_returns_nothing :: H.Property
prop_move_store_returns_nothing = H.withTests 1 . H.property $
    move (TStore 0x10 0) initState H.=== Nothing

-- ---------------------------------------------------------------------------
-- ALU.compute tests
-- ---------------------------------------------------------------------------

prop_compute_add_updates_register :: H.Property
prop_compute_add_updates_register = H.withTests 1 . H.property $ do
    let s = setReg 0 3 (setReg 1 4 initState)
    getReg 0 (compute (TAdd 0 1) Nothing s) H.=== 7

prop_compute_add_sets_zero_flag_on_overflow :: H.Property
prop_compute_add_sets_zero_flag_on_overflow = H.withTests 1 . H.property $ do
    -- 0xFF + 0x01 wraps to 0x00 → zero flag set
    let s = setReg 0 0xFF (setReg 1 0x01 initState)
    tZero (compute (TAdd 0 1) Nothing s) H.=== True

prop_compute_add_clears_zero_flag_on_nonzero :: H.Property
prop_compute_add_clears_zero_flag_on_nonzero = H.withTests 1 . H.property $ do
    let s = setReg 0 1 (setReg 1 1 (withZero True initState))
    tZero (compute (TAdd 0 1) Nothing s) H.=== False

prop_compute_load_stores_supplied_value :: H.Property
prop_compute_load_stores_supplied_value = H.withTests 1 . H.property $
    getReg 2 (compute (TLoad 2 0x00) (Just 0xBE) initState) H.=== 0xBE

prop_compute_load_without_value_leaves_state_unchanged :: H.Property
prop_compute_load_without_value_leaves_state_unchanged = H.withTests 1 . H.property $
    compute (TLoad 0 0x00) Nothing initState H.=== initState

prop_compute_nop_leaves_state_unchanged :: H.Property
prop_compute_nop_leaves_state_unchanged = H.withTests 1 . H.property $
    compute TNop Nothing initState H.=== initState

prop_compute_jump_leaves_state_unchanged :: H.Property
prop_compute_jump_leaves_state_unchanged = H.withTests 1 . H.property $
    compute (TJump 0x20) Nothing initState H.=== initState

-- ---------------------------------------------------------------------------
-- Flush tests
-- ---------------------------------------------------------------------------

prop_flush_jump_produces_flush_branch :: H.Property
prop_flush_jump_produces_flush_branch = H.withTests 1 . H.property $
    flushCondition (TJump 0x20) initState H.=== Just (FlushBranch 0x20)

prop_flush_brz_taken_produces_flush_branch :: H.Property
prop_flush_brz_taken_produces_flush_branch = H.withTests 1 . H.property $
    flushCondition (TBrZ 0x30) (withZero True initState) H.=== Just (FlushBranch 0x30)

prop_flush_brz_not_taken_produces_nothing :: H.Property
prop_flush_brz_not_taken_produces_nothing = H.withTests 1 . H.property $
    flushCondition (TBrZ 0x30) (withZero False initState) H.=== Nothing

prop_flush_nop_produces_nothing :: H.Property
prop_flush_nop_produces_nothing = H.withTests 1 . H.property $
    flushCondition TNop initState H.=== Nothing

prop_flush_store_produces_nothing :: H.Property
prop_flush_store_produces_nothing = H.withTests 1 . H.property $
    flushCondition (TStore 0x10 0) initState H.=== Nothing

prop_flush_load_produces_nothing :: H.Property
prop_flush_load_produces_nothing = H.withTests 1 . H.property $
    flushCondition (TLoad 0 0x10) initState H.=== Nothing

prop_flush_add_produces_nothing :: H.Property
prop_flush_add_produces_nothing = H.withTests 1 . H.property $
    flushCondition (TAdd 0 1) initState H.=== Nothing

-- ---------------------------------------------------------------------------
-- Stall (read-after-write) tests
-- ---------------------------------------------------------------------------

prop_stall_same_address_produces_stall :: H.Property
prop_stall_same_address_produces_stall = H.withTests 1 . H.property $
    stall 0x42 0x42 H.=== Just (StallReadAfterWrite 0x42)

prop_stall_different_addresses_produces_nothing :: H.Property
prop_stall_different_addresses_produces_nothing = H.withTests 1 . H.property $
    stall 0x42 0x43 H.=== Nothing

prop_stall_adjacent_addresses_produces_nothing :: H.Property
prop_stall_adjacent_addresses_produces_nothing = H.withTests 1 . H.property $
    stall 0x00 0x01 H.=== Nothing

prop_stall_zero_address_produces_stall :: H.Property
prop_stall_zero_address_produces_stall = H.withTests 1 . H.property $
    stall 0x00 0x00 H.=== Just (StallReadAfterWrite 0x00)

prop_stall_max_address_produces_stall :: H.Property
prop_stall_max_address_produces_stall = H.withTests 1 . H.property $
    stall 0xFF 0xFF H.=== Just (StallReadAfterWrite 0xFF)

prop_stall_write_higher_than_read_produces_nothing :: H.Property
prop_stall_write_higher_than_read_produces_nothing = H.withTests 1 . H.property $
    stall 0x43 0x42 H.=== Nothing

-- ---------------------------------------------------------------------------
-- Latency tests
-- ---------------------------------------------------------------------------

prop_latency_nop_is_one :: H.Property
prop_latency_nop_is_one = H.withTests 1 . H.property $
    latency @TState TNop H.=== 1

prop_latency_add_is_one :: H.Property
prop_latency_add_is_one = H.withTests 1 . H.property $
    latency @TState (TAdd 0 1) H.=== 1

prop_latency_load_is_one :: H.Property
prop_latency_load_is_one = H.withTests 1 . H.property $
    latency @TState (TLoad 0 0x42) H.=== 1

prop_latency_store_is_one :: H.Property
prop_latency_store_is_one = H.withTests 1 . H.property $
    latency @TState (TStore 0x42 0) H.=== 1

prop_latency_mul_is_two :: H.Property
prop_latency_mul_is_two = H.withTests 1 . H.property $
    latency @TState (TMul 0 1) H.=== 2

prop_latency_jump_is_one :: H.Property
prop_latency_jump_is_one = H.withTests 1 . H.property $
    latency @TState (TJump 0x20) H.=== 1

-- ---------------------------------------------------------------------------
-- instrFetch tests
-- ---------------------------------------------------------------------------

prop_instrFetch_defaults_to_one :: H.Property
prop_instrFetch_defaults_to_one = H.withTests 1 . H.property $
    instrFetch @TState TNop H.=== 1

prop_instrFetch_mul_is_one :: H.Property
prop_instrFetch_mul_is_one = H.withTests 1 . H.property $
    instrFetch @TState (TMul 0 1) H.=== 1

-- ---------------------------------------------------------------------------
-- Interrupt tests
-- ---------------------------------------------------------------------------

prop_interruptible_returns_true :: H.Property
prop_interruptible_returns_true = H.withTests 1 . H.property $
    interruptible initState H.=== True

prop_acceptIrq_returns_no_isa_stage :: H.Property
prop_acceptIrq_returns_no_isa_stage = H.withTests 1 . H.property $ do
    let (_, mstage) = acceptIrq initState (0x10 :: TAddr)
    mstage H.=== Nothing

prop_acceptIrq_does_not_modify_state :: H.Property
prop_acceptIrq_does_not_modify_state = H.withTests 1 . H.property $ do
    let s       = setReg 0 0x42 initState
        (s', _) = acceptIrq s (0x10 :: TAddr)
    s' H.=== s

-- ---------------------------------------------------------------------------
-- Slot construction tests
-- ---------------------------------------------------------------------------

prop_slot_sempty_is_a_bubble :: H.Property
prop_slot_sempty_is_a_bubble = H.withTests 1 . H.property $
    (SEmpty :: Slot TInstr TIsaStage) H.=== SEmpty

prop_slot_sready_holds_instruction :: H.Property
prop_slot_sready_holds_instruction = H.withTests 1 . H.property $
    (SReady TNop :: Slot TInstr TIsaStage) H.=== SReady TNop

prop_slot_smemread_holds_instruction :: H.Property
prop_slot_smemread_holds_instruction = H.withTests 1 . H.property $
    (SMemRead (TLoad 0 0x42) :: Slot TInstr TIsaStage) H.=== SMemRead (TLoad 0 0x42)

prop_slot_sisa_holds_stage :: H.Property
prop_slot_sisa_holds_stage = H.withTests 1 . H.property $
    (SIsa TIsaStage :: Slot TInstr TIsaStage) H.=== SIsa TIsaStage

prop_slot_different_variants_not_equal :: H.Property
prop_slot_different_variants_not_equal = H.withTests 1 . H.property $
    (SEmpty :: Slot TInstr TIsaStage) H./== SReady TNop

isaTests :: TestTree
isaTests = $(testGroupGenerator)
