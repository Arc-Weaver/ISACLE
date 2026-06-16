{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Tests.Isacle.Harvard.ISA where

import Prelude hiding (read)
import Data.Word (Word8)
import System.Exit (exitFailure)

import Isacle.Harvard.ISA

-- ---------------------------------------------------------------------------
-- Assert helper
-- ---------------------------------------------------------------------------

assert :: String -> Bool -> IO ()
assert msg False = putStrLn ("FAIL: " ++ msg) >> exitFailure
assert msg True  = putStrLn ("ok:   " ++ msg)

-- ---------------------------------------------------------------------------
-- Minimal test ISA
-- ---------------------------------------------------------------------------

type TAddr = Word8
type TVal  = Word8
type TReg  = Int   -- was Index 4; valid range 0..3

data TState = TState
    { tRegs :: [TVal]   -- 4-element list; was Vec 4 TVal
    , tPC   :: TAddr
    , tZero :: Bool
    } deriving (Show, Eq)

data TInstr
    = TNop
    | TAdd   TReg TReg
    | TLoad  TReg TAddr
    | TStore TAddr TReg
    | TJump  TAddr
    | TBrZ   TAddr
    | TMul   TReg TReg
    deriving (Show, Eq)

data TIsaStage = TIsaStage
    deriving (Show, Eq)

initState :: TState
initState = TState [0,0,0,0] 0 False

withZero :: Bool -> TState -> TState
withZero z s = s { tZero = z }

getReg :: TReg -> TState -> TVal
getReg r s = tRegs s !! r

setReg :: TReg -> TVal -> TState -> TState
setReg r v s = s { tRegs = take r (tRegs s) ++ [v] ++ drop (r+1) (tRegs s) }

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
        in setReg rd r s { tZero = r == 0 }
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
    type IsaStage  TState = TIsaStage
    type FetchWord TState = Word8
    type MaxFetch  TState = 1

    latency (TMul _ _) = 2
    latency _          = 1

    toIsaStage _ _ = Nothing

    isaStageStep TIsaStage _ s = (s, Right ())

    interruptible _ = True

    acceptIrq s _ = (s, Nothing)

instance HasFlush TState

stall :: TAddr -> TAddr -> Maybe (StallEvent TAddr)
stall = stallCondition @TState

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

runIsaTests :: IO ()
runIsaTests = do
    putStrLn "\n-- ALU.read --"
    assert "read TLoad returns address"  (read (TLoad 0 0x42) initState == Just 0x42)
    assert "read TNop returns Nothing"   (read TNop initState == Nothing)
    assert "read TStore returns Nothing" (read (TStore 0x10 0) initState == Nothing)
    assert "read TJump returns Nothing"  (read (TJump 0x20) initState == Nothing)
    assert "read TAdd returns Nothing"   (read (TAdd 0 1) initState == Nothing)

    putStrLn "\n-- ALU.write --"
    let sw = setReg 1 0xAB initState
    assert "write TStore returns addr+val" (write (TStore 0x10 1) sw == Just (0x10, 0xAB))
    assert "write TNop returns Nothing"    (write TNop initState == Nothing)
    assert "write TLoad returns Nothing"   (write (TLoad 0 0x42) initState == Nothing)
    assert "write TJump returns Nothing"   (write (TJump 0x20) initState == Nothing)

    putStrLn "\n-- ALU.move --"
    assert "move TJump always taken"          (move (TJump 0x20) initState == Just 0x20)
    assert "move TBrZ taken when zero set"    (move (TBrZ 0x30) (withZero True initState) == Just 0x30)
    assert "move TBrZ not taken when clear"   (move (TBrZ 0x30) (withZero False initState) == Nothing)
    assert "move TNop returns Nothing"        (move TNop initState == Nothing)
    assert "move TStore returns Nothing"      (move (TStore 0x10 0) initState == Nothing)

    putStrLn "\n-- ALU.compute --"
    let sadd = setReg 0 3 (setReg 1 4 initState)
    assert "compute TAdd updates reg"         (getReg 0 (compute (TAdd 0 1) Nothing sadd) == 7)
    let sovf = setReg 0 0xFF (setReg 1 0x01 initState)
    assert "compute TAdd sets zero on wrap"   (tZero (compute (TAdd 0 1) Nothing sovf) == True)
    let sclr = setReg 0 1 (setReg 1 1 (withZero True initState))
    assert "compute TAdd clears zero on nz"   (tZero (compute (TAdd 0 1) Nothing sclr) == False)
    assert "compute TLoad stores value"       (getReg 2 (compute (TLoad 2 0x00) (Just 0xBE) initState) == 0xBE)
    assert "compute TLoad no val unchanged"   (compute (TLoad 0 0x00) Nothing initState == initState)
    assert "compute TNop unchanged"           (compute TNop Nothing initState == initState)
    assert "compute TJump unchanged"          (compute (TJump 0x20) Nothing initState == initState)

    putStrLn "\n-- flushCondition --"
    assert "flush TJump → FlushBranch"        (flushCondition (TJump 0x20) initState == Just (FlushBranch 0x20))
    assert "flush TBrZ taken → FlushBranch"   (flushCondition (TBrZ 0x30) (withZero True initState) == Just (FlushBranch 0x30))
    assert "flush TBrZ not taken → Nothing"   (flushCondition (TBrZ 0x30) (withZero False initState) == Nothing)
    assert "flush TNop → Nothing"             (flushCondition TNop initState == Nothing)
    assert "flush TStore → Nothing"           (flushCondition (TStore 0x10 0) initState == Nothing)
    assert "flush TLoad → Nothing"            (flushCondition (TLoad 0 0x10) initState == Nothing)
    assert "flush TAdd → Nothing"             (flushCondition (TAdd 0 1) initState == Nothing)

    putStrLn "\n-- stallCondition --"
    assert "stall same addr"      (stall 0x42 0x42 == Just (StallReadAfterWrite 0x42))
    assert "stall diff addrs"     (stall 0x42 0x43 == Nothing)
    assert "stall adjacent"       (stall 0x00 0x01 == Nothing)
    assert "stall zero addr"      (stall 0x00 0x00 == Just (StallReadAfterWrite 0x00))
    assert "stall max addr"       (stall 0xFF 0xFF == Just (StallReadAfterWrite 0xFF))
    assert "stall write > read"   (stall 0x43 0x42 == Nothing)

    putStrLn "\n-- latency --"
    assert "latency TNop = 1"     (latency @TState TNop == 1)
    assert "latency TAdd = 1"     (latency @TState (TAdd 0 1) == 1)
    assert "latency TLoad = 1"    (latency @TState (TLoad 0 0x42) == 1)
    assert "latency TStore = 1"   (latency @TState (TStore 0x42 0) == 1)
    assert "latency TMul = 2"     (latency @TState (TMul 0 1) == 2)
    assert "latency TJump = 1"    (latency @TState (TJump 0x20) == 1)

    putStrLn "\n-- instrFetch --"
    assert "instrFetch TNop = 1"  (instrFetch @TState TNop == 1)
    assert "instrFetch TMul = 1"  (instrFetch @TState (TMul 0 1) == 1)

    putStrLn "\n-- interrupts --"
    assert "interruptible = True"              (interruptible initState == True)
    assert "acceptIrq returns no stage"        (snd (acceptIrq initState (0x10 :: TAddr)) == Nothing)
    let sr = setReg 0 0x42 initState
    assert "acceptIrq does not modify state"   (fst (acceptIrq sr (0x10 :: TAddr)) == sr)

    putStrLn "\n-- Slot constructors --"
    assert "SEmpty == SEmpty"  ((SEmpty :: Slot TInstr TIsaStage) == SEmpty)
    assert "SReady holds instr" ((SReady TNop :: Slot TInstr TIsaStage) == SReady TNop)
    assert "SMemRead holds instr" ((SMemRead (TLoad 0 0x42) :: Slot TInstr TIsaStage) == SMemRead (TLoad 0 0x42))
    assert "SIsa holds stage"  ((SIsa TIsaStage :: Slot TInstr TIsaStage) == SIsa TIsaStage)
    assert "SEmpty /= SReady"  ((SEmpty :: Slot TInstr TIsaStage) /= SReady TNop)
