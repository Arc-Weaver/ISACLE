module Tests.Isacle.Harvard.Pipeline where

import Prelude hiding (read)
import System.Exit (exitFailure)

import Isacle.Harvard.ISA
import Isacle.Harvard.Pipeline

import Tests.Isacle.Harvard.ISA
    ( TState(..), TInstr(..), TIsaStage(..)
    , initState, withZero, setReg, getReg
    )

assert :: String -> Bool -> IO ()
assert msg False = putStrLn ("FAIL: " ++ msg) >> exitFailure
assert msg True  = putStrLn ("ok:   " ++ msg)

-- ---------------------------------------------------------------------------
-- 2-deep pipeline helpers
-- ---------------------------------------------------------------------------

type P = PipeState TInstr TIsaStage

emptyP :: P
emptyP = emptyPipe 2

step :: P -> TState -> PipeInput TState -> (P, TState, PipeOutput TState)
step = pipelineStep

noInp :: PipeInput TState
noInp = PipeInput Nothing Nothing Nothing

withInstr :: TInstr -> PipeInput TState
withInstr i = PipeInput (Just i) Nothing Nothing

-- | Advance instruction to execute head (slot 0).
primeExec :: TInstr -> TState -> (P, TState)
primeExec instr s =
    let (ps1, s1, _) = step emptyP s  (withInstr instr)
        (ps2, s2, _) = step ps1    s1 noInp
    in (ps2, s2)

depth :: Int
depth = 2

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

runPipelineTests :: IO ()
runPipelineTests = do
    putStrLn "\n-- bubble behaviour --"
    let (_, _, out0) = step emptyP initState noInp
    assert "empty pipe no output"     (pipeMemRead out0 == Nothing && pipeMemWrite out0 == Nothing
                                     && pipeFlush out0 == Nothing && pipeStalled out0 == False)

    let (ps', _, _) = step emptyP initState (withInstr TNop)
    assert "new instr enters tail"    (head (psSlots ps') == SEmpty)
    assert "new instr in slot 1"      (psSlots ps' !! 1 == SReady TNop)

    putStrLn "\n-- single-cycle execution --"
    let (ps0, s0) = primeExec TNop initState
    let (ps1, s1, out1) = step ps0 s0 noInp
    assert "nop state unchanged"      (s1 == initState)
    assert "nop no flush"             (pipeFlush out1 == Nothing)
    assert "nop not stalled"          (pipeStalled out1 == False)
    assert "nop head cleared"         (head (psSlots ps1) == SEmpty)

    let sa = setReg 0 3 (setReg 1 4 initState)
    let (pa0, sa0) = primeExec (TAdd 0 1) sa
    let (_, sa1, _) = step pa0 sa0 noInp
    assert "add updates register"     (getReg 0 sa1 == 7)

    let (pj0, sj0) = primeExec (TJump 0x42) initState
    let (pj1, _, outj) = step pj0 sj0 noInp
    assert "jump flushes"             (pipeFlush outj == Just (FlushBranch 0x42))
    assert "jump not stalled"         (pipeStalled outj == False)
    assert "jump clears pipeline"     (psSlots pj1 == replicate depth SEmpty)

    putStrLn "\n-- memory read (load) --"
    let (pl0, sl0) = primeExec (TLoad 0 0x10) initState
    let (pl1, sl1, outl1) = step pl0 sl0 noInp
    assert "load issues read"         (pipeMemRead outl1 == Just 0x10)
    assert "load stalls"              (pipeStalled outl1 == True)
    assert "load head is SMemRead"    (head (psSlots pl1) == SMemRead (TLoad 0 0x10))

    let (pl2, sl2, outl2) = step pl1 sl1 noInp
    assert "load stalls w/o response" (pipeStalled outl2 == True)
    assert "load stays SMemRead"      (head (psSlots pl2) == SMemRead (TLoad 0 0x10))

    let (_, sl3, outl3) = step pl2 sl2 (noInp { pipeMemResp = Just 0xAB })
    assert "load completes w/ resp"   (pipeStalled outl3 == False)
    assert "load result in reg"       (getReg 0 sl3 == 0xAB)

    putStrLn "\n-- memory write (store) --"
    let ss0 = setReg 1 0xBE initState
    let (pst0, ss0') = primeExec (TStore 0x20 1) ss0
    let (_, _, outst) = step pst0 ss0' noInp
    assert "store issues write"       (pipeMemWrite outst == Just (0x20, 0xBE))

    putStrLn "\n-- multi-cycle latency --"
    let sm0 = setReg 0 3 (setReg 1 4 initState)
    let (pm0, sm0') = primeExec (TMul 0 1) sm0
    let (pm1, sm1, om1) = step pm0 sm0' noInp
    assert "mul first cycle stalls"   (pipeStalled om1 == True)
    let (_, sm2, om2) = step pm1 sm1 noInp
    assert "mul second cycle executes" (pipeStalled om2 == False)
    assert "mul result correct"        (getReg 0 sm2 == 12)

    putStrLn "\n-- conditional branch --"
    let (pb0, sb0) = primeExec (TBrZ 0x30) (withZero False initState)
    let (_, _, outbf) = step pb0 sb0 noInp
    assert "brz not taken: no flush"  (pipeFlush outbf == Nothing)

    let (pb1, sb1) = primeExec (TBrZ 0x30) (withZero True initState)
    let (_, _, outbt) = step pb1 sb1 noInp
    assert "brz taken: flush"         (pipeFlush outbt == Just (FlushBranch 0x30))

    putStrLn "\n-- interrupt --"
    let irqInp = PipeInput Nothing Nothing (Just 0xFF)
    let (_, _, outi) = step emptyP initState irqInp
    assert "irq at bubble accepted"   (pipeFlush outi == Just (FlushInterrupt 0xFF))
    assert "irq not stalled"          (pipeStalled outi == False)

    let irqInp2 = PipeInput (Just TNop) Nothing (Just 0xFF)
    let (pir2, _, _) = step emptyP initState irqInp2
    assert "irq clears pipeline"      (psSlots pir2 == replicate depth SEmpty)

    putStrLn "\n-- complex sequences --"
    let sc0 = setReg 0 1 (setReg 1 2 (setReg 2 3 initState))
    let (pc1, sc1, _) = step emptyP sc0 (withInstr (TAdd 0 1))
    let (pc2, sc2, _) = step pc1 sc1 (withInstr (TAdd 1 2))
    assert "back-to-back: slot 0 filled"  (head (psSlots pc2) == SReady (TAdd 0 1))
    assert "back-to-back: slot 1 filled"  (psSlots pc2 !! 1 == SReady (TAdd 1 2))
    let (pc3, sc3, _) = step pc2 sc2 noInp
    assert "first add executes"           (getReg 0 sc3 == 3)
    let (_, sc4, _) = step pc3 sc3 noInp
    assert "second add executes"          (getReg 1 sc4 == 5)
    assert "first result preserved"       (getReg 0 sc4 == 3)

    let (pfj1, sfj1, _) = step emptyP initState (withInstr (TJump 0x42))
    let (pfj2, sfj2, _) = step pfj1 sfj1 (withInstr TNop)
    let (pfj3, _, outfj) = step pfj2 sfj2 noInp
    assert "flush discards in-flight"     (pipeFlush outfj == Just (FlushBranch 0x42))
    assert "pipeline cleared on flush"    (psSlots pfj3 == replicate depth SEmpty)

    let sl_s0 = setReg 1 5 initState
    let (pll1, sll1, _) = step emptyP sl_s0 (withInstr (TLoad 0 0x10))
    let (pll2, sll2, _) = step pll1 sll1 (withInstr (TAdd 0 1))
    let (pll3, sll3, oll3) = step pll2 sll2 noInp
    assert "load issues read"             (pipeMemRead oll3 == Just 0x10)
    assert "load stalls"                  (pipeStalled oll3 == True)
    assert "add in slot 1 during stall"   (psSlots pll3 !! 1 == SReady (TAdd 0 1))
    let (pll4, sll4, oll4) = step pll3 sll3 noInp
    assert "no resp: still stalled"       (pipeStalled oll4 == True)
    let (pll5, sll5, _) = step pll4 sll4 (noInp { pipeMemResp = Just 7 })
    assert "load completes: r0=7"         (getReg 0 sll5 == 7)
    assert "add advanced to head"         (head (psSlots pll5) == SReady (TAdd 0 1))
    let (_, sll6, _) = step pll5 sll5 noInp
    assert "add uses loaded value"        (getReg 0 sll6 == 12)

    let sb_s0 = setReg 0 0xAA (setReg 1 0xBB initState)
    let (pbs1, sbs1, _) = step emptyP sb_s0 (withInstr (TStore 0x10 0))
    let (pbs2, sbs2, _) = step pbs1 sbs1 (withInstr (TStore 0x20 1))
    let (pbs3, sbs3, obs3) = step pbs2 sbs2 noInp
    assert "first store issues write"     (pipeMemWrite obs3 == Just (0x10, 0xAA))
    let (_, _, obs4) = step pbs3 sbs3 noInp
    assert "second store issues write"    (pipeMemWrite obs4 == Just (0x20, 0xBB))
