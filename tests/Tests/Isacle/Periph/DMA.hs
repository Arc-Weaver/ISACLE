module Tests.Isacle.Periph.DMA where

import Prelude
import Data.Word (Word8, Word16)
import System.Exit (exitFailure)

import Isacle.Hdl.Prim (Unsigned)
import Isacle.Periph.DMA (DMAState(..))

assert :: String -> Bool -> IO ()
assert msg False = putStrLn ("FAIL: " ++ msg) >> exitFailure
assert msg True  = putStrLn ("ok:   " ++ msg)

-- ---------------------------------------------------------------------------
-- Pure DMA step function (mirrors the original mealy step logic)
-- ---------------------------------------------------------------------------

type Addr = Word16
type Dat  = Word8

dmaSimStep
    :: DMAState Addr Dat
    -> (Maybe (Addr, Addr, Unsigned 16, Bool, Bool), Dat)
    -> ( DMAState Addr Dat
       , (Maybe Addr, Maybe (Addr, Dat), Bool, Bool)
       )
dmaSimStep DMAIdle (Just (src, dst, n, iSrc, iDst), _)
    | n > 0
    = ( DMARead src dst n iSrc iDst
      , (Just src, Nothing, True, False)
      )
dmaSimStep DMAIdle _
    = (DMAIdle, (Nothing, Nothing, False, False))
dmaSimStep (DMARead src dst n iSrc iDst) (_, dat) =
    let nextSrc = if iSrc then src + 1 else src
        nextDst = if iDst then dst + 1 else dst
        n'      = n - 1
        isDone  = n' == 0
        nextSt  = if isDone then DMAIdle else DMARead nextSrc nextDst n' iSrc iDst
        nextRd  = if isDone then Nothing else Just nextSrc
    in (nextSt, (nextRd, Just (dst, dat), True, isDone))

runDMA :: Int
       -> [Maybe (Addr, Addr, Unsigned 16, Bool, Bool)]
       -> [Dat]
       -> ([Maybe Addr], [Maybe (Addr, Dat)], [Bool], [Bool])
runDMA n starts rdResps =
    let inputs  = zip (starts ++ repeat Nothing) (rdResps ++ repeat 0)
        go _ [] = []
        go st (x:xs) = let (st', out) = dmaSimStep st x in out : go st' xs
        outs = take n (go DMAIdle inputs)
    in ( map (\(a,_,_,_) -> a) outs
       , map (\(_,b,_,_) -> b) outs
       , map (\(_,_,c,_) -> c) outs
       , map (\(_,_,_,d) -> d) outs
       )

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

runDmaTests :: IO ()
runDmaTests = do
    putStrLn "\n-- DMA idle --"
    let (_, _, busy1, _) = runDMA 3 (repeat Nothing) (repeat 0)
    assert "idle: not busy"            (all (== False) busy1)

    putStrLn "\n-- single-element transfer --"
    let starts1 = [Nothing, Just (0x100, 0x200, 1, True, True), Nothing, Nothing]
    let (rd1, _, _, _) = runDMA 4 starts1 [0,0,0,0]
    assert "issues read on start"      (Just 0x100 `elem` rd1)

    let (_, wr1, _, _) = runDMA 5 starts1 [0, 0, 0xAB, 0, 0]
    assert "writes read data"          (Just (0x200, 0xAB) `elem` wr1)

    let (_, _, _, done1) = runDMA 6 (starts1 ++ [Nothing,Nothing]) [0,0,0xAB,0,0,0]
    assert "done fires exactly once"   (length (filter id done1) == 1)

    let (_, _, busy2, done2) = runDMA 6 (starts1 ++ [Nothing,Nothing]) [0,0,0xAB,0,0,0]
    let doneIdx = length (takeWhile not done2)
    assert "done index in range"       (doneIdx < 6)
    assert "idle after done"           (all (== False) (drop (doneIdx + 1) busy2))

    putStrLn "\n-- multi-element transfer --"
    let starts3 = [Nothing, Just (0x100, 0x200, 3, True, True)
                  , Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
    let (rd3, wr3, _, done3) = runDMA 8 starts3 [0, 0, 0xAA, 0xBB, 0xCC, 0, 0, 0]
    assert "multi: read addr 0x100"    (Just 0x100 `elem` rd3)
    assert "multi: read addr 0x101"    (Just 0x101 `elem` rd3)
    assert "multi: read addr 0x102"    (Just 0x102 `elem` rd3)
    assert "multi: write (0x200,0xAA)" (Just (0x200, 0xAA) `elem` wr3)
    assert "multi: write (0x201,0xBB)" (Just (0x201, 0xBB) `elem` wr3)
    assert "multi: write (0x202,0xCC)" (Just (0x202, 0xCC) `elem` wr3)
    assert "multi: done once"          (length (filter id done3) == 1)

    putStrLn "\n-- transfer modes --"
    let starts4 = [Nothing, Just (0x100, 0x40, 3, True, False)
                  , Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
    let (_, wr4, _, _) = runDMA 8 starts4 [0, 0, 0xAA, 0xBB, 0xCC, 0, 0, 0]
    assert "m2p: dst fixed (0x40,AA)"  (Just (0x40, 0xAA) `elem` wr4)
    assert "m2p: dst fixed (0x40,BB)"  (Just (0x40, 0xBB) `elem` wr4)
    assert "m2p: dst fixed (0x40,CC)"  (Just (0x40, 0xCC) `elem` wr4)

    let starts5 = [Nothing, Just (0x40, 0x200, 3, False, True)
                  , Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
    let (rd5, wr5, _, _) = runDMA 8 starts5 [0, 0, 0xDD, 0xEE, 0xFF, 0, 0, 0]
    assert "p2m: src fixed"            (all (== Just 0x40) (filter (/= Nothing) rd5))
    assert "p2m: write (0x200,DD)"     (Just (0x200, 0xDD) `elem` wr5)
    assert "p2m: write (0x201,EE)"     (Just (0x201, 0xEE) `elem` wr5)
    assert "p2m: write (0x202,FF)"     (Just (0x202, 0xFF) `elem` wr5)
