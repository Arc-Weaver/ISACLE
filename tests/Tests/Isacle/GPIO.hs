module Tests.Isacle.GPIO where

import Prelude
import Data.Word (Word8, Word16)
import Data.List (mapAccumL)
import System.Exit (exitFailure)

assert :: String -> Bool -> IO ()
assert msg False = putStrLn ("FAIL: " ++ msg) >> exitFailure
assert msg True  = putStrLn ("ok:   " ++ msg)

-- ---------------------------------------------------------------------------
-- Pure GPIO simulation
--
-- Register layout at base 0x60:
--   0x60  PIN   read-only input
--   0x61  DDR   read/write
--   0x62  PORT  read/write
--
-- onWrite has write-through: if a write targets a register this cycle,
-- the output reflects the new value immediately (same cycle).
-- ---------------------------------------------------------------------------

type GState = (Word8, Word8)   -- (ddr, port)

gpioStep :: GState
         -> Word8
         -> Maybe Word16
         -> Maybe (Word16, Word8)
         -> (GState, Word8, Word8, Word8)   -- (newState, rdData, portOut, ddrOut)
gpioStep (ddr, port) pinIn rdAddr wrCmd =
    let pin0 = 0x60 :: Word16
        ddr0 = 0x61 :: Word16
        prt0 = 0x62 :: Word16
        curDdr  = case wrCmd of { Just (a,v) | a == ddr0 -> v; _ -> ddr }
        curPort = case wrCmd of { Just (a,v) | a == prt0 -> v; _ -> port }
        rdData  = case rdAddr of
            Just a | a == pin0 -> pinIn
                   | a == ddr0 -> curDdr
                   | a == prt0 -> curPort
            _ -> 0
    in ((curDdr, curPort), rdData, curPort, curDdr)

runGpio :: Int
        -> [Word8]
        -> [Maybe Word16]
        -> [Maybe (Word16, Word8)]
        -> ([Word8], [Word8], [Word8])  -- (rdData, portOut, ddrOut)
runGpio n pins rds wrs =
    let pad xs = xs ++ repeat (last xs)
        inputs  = take n (zip3 (pad pins) (pad rds) (pad wrs))
        step st (p, r, w) = let (st', rd, po, dr) = gpioStep st p r w in (st', (rd, po, dr))
        outs = snd (mapAccumL step (0,0) inputs)
    in unzip3 outs

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

runGpioTests :: IO ()
runGpioTests = do
    putStrLn "\n-- DDR write --"
    let (_, _, ddr1) = runGpio 3 [0,0,0] [Nothing,Nothing,Nothing]
                                 [Just (0x61,0xFF),Nothing,Nothing]
    assert "ddr write sets direction"     (0xFF `elem` ddr1)

    let (_, _, ddr2) = runGpio 6 (repeat 0) (repeat Nothing)
                                 (Nothing : Just (0x61,0xAA) : repeat Nothing)
    assert "ddr persists after write"     (all (== 0xAA) (drop 2 ddr2))

    let (_, port1, _) = runGpio 3 (repeat 0) (repeat Nothing)
                                  [Just (0x61,0xFF),Nothing,Nothing]
    assert "ddr write does not affect PORT" (all (== 0) port1)

    putStrLn "\n-- PORT write --"
    let (_, port2, _) = runGpio 3 (repeat 0) (repeat Nothing)
                                  [Just (0x62,0x55),Nothing,Nothing]
    assert "port write sets latch"        (0x55 `elem` port2)

    let (_, port3, _) = runGpio 6 (repeat 0) (repeat Nothing)
                                  (Nothing : Just (0x62,0xBB) : repeat Nothing)
    assert "port persists after write"    (all (== 0xBB) (drop 2 port3))

    let (_, _, ddr3) = runGpio 3 (repeat 0) (repeat Nothing)
                                 [Just (0x62,0xFF),Nothing,Nothing]
    assert "port write does not affect DDR" (all (== 0) ddr3)

    putStrLn "\n-- reads --"
    let (rd1, _, _) = runGpio 2 [0xAB,0xAB] [Just 0x60, Just 0x60] [Nothing,Nothing]
    assert "pin read returns input"       (0xAB `elem` rd1)

    let (rd2, _, _) = runGpio 4 (repeat 0)
                                 [Nothing,Nothing,Just 0x61,Just 0x61]
                                 [Nothing,Just (0x61,0xCC),Nothing,Nothing]
    assert "read ddr after write"         (0xCC `elem` rd2)

    let (rd3, _, _) = runGpio 4 (repeat 0)
                                 [Nothing,Nothing,Just 0x62,Just 0x62]
                                 [Nothing,Just (0x62,0x77),Nothing,Nothing]
    assert "read port after write"        (0x77 `elem` rd3)

    let (rd4, _, _) = runGpio 2 (repeat 0)
                                 [Just 0x61, Nothing]
                                 [Just (0x61,0xF0), Nothing]
    assert "write-read same cycle"        (0xF0 `elem` rd4)

    let (rd5, _, _) = runGpio 2 [0xFF,0xFF] [Just 0x00, Just 0xFF] (repeat Nothing)
    assert "unmapped read returns zero"   (all (== 0) rd5)

    putStrLn "\n-- sequential write+read --"
    let (rd6, port4, ddr4) = runGpio 7 (repeat 0)
            [Nothing,Nothing,Nothing,Just 0x61,Just 0x62,Just 0x61,Just 0x62]
            [Nothing,Just (0x61,0x0F),Just (0x62,0xF0),Nothing,Nothing,Nothing,Nothing]
    assert "sequential: ddr seen in output"  (0x0F `elem` ddr4)
    assert "sequential: port seen in output" (0xF0 `elem` port4)
    assert "sequential: read ddr"            (0x0F `elem` rd6)
    assert "sequential: read port"           (0xF0 `elem` rd6)
