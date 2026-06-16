module Tests.Isacle.Periph.Timer where

import Prelude
import Data.Word (Word8, Word16)
import Data.Bits ((.&.))
import Data.List (mapAccumL)
import System.Exit (exitFailure)

assert :: String -> Bool -> IO ()
assert msg False = putStrLn ("FAIL: " ++ msg) >> exitFailure
assert msg True  = putStrLn ("ok:   " ++ msg)

-- ---------------------------------------------------------------------------
-- Pure timer simulation
--
-- Register layout at base 0x40:
--   0x40  TCCR  control (bit 0 = CTC mode)
--   0x41  TCNT  counter (write = synchronous preset; read = current value)
--   0x42  OCR   output compare register
--
-- Semantics match counterFSM + timerDef:
--   * Registers are clocked: state holds the value from the END of last cycle.
--   * Reads return the current registered value.
--   * TCNT write-preset: cntNext = preset value (takes effect next cycle).
--   * Overflow fires when tick && !CTC && TCNT==0xFF && !preset.
--   * Compare fires when tick && CTC && TCNT==OCR && !preset.
-- ---------------------------------------------------------------------------

type TState = (Word8, Word8, Word8)  -- (tccr, tcnt, ocr)

timerStep :: TState
          -> Bool
          -> Maybe Word16
          -> Maybe (Word16, Word8)
          -> (TState, Word8, Bool, Bool)  -- (newState, rdData, ovf, cmp)
timerStep (tccr, tcnt, ocr) tick rdAddr wrCmd =
    let b0 = 0x40 :: Word16
        -- Write decoding (TCCR and OCR have write-through semantics via onWrite)
        curTccr = case wrCmd of { Just (a,v) | a == b0   -> v; _ -> tccr }
        curOcr  = case wrCmd of { Just (a,v) | a == b0+2 -> v; _ -> ocr  }
        -- TCNT preset: registered, takes effect next cycle
        (tcntPreset, tcntWritten) = case wrCmd of
            Just (a,v) | a == b0+1 -> (v, True)
            _ -> (0, False)

        ctcMode = curTccr .&. 0x01 /= 0
        atTop   = tcnt == curOcr
        atMax   = tcnt == 0xFF

        -- Counter next value
        cntTick | ctcMode && atTop         = 0      -- CTC reset
                | not ctcMode && atMax     = 0      -- overflow wrap
                | otherwise                = tcnt + 1
        cntNext | tcntWritten              = tcntPreset
                | tick                     = cntTick
                | otherwise                = tcnt

        -- IRQs (combinational, same cycle as tick / state)
        ovf = tick && not ctcMode && atMax && not tcntWritten
        cmp = tick && ctcMode    && atTop  && not tcntWritten

        -- Read data: reads current registered values
        rdData = case rdAddr of
            Just a | a == b0   -> curTccr
                   | a == b0+1 -> tcnt      -- reads current counter (registered)
                   | a == b0+2 -> curOcr
            _ -> 0

    in ((curTccr, cntNext, curOcr), rdData, ovf, cmp)

runTimer :: Int
         -> [Bool]
         -> [Maybe Word16]
         -> [Maybe (Word16, Word8)]
         -> ([Word8], [Bool], [Bool])
runTimer n ticks rds wrs =
    let pad xs = xs ++ repeat (last xs)
        inputs = take n (zip3 (pad ticks) (pad rds) (pad wrs))
        step st (t, r, w) = let (st', rd, ov, cm) = timerStep st t r w in (st', (rd, ov, cm))
        outs = snd (mapAccumL step (0,0,0) inputs)
    in unzip3 outs

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

runTimerTests :: IO ()
runTimerTests = do
    putStrLn "\n-- counter basic --"
    let (rd1, _, _) = runTimer 6 (repeat True)
            [Nothing,Nothing,Just 0x41,Just 0x41,Just 0x41,Just 0x41]
            (repeat Nothing)
    assert "counts on tick"           (1 `elem` rd1 || 2 `elem` rd1)

    let (rd2, _, _) = runTimer 4 (repeat False)
            [Nothing,Just 0x41,Just 0x41,Just 0x41]
            (repeat Nothing)
    assert "no tick no advance"       (all (== 0) rd2)

    putStrLn "\n-- overflow --"
    let (_, ovf, _) = runTimer 6
            (False : False : False : repeat True)
            (repeat Nothing)
            [Nothing, Just (0x41,0xFE), Nothing, Nothing, Nothing, Nothing]
    assert "overflow fires on wrap"   (True `elem` ovf)

    putStrLn "\n-- CTC mode --"
    let (rdC, _, cmp) = runTimer 8
            (False : False : False : repeat True)
            (Nothing : Nothing : Nothing : repeat (Just 0x41))
            [Nothing, Just (0x40,0x01), Just (0x42,0x03)
            ,Nothing,Nothing,Nothing,Nothing,Nothing]
    assert "ctc compare match fires"  (True `elem` cmp)
    assert "ctc counter resets"       (0 `elem` drop 3 rdC)

    putStrLn "\n-- TCNT write / read --"
    let (rd3, _, _) = runTimer 4 (repeat False)
            [Nothing,Nothing,Just 0x41,Just 0x41]
            [Nothing,Just (0x41,0xAB),Nothing,Nothing]
    assert "write tcnt preloads"      (0xAB `elem` rd3)

    let (rd4, _, _) = runTimer 4 (repeat False)
            [Nothing,Nothing,Just 0x42,Just 0x42]
            [Nothing,Just (0x42,0x7F),Nothing,Nothing]
    assert "read ocr reflects write"  (0x7F `elem` rd4)
