module Tests.Isacle.Periph.UART where

import Prelude
import Data.Word (Word8, Word16)
import Data.Bits ((.&.), shiftR)
import Data.List (mapAccumL)
import System.Exit (exitFailure)

assert :: String -> Bool -> IO ()
assert msg False = putStrLn ("FAIL: " ++ msg) >> exitFailure
assert msg True  = putStrLn ("ok:   " ++ msg)

-- ---------------------------------------------------------------------------
-- Pure UART simulation (8N1, TX only)
--
-- Register layout at base 0x80:
--   0x80  UDR   TX write / RX read
--   0x81  USR   status (bit 0 = UDRE / tx buffer empty)
--   0x82  UBRR  baud rate divisor
--
-- All registers are clocked: reads return values registered at the START of
-- the cycle.  UBRR and the TX buffer are updated at the end of the cycle.
--
-- TX machine:
--   State 0 (idle): line high.  If tx buffer valid, load shift register,
--                   output start bit (low), advance to state 1.
--   State 1 (data/stop): baud counter counts down.  On expiry, shift out
--                   next bit (0-7), then stop bit (high), then back to idle.
-- ---------------------------------------------------------------------------

data Us = Us
    { uUbrr   :: Word8   -- baud rate register
    , uBufVld :: Bool    -- TX buffer valid (registered)
    , uBufDat :: Word8   -- TX buffer data  (registered)
    , uTxSt   :: Int     -- 0 = idle; 1 = transmitting
    , uTxCtr  :: Int     -- baud countdown
    , uTxBit  :: Int     -- 0 = start, 1-8 = data bits, 9 = stop
    , uTxShft :: Word8   -- shift register
    , uTxLine :: Bool    -- TX output (registered)
    } deriving (Show)

initUs :: Us
initUs = Us 0 False 0 0 0 0 0 True

uartStep :: Us
         -> Bool              -- rx pin (ignored in TX-only sim)
         -> Maybe Word16      -- bus read address
         -> Maybe (Word16, Word8)  -- bus write
         -> (Us, Word8, Bool, Bool, Bool)  -- (newSt, rdData, txLine, rxIrq, txIrq)
uartStep s _rx rdAddr wrCmd =
    let base = 0x80 :: Word16

        -- Write decoding
        newUbrr    = case wrCmd of { Just (a,v) | a == base+2 -> v; _ -> uUbrr s }
        udrWritten = case wrCmd of { Just (a,_) | a == base+0 -> True; _ -> False }
        udrData    = case wrCmd of { Just (a,v) | a == base+0 -> v; _ -> 0 }

        -- TX buffer: load on write (takes effect next cycle via register)
        bvNext0 = if udrWritten then True  else uBufVld s
        bdNext  = if udrWritten then udrData else uBufDat s

        -- TX state machine (operates on current registered state)
        halfPeriod  = max 1 (fromIntegral (uUbrr s) :: Int)
        canStart    = uTxSt s == 0 && uBufVld s  -- idle and buffer has byte

        (txStN, txCtrN, txBitN, txShftN, txLineN, bvNext) =
            if canStart
            then  -- start transmitting: emit start bit, consume buffer
                ( 1, halfPeriod, 0, uBufDat s, False, False )
            else case uTxSt s of
                0 ->  -- idle, nothing to send
                    ( 0, 0, 0, uTxShft s, True, bvNext0 )
                1 ->  -- transmitting
                    if uTxCtr s > 0
                    then ( 1, uTxCtr s - 1, uTxBit s, uTxShft s, uTxLine s, bvNext0 )
                    else if uTxBit s == 0  -- start → first data bit
                    then let b = uTxShft s .&. 1 /= 0
                         in ( 1, halfPeriod, 1, uTxShft s, b, bvNext0 )
                    else if uTxBit s <= 7  -- shift next data bit
                    then let sh = uTxShft s `shiftR` 1
                             n  = uTxBit s + 1
                             b  = sh .&. 1 /= 0
                         in ( 1, halfPeriod, n, sh, b, bvNext0 )
                    else  -- stop bit → idle
                    ( 0, 0, 0, uTxShft s, True, bvNext0 )
                _ -> ( 0, 0, 0, 0, True, bvNext0 )

        -- Outputs reflect CURRENT (registered) state
        txLine  = uTxLine s
        udreIrq = not (uBufVld s)  -- UDRE = buffer empty
        rxIrq   = False

        rdData = case rdAddr of
            Just a | a == base+0 -> 0
                   | a == base+1 -> if udreIrq then 0x01 else 0x00
                   | a == base+2 -> uUbrr s
            _ -> 0

        s' = Us newUbrr bvNext bdNext txStN txCtrN txBitN txShftN txLineN
    in (s', rdData, txLine, rxIrq, udreIrq)

runUART :: Int
        -> [Bool]
        -> [Maybe Word16]
        -> [Maybe (Word16, Word8)]
        -> ([Word8], [Bool], [Bool], [Bool])  -- (rdData, txLine, rxIrq, txIrq)
runUART n rxs rds wrs =
    let pad xs = xs ++ repeat (last xs)
        inputs = take n (zip3 (pad rxs) (pad rds) (pad wrs))
        step st (rx, rd, wr) = let (st', r, tx, ri, ti) = uartStep st rx rd wr in (st', (r, tx, ri, ti))
        outs = snd (mapAccumL step initUs inputs)
    in unzip4 outs
  where
    unzip4 xs = (map (\(a,_,_,_)->a) xs, map (\(_,b,_,_)->b) xs,
                 map (\(_,_,c,_)->c) xs, map (\(_,_,_,d)->d) xs)

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

runUartTests :: IO ()
runUartTests = do
    putStrLn "\n-- TX idle --"
    let (_, tx1, _, _) = runUART 4 (repeat True) (repeat Nothing) (repeat Nothing)
    assert "tx idle high"              (all (== True) tx1)

    putStrLn "\n-- UDRE (TX buffer empty flag) --"
    let (_, _, _, txIrq1) = runUART 3 (repeat True) (repeat Nothing) (repeat Nothing)
    assert "udre set when idle"        (all (== True) txIrq1)

    let (_, _, _, txIrq2) = runUART 3 (repeat True) (repeat Nothing)
            [Nothing, Just (0x80, 0x41), Nothing]
    assert "udre clears after write"   (False `elem` txIrq2)

    putStrLn "\n-- TX start bit --"
    let (_, tx2, _, _) = runUART 10 (repeat True) (repeat Nothing)
            [ Nothing
            , Just (0x82, 2)
            , Just (0x80, 0x00)
            , Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing
            ]
    assert "tx start bit goes low"     (False `elem` tx2)

    putStrLn "\n-- UBRR read/write --"
    let (rd1, _, _, _) = runUART 4 (repeat True)
            [Nothing, Nothing, Just 0x82, Just 0x82]
            [Nothing, Just (0x82, 50), Nothing, Nothing]
    assert "read ubrr reflects write"  (50 `elem` rd1)
