module Core.Decode where

import Clash.Prelude

-- | Pipeline state for ISAs where instructions occupy 1 or 2 fetch words.
--   Parameterised on the word type @w@ so it works for any fetch width
--   (16-bit words for AVR, 8-bit bytes for 6502/8051, etc.).
data DecodeState w = AwaitFirst | AwaitSecond w
    deriving (Generic, NFDataX, Show, Eq)

-- | One step of the fetch-decode pipeline.
--
--   @needsTwo@  — given the partial (1-word) decode, does this instruction
--                 require a second fetch word?
--   @dec1@      — produce an instruction from a single word
--   @dec2@      — produce an instruction from two words (first, then second)
--
--   Returns Nothing on the stall cycle when waiting for the second word.
decodeStep
    :: (instr -> Bool)      -- True → instruction needs a second word
    -> (w -> instr)         -- 1-word decode
    -> (w -> w -> instr)    -- 2-word decode
    -> DecodeState w
    -> w
    -> (DecodeState w, Maybe instr)
decodeStep needsTwo dec1 _    AwaitFirst       w0 =
    let instr = dec1 w0
    in if needsTwo instr
       then (AwaitSecond w0, Nothing)
       else (AwaitFirst,     Just instr)
decodeStep _        _    dec2 (AwaitSecond w0) w1 =
    (AwaitFirst, Just (dec2 w0 w1))

-- | Mealy machine wrapping 'decodeStep'.
--   Input:  stream of fetch words (one per clock).
--   Output: Nothing while waiting for the second word, Just instr when complete.
instructionDecoder
    :: (NFDataX w, HiddenClockResetEnable dom)
    => (instr -> Bool)
    -> (w -> instr)
    -> (w -> w -> instr)
    -> Signal dom w
    -> Signal dom (Maybe instr)
instructionDecoder needsTwo dec1 dec2 =
    mealy (decodeStep needsTwo dec1 dec2) AwaitFirst
