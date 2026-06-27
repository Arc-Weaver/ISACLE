-- | Tests for the ISA ↔ memory-model width check ('Isacle.ISA.WidthCheck', A2):
-- a Harvard ISA runs two checks (code + data), a Von Neumann ISA runs one, and
-- mismatches on either bus are reported.
module Tests.Isacle.ISA.WidthCheck (runWidthCheckTests) where

import Prelude
import System.Exit (exitFailure)

import Isacle.ISA.Encoding  (parseEncoding)
import Isacle.ISA.WidthCheck

assert :: String -> Bool -> IO ()
assert msg False = putStrLn ("FAIL: " ++ msg) >> exitFailure
assert msg True  = putStrLn ("ok:   " ++ msg)

runWidthCheckTests :: IO ()
runWidthCheckTests = do
    -- AVR-shaped Harvard ISA: 16-bit instructions, 8-bit data, MaxFetch 2
    -- (LDS/STS are 32-bit two-word instructions).
    let avr      = Harvard 16 8
        add      = ("ADD", parseEncoding "000011rdddddrrrr")           -- 16 bits
        lds      = ("LDS", parseEncoding "1001000ddddd0000kkkkkkkkkkkkkkkk") -- 32 bits
    assert "Harvard: 16-bit + 32-bit instrs fit code bus (MaxFetch 2)"
        (null (checkInstrWidths (codeWidth avr) 2 [add, lds]))
    assert "Harvard: 8-bit register fits data bus"
        (null (checkDataWidths (dataWidth avr) [("reg", 8)]))
    assert "Harvard: 16-bit value does NOT fit 8-bit data bus"
        (not (null (checkDataWidths (dataWidth avr) [("wide", 16)])))
    assert "Harvard: 32-bit instr exceeds MaxFetch 1"
        (not (null (checkInstrWidths (codeWidth avr) 1 [lds])))

    -- RV32I-shaped Von Neumann ISA: one 32-bit bus, MaxFetch 1.
    let rv       = VonNeumann 32
        addi     = ("ADDI", parseEncoding (replicate 32 '0'))          -- 32 bits
    assert "VN: 32-bit instr fits the unified bus"
        (null (checkInstrWidths (codeWidth rv) 1 [addi]))
    assert "VN: same width governs data values"
        (codeWidth rv == dataWidth rv)

    -- The model determines the shape: VN does one combined check, Harvard two.
    -- A 16-bit value is fine on a VN-32 bus but not on a Harvard-16/8 data bus,
    -- so the same (instr, value) pair passes under one model and fails the other.
    let values   = [("v16", 16)]
    assert "checkMemModel VN-32: 16-bit value OK (one wide bus)"
        (null (checkMemModel (VonNeumann 32) 1 [addi] values))
    assert "checkMemModel Harvard-16/8: 16-bit value rejected on data bus"
        (length (checkMemModel (Harvard 16 8) 2 [add] values) == 1)
