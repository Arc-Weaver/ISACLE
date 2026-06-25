{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}
-- | Interactive TinyCPU simulator.
--
-- Write a program using the assembler helpers, optionally set initial
-- register values with 'load', then call 'run' to execute it.
--
-- Usage:
--   cabal run test-tiny-run
module Main where

import Prelude
import Data.Bits
import Data.List (intercalate)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict    as Map

import Isacle.ISA.Backend.Sim
import Isacle.ISA.CPUDef    (runCPUDef)
import Isacle.ISA.Def       (ISADef(isaInstrs))
import Isacle.ISA.Example.Tiny

-- ---------------------------------------------------------------------------
-- Assembler helpers
-- ---------------------------------------------------------------------------
-- Registers are 0–3.  Instruction words are 8-bit Integers.

nop :: Integer
nop = 0x00

-- | ADD rd, rs  →  rd = rd + rs
add :: Int -> Int -> Integer
add rd rs = 0x10 .|. fromIntegral ((rs .&. 3) `shiftL` 2) .|. fromIntegral (rd .&. 3)

-- | MOV rd, rs  →  rd = rs
mov :: Int -> Int -> Integer
mov rd rs = 0x20 .|. fromIntegral ((rs .&. 3) `shiftL` 2) .|. fromIntegral (rd .&. 3)

-- | JMP k  →  PC = k  (k is a 6-bit absolute address, 0–63)
jmp :: Int -> Integer
jmp k = 0xC0 .|. fromIntegral (k .&. 0x3F)

-- ---------------------------------------------------------------------------
-- Initial state helpers
-- ---------------------------------------------------------------------------

-- | Build an initial CPU state with the given register values.
-- Pass a list of (register-index, value) pairs; PC defaults to 0.
load :: [(Int, Integer)] -> SimState
load regs = emptySim
    { ssCPU = (ssCPU emptySim)
        { scRegs = Map.fromList $
            ("PC", 0) : [ ("GPR:" ++ show i, v) | (i, v) <- regs ]
        }
    }

-- ---------------------------------------------------------------------------
-- Simulator
-- ---------------------------------------------------------------------------

-- | Execute a program for at most 'steps' cycles starting from 'state'.
-- The program is loaded into code memory; data memory is taken from 'state'.
run :: SimState -> [Integer] -> Int -> SimState
run initSt prog steps = runN steps initSt'
  where
    initSt' = initSt { ssCodeMem = IntMap.fromList (zip [0..] prog) }
    (aluRec, _) = runCPUDef tinyCPUDef
    [nopBody, addBody, movBody, jmpBody] = isaInstrs tinyISA

    decode w
        | w == 0x00           = nopBody
        | w .&. 0xF0 == 0x10 = addBody
        | w .&. 0xF0 == 0x20 = movBody
        | w .&. 0xC0 == 0xC0 = jmpBody
        | otherwise           = nopBody   -- unknown → treat as NOP

    runN 0 st = st
    runN n st =
        let pc    = Map.findWithDefault 0 "PC" (scRegs (ssCPU st))
            word  = IntMap.findWithDefault 0x00 (fromIntegral pc) (ssCodeMem st)
            -- Increment PC before executing; JMP overwrites it
            st'   = st { ssCPU = (ssCPU st)
                { scRegs = Map.insert "PC" (pc + 1) (scRegs (ssCPU st)) } }
        in runN (n - 1) (runInstr aluRec word (decode word) st')

-- ---------------------------------------------------------------------------
-- Pretty-printer
-- ---------------------------------------------------------------------------

showState :: SimState -> String
showState st =
    let regs = scRegs (ssCPU st)
        pc   = Map.findWithDefault 0 "PC" regs
        gprs = [ "r" ++ show i ++ "=" ++
                 show (Map.findWithDefault 0 ("GPR:" ++ show i) regs)
               | i <- [0..3 :: Int] ]
    in "PC=" ++ show pc ++ "  " ++ intercalate "  " gprs

-- | Print register state after N, 2N, 3N, … steps up to 'total'.
trace :: SimState -> [Integer] -> Int -> Int -> IO ()
trace initSt prog stepSize total = do
    mapM_ step (takeWhile (<= total) [stepSize, stepSize*2 ..])
    putStrLn $ "final (" ++ show total ++ " steps): " ++ showState (run initSt prog total)
  where
    step n = putStrLn $ "after " ++ show n ++ " steps: " ++
                         showState (run initSt prog n)

-- ---------------------------------------------------------------------------
-- *** EDIT BELOW TO TRY YOUR OWN PROGRAMS ***
-- ---------------------------------------------------------------------------

main :: IO ()
main = do

    -- ------------------------------------------------------------------
    -- Example 1: count up to 6 using ADD
    -- r0 starts at 0, r1 starts at 2, r0 = r0+r1 each iteration
    -- ------------------------------------------------------------------
    putStrLn "=== Example 1: r0 += r1 (r1=2) each cycle, loop ==="
    let prog1 = [ add 0 1    -- 0x00: r0 = r0 + r1
                , jmp 0      -- 0x01: loop back to 0x00
                ]
    let st1 = load [(0, 0), (1, 2)]
    trace st1 prog1 1 6

    -- ------------------------------------------------------------------
    -- Example 2: Fibonacci (r0, r1 hold consecutive terms)
    -- r0=0, r1=1 → after each pair of ADDs: r0=fib(n), r1=fib(n+1)
    -- ------------------------------------------------------------------
    putStrLn "\n=== Example 2: Fibonacci steps ==="
    let prog2 = [ add 0 1    -- 0x00: r0 = r0 + r1  (new r0 = fib(n+1))
                , mov 2 0    -- 0x01: r2 = r0        (save new r0)
                , mov 0 1    -- 0x02: r0 = r1        (old r1)
                , add 1 2    -- 0x03: r1 = r1 + r2   (r1 = old r1 + new r0 = fib(n+2))
                , jmp 0      -- 0x04: loop
                ]
    let st2 = load [(0, 0), (1, 1)]
    trace st2 prog2 5 25

    -- ------------------------------------------------------------------
    -- Example 3: copy r3 into r0 via r2 (shows MOV chains)
    -- ------------------------------------------------------------------
    putStrLn "\n=== Example 3: MOV chain r3→r2→r1→r0 ==="
    let prog3 = [ mov 2 3    -- r2 = r3
                , mov 1 2    -- r1 = r2
                , mov 0 1    -- r0 = r1
                , nop
                ]
    let st3 = load [(3, 42)]
    putStrLn $ "after 3 steps: " ++ showState (run st3 prog3 3)

    -- ------------------------------------------------------------------
    -- *** YOUR PROGRAM HERE ***
    -- Uncomment and edit this block to try your own code:
    -- ------------------------------------------------------------------
    -- putStrLn "\n=== My program ==="
    -- let myProg = [ add 0 1
    --              , jmp 0
    --              ]
    -- let mySt = load [(0, 0), (1, 5)]
    -- trace mySt myProg 1 10
