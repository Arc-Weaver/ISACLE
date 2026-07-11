{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
import Prelude
import qualified Data.Map.Strict as Map
import System.Exit (exitFailure)

import Hdl.Net
import Hdl.Sig
import Hdl.Prim
import Hdl.Class
import Hdl.Entity hiding (entity)
import Hdl.IO (bind, entity)
import Hdl.Emit.Vhdl

-- ---------------------------------------------------------------------------
-- Test clock domain
-- ---------------------------------------------------------------------------

data Clk

instance KnownDom Clk where
    domId _ = DomId "clk" 100000000 Rising ActiveHigh "rst"

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

assert :: String -> Bool -> IO ()
assert msg False = putStrLn ("FAIL: " ++ msg) >> exitFailure
assert msg True  = putStrLn ("ok:   " ++ msg)

isSubstr :: String -> String -> Bool
isSubstr needle haystack = go needle haystack
  where
    go [] _               = True
    go _  []              = False
    go n@(x:xs) (y:ys)
        | x == y          = go xs ys || go n ys
        | otherwise       = go n ys

countNodes :: (NetNode -> Bool) -> [NetNode] -> Int
countNodes p = length . filter p

-- ---------------------------------------------------------------------------
-- Test: single-entity adder
-- ---------------------------------------------------------------------------

testAdder :: IO ()
testAdder = do
    let nodes = execNetM $ do
            a <- inputS @Clk @(Unsigned 8) "a"
            b <- inputS @Clk @(Unsigned 8) "b"
            outputS @Clk @(Unsigned 8) "q" (a + b)
        vhdl = emitVhdl Map.empty "adder8" nodes
    assert "adder: has NInput nodes"     (length [n | n@NInput{} <- nodes]  == 2)
    assert "adder: has NOutput node"     (length [n | n@NOutput{} <- nodes] == 1)
    assert "adder: has NComb PAdd"       (any isPAdd nodes)
    assert "adder: VHDL contains entity" ("entity adder8" `isSubstr` vhdl)
    assert "adder: uses unsigned type"   ("unsigned(7 downto 0)" `isSubstr` vhdl)
  where
    isPAdd (NComb _ PAdd _) = True
    isPAdd _                = False

-- ---------------------------------------------------------------------------
-- Test: register with mdo feedback
-- ---------------------------------------------------------------------------

testCounter :: IO ()
testCounter = do
    let nodes = execNetM $ do
            en <- inputS @Clk @Bool "en"
            n  <- mdo
                    reg0 <- regEnS @Clk @(Unsigned 16) 0 en (reg0 + 1)
                    return reg0
            outputS @Clk @(Unsigned 16) "count" n
    assert "counter: has NReg node"   (any isReg nodes)
    assert "counter: has NComb PAdd"  (any isPAdd nodes)
    assert "counter: has enable wire" (any hasEn nodes)
  where
    isReg  NReg{}                   = True;  isReg _  = False
    isPAdd (NComb _ PAdd _)         = True;  isPAdd _ = False
    hasEn  (NReg _ _ (Just _) _ _) = True;  hasEn _  = False

-- ---------------------------------------------------------------------------
-- Test: multi-entity hierarchy
-- ---------------------------------------------------------------------------

testHierarchy :: IO ()
testHierarchy = do
    let adder :: Entity (Sig Clk (Unsigned 8), Sig Clk (Unsigned 8))
                        (Sig Clk (Unsigned 8))
        adder = bind "adder8" $ hdl $ \(a, b) -> return (a + b)

        design = execDesign "top" $ do
            x <- inputS @Clk @(Unsigned 8) "x"
            y <- inputS @Clk @(Unsigned 8) "y"
            s <- entity "u1" adder (x, y)
            outputS @Clk @(Unsigned 8) "result" s

    assert "hier: design has 2 entities" (Map.size design == 2)
    assert "hier: top entity present"    ("top"    `Map.member` design)
    assert "hier: adder8 present"        ("adder8" `Map.member` design)
    let topNodes = design Map.! "top"
    assert "hier: top has NSubInst"      (any isSubInst topNodes)
  where
    isSubInst NSubInst{} = True; isSubInst _ = False

-- ---------------------------------------------------------------------------
-- Test: comparison and logical operators
-- ---------------------------------------------------------------------------

testOps :: IO ()
testOps = do
    let nodes = execNetM $ do
            a  <- inputS @Clk @(Unsigned 8) "a"
            b  <- inputS @Clk @(Unsigned 8) "b"
            en <- inputS @Clk @Bool "en"
            let eq  = a .==. b
                gEn = en .&&. eq
            outputS @Clk @Bool "match_en" gEn
    assert "ops: has PEq"  (any isEq nodes)
    assert "ops: has PAnd" (any isAnd nodes)
  where
    isEq  (NComb _ PEq  _) = True; isEq _  = False
    isAnd (NComb _ PAnd _) = True; isAnd _ = False

-- ---------------------------------------------------------------------------
-- Test: mux operation and VHDL type inference
-- ---------------------------------------------------------------------------

testMux :: IO ()
testMux = do
    let nodes = execNetM $ do
            sel <- inputS @Clk @Bool          "sel"
            t   <- inputS @Clk @(Unsigned 32) "t"
            f   <- inputS @Clk @(Unsigned 32) "f"
            outputS @Clk @(Unsigned 32) "q" (mux sel t f)
        vhdl = emitVhdl Map.empty "mux32" nodes
    assert "mux: has PMux node"        (any isMux nodes)
    assert "mux: output is u32"        ("unsigned(31 downto 0)" `isSubstr` vhdl)
  where
    isMux (NComb _ PMux _) = True; isMux _ = False

-- ---------------------------------------------------------------------------
-- Test: named wire hints appear as signal names in VHDL
-- ---------------------------------------------------------------------------

testNamed :: IO ()
testNamed = do
    let nodes = execNetM $ do
            a <- inputS @Clk @(Unsigned 8) "a"
            b <- inputS @Clk @(Unsigned 8) "b"
            s <- named "sum" (a + b)
            outputS @Clk @(Unsigned 8) "q" s
        vhdl = emitVhdl Map.empty "named_test" nodes
    assert "named: NHint node present"    (any isHint nodes)
    assert "named: 'sum' appears in VHDL" ("sum" `isSubstr` vhdl)
  where
    isHint NHint{} = True; isHint _ = False

-- ---------------------------------------------------------------------------
-- Test: bit extraction
-- ---------------------------------------------------------------------------

testBitSlice :: IO ()
testBitSlice = do
    let nodes = execNetM $ do
            v   <- inputS @Clk @(Unsigned 8) "v"
            let b0 = sigBit 0 v
                b7 = sigBit 7 v
            outputS @Clk @Bool "lsb" b0
            outputS @Clk @Bool "msb" b7
    assert "slice: two PSlice nodes" (countNodes isSlice nodes == 2)
  where
    isSlice (NComb _ (PSlice _ _) _) = True; isSlice _ = False

-- ---------------------------------------------------------------------------
-- Test: static and dynamic shifts
-- ---------------------------------------------------------------------------

testShifts :: IO ()
testShifts = do
    let nodes = execNetM $ do
            v   <- inputS @Clk @(Unsigned 16) "v"
            amt <- inputS @Clk @(Unsigned 4)  "amt"
            let sl  = sigShiftL 3 v         -- static left  by 3
                sr  = sigShiftR 3 v         -- static right by 3
                dsl = sigShiftLDyn v amt    -- dynamic left
                dsr = sigShiftRDyn v amt    -- dynamic right
            outputS @Clk @(Unsigned 16) "sl"  sl
            outputS @Clk @(Unsigned 16) "sr"  sr
            outputS @Clk @(Unsigned 16) "dsl" dsl
            outputS @Clk @(Unsigned 16) "dsr" dsr
    assert "shifts: at least 4 PShiftL/R nodes" (countNodes isShift nodes >= 4)
  where
    isShift (NComb _ PShiftL _) = True
    isShift (NComb _ PShiftR _) = True
    isShift _                   = False

-- ---------------------------------------------------------------------------
-- Test: block RAM — exactly one NMem node
-- ---------------------------------------------------------------------------

testRam :: IO ()
testRam = do
    let nodes = execNetM $ do
            rdAddr <- inputS @Clk @(Unsigned 5) "rd_addr"
            wrAddr <- inputS @Clk @(Unsigned 5) "wr_addr"
            wrData <- inputS @Clk @(Unsigned 8) "wr_data"
            wrEn   <- inputS @Clk @Bool         "wr_en"
            rdData <- ram @Clk @(Unsigned 8) 32 [] rdAddr wrAddr wrData wrEn
            outputS @Clk @(Unsigned 8) "rd_data" rdData
        vhdl = emitVhdl Map.empty "ram32x8" nodes
    assert "ram: exactly 1 NMem node"         (countNodes isMem nodes == 1)
    assert "ram: NMem has size 32"            (all (\n -> nMemSize n == 32) [n | n@NMem{} <- nodes])
    assert "ram: NMem has data width 8"       (all (\n -> nMemDatW n == 8)  [n | n@NMem{} <- nodes])
    assert "ram: array type declared"         ("type ram_" `isSubstr` vhdl)
    assert "ram: write port in clock process" ("rising_edge" `isSubstr` vhdl)
    assert "ram: read port is combinational"  ("to_integer" `isSubstr` vhdl)
  where
    isMem NMem{} = True; isMem _ = False

-- ---------------------------------------------------------------------------
-- Test: ROM — exactly one NRom node
-- ---------------------------------------------------------------------------

testRom :: IO ()
testRom = do
    let contents = [0..15] :: [Integer]
        nodes = execNetM $ do
            addr <- inputS @Clk @(Unsigned 4) "addr"
            dout <- rom @Clk @(Unsigned 8) 16 contents addr
            outputS @Clk @(Unsigned 8) "dout" dout
        vhdl = emitVhdl Map.empty "rom16x8" nodes
    assert "rom: exactly 1 NRom node"    (countNodes isRom nodes == 1)
    assert "rom: NRom has size 16"       (all (\n -> nRomSize n == 16) [n | n@NRom{} <- nodes])
    assert "rom: NRom has data width 8"  (all (\n -> nRomDatW n == 8)  [n | n@NRom{} <- nodes])
    assert "rom: constant declared"      ("constant rom_" `isSubstr` vhdl)
    assert "rom: read is combinational"  ("to_integer" `isSubstr` vhdl)
  where
    isRom NRom{} = True; isRom _ = False

-- ---------------------------------------------------------------------------
-- Test: multiple instances of the same component
-- ---------------------------------------------------------------------------

testMultiInst :: IO ()
testMultiInst = do
    let half :: Entity (Sig Clk (Unsigned 8), Sig Clk (Unsigned 8))
                       (Sig Clk (Unsigned 8), Sig Clk Bool)
        half = bind "half_adder8" $ hdl $ \(a, b) -> return (a + b, a .==. b)

        design = execDesign "top" $ do
            x <- inputS @Clk @(Unsigned 8) "x"
            y <- inputS @Clk @(Unsigned 8) "y"
            z <- inputS @Clk @(Unsigned 8) "z"
            (s1, _) <- entity "u_ha0" half (x, y)
            (s2, _) <- entity "u_ha1" half (s1, z)
            outputS @Clk @(Unsigned 8) "result" s2

    let topNodes = design Map.! "top"
    assert "multi: 2 entities in design"      (Map.size design == 2)
    assert "multi: 2 NSubInst in top"         (countNodes isSubInst topNodes == 2)
    assert "multi: half_adder8 defined once"  ("half_adder8" `Map.member` design)
  where
    isSubInst NSubInst{} = True; isSubInst _ = False

-- ---------------------------------------------------------------------------
-- Test: 2-stage pipeline (two chained registers)
-- ---------------------------------------------------------------------------

testPipeline :: IO ()
testPipeline = do
    let nodes = execNetM $ do
            din  <- inputS @Clk @(Unsigned 16) "din"
            -- two pipeline stages
            s1 <- regS @Clk @(Unsigned 16) 0 din >>= named "stage1"
            s2 <- regS @Clk @(Unsigned 16) 0 s1  >>= named "stage2"
            outputS @Clk @(Unsigned 16) "dout" s2
        vhdl = emitVhdl Map.empty "pipeline2" nodes
    assert "pipe: 2 NReg nodes"          (countNodes isReg nodes == 2)
    assert "pipe: stage1 in VHDL"        ("stage1" `isSubstr` vhdl)
    assert "pipe: stage2 in VHDL"        ("stage2" `isSubstr` vhdl)
    assert "pipe: clock process present" ("rising_edge" `isSubstr` vhdl)
  where
    isReg NReg{} = True; isReg _ = False

-- ---------------------------------------------------------------------------
-- Test: saturating accumulator (clamps at max value)
--   acc <= acc + din  when en = 1 and acc < maxVal
-- ---------------------------------------------------------------------------

testAccumulator :: IO ()
testAccumulator = do
    let nodes = execNetM $ do
            din    <- inputS @Clk @(Unsigned 16) "din"
            en     <- inputS @Clk @Bool          "en"
            maxVal <- inputS @Clk @(Unsigned 16) "max_val"
            acc <- mdo
                let next      = acc + din
                    saturated = maxVal .<. next   -- next > max?
                    clamp     = mux saturated maxVal next
                    gated     = mux en clamp acc  -- hold when disabled
                acc <- regS @Clk @(Unsigned 16) 0 gated
                return acc
            outputS @Clk @(Unsigned 16) "acc" acc
    assert "accum: has NReg"     (any isReg nodes)
    assert "accum: has PAdd"     (any isPAdd nodes)
    assert "accum: has PLt"      (any isPLt nodes)
    assert "accum: has 2 PMux"   (countNodes isMux nodes == 2)
  where
    isReg  NReg{}             = True; isReg  _ = False
    isPAdd (NComb _ PAdd _)   = True; isPAdd _ = False
    isPLt  (NComb _ PLt  _)   = True; isPLt  _ = False
    isMux  (NComb _ PMux _)   = True; isMux  _ = False

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn "=== isacle-hdl tests ==="
    putStrLn "\n-- adder --"
    testAdder
    putStrLn "\n-- counter (mdo) --"
    testCounter
    putStrLn "\n-- hierarchy --"
    testHierarchy
    putStrLn "\n-- combinational ops --"
    testOps
    putStrLn "\n-- mux --"
    testMux
    putStrLn "\n-- named signals --"
    testNamed
    putStrLn "\n-- bit slice --"
    testBitSlice
    putStrLn "\n-- shifts --"
    testShifts
    putStrLn "\n-- block RAM --"
    testRam
    putStrLn "\n-- ROM --"
    testRom
    putStrLn "\n-- multiple instances --"
    testMultiInst
    putStrLn "\n-- pipeline registers --"
    testPipeline
    putStrLn "\n-- saturating accumulator --"
    testAccumulator
    putStrLn "\n=== all tests passed ==="
