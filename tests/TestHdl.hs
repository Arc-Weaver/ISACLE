{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
import Prelude
import qualified Data.Map.Strict as Map
import System.Exit (exitFailure)

import Isacle.Hdl.Net
import Isacle.Hdl.Types
import Isacle.Hdl.Prim
import Isacle.Hdl.Class
import Isacle.Hdl.Emit.Vhdl

-- ---------------------------------------------------------------------------
-- Test clock domain
-- ---------------------------------------------------------------------------

data Clk

instance KnownDom Clk where
    domId _ = DomId "clk" 100000000 Rising ActiveHigh

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

-- ---------------------------------------------------------------------------
-- Test: single-entity adder
-- ---------------------------------------------------------------------------

testAdder :: IO ()
testAdder = do
    let nodes = execNetM $ do
            a <- inputS @Clk @(Unsigned 8) "a"
            b <- inputS @Clk @(Unsigned 8) "b"
            outputS @Clk @(Unsigned 8) "q" (a + b)
        vhdl = emitVhdl "adder8" nodes
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
    let adder :: HdlComponent (Sig Clk (Unsigned 8), Sig Clk (Unsigned 8))
                               (Sig Clk (Unsigned 8))
        adder = block "adder8" $ do
            a <- inputS @Clk @(Unsigned 8) "a"
            b <- inputS @Clk @(Unsigned 8) "b"
            outputS @Clk @(Unsigned 8) "q" (a + b)

        design = execDesign "top" $ do
            x <- inputS @Clk @(Unsigned 8) "x"
            y <- inputS @Clk @(Unsigned 8) "y"
            s <- instComp adder "u1" (x, y)
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
        vhdl = emitVhdl "mux32" nodes
    assert "mux: has PMux node"        (any isMux nodes)
    assert "mux: output is u32"        ("unsigned(31 downto 0)" `isSubstr` vhdl)
  where
    isMux (NComb _ PMux _) = True; isMux _ = False

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
    putStrLn "\n=== all tests passed ==="
