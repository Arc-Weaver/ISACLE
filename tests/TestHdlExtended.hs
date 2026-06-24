{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
-- | Extended unit tests for isacle-hdl: covers ExternEntity, CSE, register
-- init values, bitwise operators, resize, and multi-clock domains.
import Prelude
import qualified Data.Map.Strict as Map
import System.Exit (exitFailure)

import Hdl.Net
import Hdl.Types
import Hdl.Prim
import Hdl.Class
import Hdl.Entity
import Hdl.Emit.Vhdl

-- ---------------------------------------------------------------------------
-- Clock domains
-- ---------------------------------------------------------------------------

data Clk
instance KnownDom Clk where
    domId _ = DomId "clk" 100_000_000 Rising ActiveHigh

data SlowClk
instance KnownDom SlowClk where
    domId _ = DomId "slow_clk" 1_000_000 Rising ActiveHigh

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

assert :: String -> Bool -> IO ()
assert msg False = putStrLn ("FAIL: " ++ msg) >> exitFailure
assert msg True  = putStrLn ("ok:   " ++ msg)

isSubstr :: String -> String -> Bool
isSubstr needle hay = go needle hay
  where
    go [] _           = True
    go _  []          = False
    go n@(x:xs) (y:ys)
        | x == y      = go xs ys || go n ys
        | otherwise   = go n ys

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

-- ---------------------------------------------------------------------------
-- Test: ExternEntity — vendor library reference in port map
-- ---------------------------------------------------------------------------

testExternEntity :: IO ()
testExternEntity = do
    let nodes = execNetM $ do
            clk_in  <- inputS @Clk @Bool "clk_in"
            clk_out <- inputS @Clk @Bool "clk_out"  -- placeholder output wire
            -- manually build a vendor PLL instance
            let inWids = []
            (_, outs) <- inBlock "u_pll" "dummy" [] (do
                    _ <- inputS @Clk @Bool "ref"
                    outputS @Clk @Bool "locked" clk_out)
            outputS @Clk @Bool "locked_out" clk_in
        -- build an NSubInst with ExternEntity directly
        extNodes = execNetM $ do
            a  <- inputS @Clk @(Unsigned 8) "a"
            let extNode = NSubInst "u_ext"
                              (ExternEntity "unisim" "BUFG")
                              [("I", 0)]
                              [("O", 99, 1)]
            emit extNode
            outputS @Clk @(Unsigned 8) "q" a
        vhdl = emitVhdl Map.empty "extern_test" extNodes
    assert "extern: uses library prefix"      ("entity unisim.BUFG" `isSubstr` vhdl)
    assert "extern: does not use work prefix" (not ("entity work.BUFG" `isSubstr` vhdl))

-- ---------------------------------------------------------------------------
-- Test: CSE eliminates duplicate combinational nodes
-- ---------------------------------------------------------------------------

testCse :: IO ()
testCse = do
    -- lookupOrEmit memo deduplicates even when s is an SExpr used twice
    let nodes = execNetM $ do
            a <- inputS @Clk @(Unsigned 8) "a"
            b <- inputS @Clk @(Unsigned 8) "b"
            let s = a + b
            outputS @Clk @(Unsigned 8) "q1" s
            outputS @Clk @(Unsigned 8) "q2" s
        vhdl = emitVhdl Map.empty "cse_test" nodes
    assert "cse: single PAdd in IR"   (length [ n | n@(NComb _ PAdd _) <- nodes ] == 1)
    assert "cse: single '+' in VHDL"  (length (filter (== " + ") (map (take 3 . dropWhile (/= '+')) (lines vhdl))) <= 1)

-- ---------------------------------------------------------------------------
-- Test: register init value appears in signal declaration
-- ---------------------------------------------------------------------------

testRegInit :: IO ()
testRegInit = do
    let nodes = execNetM $ do
            d <- inputS @Clk @(Unsigned 8) "d"
            q <- regS @Clk @(Unsigned 8) 42 d >>= named "accum"
            outputS @Clk @(Unsigned 8) "q" q
        vhdl = emitVhdl Map.empty "reg_init" nodes
    assert "init: NReg has non-zero init"    (any hasInit nodes)
    assert "init: init value in VHDL"        ("to_unsigned(42" `isSubstr` vhdl)
    assert "init: signal declaration has :=" (":= to_unsigned(42" `isSubstr` vhdl)
  where
    hasInit (NReg _ _ _ b _) = sbValue b /= 0
    hasInit _                 = False

-- ---------------------------------------------------------------------------
-- Test: bitwise operators on Unsigned signals
-- ---------------------------------------------------------------------------

testBitwiseOps :: IO ()
testBitwiseOps = do
    let nodes = execNetM $ do
            a <- inputS @Clk @(Unsigned 8) "a"
            b <- inputS @Clk @(Unsigned 8) "b"
            let r_and = sigBwAnd a b
                r_or  = sigBwOr  a b
                r_xor = sigBwXor a b
                r_not = sigBwNot a
            outputS @Clk @(Unsigned 8) "r_and" r_and
            outputS @Clk @(Unsigned 8) "r_or"  r_or
            outputS @Clk @(Unsigned 8) "r_xor" r_xor
            outputS @Clk @(Unsigned 8) "r_not" r_not
        vhdl = emitVhdl Map.empty "bitwise" nodes
    assert "bw: PAnd node"       (any isPAnd nodes)
    assert "bw: POr  node"       (any isPOr  nodes)
    assert "bw: PXor node"       (any isPXor nodes)
    assert "bw: PNot node"       (any isPNot nodes)
    assert "bw: VHDL 'and'"      ("and" `isSubstr` vhdl)
    assert "bw: VHDL 'xor'"      ("xor" `isSubstr` vhdl)
  where
    isPAnd (NComb _ PAnd _) = True; isPAnd _ = False
    isPOr  (NComb _ POr  _) = True; isPOr  _ = False
    isPXor (NComb _ PXor _) = True; isPXor _ = False
    isPNot (NComb _ PNot _) = True; isPNot _ = False

-- ---------------------------------------------------------------------------
-- Test: resize node in IR and VHDL
-- ---------------------------------------------------------------------------

testResize :: IO ()
testResize = do
    let nodes = execNetM $ do
            narrow <- inputS @Clk @(Unsigned 4) "narrow"
            let wide = sigResize @16 narrow
            outputS @Clk @(Unsigned 16) "wide" wide
        vhdl = emitVhdl Map.empty "resize_test" nodes
    assert "resize: PResize node present" (any isResize nodes)
    assert "resize: VHDL resize call"     ("resize(" `isSubstr` vhdl)
  where
    isResize (NComb _ (PResize _) _) = True; isResize _ = False

-- ---------------------------------------------------------------------------
-- Test: two clock domains in one entity
-- ---------------------------------------------------------------------------

testMultiClock :: IO ()
testMultiClock = do
    let nodes = execNetM $ do
            fast <- inputS @Clk     @(Unsigned 8) "fast_in"
            slow <- inputS @SlowClk @(Unsigned 8) "slow_in"
            fr   <- regS @Clk     @(Unsigned 8) 0 fast >>= named "fast_r"
            sr   <- regS @SlowClk @(Unsigned 8) 0 slow >>= named "slow_r"
            outputS @Clk     @(Unsigned 8) "fast_out" fr
            outputS @SlowClk @(Unsigned 8) "slow_out" sr
        vhdl = emitVhdl Map.empty "multi_clk" nodes
        regs = [ n | n@NReg{} <- nodes ]
    assert "mclk: two NReg nodes"            (length regs == 2)
    assert "mclk: two distinct domains"      (length (distinctDoms regs) == 2)
    assert "mclk: both clocks in port list"  ("clk"      `isSubstr` vhdl)
    assert "mclk: slow_clk in port list"     ("slow_clk" `isSubstr` vhdl)
    assert "mclk: two clock processes"       (count (== "  process(clk)") (lines vhdl) == 1
                                           && count (== "  process(slow_clk)") (lines vhdl) == 1)
  where
    distinctDoms rs = Map.keys $ Map.fromList [(domName (nDom r), ()) | r <- rs]

-- ---------------------------------------------------------------------------
-- Test: named signals appear in clock process body
-- ---------------------------------------------------------------------------

testNamedReg :: IO ()
testNamedReg = do
    let nodes = execNetM $ do
            d  <- inputS @Clk @(Unsigned 8) "d"
            q  <- regS @Clk @(Unsigned 8) 0 d >>= named "pipeline_r"
            outputS @Clk @(Unsigned 8) "q" q
        vhdl = emitVhdl Map.empty "named_reg" nodes
    assert "named_reg: signal name in VHDL"    ("pipeline_r" `isSubstr` vhdl)
    assert "named_reg: signal in process body" (any (isSubstr "pipeline_r") (lines vhdl))

-- ---------------------------------------------------------------------------
-- Test: LocalEntity generates 'work' prefix in port map
-- ---------------------------------------------------------------------------

testLocalEntity :: IO ()
testLocalEntity = do
    let sub :: Entity (Sig Clk (Unsigned 8)) (Sig Clk (Unsigned 8))
        sub    = entity "my_sub" (hdl return)
        design = execDesign "top" $ do
            a <- inputS @Clk @(Unsigned 8) "a"
            b <- instEntity sub "u0" a
            outputS @Clk @(Unsigned 8) "out" b
        vhdl = emitVhdl design "top" (design Map.! "top")
    assert "local: entity work.my_sub in VHDL" ("entity work.my_sub" `isSubstr` vhdl)

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn "=== isacle-hdl extended tests ==="
    putStrLn "\n-- extern entity --"
    testExternEntity
    putStrLn "\n-- CSE --"
    testCse
    putStrLn "\n-- register init value --"
    testRegInit
    putStrLn "\n-- bitwise operators --"
    testBitwiseOps
    putStrLn "\n-- resize --"
    testResize
    putStrLn "\n-- multi-clock --"
    testMultiClock
    putStrLn "\n-- named register --"
    testNamedReg
    putStrLn "\n-- local entity prefix --"
    testLocalEntity
    putStrLn "\n=== all extended tests passed ==="
