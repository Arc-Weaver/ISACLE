{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
-- | Tests for the simulation interpreter ('Hdl.Sim'): the Signal-level 'SimSig'
-- and the graph-level 'simulateDesign', including a cross-check that the sim
-- agrees with the GHDL-verified signed ramp datapath.
module Tests.Isacle.Sim (runSimTests) where

import Prelude
import qualified Data.Map.Strict as M
import System.Exit (exitFailure)

import Hdl.Net
import Hdl.Sig
import Hdl.Class (inputS, outputS)
import Hdl.Bits  (Signed)
import Hdl.Prim  (Unsigned)
import Hdl.Sim
import Isacle.Periph.Ramp (rampFSM)

assert :: String -> Bool -> IO ()
assert msg False = putStrLn ("FAIL: " ++ msg) >> exitFailure
assert msg True  = putStrLn ("ok:   " ++ msg)

data D
instance KnownDom D where
    domId _ = DomId "d" 1 Rising ActiveHigh "rst"

dom :: DomId
dom = DomId "d" 1 Rising ActiveHigh "rst"

runSimTests :: IO ()
runSimTests = do
    -- Signal-level: operation semantics anchored on the value type.
    assert "signed -3 < 2 = True"
        (simResult ((simLit (-3 :: Signed 8) :: SimSig () (Signed 8)) .<. simLit 2))
    assert "unsigned 250 < 2 = False"
        (not (simResult ((simLit (250 :: Unsigned 8) :: SimSig () (Unsigned 8)) .<. simLit 2)))
    assert "signed 3 + 4 = 7"
        (simResult ((simLit (3 :: Signed 8) :: SimSig () (Signed 8)) + simLit 4) == (7 :: Signed 8))

    -- Graph-level: a counter traces 0,1,2,…
    let counter = execDesign "counter" $ do
            stW   <- freshWire
            oneW  <- freshWire; emit (NComb oneW (PLit 1 8) [])
            nextW <- freshWire; emit (NComb nextW PAdd [stW, oneW])
            emit (NReg stW nextW Nothing (SomeBits 0 8) dom)
            emit (NOutput stW "count" 8 dom)
        ctrace = map (M.findWithDefault (-1) "count")
                     (simulateDesign (counter M.! "counter") M.empty 6)
    assert "counter trace == [0..5]" (ctrace == [0,1,2,3,4,5])

    -- Graph-level: the signed ramp datapath matches GHDL (0 → -2 → -4 → -6,
    -- unsigned-encoded as 0,254,252,250).
    let ramp = execDesign "ramp" $ do
            sp   <- inputS @D @(Signed 8) "sp"
            st   <- inputS @D @(Signed 8) "st"
            tick <- inputS @D @Bool "tick"
            cur  <- rampFSM @D tick (withRepr sp) (withRepr st)
            outputS @D @(Unsigned 8) "cur" cur
        rtrace = map (M.findWithDefault (-1) "cur")
                     (simulateDesign (ramp M.! "ramp")
                                     (M.fromList [("sp", 250), ("st", 2), ("tick", 1)]) 6)
    assert "signed ramp sim == GHDL" (rtrace == [0, 254, 252, 250, 250, 250])

    -- Whole-system: a top entity that instantiates a "+1" sub-entity, flattened
    -- and simulated (dout = din + 1).
    let hier = execDesign "top" $ do
            din <- freshWire; emit (NInput din "din" 8 dom)
            (_, subOuts) <- inBlock "u_inc" "inc" [din] $ do
                xW   <- freshWire; emit (NInput xW "x" 8 dom)
                oneW <- freshWire; emit (NComb oneW (PLit 1 8) [])
                yW   <- freshWire; emit (NComb yW PAdd [xW, oneW])
                emit (NOutput yW "y" 8 dom)
            emit (NOutput (case subOuts of { ((_,w,_):_) -> w; _ -> 0 }) "dout" 8 dom)
    let hout = M.lookup "dout" (head (simulateSystem hier "top" (M.fromList [("din", 5)]) 1))
    assert "flatten+sim hierarchy (dout = din+1)" (hout == Just 6)

    -- Three-level hierarchy: top → mid → leaf (+1), to check the flattening
    -- chains cross-instance connections through more than one level.
    let firstW os = case os of { ((_,w,_):_) -> w; _ -> 0 }
        deep = execDesign "top3" $ do
            din <- freshWire; emit (NInput din "din" 8 dom)
            (_, midOuts) <- inBlock "u_mid" "mid" [din] $ do
                mx <- freshWire; emit (NInput mx "mx" 8 dom)
                (_, leafOuts) <- inBlock "u_leaf" "leaf" [mx] $ do
                    lx  <- freshWire; emit (NInput lx "lx" 8 dom)
                    one <- freshWire; emit (NComb one (PLit 1 8) [])
                    ly  <- freshWire; emit (NComb ly PAdd [lx, one])
                    emit (NOutput ly "ly" 8 dom)
                emit (NOutput (firstW leafOuts) "my" 8 dom)
            emit (NOutput (firstW midOuts) "dout" 8 dom)
        dout3 = M.lookup "dout" (head (simulateSystem deep "top3" (M.fromList [("din", 9)]) 1))
    assert "flatten+sim 3-level hierarchy (dout = din+1)" (dout3 == Just 10)

