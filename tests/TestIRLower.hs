{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Prelude
import Hdl.Net
import Isacle.ISA.IR
import Isacle.ISA.Backend.Lower

dom :: DomId
dom = DomId "sys" 1 Rising ActiveHigh "rst"

-- A stub lowering context: leaves become named input wires.
ctx :: LowerCtx
ctx = LowerCtx
    { lcReadReg   = \_ -> do { w <- freshWire; emit (NInput w "r"  16 dom); pure w }
    , lcField     = \_ -> do { w <- freshWire; emit (NInput w "f"  16 dom); pure w }
    , lcReadRes   = \_ -> do { w <- freshWire; emit (NInput w "rd" 16 dom); pure w }
    , lcReadFlag  = \_ -> do { w <- freshWire; emit (NInput w "fl" 1  dom); pure w }
    , lcIrqVector = do { w <- freshWire; emit (NInput w "iv" 16 dom); pure w }
    , lcMnemonic  = Just "RCALL"
    }

main :: IO ()
main = do
    -- ret = PC + 1 (named injection), ret - SP (following), + a field operand.
    let pc  = IReadReg (RegScalar "PC") :: IExpr 16
        sp  = IReadReg (RegScalar "SP") :: IExpr 16
        ret = INamed "ret" (pc + 1)
        d   = IField (FieldRef "k") :: IExpr 16
        top = (ret - sp) + d
    let (_, nodes, _) = runNetM (lowerExpr_ ctx top)
    putStrLn "== naming injection + following =="
    mapM_ putStrLn [ "  w" ++ show w ++ " => " ++ n | NHint w n <- nodes ]
