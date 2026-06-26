{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Prelude
import Hdl.Net
import Isacle.ISA.IR
import Isacle.ISA.Build
import Isacle.ISA.Backend.Lower

dom :: DomId
dom = DomId "sys" 1 Rising ActiveHigh "rst"

-- A stub lowering context: register reads / fields become named input wires.
ctx :: LowerCtx
ctx = LowerCtx
    { lcReadReg  = \_ -> do { w <- freshWire; emit (NInput w "r"  16 dom); pure w }
    , lcField    = \_ -> do { w <- freshWire; emit (NInput w "f"  16 dom); pure w }
    , lcReadRes  = \_ -> do { w <- freshWire; emit (NInput w "rd" 16 dom); pure w }
    , lcMnemonic = Just "RCALL"
    }

-- ---------------------------------------------------------------------------
-- Demo 1: naming injection + following on a hand-built expression tree
-- ---------------------------------------------------------------------------

demoNaming :: IO ()
demoNaming = do
    let pc  = IReadReg (RegScalar "PC") :: IExpr 16
        sp  = IReadReg (RegScalar "SP") :: IExpr 16
        ret = INamed "ret" (pc + 1)          -- naming injection
        e   = ret - sp                       -- following: ret_sub_SP
        d   = IField (FieldRef "k") :: IExpr 16
        top = e + d
    let (_, nodes, _) = runNetM (lowerExpr_ ctx top)
    putStrLn "== naming injection + following =="
    mapM_ putStrLn [ "  w" ++ show w ++ " => " ++ n | NHint w n <- nodes ]

-- ---------------------------------------------------------------------------
-- Demo 2: a real instruction body built through the IR builder.
-- PUSH Rr:  read SP, read GPR[d], mem[SP] <- val, SP <- SP - 1.
-- The `sp - 1` that used to corrupt a wire id now builds IBin PSub and lowers
-- to a real subtractor — shown by the emitted PSub node and its name.
-- ---------------------------------------------------------------------------

pushBody :: ISABuild ()
pushBody = do
    setMnemonic "PUSH"
    setEncoding "1001_001d_dddd_1111"
    spV <- readRegB (RegScalar "SP" :: RegRef 16)
    val <- readRegB (RegFile "GPR" (FieldRef "d") :: RegRef 8)
    writeMemB spV val
    one <- litB 1
    writeRegB (RegScalar "SP") (spV - one)   -- <-- the previously-broken decrement

demoBuild :: IO ()
demoBuild = do
    let ir = runISABuild pushBody
    putStrLn "== InstrIR for PUSH =="
    putStrLn ("  mnemonic = " ++ show (iirMnemonic ir))
    putStrLn ("  encoding = " ++ show (iirEncoding ir))
    putStrLn ("  #stmts   = " ++ show (length (iirStmts ir)))
    -- Lower the SP write-back expression (SP - 1) and show it is a subtractor.
    -- Built via the IExpr Num instance — exactly what `sp - 1` does in a body.
    let spMinus1 = (IReadReg (RegScalar "SP") :: IExpr 16) - 1
        (_, nodes, _) = runNetM (lowerExpr_ ctx spMinus1)
    putStrLn "  SP-1 lowers to:"
    mapM_ putStrLn [ "    w" ++ show o ++ " = " ++ show op ++ " " ++ show ins
                   | NComb o op ins <- nodes ]
    mapM_ putStrLn [ "    name w" ++ show w ++ " => " ++ n | NHint w n <- nodes ]

main :: IO ()
main = do
    demoNaming
    putStrLn ""
    demoBuild
