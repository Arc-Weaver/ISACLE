{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Prelude
import Hdl.Net
import Hdl.Types (Sig(..), materialize)
import Hdl.Monad ()            -- the Hdl Sig NetM instance (provides 'named')
import Hdl.Bits (Unsigned)
import Isacle.ISA.IR
import Isacle.ISA.Backend.Lower

domVal :: DomId
domVal = DomId "sys" 1 Rising ActiveHigh "rst"

-- A stub lowering context: each leaf becomes a fresh named input signal.
ctx :: LowerCtx Sig ()
ctx = LowerCtx
    { lcReadReg   = \_ -> inS "r"  16
    , lcField     = \_ -> inS "f"  16
    , lcReadRes   = \_ -> inS "rd" 16
    , lcReadFlag  = \_ -> inS "fl" 1
    , lcIrqVector = inS "iv" 16
    , lcMnemonic  = Just "RCALL"
    }
  where
    inS :: String -> Int -> Sig () ()
    inS nm w = SExpr (do { wid <- freshWire; emit (NInput wid nm w domVal); pure wid })

main :: IO ()
main = do
    -- ret = PC + 1 (named injection), ret - SP (following), + a field operand.
    let pc  = IReadReg (RegScalar "PC") :: IExpr (Unsigned 16)
        sp  = IReadReg (RegScalar "SP") :: IExpr (Unsigned 16)
        ret = INamed "ret" (pc + 1)
        d   = IField (FieldRef "k") :: IExpr (Unsigned 16)
        top = (ret - sp) + d
    let (_, nodes, _) = runNetM (lowerExpr_ ctx top >>= materialize)
    putStrLn "== naming injection + following =="
    mapM_ putStrLn [ "  w" ++ show w ++ " => " ++ n | NHint w n <- nodes ]
