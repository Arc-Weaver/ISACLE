{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

-- | A small monad for /building/ an instruction encoding from fixed bits and
-- typed field placeholders, instead of a hand-written character string.
--
-- @
-- instr = do
--     d <- defineInstruction $ do
--         fixed "1001000"
--         d <- placeholder \@(Unsigned 5)   -- allocate the Rd field (typed, 5 bits)
--         bind d                            -- place its bits here
--         fixed "0001"
--         return d
--     v <- readMem =<< readField avrZ
--     writeRegFileF avrGPR d v              -- d is the field, no string
-- @
--
-- A 'Field' is the only name a field has: it carries the field's width and a
-- generated key, is /placed/ in the encoding by 'bind' (a split field is just
-- two 'bind's of the same placeholder), and is read as a value by 'fieldVal'.
-- 'defineInstruction' assembles the fragments into the existing encoding-string
-- form and returns the placeholders for the body.
module Isacle.ISA.EncodingDSL
    ( Encoding
    , Field
    , fldWidth
    , fldKey
    , fixed
    , placeholder
    , bind
    , field
    , bindBits
    , runEncoding
    , fieldVal
    ) where

import Prelude
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Control.Monad.State.Strict
import GHC.TypeLits (KnownNat, natVal)

import Hdl.Types (HdlType, Width)
import Isacle.ISA.IR (IExpr(..), FieldRef(..))

-- | The encoding-builder monad: accumulates the encoding characters left to
-- right (MSB first) and hands out fresh field keys.
type Encoding = State EncSt

data EncSt = EncSt
    { esChars   :: String   -- ^ accumulated encoding characters (MSB→LSB)
    , esNextKey :: Char     -- ^ next field key to allocate
    }

-- | A typed field placeholder: its value type fixes the width; the key names it
-- in the assembled encoding (and in the IR).
data Field (t :: Type) = Field
    { fldKey   :: Char
    , fldWidth :: Int
    }

-- | Append fixed opcode bits (@\'0\'@/@\'1\'@/@\'.\'@; underscores ignored).
fixed :: String -> Encoding ()
fixed bits = modify $ \s ->
    s { esChars = esChars s ++ filter (/= '_') bits }

-- | Allocate a typed field placeholder of width @'Width' t@. Does not place any
-- bits — use 'bind' to put them in the encoding.
placeholder :: forall t. HdlType t => Encoding (Field t)
placeholder = do
    s <- get
    let k = esNextKey s
    put s { esNextKey = succ k }
    pure (Field k (fromIntegral (natVal (Proxy @(Width t)))))

-- | Place a placeholder's bits at the current position (MSB→LSB). Calling 'bind'
-- twice on one placeholder lays it out split across the encoding.
bind :: Field t -> Encoding ()
bind (Field k w) = modify $ \s ->
    s { esChars = esChars s ++ replicate w k }

-- | Allocate /and/ place a contiguous field in one step (the common case):
-- @field \@(Unsigned 5)@ ≡ @placeholder \@(Unsigned 5) >>= \\f -> bind f >> pure f@.
field :: forall t. HdlType t => Encoding (Field t)
field = do { f <- placeholder @t; bind f; pure f }

-- | Place @n@ bits of a placeholder at the current position — for a split field,
-- emit its high group then (later) its low group. The placements of one field,
-- read MSB-first by position, reconstruct its value. (@bind f ≡ bindBits f (fldWidth f)@.)
bindBits :: Field t -> Int -> Encoding ()
bindBits (Field k _) n = modify $ \s ->
    s { esChars = esChars s ++ replicate n k }

-- | Run an encoding builder: the result (the placeholders) and the assembled
-- encoding string in the existing character format.
runEncoding :: Encoding a -> (a, String)
runEncoding m =
    let (a, s) = runState m (EncSt "" 'a') in (a, esChars s)

-- | Read a field as a width-typed value expression (the decoded field bits).
fieldVal :: KnownNat (Width t) => Field t -> IExpr (Width t)
fieldVal (Field k _) = IField (FieldRef [k])
