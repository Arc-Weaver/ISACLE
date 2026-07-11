{-# LANGUAGE GADTs                #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
-- | The ISA instruction IR — the well-typed /source of truth/ an instruction
-- body constructs, mirroring how 'Isacle.System.BusDef' is the source of truth
-- for a bus.  Backends are /renderers/ over this IR (synthesis → netlist,
-- simulation → state transition, documentation → prose) rather than leaky
-- typeclass interpreters.
--
-- = Why this exists, and why it is value-typed
--
-- An 'IExpr' is indexed by the **HDL value type** it computes — @IExpr (Unsigned
-- 8)@, @IExpr (Signed 8)@ — not by a bare width.  The type drives /both/ the wire
-- width (@'Width' a@) /and/ the signedness (@'hdlRepr' a@) at lowering, so there
-- is no @PMulSigned@/@aluOp@ opcode zoo: signed vs unsigned is the operand type,
-- and a body switches between them with an explicit reinterpret cast
-- ('asSigned' \/ 'asUnsigned'), exactly like the HDL layer's @sigReinterpret@.
--
-- Reading a register always yields the register's /declared/ type
-- (@readReg :: CPURegister t -> m (IExpr t)@); you never \"read as signed\".
--
-- Two arithmetic surfaces mirror the HDL layer:
--
--   * 'Num' (@+@ @-@ @*@) — **modular, same width** (PC+1, address wrap).
--   * 'add' \/ 'mul'      — **width-growing, carry\/sign-correct** (the 'Arith'
--                           lift): @add :: IExpr a -> IExpr b -> IExpr (AddR a b)@,
--                           the extra top bit /is/ the carry.
--
-- @add@\/@mul@ are smart constructors: they resize each operand (repr-correctly:
-- sign-extend signed, zero-extend unsigned) to the result type and emit an
-- ordinary @IBin@ at that width — a modular multiply at @n+m@ bits /is/ the full
-- product.  So no new lowering case is needed for growing arithmetic; the only
-- genuinely new node is 'IReinterpret' (the same-width repr cast).
--
-- = Annotations
--
-- The IR is the only place names live, so it carries them: field keys
-- ('FieldRef'), register/file names ('RegRef'), the mnemonic and encoding on
-- 'InstrIR', and an explicit 'INamed' wrapper.  A renderer turns these into
-- @hintWire@ calls so the generated VHDL reads as @match_ADD@, @ADD_d@,
-- @GPR_rd0@, @SP_en@, … instead of anonymous @wN@.
module Isacle.ISA.IR
    ( -- * Annotations
      FieldRef(..)
    , RegRef(..)
    , ReadTok(..)
      -- * The value-typed IR expression
    , IExpr(..)
      -- * Width recovery / repr for renderers
    , exprWidth
    , exprRepr
      -- * Width-growing arithmetic (the Arith lift; re-uses Hdl.Bits AddR/MulR)
    , add
    , mul
      -- * Reinterpret casts (same width, change of representation)
    , asSigned
    , asUnsigned
    , reinterpret
      -- * Bit-width adapters (named to match Hdl.Bits so bodies are unchanged)
    , zeroExtend
    , signExtend
    , truncateB
    , bitCoerce
    , slice
      -- * Width-checked variants (type-level width laws; opt-in)
    , zeroExtendC
    , signExtendC
    , truncateC
    , sliceC
      -- * Bitwise / logic combinators (replace the old aluOp surface)
    , (.&.)
    , (.|.)
    , xor
    , inv
    , ifexp
    , shiftL
    , shiftR
    , arithShiftR
    , isZeroE
      -- * Ordered effects and the per-instruction IR
    , IStmt(..)
    , InstrIR(..)
    , emptyInstrIR
    ) where

import Prelude
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (natVal, type (<=))

import Hdl.Net (Repr(..))
import Hdl.Sig (HdlType(..), Width)
import Hdl.Bits (Unsigned, Signed, Arith, AddR, MulR)
import Isacle.ISA.Types (ALUPrim(..), CPUFlag(..))

-- ---------------------------------------------------------------------------
-- Annotations
-- ---------------------------------------------------------------------------

-- | A decoded instruction field, keyed by its pattern letter(s) — @"d"@, @"r"@,
-- @"k"@ — used to name extracted field wires (@\<mnemonic\>_\<key\>@).
newtype FieldRef = FieldRef { frKey :: String }
    deriving (Eq, Show)

-- | A reference to a register or a register-file slot, carrying the name used
-- for signal naming.  Indexed by the register's value type @a@.
data RegRef (a :: Type)
    = RegScalar String              -- ^ scalar register (e.g. @"SP"@, @"PC"@)
    | RegFile   String FieldRef Int Int
                                    -- ^ register-file slot: file name, index field,
                                    --   index /scale/ and /offset/ — the runtime index
                                    --   is @scale * field + offset@.  Scale 1 is the
                                    --   plain case; sub-range encodings add an offset
                                    --   (e.g. AVR R16–R31 → +16) and register-/pair/
                                    --   encodings use scale 2 (e.g. ADIW 24+2·d).
    | RegEntries String Int [Int]   -- ^ a /view/ register: a value spanning several
                                    --   register-file entries (file name, element
                                    --   width, indices low entry first), e.g. AVR
                                    --   X = GPR[26]:GPR[27].  Reads concatenate the
                                    --   entries; writes split across them.
    deriving (Eq, Show)

-- | Identifies one ordered memory/code read so its result expression
-- ('IReadRes') can refer back to it, and so the synthesiser can sequence and
-- name it.
newtype ReadTok = ReadTok Int
    deriving (Eq, Ord, Show)

-- ---------------------------------------------------------------------------
-- The value-typed expression
-- ---------------------------------------------------------------------------

-- | A pure, value-typed expression: the dataflow source of truth.  Each
-- constructor captures an 'HdlType' dictionary so a renderer can recover the
-- width (@'Width' a@) and representation (@'hdlRepr' a@).  An 'IExpr' never
-- contains a 'Hdl.Net.WireId'.
data IExpr (a :: Type) where
    ILit      :: HdlType a => Integer -> IExpr a
    IField    :: HdlType a => FieldRef -> IExpr a
    IReadReg  :: HdlType a => RegRef a -> IExpr a
    IReadRes  :: HdlType a => ReadTok -> IExpr a
    IFlagRead :: CPUFlag -> IExpr Bool             -- ^ read one status-register bit
    IIrqVector :: HdlType a => IExpr a             -- ^ external interrupt-vector input
    IBin      :: HdlType a => ALUPrim -> IExpr a -> IExpr a -> IExpr a
    IUn       :: HdlType a => ALUPrim -> IExpr a -> IExpr a
    -- | Multiplexer: @IMux c t f@ is @t@ when the 1-bit @c@ is set, else @f@.
    -- Lowers to a real mux (@… when … else …@), not a bit-mask.
    IMux      :: HdlType a => IExpr Bool -> IExpr a -> IExpr a -> IExpr a
    -- | Same-width reinterpretation: keep the bits, change the representation
    -- (e.g. @Unsigned 8 → Signed 8@).  Lowers to a real VHDL @signed()@\/
    -- @unsigned()@ cast (the HDL layer's @PReinterpret@).
    IReinterpret :: (HdlType a, HdlType b, Width a ~ Width b) => IExpr a -> IExpr b
    IResize   :: (HdlType a, HdlType b) => IExpr a -> IExpr b
    ISignExt  :: (HdlType a, HdlType b) => IExpr a -> IExpr b
    IZeroExt  :: (HdlType a, HdlType b) => IExpr a -> IExpr b
    ITrunc    :: (HdlType a, HdlType b) => IExpr a -> IExpr b
    IIsZero   :: HdlType a => IExpr a -> IExpr Bool
    ISlice    :: (HdlType a, HdlType b) => Int -> Int -> IExpr a -> IExpr b
      -- ^ @ISlice hi lo e@ — bits [hi..lo] inclusive of @e@, as a @b@-typed value.
    INamed    :: HdlType a => String -> IExpr a -> IExpr a

deriving instance Show (IExpr a)

-- | The width of an expression's value type, for renderers.
exprWidth :: forall a. HdlType a => IExpr a -> Int
exprWidth _ = fromIntegral (natVal (Proxy @(Width a)))

-- | The representation (unsigned\/signed\/…) of an expression's value type.
exprRepr :: forall a. HdlType a => IExpr a -> Repr
exprRepr _ = hdlRepr (Proxy @a)

-- | @sp - 1@ builds @'IBin' 'PSub' sp ('ILit' 1)@ — a value — at the register's
-- own width (modular), instead of doing integer math on a payload.
instance HdlType a => Num (IExpr a) where
    a + b       = IBin PAdd a b
    a - b       = IBin PSub a b
    a * b       = IBin PMul a b
    negate a    = IBin PSub (ILit 0) a
    abs a       = a
    signum _    = ILit 1
    fromInteger = ILit

-- ---------------------------------------------------------------------------
-- Width-growing arithmetic — the 'Arith' lift, as smart constructors.
--
-- Resize each operand (sign-extend if signed, zero-extend if unsigned) to the
-- result type, then a plain modular 'IBin' at that width: a + b fits in
-- @Max n m + 1@ bits (top bit = carry) and a * b fits in @n + m@ bits, so the
-- modular op at the result width /is/ the exact result.  No new lowering case.
-- ---------------------------------------------------------------------------

-- | Repr-correct widen: sign-extend a signed source, zero-extend an unsigned one.
widenTo :: forall a c. (HdlType a, HdlType c) => IExpr a -> IExpr c
widenTo = case hdlRepr (Proxy @a) of
    RSigned -> ISignExt
    _       -> IZeroExt

-- | Width-growing add: the result type holds the carry in its extra top bit.
add :: forall a b. (Arith a b, HdlType (AddR a b))
    => IExpr a -> IExpr b -> IExpr (AddR a b)
add a b = IBin PAdd (widenTo a) (widenTo b)

-- | Width-growing multiply: full @n + m@-bit product (signed when operands are).
mul :: forall a b. (Arith a b, HdlType (MulR a b))
    => IExpr a -> IExpr b -> IExpr (MulR a b)
mul a b = IBin PMul (widenTo a) (widenTo b)

-- ---------------------------------------------------------------------------
-- Reinterpret casts — same width, change of representation (the unsigned↔signed
-- seam).  This is how a body opts a value into signed arithmetic.
-- ---------------------------------------------------------------------------

-- | Reinterpret the same bits at a different representation/type of equal width.
reinterpret :: (HdlType a, HdlType b, Width a ~ Width b) => IExpr a -> IExpr b
reinterpret = IReinterpret

-- | View an unsigned value as signed (two's-complement), same bits.
asSigned :: HdlType (Unsigned n) => IExpr (Unsigned n) -> IExpr (Signed n)
asSigned = IReinterpret

-- | View a signed value as unsigned, same bits.
asUnsigned :: HdlType (Signed n) => IExpr (Signed n) -> IExpr (Unsigned n)
asUnsigned = IReinterpret

-- ---------------------------------------------------------------------------
-- Bit-width adapters — named to match the Hdl.Bits functions bodies call, so
-- migrating a body only changes its value type, not its call sites.
-- ---------------------------------------------------------------------------

zeroExtend :: (HdlType a, HdlType b) => IExpr a -> IExpr b
zeroExtend = IZeroExt

signExtend :: (HdlType a, HdlType b) => IExpr a -> IExpr b
signExtend = ISignExt

truncateB :: (HdlType a, HdlType b) => IExpr a -> IExpr b
truncateB = ITrunc

bitCoerce :: (HdlType a, HdlType b) => IExpr a -> IExpr b
bitCoerce = IResize

slice :: (HdlType a, HdlType b) => Int -> Int -> IExpr a -> IExpr b
slice = ISlice

-- ---------------------------------------------------------------------------
-- Width-checked variants — identical lowering to the adapters above, but the
-- type-level width law is enforced in the signature.  Opt-in.
-- ---------------------------------------------------------------------------

-- | Zero-extend, statically guaranteed to grow (or stay): @Width a <= Width b@.
zeroExtendC :: (HdlType a, HdlType b, Width a <= Width b) => IExpr a -> IExpr b
zeroExtendC = IZeroExt

-- | Sign-extend, statically guaranteed to grow (or stay): @Width a <= Width b@.
signExtendC :: (HdlType a, HdlType b, Width a <= Width b) => IExpr a -> IExpr b
signExtendC = ISignExt

-- | Truncate, statically guaranteed to shrink (or stay): @Width b <= Width a@.
truncateC :: (HdlType a, HdlType b, Width b <= Width a) => IExpr a -> IExpr b
truncateC = ITrunc

-- | Slice bits @[hi..lo]@ inclusive.  Bounds are runtime 'Int's here (the
-- type-level-checked form lived on the old Nat index); the result type's width
-- must be @hi - lo + 1@.
sliceC :: (HdlType a, HdlType b) => Int -> Int -> IExpr a -> IExpr b
sliceC = ISlice

-- ---------------------------------------------------------------------------
-- Bitwise / logic combinators — the typed surface that replaces @aluOp PAnd@…
-- Signedness of shifts is the operand type, resolved at lowering by repr.
-- ---------------------------------------------------------------------------

infixl 7 .&.
infixl 5 .|.

(.&.) :: HdlType a => IExpr a -> IExpr a -> IExpr a
a .&. b = IBin PAnd a b

(.|.) :: HdlType a => IExpr a -> IExpr a -> IExpr a
a .|. b = IBin POr a b

xor :: HdlType a => IExpr a -> IExpr a -> IExpr a
xor a b = IBin PXor a b

-- | Bitwise complement (NOT).  Named 'inv' to avoid the Data.Bits clash.
inv :: HdlType a => IExpr a -> IExpr a
inv = IUn PNot

-- | @ifexp c t f@ — a value-typed conditional expression (multiplexer): @t@ when
-- the 1-bit @c@ is set, else @f@.  A first-class mux in the expression system.
ifexp :: HdlType a => IExpr Bool -> IExpr a -> IExpr a -> IExpr a
ifexp = IMux

shiftL :: HdlType a => IExpr a -> IExpr a -> IExpr a
shiftL a n = IBin PShiftL a n

shiftR :: HdlType a => IExpr a -> IExpr a -> IExpr a
shiftR a n = IBin PShiftR a n

-- | Arithmetic (sign-replicating) shift right — a genuinely distinct operation
-- from the logical 'shiftR', kept as its own node (AVR @ASR@).
arithShiftR :: HdlType a => IExpr a -> IExpr a -> IExpr a
arithShiftR a n = IBin PArithShiftR a n

-- | Test whether a value is zero, as a 1-bit ('Bool') result.
isZeroE :: HdlType a => IExpr a -> IExpr Bool
isZeroE = IIsZero

-- ---------------------------------------------------------------------------
-- Ordered effects and the per-instruction IR
-- ---------------------------------------------------------------------------

-- | The ordered effects of an instruction, in program order.  Memory and code
-- reads are statements (not just expressions) because their order drives the
-- execution sequencer; their results are referred to via 'IReadRes' / 'ReadTok'.
data IStmt where
    SReadMem  :: HdlType aw => ReadTok -> IExpr aw -> IStmt
    SReadCode :: HdlType aw => ReadTok -> IExpr aw -> IStmt
    SWriteReg :: HdlType a => RegRef a -> IExpr a -> IStmt
    SWriteMem :: (HdlType aw, HdlType ww) => IExpr aw -> IExpr ww -> IStmt
    SWriteFlag :: CPUFlag -> IExpr Bool -> IStmt
    SJumpIf   :: HdlType a => RegRef a -> IExpr Bool -> IExpr a -> IStmt

deriving instance Show IStmt

-- | One instruction's IR: annotations plus its ordered statements.
data InstrIR = InstrIR
    { iirMnemonic :: Maybe String   -- ^ → @match_\<mnemonic\>@, field-wire prefix
    , iirDoc      :: Maybe String
    , iirEncoding :: Maybe String   -- ^ encoding pattern (e.g. @"0000_11rd…"@)
    , iirGate     :: Maybe (IExpr Bool)-- ^ extra match condition (irqGate); ANDed in
    , iirStmts    :: [IStmt]        -- ^ program order
    } deriving (Show)

emptyInstrIR :: InstrIR
emptyInstrIR = InstrIR Nothing Nothing Nothing Nothing []
