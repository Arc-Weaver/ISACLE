{-# LANGUAGE GADTs                #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
-- | The ISA instruction IR — the well-typed /source of truth/ an instruction
-- body constructs, mirroring how 'Isacle.System.BusDef' is the source of truth
-- for a bus.  Backends are /renderers/ over this IR (synthesis → netlist,
-- simulation → state transition, documentation → prose) rather than leaky
-- typeclass interpreters.
--
-- = Why this exists
--
-- The previous design made the value type the concrete @Unsigned w@ and had the
-- synthesis backend smuggle a 'Hdl.Net.WireId' into its @Integer@ payload.  That
-- let value arithmetic (e.g. @sp - 1@) silently corrupt wire references — the
-- type system could not object because a value type with a value 'Num' is
-- exactly what makes the mistake well-typed.
--
-- Here the value a body manipulates is an 'IExpr' (via the 'Term' class): a
-- pure, width-typed expression that /builds IR/.  @sp - 1@ constructs
-- @'IBin' 'PSub' (…) ('ILit' 1)@ — a value, by construction — which the
-- synthesiser lowers to a subtractor.  A 'WireId' is minted only during
-- lowering and never appears in the IR.
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
      -- * The value-term interface (bodies are written against this)
    , Term(..)
      -- * Canonical instance: the annotated IR expression
    , IExpr(..)
      -- * Ordered effects and the per-instruction IR
    , IStmt(..)
    , InstrIR(..)
    , emptyInstrIR
    ) where

import Prelude
import Data.Kind (Type)
import GHC.TypeLits (Nat, KnownNat)

import Isacle.ISA.Types (ALUPrim(..), CPUFlag(..))

-- ---------------------------------------------------------------------------
-- Annotations
-- ---------------------------------------------------------------------------

-- | A decoded instruction field, keyed by its pattern letter(s) — @"d"@, @"r"@,
-- @"k"@ — used to name extracted field wires (@\<mnemonic\>_\<key\>@).
newtype FieldRef = FieldRef { frKey :: String }
    deriving (Eq, Show)

-- | A reference to a register or a register-file slot, carrying the name used
-- for signal naming.
data RegRef (w :: Nat)
    = RegScalar String              -- ^ scalar register (e.g. @"SP"@, @"PC"@)
    | RegFile   String FieldRef     -- ^ register-file slot: file name + index field
    deriving (Eq, Show)

-- | Identifies one ordered memory/code read so its result expression
-- ('IReadRes') can refer back to it, and so the synthesiser can sequence and
-- name it.
newtype ReadTok = ReadTok Int
    deriving (Eq, Ord, Show)

-- ---------------------------------------------------------------------------
-- The value term — pure, width-typed, annotated
-- ---------------------------------------------------------------------------

-- | A pure, width-typed value expression: the dataflow source of truth.  Each
-- constructor captures its @KnownNat@ dictionaries so a renderer can recover
-- widths, and carries the annotations needed to name signals.  An 'IExpr'
-- never contains a 'Hdl.Net.WireId'.
data IExpr (w :: Nat) where
    ILit     :: KnownNat w => Integer -> IExpr w
    IField   :: KnownNat w => FieldRef -> IExpr w
    IReadReg :: KnownNat w => RegRef w -> IExpr w
    IReadRes :: KnownNat w => ReadTok -> IExpr w
    IBin     :: KnownNat w => ALUPrim -> IExpr w -> IExpr w -> IExpr w
    IUn      :: KnownNat w => ALUPrim -> IExpr w -> IExpr w
    IResize  :: (KnownNat k, KnownNat w) => IExpr k -> IExpr w
    ISignExt :: (KnownNat k, KnownNat w) => IExpr k -> IExpr w
    IZeroExt :: (KnownNat k, KnownNat w) => IExpr k -> IExpr w
    ITrunc   :: (KnownNat k, KnownNat w) => IExpr k -> IExpr w
    IIsZero  :: KnownNat k => IExpr k -> IExpr 1
    INamed   :: KnownNat w => String -> IExpr w -> IExpr w

deriving instance Show (IExpr w)

-- | The interface an instruction body uses to build values.  'IExpr' is the
-- canonical instance (it builds annotated IR); the class keeps bodies decoupled
-- from the IR representation.
class Term (t :: Nat -> Type) where
    tLit     :: KnownNat w => Integer -> t w
    tBin     :: KnownNat w => ALUPrim -> t w -> t w -> t w
    tUn      :: KnownNat w => ALUPrim -> t w -> t w
    tResize  :: (KnownNat k, KnownNat w) => t k -> t w
    tSignExt :: (KnownNat k, KnownNat w) => t k -> t w
    tZeroExt :: (KnownNat k, KnownNat w) => t k -> t w
    tTrunc   :: (KnownNat k, KnownNat w) => t k -> t w
    tIsZero  :: KnownNat k => t k -> t 1
    tNamed   :: KnownNat w => String -> t w -> t w

instance Term IExpr where
    tLit     = ILit
    tBin     = IBin
    tUn      = IUn
    tResize  = IResize
    tSignExt = ISignExt
    tZeroExt = IZeroExt
    tTrunc   = ITrunc
    tIsZero  = IIsZero
    tNamed   = INamed

-- | The instance that matters: @sp - 1@ builds @'IBin' 'PSub' sp ('ILit' 1)@,
-- a value, instead of doing integer math on a payload.
instance KnownNat w => Num (IExpr w) where
    a + b       = IBin PAdd a b
    a - b       = IBin PSub a b
    a * b       = IBin PMul a b
    negate a    = IBin PSub (ILit 0) a
    abs a       = a
    signum _    = ILit 1
    fromInteger = ILit

-- ---------------------------------------------------------------------------
-- Ordered effects and the per-instruction IR
-- ---------------------------------------------------------------------------

-- | The ordered effects of an instruction, in program order.  Memory and code
-- reads are statements (not just expressions) because their order drives the
-- execution sequencer; their results are referred to via 'IReadRes' / 'ReadTok'.
data IStmt where
    SReadMem  :: ReadTok -> IExpr aw -> IStmt
    SReadCode :: ReadTok -> IExpr aw -> IStmt
    SWriteReg :: RegRef w -> IExpr w -> IStmt
    SWriteMem :: IExpr aw -> IExpr ww -> IStmt
    SWriteFlag :: CPUFlag -> IExpr 1 -> IStmt
    SJumpIf   :: RegRef w -> IExpr 1 -> IExpr w -> IStmt

deriving instance Show IStmt

-- | One instruction's IR: annotations plus its ordered statements.
data InstrIR = InstrIR
    { iirMnemonic :: Maybe String   -- ^ → @match_\<mnemonic\>@, field-wire prefix
    , iirDoc      :: Maybe String
    , iirEncoding :: Maybe String   -- ^ encoding pattern (e.g. @"0000_11rd…"@)
    , iirStmts    :: [IStmt]        -- ^ program order
    } deriving (Show)

emptyInstrIR :: InstrIR
emptyInstrIR = InstrIR Nothing Nothing Nothing []
