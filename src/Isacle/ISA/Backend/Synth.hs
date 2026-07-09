{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Synthesis renderer for the ISA IR.
--
-- An instruction body is run through 'Isacle.ISA.Build.ISABuild' to produce an
-- 'InstrIR' (the source of truth); 'renderSynth' lowers that IR into the
-- per-instruction request structures the CPU pass
-- ('Isacle.ISA.Backend.SynthCPU') assembles into register files, memory ports
-- and the execution sequencer.  No instruction body is interpreted here, and no
-- 'WireId' is ever smuggled into a value — wires are minted only during
-- lowering ('Isacle.ISA.Backend.Lower').
module Isacle.ISA.Backend.Synth
    ( -- * Collected per-instruction outputs
      SynthResult(..)
    , RegWriteReq(..)
    , ScalarWriteReq(..)
    , RegReadReq(..)
    , MemWriteReq(..)
    , MemReadReq(..)
    , CodeReadReq(..)
    , FlagWriteReq(..)
      -- * Rendering
    , RenderCtx(..)
    , renderSynth
      -- * Combinational helpers reused by the CPU pass
    , buildMatch
    , extractFieldSig
    ) where

import Prelude hiding (Word)
import Control.Monad (forM)
import Data.Either (partitionEithers)
import Data.List (elemIndex)
import Data.Proxy (Proxy(..))
import qualified Data.Map.Strict as Map
import GHC.TypeLits (natVal, someNatVal, SomeNat(..))

import qualified Hdl.Net as N
import Hdl.Types (Signal(..), KnownDom, HdlType, Width)
import Hdl.Prim (Unsigned)
import Hdl.Monad (Hdl(..))
import Isacle.ISA.Types (CPUFlag(..))
import Isacle.ISA.Encoding
import Isacle.ISA.IR
import Isacle.ISA.Backend.Lower

-- ---------------------------------------------------------------------------
-- Per-instruction output signals (consumed by SynthCPU's arbiters/sequencer).
-- Every "wire" is now a typed 'Signal' value — no NetM, no WireId.
-- ---------------------------------------------------------------------------

-- | Register-file (indexed) write request — guarded by 'rwMatchWire'.  The index
-- and data carry their own value types (existential — different files differ).
data RegWriteReq s dom = forall iw dw. (HdlType iw, HdlType dw) => RegWriteReq
    { rwMatchWire :: s dom Bool
    , rwRfName    :: String
    , rwIdxWire   :: s dom iw
    , rwDataWire  :: s dom dw
    }

-- | Scalar-register write request (PC, SP, …) — guarded by 'swMatchWire'.  The
-- data is at the register's value type @w@.
data ScalarWriteReq s dom = forall w. HdlType w => ScalarWriteReq
    { swMatchWire :: s dom Bool
    , swRegName   :: String
    , swDataWire  :: s dom w
    }

-- | Register-file read request (index signal + the read-result signal).
data RegReadReq s dom = forall iw ow. (HdlType iw, HdlType ow) => RegReadReq
    { rrRfName  :: String
    , rrIdxWire :: s dom iw
    , rrOutWire :: s dom ow
    }

-- | Data-memory write request.
data MemWriteReq s dom = forall aw dw. (HdlType aw, HdlType dw) => MemWriteReq
    { mwMatchWire :: s dom Bool
    , mwAddrWire  :: s dom aw
    , mwDataWire  :: s dom dw
    }

-- | Data-memory read request.  The CPU sequencer produces its result from
-- 'mrBusWire' (the bus, directly or via a per-cycle select + holding latch).
data MemReadReq s dom = forall aw dw. (HdlType aw, HdlType dw) => MemReadReq
    { mrMatchWire  :: s dom Bool
    , mrTok        :: Int          -- ^ the read's 'ReadTok' (NOT its position —
                                   --   code reads also consume tokens)
    , mrAddrWire   :: s dom aw
    , mrBusWire    :: s dom dw
    }

-- | Code-memory read request (an instruction operand word).  Like 'MemReadReq'
-- but on the code bus: the CPU sequencer drives 'crAddrWire' (a code-word
-- address, e.g. @PC+1@, @PC+2@) onto the code operand address and latches the
-- code data bus.  "The code memory is a bus; more words are more reads."
data CodeReadReq s dom = forall aw. HdlType aw => CodeReadReq
    { crMatchWire :: s dom Bool
    , crTok       :: Int
    , crAddrWire  :: s dom aw
    }

-- | Flag write: set one status-register bit when the instruction fires.
data FlagWriteReq s dom = FlagWriteReq
    { fwMatchWire :: s dom Bool
    , fwRegName   :: String
    , fwBitPos    :: Int
    , fwValueWire :: s dom Bool
    }

-- | All combinational outputs of one instruction.
data SynthResult s dom = SynthResult
    { srMatchWire    :: Maybe (s dom Bool)
    , srRegWrites    :: [RegWriteReq s dom]
    , srScalarWrites :: [ScalarWriteReq s dom]
    , srRegReads     :: [RegReadReq s dom]
    , srMemWrites    :: [MemWriteReq s dom]
    , srMemReads     :: [MemReadReq s dom]
    , srCodeReads    :: [CodeReadReq s dom]
    , srFlagWrites   :: [FlagWriteReq s dom]
    }

-- ---------------------------------------------------------------------------
-- Render context
-- ---------------------------------------------------------------------------

-- | Resolution the CPU pass supplies for one instruction slot.  Register/field
-- reads are /typed by the demand/ (@forall a. HdlType a => … -> s dom a@) — the
-- register was created at its type, and the read produces that type, so nothing
-- is erased.  @rcReadRes@ is the per-read result the sequencer drives (the CPU
-- renders twice — see 'SynthCPU').
data RenderCtx s dom = RenderCtx
    { rcInstrWire  :: forall a. HdlType a => s dom a          -- ^ instruction word (field source)
    , rcReadScalar :: forall a. HdlType a => String -> s dom a -- ^ scalar register reader
    , rcDataBus    :: forall a. HdlType a => s dom a          -- ^ data_rd_data
    , rcCodeBus    :: forall a. HdlType a => s dom a          -- ^ code read bus (LPM/2nd word)
    , rcCodeWord   :: Int -> (forall a. HdlType a => s dom a) -- ^ the @j@-th sequentially-fetched
                                                              --   operand word (a latched register)
    , rcReadRes    :: forall a. HdlType a => ReadTok -> s dom a -- ^ per-read result (from sequencer)
    , rcGetFlag    :: String -> Int -> s dom Bool             -- ^ status-bit reader
    , rcRegCount   :: String -> Int                           -- ^ register-file entry count
    , rcIrqVector  :: forall a. HdlType a => Maybe (s dom a)  -- ^ irq_vector (in an IRQ body)
    , rcWordW      :: Int                                      -- ^ data word width (write clamp)
    }

-- ---------------------------------------------------------------------------
-- Render
-- ---------------------------------------------------------------------------

-- | Lower one instruction's 'InstrIR' into a 'SynthResult'.
--
-- @mBase@ is the base match condition: 'Nothing' to derive it from the encoding
-- (normal instructions), or @Just w@ to seed it (e.g. @irq_pending@).
renderSynth :: forall s m dom. (Hdl s m, Signal s, KnownDom dom)
            => RenderCtx s dom -> Maybe (s dom Bool) -> InstrIR -> m (SynthResult s dom)
renderSynth ctx mBase ir = do
    let mEnc = fmap parseEncoding (iirEncoding ir)
        instrW :: forall a. HdlType a => s dom a
        instrW = rcInstrWire ctx

    -- Base match signal.
    base <- case mBase of
        Just w  -> pure w
        Nothing -> case (mEnc, iirMnemonic ir) of
            (Just e, Just nm) -> named ("match_" ++ nm) (buildMatch e instrW)
            (Just e, Nothing) -> pure (buildMatch e instrW)
            (Nothing, _)      -> pure (sigLitW 1 1)

    -- A field of the instruction word, at the demanded type; and a register-file
    -- index derived from it (raw field, scaled, offset for sub-range encodings).
    let fieldOf :: forall a. HdlType a => String -> s dom a
        fieldOf k = case mEnc >>= Map.lookup k . encFields of
            Just bits -> extractFieldSig bits (instrW :: s dom a)
            Nothing   -> sigLit 0
        idxOf :: forall a. HdlType a => String -> Int -> Int -> s dom a
        idxOf k sc off
            | null k    = sigLit (fromIntegral off)
            | otherwise =
                let fw     = fieldOf k :: s dom a
                    scaled = if sc == 1 then fw
                             else sigPrim2 N.PMul fw (sigLit (fromIntegral sc) :: s dom a) :: s dom a
                in if off == 0 then scaled
                   else sigPrim2 N.PAdd scaled (sigLit (fromIntegral off) :: s dom a) :: s dom a

    -- Read-result routing.  Code operand words are fetched SEQUENTIALLY into
    -- registers, one per fetch cycle (the CPU walks PC, PC+1, PC+2, latching each
    -- word).  The @j@-th @readCode@ in program order therefore reads back the
    -- @j@-th latched operand register — @rcCodeWord j@ — which is a plain register,
    -- so forcing it forces a register (not the exec sequencer) and the mfix knot
    -- is broken.  Data reads still go through the sequencer.
    let codeToks = [ t | SReadCode (ReadTok t) _ <- iirStmts ir ]
        readResSig :: forall a. HdlType a => ReadTok -> s dom a
        readResSig tok@(ReadTok t)
            | Just j <- elemIndex t codeToks = rcCodeWord ctx j
            | otherwise                      = rcReadRes ctx tok

    -- A view register: concat its constant-index entry reads (low first), at @a@.
    let readViewM :: forall a. HdlType a => String -> Int -> [Int] -> m (s dom a)
        readViewM file ew idxs = do
            entries <- forM idxs $ \idx ->
                regBankRead "cpu_state" file (rcRegCount ctx file)
                            (sigLitW (fromIntegral idx) 8 :: s dom (Unsigned 8))
            let wa     = fromIntegral (natVal (Proxy @(Width a)))
                part p e = sigPrim2 N.PShiftL
                             (sigPrim1 (N.PResize wa) (e :: s dom (Unsigned 8)) :: s dom a)
                             (sigLitW (fromIntegral (p * ew)) wa :: s dom a)
            pure $ case zip [0 :: Int ..] entries of
                     [] -> sigLitW 0 (max wa 1)
                     ps -> foldr1 (sigPrim2 N.POr) [ part p e | (p, e) <- ps ]

    let lctx = LowerCtx
            { lcReadReg = \ref -> case ref of
                  RegScalar n                    -> pure (rcReadScalar ctx n)
                  RegFile rf (FieldRef k) sc off -> do
                      out <- regBankRead "cpu_state" rf (rcRegCount ctx rf)
                                         (idxOf k sc off :: s dom (Unsigned 8))
                      named (rf ++ "_" ++ slotTag k off) out
                  RegEntries file ew idxs        -> readViewM file ew idxs
            , lcField     = \(FieldRef k) -> pure (fieldOf k)
            , lcReadRes   = \t -> pure (readResSig t)
            , lcReadFlag  = \f -> pure (rcGetFlag ctx (cpuFlagReg f) (cpuFlagBit f))
            , lcIrqVector = pure (maybe (sigLit 0) id (rcIrqVector ctx))
            , lcMnemonic  = iirMnemonic ir
            }

    -- irqGate refines the match condition.
    matchW <- case iirGate ir of
        Nothing -> pure base
        Just g  -> sigPrim2 N.PAnd base <$> lowerExpr_ lctx g

    r <- renderInstr lctx ir

    -- Map the (typed) lowered 'Rendered' into request structures.
    let splitW (RegWrite (RegScalar n) w)                 = Left  (ScalarWriteReq matchW n w)
        splitW (RegWrite (RegFile rf (FieldRef k) sc off) w) =
            Right (RegWriteReq matchW rf (idxOf k sc off :: s dom (Unsigned 8)) w)
        splitW (RegWrite (RegEntries{}) _) =
            error "view-register write should have been fanned out in renderInstr"
        (scalarWs, regWs) = partitionEithers (map splitW (rRegWrites r))

        memWrites = [ MemWriteReq matchW a d          | MemWrite a d      <- rMemWrites r ]
        memReads  = [ MemReadReq matchW t a (rcDataBus ctx :: s dom (Unsigned 8))
                    | MemRead (ReadTok t) a <- rMemReads r ]
        codeReads = [ CodeReadReq matchW t a | MemRead (ReadTok t) a <- rCodeReads r ]
        flagWrites = [ FlagWriteReq matchW (cpuFlagReg f) (cpuFlagBit f) w
                     | FlagWrite f w <- rFlagWrites r ]
        jumpWs = [ ScalarWriteReq (sigPrim2 N.PAnd matchW cond) (regRefName rr) tgt
                 | Jump rr cond tgt <- rJumps r ]

    pure SynthResult
        { srMatchWire    = Just matchW
        , srRegWrites    = regWs
        , srScalarWrites = scalarWs ++ jumpWs
        , srRegReads     = []
        , srMemWrites    = memWrites
        , srMemReads     = memReads
        , srCodeReads    = codeReads
        , srFlagWrites   = flagWrites
        }

regRefName :: RegRef w -> String
regRefName (RegScalar n)      = n
regRefName (RegFile  n _ _ _) = n
regRefName (RegEntries n _ _) = n

-- | Name a register-file slot: a field key (with optional @_pN@ offset) or, for
-- a constant index (empty key), the index number @rN@.
slotTag :: String -> Int -> String
slotTag k off
    | null k    = "r" ++ show off
    | off == 0  = k
    | otherwise = k ++ "_p" ++ show off

-- ---------------------------------------------------------------------------
-- Combinational helpers (pure Signal, shared with the CPU pass)
-- ---------------------------------------------------------------------------

-- | The combinational instruction-match signal: @(instr AND mask) == value@.
-- The instruction word is used at its encoding width (reflected to a type).
buildMatch :: forall s dom. Signal s
           => EncodingInfo -> (forall a. HdlType a => s dom a) -> s dom Bool
buildMatch enc instrW =
    case someNatVal (fromIntegral (encTotalBits enc)) of
        Just (SomeNat (_ :: Proxy n)) ->
            let iw = instrW :: s dom (Unsigned n)
                masked = sigPrim2 N.PAnd iw (sigLitW (encMask enc) (encTotalBits enc) :: s dom (Unsigned n))
                           :: s dom (Unsigned n)
            in sigPrim2 N.PEq masked
                 (sigLitW (encValue enc) (encTotalBits enc) :: s dom (Unsigned n))
        Nothing -> sigLitW 1 1

-- | Extract non-contiguous field bits (MSB-first), placed into a value of type
-- @a@ by shift-or (each bit at its position within the field).
extractFieldSig :: forall s dom x a. (Signal s, HdlType x, HdlType a)
                => [Int] -> s dom x -> s dom a
extractFieldSig bps instrW =
    let n  = length bps
        wa = fromIntegral (natVal (Proxy @(Width a)))
        part i bp = sigPrim2 N.PShiftL
                      (sigPrim1 (N.PResize wa) (sigPrim1 (N.PSlice bp bp) instrW :: s dom (Unsigned 1)) :: s dom a)
                      (sigLitW (fromIntegral (n - 1 - i)) wa :: s dom a)
    in case bps of
         [] -> sigLitW 0 (max wa 1)
         _  -> foldr1 (sigPrim2 N.POr) [ part i bp | (i, bp) <- zip [0 ..] bps ]
