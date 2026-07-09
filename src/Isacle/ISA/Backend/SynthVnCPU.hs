{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE KindSignatures      #-}
-- | CPU-level synthesis for von Neumann architectures, as 'Hdl' (no 'NetM').
--
-- Mirrors 'Isacle.ISA.Backend.SynthCPU' but simpler: a unified bus (instruction
-- words and data loads share one port), single-cycle accesses (the cache
-- @stall@ freezes the whole pipeline for bus latency, so there is no execution
-- sequencer), and every register enable gated with @~stall@.
module Isacle.ISA.Backend.SynthVnCPU
    ( VnMemIface(..)
    , synthVonNeumannCPU'
    ) where

import Prelude hiding (Word)
import Data.List (foldl', nub, find)
import Data.Proxy (Proxy(..))
import qualified Data.Map.Strict as Map
import Control.Monad (forM)
import Data.Bits ((.|.), shiftL)
import GHC.TypeLits (natVal, KnownNat, someNatVal, SomeNat(..))
import GHC.Generics (Generic, Rep)

import Hdl.Bits (Unsigned(..), Bit(..))
import qualified Hdl.Net as N
import Hdl.Types (KnownDom(..), Signal(..), HdlType, Width, fromBits, GFields, recordFields, projectField, updateField)
import Hdl.Monad (Hdl(..))
import Isacle.ISA.Types
import Isacle.ISA.CPUDef
import Isacle.ISA.Def
import Isacle.ISA.Build (ISABuild, runISABuild)
import Isacle.ISA.Backend.Synth
import Isacle.ISA.Backend.SynthCPU
    ( extractPcName
    , litS, andS, notS, toBool, eqS, muxS, sliceS, resizeS, addS, bwAndS, bwOrS
    , orReduce, priorityMux )

-- ---------------------------------------------------------------------------
-- VN memory interface (CPU outputs; inputs are signal arguments)
-- ---------------------------------------------------------------------------

data VnMemIface s dom addrW wordW = VnMemIface
    { vniFetchAddr  :: s dom (Unsigned addrW)  -- ^ instruction fetch address (PC value)
    , vniDataRdAddr :: s dom (Unsigned addrW)  -- ^ data load address
    , vniDataWrEn   :: s dom Bool              -- ^ data store enable (1-bit)
    , vniDataWrAddr :: s dom (Unsigned addrW)  -- ^ data store address
    , vniDataWrData :: s dom (Unsigned wordW)  -- ^ data store word
    , vniAddrW      :: Int
    , vniWordW      :: Int
    }

-- ---------------------------------------------------------------------------
-- Entry point
-- ---------------------------------------------------------------------------

-- | Synthesise a von Neumann CPU as 'Hdl'.  Inputs (signals): instruction word,
-- data read data, stall (Hi on a cache miss), irq pending / vector.
synthVonNeumannCPU'
    :: forall core s m dom wordW addrW alu.
       ( Hdl s m, Signal s, KnownDom dom
       , HdlType core, Generic core, GFields (Rep core)
       , KnownNat wordW, KnownNat addrW )
    => CPUDef alu
    -> ISADef (ISABuild alu wordW addrW wordW addrW)
    -> s dom (Unsigned wordW)   -- ^ instr_word
    -> s dom (Unsigned wordW)   -- ^ data_rd_data
    -> s dom Bool               -- ^ stall
    -> s dom Bool               -- ^ irq_pending
    -> s dom (Unsigned addrW)   -- ^ irq_vector
    -> m (VnMemIface s dom addrW wordW)
synthVonNeumannCPU' cpuDef isaDef instrSig dmemRdData stallSig irqPendSig irqVecSig = do
    let (aluRec, schema) = runCPUDef cpuDef
        wordBits = fromIntegral (natVal (Proxy @wordW)) :: Int
        addrBits = fromIntegral (natVal (Proxy @addrW)) :: Int

        resetEntries = runResetDef (isaReset isaDef) aluRec
        resetRegMap  = Map.fromList [ (n, v) | ResetRegEntry n (Unsigned v) <- resetEntries ]
        resetFlagContribs = Map.fromListWith (.|.)
            [ (rn, if b == Hi then 1 `shiftL` bp else 0)
            | ResetFlagEntry rn bp b <- resetEntries ]
        initOf name = Map.findWithDefault 0 name resetRegMap
                  .|. Map.findWithDefault 0 name resetFlagContribs

        regList      = schRegisters schema            -- [RegDecl] (typed)
        widthOf name = maybe wordBits rdWidth (find ((== name) . rdName) regList)
        rfInfoMap    = Map.fromList [ (n, (c, w)) | (n, c, w) <- schRegFiles schema ]
        regCount rf  = maybe 1 fst (Map.lookup rf rfInfoMap)
        statusRegMap = Map.fromList [ (n, (w, fs)) | (n, w, fs) <- schStatusRegs schema ]
        pcName       = extractPcName aluRec isaDef
        notStall     = notS stallSig

        -- The Core record's fields (MSB-first) and its packed reset value.
        coreFields = recordFields (Proxy @core)
        coreReset  = fromBits
            (foldl' (\acc (fn, w) -> (acc `shiftL` w) .|. initOf (drop 4 fn)) 0 coreFields) :: core

        -- A register read: project the named field of the core signal at the
        -- demanded type.  Kept /outside/ the @mdo@ knot, applied to @coreState@
        -- inline (a binding in the recursive group would be monomorphised).
        readScalarOf :: forall a. HdlType a => s dom core -> String -> s dom a
        readScalarOf cst n = projectField ("core" ++ n) cst

    mdo
        -- The scalar core (SP/PC/SREG) is one clocked register; the arbiter below
        -- ties its next value.  A register read projects the field at the demanded
        -- type — no erased signal, no per-name map.
        coreState <- named "cpu_core" =<< register coreReset nextCore
        let getFlag :: String -> Int -> s dom Bool
            getFlag rn bp =
                case someNatVal (fromIntegral (maybe wordBits fst (Map.lookup rn statusRegMap))) of
                    Just (SomeNat (_ :: Proxy n)) -> sliceS bp bp (readScalarOf coreState rn :: s dom (Unsigned n))
                    Nothing                       -> litS 0 1
            mkCtx rcIrq = RenderCtx
                { rcInstrWire  = sigRetype instrSig
                , rcReadScalar = readScalarOf coreState
                , rcDataBus    = sigRetype dmemRdData
                , rcCodeBus    = sigRetype dmemRdData   -- VN: unified bus, no separate code port
                , rcReadRes    = const (sigRetype dmemRdData)  -- single-cycle: read result is the bus
                , rcGetFlag    = getFlag
                , rcRegCount   = regCount
                , rcIrqVector  = fmap sigRetype rcIrq
                , rcWordW      = wordBits
                }

        results <- forM (isaInstrs isaDef) $ \instr ->
            renderSynth (mkCtx Nothing) Nothing (runISABuild aluRec instr)
        irqResult <- case isaInterruptBody isaDef of
            Nothing   -> pure Nothing
            Just body -> Just <$>
                renderSynth (mkCtx (Just irqVecSig)) (Just (toBool irqPendSig))
                            (runISABuild aluRec body)
        let allResults   = results ++ maybe [] (: []) irqResult
            allRegWrites = concatMap srRegWrites allResults
            allMemWrites = concatMap srMemWrites allResults
            allMemReads  = concatMap srMemReads allResults

        -- Register files: indexed writes gated by match AND ~stall.  The entry
        -- width is reflected to a type so the ports are typed.
        let involvedRfs = nub ([ nm | RegWriteReq _ nm _ _ <- allRegWrites ]
                               ++ map (\(n,_,_) -> n) (schRegFiles schema))
        mapM_ (\rfname ->
                  let (rfCount, rfWidth) = maybe (1, wordBits) id (Map.lookup rfname rfInfoMap)
                  in case someNatVal (fromIntegral rfWidth) of
                       Just (SomeNat (_ :: Proxy ew)) ->
                         let writes = [ ( sigRetype idx :: s dom (Unsigned 8)
                                        , sigRetype dat :: s dom (Unsigned ew)
                                        , andS match notStall )
                                      | RegWriteReq match nm idx dat <- allRegWrites, nm == rfname ]
                         in regBank "cpu_state" rfname rfCount rfWidth writes
                       Nothing -> pure ())
              involvedRfs

        -- Data writes to a memory-mapped scalar register → a ScalarWriteReq (a
        -- store to the low word of a wide register keeps its high words).
        let aliasScalarWrites =
                [ ScalarWriteReq gated regName full
                | (regName, aliasAddr) <- schAliasRegs schema
                , let regW = widthOf regName
                , MemWriteReq match adr dt <- allMemWrites
                , Just (SomeNat (_ :: Proxy rw)) <- [someNatVal (fromIntegral (max 1 regW))]
                , let addr   = sigRetype adr :: s dom (Unsigned addrW)
                      gated  = andS match (eqS addr (litS aliasAddr addrBits :: s dom (Unsigned addrW)))
                      regSig = readScalarOf coreState regName :: s dom (Unsigned rw)
                      datV   = sigRetype dt :: s dom (Unsigned wordW)
                      highMask = (2 ^ regW - 1) - (2 ^ min regW wordBits - 1) :: Integer
                      full   = bwOrS (bwAndS regSig (litS highMask regW))
                                     (resizeS regW datV :: s dom (Unsigned rw)) ]

        -- Scalar / status write arbiter → the next Core value.  Enables gated ~stall.
        let allScalarWrites = concatMap srScalarWrites allResults ++ aliasScalarWrites
            allFlagWrites   = concatMap srFlagWrites allResults
            writesOfAt :: forall t. HdlType t => String -> [(s dom Bool, s dom t)]
            writesOfAt name = [ (m, sigRetype d :: s dom t)
                              | ScalarWriteReq m nm d <- allScalarWrites, nm == name ]
            statusBits :: forall t. HdlType t => String -> s dom t -> s dom t
            statusBits name cur =
                let w      = fromIntegral (natVal (Proxy @(Width t)))
                    writes = writesOfAt name :: [(s dom Bool, s dom t)]
                    bitNext bitPos =
                        let curBit  = sliceS bitPos bitPos cur :: s dom Bool
                            fwPairs = [ (fwMatchWire fw, fwValueWire fw)
                                      | fw <- allFlagWrites
                                      , fwRegName fw == name, fwBitPos fw == bitPos ]
                            scPairs = [ (m, sliceS bitPos bitPos d :: s dom Bool) | (m, d) <- writes ]
                        in priorityMux (fwPairs ++ scPairs) curBit
                    place i b = sigPrim2 N.PShiftL
                                  (resizeS w (b :: s dom Bool) :: s dom t)
                                  (litS (fromIntegral i) w     :: s dom t) :: s dom t
                in case [ place i (bitNext i) | i <- [0 .. w - 1] ] of
                     []     -> litS 0 (max 1 w)
                     (x:xs) -> foldl' (sigPrim2 N.POr) x xs
            updateReg acc (RegDecl name (_ :: Proxy t)) =
                let cur     = readScalarOf coreState name :: s dom t
                    w       = fromIntegral (natVal (Proxy @(Width t)))
                    writes  = writesOfAt name :: [(s dom Bool, s dom t)]
                    matches = map fst writes
                           ++ [ fwMatchWire fw | fw <- allFlagWrites, fwRegName fw == name ]
                    nxt | name == pcName               = priorityMux writes (addS cur (litS 1 w))
                        | Map.member name statusRegMap = statusBits name cur
                        | otherwise                    = priorityMux writes cur
                    en  | name == pcName = notStall
                        | otherwise      = andS notStall (orReduce matches)
                in updateField ("core" ++ name) (muxS en nxt cur) acc
            nextCore = foldl' updateReg coreState regList

        -- Data-memory address muxes (match directly — single-cycle, no gating).
        let dmemRdAddr = priorityMux [ (mrMatchWire r, sigRetype adr :: s dom (Unsigned addrW))
                                     | r@(MemReadReq _ _ adr _) <- allMemReads ]
                                     (litS 0 addrBits) :: s dom (Unsigned addrW)
            dmemWrEn   = orReduce (map mwMatchWire allMemWrites)
            dmemWrAddr = priorityMux [ (match, sigRetype adr :: s dom (Unsigned addrW))
                                     | MemWriteReq match adr _ <- allMemWrites ]
                                     (litS 0 addrBits) :: s dom (Unsigned addrW)
            dmemWrData = priorityMux [ (match, sigRetype dt :: s dom (Unsigned wordW))
                                     | MemWriteReq match _ dt <- allMemWrites ]
                                     (litS 0 wordBits) :: s dom (Unsigned wordW)

        -- PC → fetch address (trim/extend if PC register width ≠ addrW).
        let pcRegW    = widthOf pcName
            fetchAddr = (case someNatVal (fromIntegral (max 1 pcRegW)) of
                Just (SomeNat (_ :: Proxy pw)) ->
                    resizeS addrBits (readScalarOf coreState pcName :: s dom (Unsigned pw))
                Nothing -> litS 0 addrBits) :: s dom (Unsigned addrW)

        pure VnMemIface
            { vniFetchAddr  = fetchAddr
            , vniDataRdAddr = dmemRdAddr
            , vniDataWrEn   = dmemWrEn
            , vniDataWrAddr = dmemWrAddr
            , vniDataWrData = dmemWrData
            , vniAddrW      = addrBits
            , vniWordW      = wordBits
            }
