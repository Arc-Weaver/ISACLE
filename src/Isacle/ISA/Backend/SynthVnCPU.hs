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
    , synthVonNeumannCPU
    , synthVonNeumannCPU'
    ) where

import Prelude hiding (Word)
import Data.Kind (Type)
import Data.List (foldl', nub)
import Data.Proxy (Proxy(..))
import qualified Data.Map.Strict as Map
import Control.Monad (forM)
import Data.Bits ((.|.), shiftL)
import GHC.TypeLits (natVal, KnownNat)

import Hdl.Bits (Unsigned(..), Bit(..))
import Hdl.Net (NetM, freshWire, emit, NetNode(..))
import qualified Hdl.Net as N
import Hdl.Types (KnownDom(..), Signal(..), Sig(..), materialize)
import Hdl.Monad (Hdl(..))
import Isacle.ISA.Types
import Isacle.ISA.CPUDef
import Isacle.ISA.Def
import Isacle.ISA.Build (ISABuild, runISABuild)
import Isacle.ISA.Backend.Synth
import Isacle.ISA.Backend.SynthCPU
    ( extractPcName, addrBitsFor
    , litS, andS, notS, toBool, eqS, muxS, sliceS, resizeS, addS, orReduce, priorityMux )

-- ---------------------------------------------------------------------------
-- VN memory interface (CPU outputs; inputs are signal arguments)
-- ---------------------------------------------------------------------------

data VnMemIface s dom = VnMemIface
    { vniFetchAddr  :: s dom ()    -- ^ instruction fetch address (PC value)
    , vniDataRdAddr :: s dom ()    -- ^ data load address
    , vniDataWrEn   :: s dom Bool  -- ^ data store enable (1-bit)
    , vniDataWrAddr :: s dom ()    -- ^ data store address
    , vniDataWrData :: s dom ()    -- ^ data store word
    , vniAddrW      :: Int
    , vniWordW      :: Int
    }

-- ---------------------------------------------------------------------------
-- Entry point
-- ---------------------------------------------------------------------------

-- | Synthesise a von Neumann CPU as 'Hdl'.  Inputs (signals): instruction word,
-- data read data, stall (Hi on a cache miss), irq pending / vector.
synthVonNeumannCPU'
    :: forall s m dom wordW addrW alu.
       ( Hdl s m, Signal s, KnownDom dom, KnownNat wordW, KnownNat addrW )
    => CPUDef alu
    -> ISADef (ISABuild alu wordW addrW wordW addrW)
    -> s dom ()   -- ^ instr_word
    -> s dom ()   -- ^ data_rd_data
    -> s dom ()   -- ^ stall
    -> s dom ()   -- ^ irq_pending
    -> s dom ()   -- ^ irq_vector
    -> m (VnMemIface s dom)
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

        regList      = schRegisters schema
        widthOf name = maybe wordBits id (lookup name regList)
        rfInfoMap    = Map.fromList [ (n, (c, w)) | (n, c, w) <- schRegFiles schema ]
        regCount rf  = maybe 1 fst (Map.lookup rf rfInfoMap)
        statusRegMap = Map.fromList [ (n, (w, fs)) | (n, w, fs) <- schStatusRegs schema ]
        pcName       = extractPcName aluRec isaDef
        notStall     = notS stallSig

    mdo
        -- Scalar registers (enable from the arbiters, gated with ~stall).
        scalarOuts <- forM regList $ \(name, w) ->
            named name =<< registerW w (initOf name) (enOf name) (nxtOf name)
        let scalarMap    = Map.fromList (zip (map fst regList) scalarOuts)
            readScalar n = Map.findWithDefault (litS 0 wordBits) n scalarMap
            getFlag rn bp = maybe (litS 0 1) (sliceS bp bp) (Map.lookup rn scalarMap)
            mkCtx rcIrq = RenderCtx
                { rcInstrWire  = instrSig
                , rcReadScalar = readScalar
                , rcDataBus    = dmemRdData
                , rcCodeBus    = dmemRdData   -- VN: unified bus, no separate code port
                , rcReadRes    = const dmemRdData  -- single-cycle: read result is the bus
                , rcGetFlag    = getFlag
                , rcRegCount   = regCount
                , rcIrqVector  = rcIrq
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

        -- Register files: indexed writes gated by match AND ~stall.
        let regWritesByRf = foldl' (\mp r -> Map.insertWith (++) (rwRfName r) [r] mp)
                                   Map.empty allRegWrites
            readRfNames   = nub [ rrRfName rr | r <- allResults, rr <- srRegReads r ]
            involvedRfs   = Map.keys regWritesByRf
                         ++ filter (`Map.notMember` regWritesByRf) readRfNames
        mapM_ (\rfname -> do
                  let (rfCount, rfWidth) = maybe (1, wordBits) id (Map.lookup rfname rfInfoMap)
                      writes = [ (rwIdxWire w, rwDataWire w, andS (rwMatchWire w) notStall)
                               | w <- allRegWrites, rwRfName w == rfname ]
                  regBank "cpu_state" rfname rfCount rfWidth writes)
              involvedRfs

        -- Data writes to an aliased register route to a ScalarWriteReq (a store
        -- to the low word of a 2-word register keeps its high word).
        let aliasScalarWrites =
                [ ScalarWriteReq gated regName full
                | (regName, aliasAddr) <- schAliasRegs schema
                , Map.member regName scalarMap
                , let regW   = widthOf regName
                      regSig = scalarMap Map.! regName
                , mw <- allMemWrites
                , let gated = andS (mwMatchWire mw)
                                   (eqS (mwAddrWire mw) (litS aliasAddr addrBits))
                      full  = if regW == wordBits then mwDataWire mw
                              else sigPrim2 N.PConcat (sliceS (regW - 1) wordBits regSig)
                                                      (mwDataWire mw)
                ]

        -- Scalar / status write arbiters — every enable gated with ~stall.
        let allScalarWrites = concatMap srScalarWrites allResults ++ aliasScalarWrites
            allFlagWrites   = concatMap srFlagWrites allResults
            scalarWritesByReg = foldl' (\mp r -> Map.insertWith (++) (swRegName r) [r] mp)
                                       Map.empty allScalarWrites
            arbiterOf (name, w) =
                let scWrites   = Map.findWithDefault [] name scalarWritesByReg
                    writePairs = [ (swMatchWire r, swDataWire r) | r <- scWrites ]
                    regSig     = scalarMap Map.! name
                in if name == pcName
                   then (name, notStall, priorityMux writePairs (addS regSig (litS 1 w)))
                   else case Map.lookup name statusRegMap of
                     Just (_, flagNames) ->
                       let bitNext (bitPos, _) =
                             let cur     = sliceS bitPos bitPos regSig
                                 fwPairs = [ (fwMatchWire fw, sliceS 0 0 (fwValueWire fw))
                                           | fw <- allFlagWrites
                                           , fwRegName fw == name, fwBitPos fw == bitPos ]
                                 scPairs = [ (swMatchWire sw, sliceS bitPos bitPos (swDataWire sw))
                                           | sw <- scWrites ]
                             in priorityMux (fwPairs ++ scPairs) cur
                           bits = map bitNext (zip (reverse [0 .. w - 1]) flagNames)
                           nxt  = case bits of
                                    []     -> litS 0 w
                                    (b:bs) -> foldl' (sigPrim2 N.PConcat) b bs
                           matches = [ fwMatchWire fw | fw <- allFlagWrites, fwRegName fw == name ]
                                  ++ map swMatchWire scWrites
                       in (name, andS notStall (orReduce matches), nxt)
                     Nothing -> case scWrites of
                       [] -> (name, litS 0 1, litS 0 w)
                       ws -> ( name
                             , andS notStall (orReduce (map swMatchWire ws))
                             , priorityMux writePairs (litS 0 w) )
            arbiters = map arbiterOf regList
            enMap  = Map.fromList [ (n, e) | (n, e, _) <- arbiters ]
            nxtMap = Map.fromList [ (n, x) | (n, _, x) <- arbiters ]
            enOf  n = Map.findWithDefault (litS 0 1)           n enMap
            nxtOf n = Map.findWithDefault (litS 0 (widthOf n)) n nxtMap

        -- Data-memory address muxes (match directly — single-cycle, no gating).
        let dmemRdAddr = priorityMux [ (mrMatchWire r, mrAddrWire r) | r <- allMemReads ]
                                     (litS 0 addrBits)
            dmemWrEn   = orReduce (map mwMatchWire allMemWrites)
            dmemWrAddr = priorityMux [ (mwMatchWire mw, mwAddrWire mw) | mw <- allMemWrites ]
                                     (litS 0 addrBits)
            dmemWrData = priorityMux [ (mwMatchWire mw, mwDataWire mw) | mw <- allMemWrites ]
                                     (litS 0 wordBits)

        -- PC → fetch address (trim if PC register is wider than addrW).
        let pcSig     = readScalar pcName
            fetchAddr = if addrBits == widthOf pcName then pcSig
                        else sliceS (addrBits - 1) 0 pcSig

        pure VnMemIface
            { vniFetchAddr  = fetchAddr
            , vniDataRdAddr = dmemRdAddr
            , vniDataWrEn   = dmemWrEn
            , vniDataWrAddr = dmemWrAddr
            , vniDataWrData = dmemWrData
            , vniAddrW      = addrBits
            , vniWordW      = wordBits
            }

-- ---------------------------------------------------------------------------
-- Concrete instantiation boundary (NetM): top-level ports for isolated testing.
-- ---------------------------------------------------------------------------

synthVonNeumannCPU :: forall (dom :: Type) wordW addrW alu.
                      ( KnownDom dom, KnownNat wordW, KnownNat addrW )
                   => CPUDef alu
                   -> ISADef (ISABuild alu wordW addrW wordW addrW)
                   -> NetM ()
synthVonNeumannCPU cpuDef isaDef = do
    let domInfo  = domId (Proxy @dom)
        wordBits = fromIntegral (natVal (Proxy @wordW)) :: Int
        addrBits = fromIntegral (natVal (Proxy @addrW)) :: Int
        inPort name w = do { wid <- freshWire; emit (NInput wid name w domInfo)
                           ; pure (SWire wid :: Sig dom ()) }
        outPort :: forall a. String -> Int -> Sig dom a -> NetM ()
        outPort name w sig = do { wid <- materialize sig; emit (NOutput wid name w domInfo) }
    instrW <- inPort "instr_word"   wordBits
    dmemRd <- inPort "data_rd_data" wordBits
    stall  <- inPort "stall"        1
    irqP   <- inPort "irq_pending"  1
    irqV   <- inPort "irq_vector"   addrBits
    vmi <- synthVonNeumannCPU' @Sig @NetM @dom @wordW @addrW
               cpuDef isaDef instrW dmemRd stall irqP irqV
    outPort "fetch_addr"   (vniAddrW vmi) (vniFetchAddr  vmi)
    outPort "data_rd_addr" (vniAddrW vmi) (vniDataRdAddr vmi)
    outPort "data_wr_en"   1              (vniDataWrEn   vmi)
    outPort "data_wr_addr" (vniAddrW vmi) (vniDataWrAddr vmi)
    outPort "data_wr_data" (vniWordW vmi) (vniDataWrData vmi)
