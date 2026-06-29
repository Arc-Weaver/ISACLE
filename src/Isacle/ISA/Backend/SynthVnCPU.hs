{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- | CPU-level synthesis for von Neumann architectures.
--
-- Mirrors 'Isacle.ISA.Backend.SynthCPU' for Harvard cores.  The key
-- differences from the Harvard path:
--
-- * No separate code bus — instruction words arrive from the same unified bus
--   as data loads.  @instrWord@ and @dataRdData@ are both sourced from the L1
--   cache (or directly from the system bus in a non-cached design).
--
-- * A @stall@ input wire freezes the entire pipeline.  The cache drives this
--   Hi on a miss; the CPU must not advance any architectural state while it is
--   asserted.  All register enables (including the PC) are gated with @~stall@.
--
-- * The PC register drives @vniFetchAddr@ on the unified address bus (same
--   width as @addrW@), not a separate code-address port.
module Isacle.ISA.Backend.SynthVnCPU
    ( VnMemIface(..)
    , synthVonNeumannCPU
    , synthVonNeumannCPU'
    ) where

import Prelude hiding (Word)
import Data.List (foldl', nub)
import Data.Proxy (Proxy(..))
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Control.Monad (forM, forM_)
import GHC.TypeLits (natVal)

import Hdl.Bits
import Hdl.Net
import qualified Hdl.Net as N
import Hdl.Types (KnownDom(..))
import Isacle.ISA.Types
import Isacle.ISA.CPUDef
import Isacle.ISA.Def
import Isacle.ISA.Build (ISABuild, runISABuild, evalISABuild)
import Isacle.ISA.Backend.Synth
import Isacle.ISA.Backend.SynthCPU
    ( extractPcName, litWire, driveWire, buildOrTree, buildMuxTree, addrBitsFor )

-- ---------------------------------------------------------------------------
-- VN memory interface
-- ---------------------------------------------------------------------------

-- | CPU ↔ cache/bus interface wires for a von Neumann core.
--
-- The CPU and the L1 cache (or system bus for a non-cached design) share this
-- interface.  The CPU drives fetch and data addresses; the cache drives the
-- instruction and data words back, plus a stall signal on a miss.
data VnMemIface = VnMemIface
    -- CPU → cache
    { vniFetchAddr   :: WireId  -- ^ instruction fetch address (PC value)
    , vniDataRdAddr  :: WireId  -- ^ data load address
    , vniDataWrEn    :: WireId  -- ^ data store enable (1-bit)
    , vniDataWrAddr  :: WireId  -- ^ data store address
    , vniDataWrData  :: WireId  -- ^ data store word
    -- cache → CPU (pre-allocated; must be driven externally)
    , vniInstrWord   :: WireId  -- ^ fetched instruction word
    , vniDataRdData  :: WireId  -- ^ loaded data word
    , vniStall       :: WireId  -- ^ 1 = cache miss, freeze pipeline
    -- widths
    , vniAddrW       :: Int
    , vniWordW       :: Int
    }

-- ---------------------------------------------------------------------------
-- Entry point
-- ---------------------------------------------------------------------------

-- | Synthesise a complete single-cycle von Neumann CPU.
--
-- Three input wires must be pre-allocated and driven externally after calling:
--
--   * @instrWireId@  — instruction word from the cache / system bus
--   * @dmemRdDataW@ — read data from the cache / system bus
--   * @stallWireId@  — Hi while the cache has a miss pending
--
-- Returns 'VnMemIface' with all interface wire IDs and widths.
synthVonNeumannCPU'
    :: forall dom wordW addrW alu.
       ( KnownDom dom
       , KnownNat wordW, KnownNat addrW )
    => CPUDef alu
    -> ISADef (ISABuild alu wordW addrW wordW addrW)
    -> WireId   -- ^ pre-allocated instr_word wire
    -> WireId   -- ^ pre-allocated data_rd_data wire
    -> WireId   -- ^ stall wire (from cache)
    -> NetM VnMemIface
synthVonNeumannCPU' cpuDef isaDef instrWireId dmemRdDataW stallWireId = do
    let (aluRec, schema) = runCPUDef cpuDef
    let domInfo  = domId (Proxy @dom)
    let wordBits = fromIntegral (natVal (Proxy @wordW)) :: Int
    let addrBits = fromIntegral (natVal (Proxy @addrW)) :: Int

    -- Build reset value maps from isaReset.
    let resetEntries = runResetDef (isaReset isaDef) aluRec
    let resetRegMap  = Map.fromList
            [ (n, v) | ResetRegEntry n (Unsigned v) <- resetEntries ]
    let resetFlagContribs = Map.fromListWith (.|.)
            [ (rn, if b == Hi then 1 `shiftL` bp else 0)
            | ResetFlagEntry rn bp b <- resetEntries ]

    -- ~stall: gates all register enables so no state advances during a miss.
    notStallW <- freshWire
    emit $ NComb notStallW N.PNot [stallWireId]

    -- -----------------------------------------------------------------------
    -- Scalar registers
    -- -----------------------------------------------------------------------

    scalarRegs <- forM (schRegisters schema) $ \(name, w) -> do
        outW <- freshWire
        nxtW <- freshWire
        enW  <- freshWire
        let initVal = Map.findWithDefault 0 name resetRegMap
                  .|. Map.findWithDefault 0 name resetFlagContribs
        defer $ emit $ NReg outW nxtW (Just enW) (SomeBits initVal w) domInfo
        hintWire outW name
        hintWire nxtW (name ++ "_nxt")
        return (name, w, outW, nxtW, enW)

    let scalarRegMap :: Map String (Int, WireId, WireId, WireId)
        scalarRegMap = Map.fromList
            [ (n, (w, outW, nxtW, enW)) | (n, w, outW, nxtW, enW) <- scalarRegs ]

    let scReadRegFn :: String -> WireId -> NetM WireId
        scReadRegFn name _ = case Map.lookup name scalarRegMap of
            Just (_, outW, _, _) -> return outW
            Nothing              -> freshWire

    -- -----------------------------------------------------------------------
    -- Register-file info table
    -- -----------------------------------------------------------------------

    let rfInfoMap :: Map String (Int, Int)
        rfInfoMap = Map.fromList
            [ (n, (c, w)) | (n, c, w) <- schRegFiles schema ]

    -- -----------------------------------------------------------------------
    -- Status register and flag reader
    -- -----------------------------------------------------------------------

    let statusRegMap = Map.fromList
            [ (n, (w, fs)) | (n, w, fs) <- schStatusRegs schema ]

    let getFlagFn :: String -> Int -> NetM WireId
        getFlagFn regName bitPos = case Map.lookup regName scalarRegMap of
            Nothing -> freshWire
            Just (_, regOutW, _, _) -> do
                out <- freshWire
                emit $ NComb out (N.PSlice bitPos bitPos) [regOutW]
                return out

    -- -----------------------------------------------------------------------
    -- Alias-aware data memory reader
    -- -----------------------------------------------------------------------

    let scReadMemFnAlias :: WireId -> NetM WireId
        scReadMemFnAlias addrW =
            foldl' (\accM (regName, aliasAddr) -> do
                acc <- accM
                let mSrc = fmap (\(rw, rout, _, _) -> (rout, rw))
                                (Map.lookup regName scalarRegMap)
                case mSrc of
                    Nothing -> return acc
                    Just (srcW, srcBits) -> do
                        addrLitW <- litWire aliasAddr addrBits
                        cmpW <- freshWire
                        emit $ NComb cmpW N.PEq [addrW, addrLitW]
                        dataW <- case compare srcBits wordBits of
                            EQ -> return srcW
                            LT -> do { w <- freshWire
                                     ; emit $ NComb w (N.PResize wordBits) [srcW]
                                     ; return w }
                            GT -> do { w <- freshWire
                                     ; emit $ NComb w (N.PSlice (wordBits - 1) 0) [srcW]
                                     ; return w }
                        muxW <- freshWire
                        emit $ NComb muxW N.PMux [cmpW, dataW, acc]
                        return muxW)
                (return dmemRdDataW)
                (schAliasRegs schema)

    -- VN ISAs have no separate code bus; reads come from the same data bus.
    let renderCtx = RenderCtx
            { rcInstrWire  = instrWireId
            , rcReadScalar = \n -> scReadRegFn n 0
            , rcDataBus    = dmemRdDataW
            , rcCodeBus    = dmemRdDataW
            , rcGetFlag    = getFlagFn
            , rcIrqVector  = Nothing
            , rcWordW      = wordBits
            }

    results <- forM (isaInstrs isaDef) $ \instr ->
        renderSynth renderCtx Nothing (runISABuild aluRec instr)

    irqData <- case isaInterruptBody isaDef of
        Nothing   -> return Nothing
        Just body -> do
            irqPendW <- freshWire
            emit $ NInput irqPendW "irq_pending" 1 domInfo
            irqVecW  <- freshWire
            emit $ NInput irqVecW "irq_vector" addrBits domInfo
            r <- renderSynth renderCtx { rcIrqVector = Just irqVecW }
                             (Just irqPendW) (runISABuild aluRec body)
            return (Just (irqPendW, irqVecW, r))

    let allResults = results ++ maybe [] (\(_, _, r) -> [r]) irqData

    -- Drive each read's result wire from its bus value.  The VN core is
    -- single-cycle per access (the cache 'stall' freezes the whole pipeline for
    -- bus latency); multi-cycle port-contention sequencing is a follow-up that
    -- mirrors 'Isacle.ISA.Backend.SynthCPU.buildExecSequencer'.
    forM_ allResults $ \r ->
        forM_ (srMemReads r) $ \rr ->
            driveWire (mrResultWire rr) (mrBusWire rr)

    -- -----------------------------------------------------------------------
    -- Write arbiters — register files
    -- -----------------------------------------------------------------------

    let allRegWrites = concatMap srRegWrites allResults

    let regWritesByRf :: Map String [RegWriteReq]
        regWritesByRf = foldl' (\m r -> Map.insertWith (++) (rwRfName r) [r] m)
                                Map.empty allRegWrites

    let readRfNames :: [String]
        readRfNames = nub [ rrRfName rr | r <- allResults, rr <- srRegReads r ]

    let involvedRfs = Map.keys regWritesByRf
                   ++ filter (`Map.notMember` regWritesByRf) readRfNames

    -- A register file is just a block of registers (an array field of cpu_state):
    -- every write is an independent enable-gated indexed assignment GPR(idx)<=data
    -- (gated with ~stall), no bank/port arbiter; VHDL applies distinct indices
    -- independently and matches are exclusive.
    forM_ involvedRfs $ \rfname -> do
        let (rfCount, rfWidth) = case Map.lookup rfname rfInfoMap of
                Just p  -> p
                Nothing -> (1, wordBits)
            aBits = addrBitsFor rfCount

        writes <- forM [ w | w <- allRegWrites, rwRfName w == rfname ] $ \w -> do
            enW <- freshWire; emit $ NComb enW N.PAnd [rwMatchWire w, notStallW]
            return (rwIdxWire w, rwDataWire w, enW)

        defer $ emit $ N.NRegFile "cpu_state" rfname rfCount rfWidth writes domInfo

        let instrSlots :: [(WireId, [RegReadReq])]
            instrSlots =
                [ (matchW, filter ((== rfname) . rrRfName) (srRegReads r))
                | r <- allResults
                , any ((== rfname) . rrRfName) (srRegReads r)
                , Just matchW <- [srMatchWire r]
                ]
            maxSlots = maximum (0 : map (length . snd) instrSlots)

        forM_ [0 .. maxSlots - 1] $ \slot -> do
            let slotEntries = [ (matchW, rr)
                              | (matchW, rrs) <- instrSlots
                              , (k, rr) <- zip [0 ..] rrs
                              , k == slot ]
            rdAddrW <- buildMuxTree
                           [(matchW, rrIdxWire rr) | (matchW, rr) <- slotEntries]
                           =<< litWire 0 aBits
            rdOutW <- freshWire
            emit $ N.NRegFileRead rdOutW "cpu_state" rfname rdAddrW rfCount
            forM_ slotEntries $ \(_, rr) ->
                emit $ NComb (rrOutWire rr) N.POr [rdOutW, rdOutW]

    -- -----------------------------------------------------------------------
    -- Alias register write decode
    -- -----------------------------------------------------------------------

    let allMemWrites = concatMap srMemWrites allResults

    aliasScalarWriteReqs <- fmap concat $ forM (schAliasRegs schema) $
        \(regName, aliasAddr) ->
            case Map.lookup regName scalarRegMap of
                Nothing -> return []
                Just (regW, regOutW, _, _) -> forM allMemWrites $ \mw -> do
                    addrLitW <- litWire aliasAddr addrBits
                    cmpW  <- freshWire
                    emit  $ NComb cmpW N.PEq [mwAddrWire mw, addrLitW]
                    gated <- freshWire
                    emit  $ NComb gated N.PAnd [mwMatchWire mw, cmpW]
                    if regW == wordBits
                        then return (ScalarWriteReq gated regName (mwDataWire mw))
                        else do
                            hiW   <- freshWire
                            emit  $ NComb hiW (N.PSlice (regW - 1) wordBits) [regOutW]
                            fullW <- freshWire
                            emit  $ NComb fullW N.PConcat [hiW, mwDataWire mw]
                            return (ScalarWriteReq gated regName fullW)

    -- -----------------------------------------------------------------------
    -- Write arbiters — scalar registers and status registers
    -- All enables are gated with ~stall.
    -- -----------------------------------------------------------------------

    let allScalarWrites = concatMap srScalarWrites allResults ++ aliasScalarWriteReqs
    let allFlagWrites   = concatMap srFlagWrites allResults
    let scalarWritesByReg :: Map String [ScalarWriteReq]
        scalarWritesByReg = foldl' (\m r -> Map.insertWith (++) (swRegName r) [r] m)
                                    Map.empty allScalarWrites

    pcName <- extractPcName aluRec isaDef

    forM_ scalarRegs $ \(name, w, regOutW, nxtW, enW) -> do
        let scWrites = Map.findWithDefault [] name scalarWritesByReg

        if name == pcName
            then do
                -- PC always advances (or jumps) unless stalled.
                litOneW  <- litWire 1 w
                pcIncW   <- do { o <- freshWire
                               ; emit $ NComb o N.PAdd [regOutW, litOneW]
                               ; return o }
                pcNxtW   <- buildMuxTree
                                [(swMatchWire r, swDataWire r) | r <- scWrites]
                                pcIncW
                -- Enable = ~stall (PC freezes on miss).
                driveWire nxtW pcNxtW
                driveWire enW  notStallW

        else case Map.lookup name statusRegMap of
            Just (_, flagNames) -> do
                let bitAssigns = zip (reverse [0 .. w - 1]) flagNames
                bitNextWires <- forM bitAssigns $ \(bitPos, _) -> do
                    curBitW <- freshWire
                    emit $ NComb curBitW (N.PSlice bitPos bitPos) [regOutW]
                    let fwPairs = [ (fwMatchWire fw, fwValueWire fw)
                                  | fw <- allFlagWrites
                                  , fwRegName fw == name
                                  , fwBitPos  fw == bitPos ]
                    scPairs <- forM scWrites $ \sw -> do
                        bitExtW <- freshWire
                        emit $ NComb bitExtW (N.PSlice bitPos bitPos) [swDataWire sw]
                        return (swMatchWire sw, bitExtW)
                    buildMuxTree (fwPairs ++ scPairs) curBitW
                sregNextW <- case bitNextWires of
                    [] -> litWire 0 w
                    (msbW : restBits) -> foldl' (\accM bw -> do
                        acc <- accM
                        out <- freshWire
                        emit $ NComb out N.PConcat [acc, bw]
                        return out) (return msbW) restBits
                let allMatchWires = map fwMatchWire (filter ((== name) . fwRegName) allFlagWrites)
                                 ++ map swMatchWire scWrites
                enOrW    <- buildOrTree allMatchWires
                -- Gate with ~stall before driving the register enable.
                enGatedW <- freshWire
                emit $ NComb enGatedW N.PAnd [enOrW, notStallW]
                driveWire enW  enGatedW
                driveWire nxtW sregNextW

            Nothing -> do
                case scWrites of
                    [] -> do
                        litZeroEn <- litWire 0 1
                        litZeroD  <- litWire 0 w
                        driveWire enW  litZeroEn
                        driveWire nxtW litZeroD
                    ws -> do
                        enOrW  <- buildOrTree (map swMatchWire ws)
                        defW   <- litWire 0 w
                        nxtMux <- buildMuxTree
                                      [(swMatchWire r, swDataWire r) | r <- ws]
                                      defW
                        -- Gate with ~stall.
                        enGatedW <- freshWire
                        emit $ NComb enGatedW N.PAnd [enOrW, notStallW]
                        driveWire enW  enGatedW
                        driveWire nxtW nxtMux

    -- -----------------------------------------------------------------------
    -- Data memory read address mux
    -- -----------------------------------------------------------------------

    let allMemReads = concatMap srMemReads allResults
    dmemRdAddrW <- case allMemReads of
        [] -> litWire 0 addrBits
        rs -> buildMuxTree [ (mrMatchWire r, mrAddrWire r) | r <- rs ]
                           =<< litWire 0 addrBits

    -- -----------------------------------------------------------------------
    -- Data memory write arbiter
    -- -----------------------------------------------------------------------

    (dmemWrEnW, dmemWrAddrW, dmemWrDatW) <- case allMemWrites of
        [] -> (,,) <$> litWire 0 1
                   <*> litWire 0 addrBits
                   <*> litWire 0 wordBits
        ws -> do
            enW  <- buildOrTree (map mwMatchWire ws)
            defA <- litWire 0 addrBits
            defD <- litWire 0 wordBits
            adW  <- buildMuxTree [(mwMatchWire r, mwAddrWire r) | r <- ws] defA
            daW  <- buildMuxTree [(mwMatchWire r, mwDataWire r) | r <- ws] defD
            return (enW, adW, daW)

    -- -----------------------------------------------------------------------
    -- PC → fetch address (trim to addrW if PC register is wider)
    -- -----------------------------------------------------------------------

    let pcOutWire = case Map.lookup pcName scalarRegMap of
            Just (_, outW, _, _) -> outW
            Nothing              -> instrWireId

    let pcRegWidth = case Map.lookup pcName scalarRegMap of
            Just (w, _, _, _) -> w
            Nothing            -> 0
    fetchAddrW <- if addrBits == pcRegWidth
        then return pcOutWire
        else do
            w <- freshWire
            emit $ NComb w (N.PSlice 0 (addrBits - 1)) [pcOutWire]
            return w

    return VnMemIface
        { vniFetchAddr  = fetchAddrW
        , vniDataRdAddr = dmemRdAddrW
        , vniDataWrEn   = dmemWrEnW
        , vniDataWrAddr = dmemWrAddrW
        , vniDataWrData = dmemWrDatW
        , vniInstrWord  = instrWireId
        , vniDataRdData = dmemRdDataW
        , vniStall      = stallWireId
        , vniAddrW      = addrBits
        , vniWordW      = wordBits
        }

-- | Standalone wrapper: synthesises the CPU with all memory interface signals
-- exposed as top-level input/output ports.  Suitable for unit testing the CPU
-- in isolation without a cache.
synthVonNeumannCPU
    :: forall dom wordW addrW alu.
       ( KnownDom dom
       , KnownNat wordW, KnownNat addrW )
    => CPUDef alu
    -> ISADef (ISABuild alu wordW addrW wordW addrW)
    -> NetM ()
synthVonNeumannCPU cpuDef isaDef = do
    let domInfo  = domId (Proxy @dom)
    let wordBits = fromIntegral (natVal (Proxy @wordW)) :: Int
    let addrBits = fromIntegral (natVal (Proxy @addrW)) :: Int

    instrWireId <- freshWire
    emit $ NInput instrWireId "instr_word"   wordBits domInfo
    dmemRdDataW <- freshWire
    emit $ NInput dmemRdDataW "data_rd_data" wordBits domInfo
    stallWireId <- freshWire
    emit $ NInput stallWireId "stall"        1        domInfo

    vmi <- synthVonNeumannCPU' @dom @wordW @addrW
               cpuDef isaDef instrWireId dmemRdDataW stallWireId

    emit $ NOutput (vniFetchAddr   vmi) "fetch_addr"    addrBits domInfo
    emit $ NOutput (vniDataRdAddr  vmi) "data_rd_addr"  addrBits domInfo
    emit $ NOutput (vniDataWrEn    vmi) "data_wr_en"    1        domInfo
    emit $ NOutput (vniDataWrAddr  vmi) "data_wr_addr"  addrBits domInfo
    emit $ NOutput (vniDataWrData  vmi) "data_wr_data"  wordBits domInfo

