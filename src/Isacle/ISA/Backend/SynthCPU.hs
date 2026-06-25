{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- | CPU-level synthesis: adjoins a 'CPUDef' with a 'ISADef' to produce a
-- complete Harvard-architecture decode-execute circuit in the 'NetM' IR.
--
-- = Circuit structure
--
-- * Scalar registers (PC, SP, …) become 'NReg' nodes, initialised from
--   'isaReset'.
-- * Flags become 1-bit 'NReg' nodes.
-- * Register files become 'NMem' nodes (one node per read port, shared
--   write side — synthesis tools merge them into a multi-port BRAM/LUTRAM).
-- * Each instruction body in 'isaInstrs' is elaborated by 'runSynthM'; the
--   resulting write requests are combined into write arbiters.
-- * The PC is always updated: by a jump ('ScalarWriteReq' to PC) or by the
--   default single-word increment (PC + 1).
--
-- = Port names emitted
--
-- * @\"instr_word\"@   — input, @codeWordW@-bit instruction from code memory
-- * @\"data_rd_data\"@ — input, @wordW@-bit read data from data memory
-- * @\"code_rd_data\"@ — input, @codeWordW@-bit read data from code memory (LPM)
-- * @\"code_rd_addr\"@ — output, PC value driving code memory address
-- * @\"data_rd_addr\"@ — output, load address driving data memory read
-- * @\"data_wr_en\"@   — output, data memory write enable
-- * @\"data_wr_addr\"@ — output, data memory write address
-- * @\"data_wr_data\"@ — output, data memory write data
module Isacle.ISA.Backend.SynthCPU
    ( synthHarvardCPU
    , synthHarvardCPU'
    , CpuMemIface(..)
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
import Isacle.ISA.Backend.Synth

-- ---------------------------------------------------------------------------
-- Entry point
-- ---------------------------------------------------------------------------

-- | Memory interface wires returned by 'synthHarvardCPU''.
-- Input wires (@cmiInstrWord@, @cmiDataRdData@) must be driven externally
-- (by a code ROM and data bus respectively) after synthesis.
data CpuMemIface = CpuMemIface
    { cmiInstrWord  :: WireId  -- ^ pre-allocated; must be driven by code ROM
    , cmiDataRdData :: WireId  -- ^ pre-allocated; must be driven by data bus
    , cmiCodeRdAddr :: WireId  -- ^ PC output (width = cmiCodeAddrW)
    , cmiDataRdAddr :: WireId  -- ^ data read address (width = cmiDataAddrW)
    , cmiDataWrEn   :: WireId  -- ^ data write enable (1-bit)
    , cmiDataWrAddr :: WireId  -- ^ data write address (width = cmiDataAddrW)
    , cmiDataWrData :: WireId  -- ^ data write data (width = cmiWordW)
    , cmiCodeAddrW  :: Int
    , cmiDataAddrW  :: Int
    , cmiWordW      :: Int
    , cmiCodeWordW  :: Int
    , cmiIrqPending :: Maybe WireId  -- ^ Just w when ISA has an interrupt body
    , cmiIrqVector  :: Maybe WireId  -- ^ Just w when ISA has an interrupt body
    }

-- | Synthesise a complete single-cycle Harvard CPU from a 'CPUDef' and
-- 'ISADef'.  Emits all 'NetNode' IR nodes into the current 'NetM' context.
-- The caller must pre-allocate three input wires and drive them after calling:
--   * @instrWordW@  — instruction word from code ROM
--   * @dmemRdDataW@ — read data from data memory
--   * @cmemRdDataW@ — read data from code memory (LPM; stub OK)
-- Returns 'CpuMemIface' with all interface wire IDs and their widths.
synthHarvardCPU' :: forall dom wordW addrW codeWordW codeAddrW alu.
                    ( KnownDom dom
                    , KnownNat wordW, KnownNat addrW
                    , KnownNat codeWordW, KnownNat codeAddrW )
                 => CPUDef alu
                 -> ISADef (SynthM alu wordW addrW codeWordW codeAddrW)
                 -> WireId   -- ^ pre-allocated instr_word wire
                 -> WireId   -- ^ pre-allocated data_rd_data wire
                 -> WireId   -- ^ pre-allocated code_rd_data wire (LPM stub)
                 -> NetM CpuMemIface
synthHarvardCPU' cpuDef isaDef instrWireId dmemRdDataW cmemRdDataW = do
    let (aluRec, schema) = runCPUDef cpuDef
    let domInfo      = domId (Proxy @dom)
    let wordBits     = fromIntegral (natVal (Proxy @wordW))     :: Int
    let addrBits     = fromIntegral (natVal (Proxy @addrW))     :: Int
    let codeBits     = fromIntegral (natVal (Proxy @codeWordW)) :: Int
    let codeAddrBits = fromIntegral (natVal (Proxy @codeAddrW)) :: Int

    -- Build reset value maps from isaReset
    let resetEntries = runResetDef (isaReset isaDef) aluRec
    let resetRegMap  = Map.fromList
            [ (n, v) | ResetRegEntry n (Unsigned v) <- resetEntries ]
    -- Accumulate per-bit flag reset contributions into per-register initial values.
    let resetFlagContribs = Map.fromListWith (.|.)
            [ (rn, if b == Hi then 1 `shiftL` bp else 0)
            | ResetFlagEntry rn bp b <- resetEntries ]

    -- -----------------------------------------------------------------------
    -- Scalar registers (NReg, deferred)
    -- Status registers are included here — flagPack adds them to schRegisters.
    -- Their initial value merges resetReg and resetFlag contributions.
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
    -- Register-file info table (from CPUSchema)
    -- -----------------------------------------------------------------------

    let rfInfoMap :: Map String (Int, Int)
        rfInfoMap = Map.fromList
            [ (n, (c, w)) | (n, c, w) <- schRegFiles schema ]

    -- -----------------------------------------------------------------------
    -- Status register map: name → (width, [flag names MSB-first])
    -- Used for the combined flag+scalar write arbiter.
    -- -----------------------------------------------------------------------

    let statusRegMap = Map.fromList
            [ (n, (w, fs)) | (n, w, fs) <- schStatusRegs schema ]

    -- -----------------------------------------------------------------------
    -- Flag reader: extract a single bit from the status register output wire.
    -- -----------------------------------------------------------------------

    let getFlagFn :: String -> Int -> NetM WireId
        getFlagFn regName bitPos = case Map.lookup regName scalarRegMap of
            Nothing -> freshWire
            Just (_, regOutW, _, _) -> do
                out <- freshWire
                emit $ NComb out (N.PSlice bitPos bitPos) [regOutW]
                return out

    -- -----------------------------------------------------------------------
    -- Alias-aware data memory reader
    -- For each aliasReg entry, builds a comparator and mux so that reads from
    -- the alias address return the register value instead of SRAM data.
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

    results <- forM (isaInstrs isaDef) $ \instr ->
        runSynthM aluRec instrWireId scReadRegFn
                  scReadMemFnAlias
                  (const (return cmemRdDataW))
                  getFlagFn
                  instr

    -- Interrupt body synthesis (if ISA declares one)
    irqData <- case isaInterruptBody isaDef of
        Nothing   -> return Nothing
        Just body -> do
            irqPendW <- freshWire
            emit $ NInput irqPendW "irq_pending" 1 domInfo
            irqVecW  <- freshWire
            emit $ NInput irqVecW "irq_vector" codeAddrBits domInfo
            r <- runSynthMIrq aluRec irqPendW irqVecW
                     scReadRegFn scReadMemFnAlias
                     (const (return cmemRdDataW)) getFlagFn body
            return (Just (irqPendW, irqVecW, r))

    let allResults = results ++ maybe [] (\(_, _, r) -> [r]) irqData

    -- -----------------------------------------------------------------------
    -- Write arbiters — register files
    -- -----------------------------------------------------------------------

    let allRegWrites = concatMap srRegWrites allResults

    let regWritesByRf :: Map String [RegWriteReq]
        regWritesByRf = foldl' (\m r -> Map.insertWith (++) (rwRfName r) [r] m)
                                Map.empty allRegWrites

    -- Collect the set of rf names that appear in reads (without flattening order).
    let readRfNames :: [String]
        readRfNames = nub [ rrRfName rr | r <- allResults, rr <- srRegReads r ]

    let involvedRfs = Map.keys regWritesByRf
                   ++ filter (`Map.notMember` regWritesByRf) readRfNames

    forM_ involvedRfs $ \rfname -> do
        let writes  = Map.findWithDefault [] rfname regWritesByRf
        let (rfCount, rfWidth) = case Map.lookup rfname rfInfoMap of
                Just p  -> p
                Nothing -> (1, wordBits)

        wrEnW   <- buildOrTree (map rwMatchWire writes)
        wrAddrW <- buildMuxTree
                      [(rwMatchWire r, rwIdxWire r) | r <- writes]
                      =<< litWire 0 (addrBitsFor rfCount)
        wrDatW  <- buildMuxTree
                      [(rwMatchWire r, rwDataWire r) | r <- writes]
                      =<< litWire 0 rfWidth

        -- Per-instruction reads for this rf, preserving slot order within
        -- each instruction.  Slot k = the k-th readReg call on rfname.
        let instrSlots :: [(WireId, [RegReadReq])]
            instrSlots =
                [ (matchW, filter ((== rfname) . rrRfName) (srRegReads r))
                | r <- allResults
                , any ((== rfname) . rrRfName) (srRegReads r)
                , Just matchW <- [srMatchWire r]
                ]

        let maxSlots = maximum (0 : map (length . snd) instrSlots)

        -- One NMem per read slot — one physical read port per simultaneous
        -- register read.  For a 2-operand ISA this is exactly 2 instances.
        forM_ [0 .. maxSlots - 1] $ \slot -> do
            let slotEntries = [ (matchW, rr)
                              | (matchW, rrs) <- instrSlots
                              , (k, rr) <- zip [0 ..] rrs
                              , k == slot ]
            rdAddrW <- buildMuxTree
                           [(matchW, rrIdxWire rr) | (matchW, rr) <- slotEntries]
                           =<< litWire 0 (addrBitsFor rfCount)
            rdOutW  <- freshWire
            defer $ emit $ NMem rdOutW rdAddrW wrAddrW wrDatW wrEnW
                                rfCount rfWidth [] domInfo
            -- Forward shared output to each instruction's pre-allocated wire.
            forM_ slotEntries $ \(_, rr) ->
                emit $ NComb (rrOutWire rr) N.POr [rdOutW, rdOutW]

    -- -----------------------------------------------------------------------
    -- Alias register write decode
    -- When writeMem targets an aliased data-space address, route it to the
    -- aliased architectural register via a ScalarWriteReq.
    -- Status registers (e.g. SREG at 0x5F) are now plain scalar registers,
    -- so they are handled here too — the combined flag+scalar arbiter below
    -- merges whole-register writes with individual flag writes.
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
    --
    -- Status registers (in statusRegMap) use a combined bit-level arbiter:
    -- each bit is driven by either a flag write (setFlag) or a whole-register
    -- write (writeReg / alias write), whichever fires.  Regular scalar
    -- registers use the standard mux-tree arbiter.
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
                litOneW  <- litWire 1 w
                pcIncW   <- do { o <- freshWire
                               ; emit $ NComb o N.PAdd [regOutW, litOneW]
                               ; return o }
                pcNxtW   <- buildMuxTree
                                [(swMatchWire r, swDataWire r) | r <- scWrites]
                                pcIncW
                litOneEn <- litWire 1 1
                driveWire nxtW pcNxtW
                driveWire enW  litOneEn

        else case Map.lookup name statusRegMap of
            Just (_, flagNames) -> do
                -- Combined arbiter: per-bit mux merging flag writes and
                -- whole-register scalar writes.  Flag writes take priority
                -- (they appear first in buildMuxTree).
                let bitAssigns = zip (reverse [0 .. w - 1]) flagNames
                bitNextWires <- forM bitAssigns $ \(bitPos, _) -> do
                    curBitW <- freshWire
                    emit $ NComb curBitW (N.PSlice bitPos bitPos) [regOutW]
                    -- Flag writes at this bit position
                    let fwPairs = [ (fwMatchWire fw, fwValueWire fw)
                                  | fw <- allFlagWrites
                                  , fwRegName fw == name
                                  , fwBitPos  fw == bitPos ]
                    -- Scalar writes: extract this bit from the written word
                    scPairs <- forM scWrites $ \sw -> do
                        bitExtW <- freshWire
                        emit $ NComb bitExtW (N.PSlice bitPos bitPos) [swDataWire sw]
                        return (swMatchWire sw, bitExtW)
                    buildMuxTree (fwPairs ++ scPairs) curBitW
                -- Concatenate bits MSB-first into new register value
                sregNextW <- case bitNextWires of
                    [] -> litWire 0 w
                    (msbW : restBits) -> foldl' (\accM bw -> do
                        acc <- accM
                        out <- freshWire
                        emit $ NComb out N.PConcat [acc, bw]
                        return out) (return msbW) restBits
                -- Enable if any write (flag or scalar) fired
                let allMatchWires = map fwMatchWire (filter ((== name) . fwRegName) allFlagWrites)
                                 ++ map swMatchWire scWrites
                enOrW <- buildOrTree allMatchWires
                driveWire enW  enOrW
                driveWire nxtW sregNextW

            Nothing -> do
                -- Regular scalar register arbiter
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
                        driveWire enW  enOrW
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
    -- PC → code address (resize if PC register width ≠ codeAddrW)
    -- -----------------------------------------------------------------------

    let pcOutWire = case Map.lookup pcName scalarRegMap of
            Just (_, outW, _, _) -> outW
            Nothing              -> instrWireId

    let pcRegWidth = case Map.lookup pcName scalarRegMap of
            Just (w, _, _, _) -> w
            Nothing            -> 0
    pcAddrW <- if codeAddrBits == pcRegWidth
        then return pcOutWire
        else do
            w <- freshWire
            emit $ NComb w (N.PSlice 0 (codeAddrBits - 1)) [pcOutWire]
            return w

    return CpuMemIface
        { cmiInstrWord  = instrWireId
        , cmiDataRdData = dmemRdDataW
        , cmiCodeRdAddr = pcAddrW
        , cmiDataRdAddr = dmemRdAddrW
        , cmiDataWrEn   = dmemWrEnW
        , cmiDataWrAddr = dmemWrAddrW
        , cmiDataWrData = dmemWrDatW
        , cmiCodeAddrW  = codeAddrBits
        , cmiDataAddrW  = addrBits
        , cmiWordW      = wordBits
        , cmiCodeWordW  = codeBits
        , cmiIrqPending = fmap (\(p,_,_) -> p) irqData
        , cmiIrqVector  = fmap (\(_,v,_) -> v) irqData
        }

-- | Standalone wrapper: synthesises the CPU with all memory interface signals
-- exposed as top-level input/output ports.  Suitable for unit testing the CPU
-- in isolation.  For SoC integration use 'synthHarvardCPU'' instead.
synthHarvardCPU :: forall dom wordW addrW codeWordW codeAddrW alu.
                   ( KnownDom dom
                   , KnownNat wordW, KnownNat addrW
                   , KnownNat codeWordW, KnownNat codeAddrW )
                => CPUDef alu
                -> ISADef (SynthM alu wordW addrW codeWordW codeAddrW)
                -> NetM ()
synthHarvardCPU cpuDef isaDef = do
    let domInfo  = domId (Proxy @dom)
    let wordBits = fromIntegral (natVal (Proxy @wordW))     :: Int
    let codeBits = fromIntegral (natVal (Proxy @codeWordW)) :: Int

    instrWireId <- freshWire
    emit $ NInput instrWireId "instr_word"   codeBits domInfo
    dmemRdDataW <- freshWire
    emit $ NInput dmemRdDataW "data_rd_data" wordBits domInfo
    cmemRdDataW <- freshWire
    emit $ NInput cmemRdDataW "code_rd_data" codeBits domInfo

    cmi <- synthHarvardCPU' @dom @wordW @addrW @codeWordW @codeAddrW
               cpuDef isaDef instrWireId dmemRdDataW cmemRdDataW

    emit $ NOutput (cmiCodeRdAddr cmi) "code_rd_addr" (cmiCodeAddrW cmi) domInfo
    emit $ NOutput (cmiDataRdAddr cmi) "data_rd_addr" (cmiDataAddrW cmi) domInfo
    emit $ NOutput (cmiDataWrEn   cmi) "data_wr_en"  1                  domInfo
    emit $ NOutput (cmiDataWrAddr cmi) "data_wr_addr" (cmiDataAddrW cmi) domInfo
    emit $ NOutput (cmiDataWrData cmi) "data_wr_data" (cmiWordW     cmi) domInfo

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Extract the PC register name from 'isaPc' by running it in a dummy
-- 'SynthM' context.  'isaPc' always reduces to @cpu sel@, which only reads
-- the ALU record — no 'NetM' nodes are emitted.
extractPcName :: alu
              -> ISADef (SynthM alu wordW addrW codeWordW codeAddrW)
              -> NetM String
extractPcName aluRec isaDef = do
    SomeCPURegister (CPURegister n) <- evalSynthM aluRec (isaPc isaDef)
    return n

-- -----------------------------------------------------------------------
-- Netlist building utilities
-- -----------------------------------------------------------------------

-- | Emit a literal constant wire.
litWire :: Integer -> Int -> NetM WireId
litWire val w = do
    out <- freshWire
    emit $ NComb out (N.PLit val w) []
    return out

-- | Connect (reuse) a pre-allocated wire by emitting a combinational
-- identity buffer node.  Used to drive the pre-allocated NReg input wires
-- ('nxtWire', 'enWire') from the arbiter result.
driveWire :: WireId -> WireId -> NetM ()
driveWire dst src
    | dst == src = return ()    -- already the same wire, no node needed
    | otherwise  = emit $ NComb dst N.POr [src, src]
    -- POr(x, x) = x: a no-cost identity in synthesis.
    -- The VHDL emitter can optimise this to a direct assignment.

-- | Build a left-folded OR tree over a list of wire IDs.
-- Returns a constant-0 wire when the list is empty.
buildOrTree :: [WireId] -> NetM WireId
buildOrTree []     = litWire 0 1
buildOrTree (w:ws) = foldl' step (return w) ws
  where
    step mAcc next = do
        acc <- mAcc
        out <- freshWire
        emit $ NComb out N.POr [acc, next]
        return out

-- | Build a right-to-left priority mux tree: the /first/ (head) pair wins.
-- @buildMuxTree [(sel0, val0), (sel1, val1)] def@ →
--   @if sel0 then val0 else if sel1 then val1 else def@
buildMuxTree :: [(WireId, WireId)] -> WireId -> NetM WireId
buildMuxTree []            def = return def
buildMuxTree ((sel, v):rest) def = do
    restW <- buildMuxTree rest def
    out   <- freshWire
    emit $ NComb out N.PMux [sel, v, restW]
    return out

-- | Number of bits needed to address @count@ entries.
addrBitsFor :: Int -> Int
addrBitsFor n
    | n <= 1    = 1
    | otherwise = ceiling (logBase 2 (fromIntegral n) :: Double)
