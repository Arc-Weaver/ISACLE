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
    let domInfo  = domId (Proxy @dom)
    let wordBits = fromIntegral (natVal (Proxy @wordW))     :: Int
    let addrBits = fromIntegral (natVal (Proxy @addrW))     :: Int
    let codeBits = fromIntegral (natVal (Proxy @codeWordW)) :: Int

    -- Build reset value maps from isaReset
    let resetEntries = runResetDef (isaReset isaDef) aluRec
    let resetRegMap  = Map.fromList
            [ (n, v) | ResetRegEntry n (Unsigned v) <- resetEntries ]
    let resetFlagMap = Map.fromList
            [ (n, b) | ResetFlagEntry n b <- resetEntries ]

    -- -----------------------------------------------------------------------
    -- Scalar registers (NReg, deferred)
    -- -----------------------------------------------------------------------

    scalarRegs <- forM (schRegisters schema) $ \(name, w) -> do
        outW <- freshWire
        nxtW <- freshWire
        enW  <- freshWire
        let initVal = Map.findWithDefault 0 name resetRegMap
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
    -- Flags (1-bit NReg, deferred)
    -- -----------------------------------------------------------------------

    flags <- forM (schFlags schema) $ \name -> do
        outW <- freshWire
        nxtW <- freshWire
        enW  <- freshWire
        let initBit = Map.findWithDefault Lo name resetFlagMap
            initVal = case initBit of { Lo -> 0; Hi -> 1 }
        defer $ emit $ NReg outW nxtW (Just enW) (SomeBits initVal 1) domInfo
        hintWire outW ("flag_" ++ name)
        hintWire nxtW ("flag_" ++ name ++ "_nxt")
        return (name, outW, nxtW, enW)

    -- -----------------------------------------------------------------------
    -- Register-file info table (from CPUSchema)
    -- -----------------------------------------------------------------------

    let rfInfoMap :: Map String (Int, Int)
        rfInfoMap = Map.fromList
            [ (n, (c, w)) | (n, c, w) <- schRegFiles schema ]

    -- -----------------------------------------------------------------------
    -- Run each instruction body
    -- -----------------------------------------------------------------------

    results <- forM (isaInstrs isaDef) $ \instr ->
        runSynthM aluRec instrWireId scReadRegFn
                  (const (return dmemRdDataW))
                  (const (return cmemRdDataW))
                  instr

    -- -----------------------------------------------------------------------
    -- Write arbiters — register files
    -- -----------------------------------------------------------------------

    let allRegWrites = concatMap srRegWrites results

    let regWritesByRf :: Map String [RegWriteReq]
        regWritesByRf = foldl' (\m r -> Map.insertWith (++) (rwRfName r) [r] m)
                                Map.empty allRegWrites

    -- Collect the set of rf names that appear in reads (without flattening order).
    let readRfNames :: [String]
        readRfNames = nub [ rrRfName rr | r <- results, rr <- srRegReads r ]

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
                | r <- results
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
    -- Write arbiters — scalar registers
    -- -----------------------------------------------------------------------

    let allScalarWrites = concatMap srScalarWrites results
    let scalarWritesByReg :: Map String [ScalarWriteReq]
        scalarWritesByReg = foldl' (\m r -> Map.insertWith (++) (swRegName r) [r] m)
                                    Map.empty allScalarWrites

    pcName <- extractPcName aluRec isaDef

    forM_ scalarRegs $ \(name, w, _outW, nxtW, enW) -> do
        let writes = Map.findWithDefault [] name scalarWritesByReg

        if name == pcName
            then do
                let (_, _, pcOutW, _, _) = head
                        [ s | s@(n, _, _, _, _) <- scalarRegs, n == name ]
                litOneW  <- litWire 1 w
                pcIncW   <- do { o <- freshWire
                               ; emit $ NComb o N.PAdd [pcOutW, litOneW]
                               ; return o }
                pcNxtW   <- buildMuxTree
                                [(swMatchWire r, swDataWire r) | r <- writes]
                                pcIncW
                litOneEn <- litWire 1 1
                driveWire nxtW pcNxtW
                driveWire enW  litOneEn
            else do
                case writes of
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
    -- Write arbiters — flags
    -- -----------------------------------------------------------------------

    let allFlagWrites = concatMap srFlagWrites results
    let flagWritesByName :: Map String [FlagWriteReq]
        flagWritesByName = foldl' (\m r -> Map.insertWith (++) (fwFlagName r) [r] m)
                                   Map.empty allFlagWrites

    forM_ flags $ \(name, _outW, nxtW, enW) -> do
        let writes = Map.findWithDefault [] name flagWritesByName
        case writes of
            [] -> do
                litZeroEn <- litWire 0 1
                litZeroD  <- litWire 0 1
                driveWire enW  litZeroEn
                driveWire nxtW litZeroD
            ws -> do
                enOrW <- buildOrTree (map fwMatchWire ws)
                let FlagWriteConst _ _ b = head ws
                    bVal = case b of { Lo -> 0; Hi -> 1 }
                valW  <- litWire bVal 1
                driveWire enW  enOrW
                driveWire nxtW valW

    -- -----------------------------------------------------------------------
    -- Data memory read address mux
    -- -----------------------------------------------------------------------

    let allMemReads = concatMap srMemReads results
    dmemRdAddrW <- case allMemReads of
        [] -> litWire 0 addrBits
        rs -> buildMuxTree [ (mrMatchWire r, mrAddrWire r) | r <- rs ]
                           =<< litWire 0 addrBits

    -- -----------------------------------------------------------------------
    -- Data memory write arbiter
    -- -----------------------------------------------------------------------

    let allMemWrites = concatMap srMemWrites results
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

    let codeAddrBits = fromIntegral (natVal (Proxy @codeAddrW)) :: Int
        pcRegWidth   = case Map.lookup pcName scalarRegMap of
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
