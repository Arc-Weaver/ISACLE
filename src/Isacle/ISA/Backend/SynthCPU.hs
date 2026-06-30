{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
-- | CPU-level synthesis: adjoins a 'CPUDef' with a 'ISADef' to produce a
-- complete Harvard-architecture decode-execute circuit in the 'NetM' IR.
--
-- = Hdl structure
--
-- * Scalar registers (PC, SP, …) become 'NReg' nodes, initialised from
--   'isaReset'.
-- * Flags become 1-bit 'NReg' nodes.
-- * Register files become register banks: one 'NReg' flip-flop per entry, with
--   combinational read muxes and one decoded write port per simultaneous-write
--   slot (so e.g. MUL writes R0 and R1 in the same cycle).  Not block RAM.
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
      -- * Shared netlist utilities
    , extractPcName
    , litWire
    , driveWire
    , buildOrTree
    , buildMuxTree
    , addrBitsFor
    ) where

import Prelude hiding (Word)
import Data.Kind (Type)
import Data.List (foldl', nub)
import Data.Proxy (Proxy(..))
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Control.Monad (forM, forM_, foldM)
import GHC.TypeLits (natVal)

import Hdl.Bits
import Hdl.Net
import qualified Hdl.Net as N
import Hdl.Types (KnownDom(..), Sig(..), materialize)
import Hdl.Monad (regBank, regBankRead)
import Isacle.ISA.Types
import Isacle.ISA.CPUDef
import Isacle.ISA.Def
import Isacle.ISA.Build (ISABuild, runISABuild, evalISABuild)
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
synthHarvardCPU' :: forall (dom :: Type) wordW addrW codeWordW codeAddrW alu.
                    ( KnownDom dom
                    , KnownNat wordW, KnownNat addrW
                    , KnownNat codeWordW, KnownNat codeAddrW )
                 => CPUDef alu
                 -> ISADef (ISABuild alu wordW addrW codeWordW codeAddrW)
                 -> WireId   -- ^ pre-allocated instr_word wire
                 -> WireId   -- ^ pre-allocated data_rd_data wire
                 -> WireId   -- ^ pre-allocated code_rd_data wire (LPM stub)
                 -> WireId   -- ^ pre-allocated stall wire (1 = data txn not complete)
                 -> NetM CpuMemIface
synthHarvardCPU' cpuDef isaDef instrWireId dmemRdDataW cmemRdDataW stallWireId = do
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
        hintWire enW  (name ++ "_en")
        return (name, w, outW, nxtW, enW)

    -- Group all CPU state registers into a VHDL record so the emitter
    -- produces  "type cpu_state_t is record ..."  and references like
    -- "cpu_state.PC" instead of flat signal names.
    emit $ N.NGroup "cpu_state"
        [ (name, outW) | (name, _, outW, _, _) <- scalarRegs ]

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

    -- A multi-byte register aliased at @base@ occupies @ceil(width/8)@ consecutive
    -- data addresses; which byte each address carries follows the ISA endianness
    -- (little-endian: low byte at @base@; big-endian: high byte at @base@).
    let nBytesOf bits = (bits + wordBits - 1) `div` wordBits
        -- the register byte index addressed by data-offset @off@ from the base
        byteAtOffset nBytes off = case schEndianness schema of
            LittleEndian -> off
            BigEndian    -> nBytes - 1 - off
        -- byte @b@ of a register wire, as a wordBits-wide value
        regByteWire srcW srcBits b = do
            let lo     = wordBits * b
                hiExcl = min (wordBits * (b + 1)) srcBits
            sl <- freshWire; emit $ NComb sl (N.PSlice (hiExcl - 1) lo) [srcW]
            w  <- freshWire; emit $ NComb w  (N.PResize wordBits) [sl]
            return w
        -- replace byte @b@ of @regOutW@ with @dataW@ (keeping the other bytes)
        replaceByteWire regOutW regW b dataW = do
            let lo       = wordBits * b
                hiExcl   = min (wordBits * (b + 1)) regW
                clearVal = (2 :: Integer) ^ hiExcl - 2 ^ lo
                maskVal  = (2 ^ regW - 1) - clearVal
            maskW   <- litWire maskVal regW
            cleared <- freshWire; emit $ NComb cleared N.PAnd [regOutW, maskW]
            dExt    <- freshWire; emit $ NComb dExt (N.PResize regW) [dataW]
            shW     <- litWire (fromIntegral lo) regW
            shifted <- freshWire; emit $ NComb shifted N.PShiftL [dExt, shW]
            fullW   <- freshWire; emit $ NComb fullW N.POr [cleared, shifted]
            return fullW

    let scReadMemFnAlias :: WireId -> NetM WireId
        scReadMemFnAlias addrW =
            foldl' (\accM (regName, aliasAddr) -> do
                acc0 <- accM
                case fmap (\(rw, rout, _, _) -> (rout, rw)) (Map.lookup regName scalarRegMap) of
                    Nothing -> return acc0
                    Just (srcW, srcBits) -> do
                        let nBytes = nBytesOf srcBits
                        foldM (\acc off -> do
                            addrLitW <- litWire (aliasAddr + fromIntegral off) addrBits
                            cmpW  <- freshWire; emit $ NComb cmpW N.PEq [addrW, addrLitW]
                            byteW <- regByteWire srcW srcBits (byteAtOffset nBytes off)
                            muxW  <- freshWire; emit $ NComb muxW N.PMux [cmpW, byteW, acc]
                            return muxW)
                          acc0 [0 .. nBytes - 1])
                -- Register-file aliases first: a read in [base, base+count) returns
                -- GPR[addr-base] (the file mapped into the data address space).
                (foldl' (\accM (fileName, base) -> do
                    acc <- accM
                    case Map.lookup fileName rfInfoMap of
                        Nothing -> return acc
                        Just (count, _) -> do
                            inRangeW <- inFileRange addrW base count
                            idxW     <- fileIndexW addrW base count
                            rdW <- materialize =<<
                                (regBankRead "cpu_state" fileName count (SWire idxW)
                                   :: NetM (Sig dom ()))
                            muxW <- freshWire
                            emit $ NComb muxW N.PMux [inRangeW, rdW, acc]
                            return muxW)
                  (return dmemRdDataW)
                  (schAliasFiles schema))
                (schAliasRegs schema)
        -- @addr@ in @[base, base+count)@ ?
        inFileRange addrW base count = do
            loW  <- litWire base addrBits
            hiW  <- litWire (base + fromIntegral count) addrBits
            ltLo <- freshWire; emit $ NComb ltLo N.PLt  [addrW, loW]
            geLo <- freshWire; emit $ NComb geLo N.PNot [ltLo]
            ltHi <- freshWire; emit $ NComb ltHi N.PLt  [addrW, hiW]
            o    <- freshWire; emit $ NComb o    N.PAnd [geLo, ltHi]
            return o
        -- @addr - base@, narrowed to the file's index width.
        fileIndexW addrW base count = do
            baseW <- litWire base addrBits
            diffW <- freshWire; emit $ NComb diffW N.PSub [addrW, baseW]
            idxW  <- freshWire; emit $ NComb idxW (N.PResize (addrBitsFor count)) [diffW]
            return idxW

    let renderCtx = RenderCtx
            { rcInstrWire  = instrWireId
            , rcReadScalar = \n -> scReadRegFn n 0
            , rcDataBus    = dmemRdDataW
            , rcCodeBus    = cmemRdDataW
            , rcGetFlag    = getFlagFn
            , rcIrqVector  = Nothing
            , rcWordW      = wordBits
            }

    -- Each body is built into an InstrIR, then lowered to a SynthResult.
    results <- forM (isaInstrs isaDef) $ \instr ->
        renderSynth renderCtx Nothing (runISABuild aluRec instr)

    -- Interrupt body synthesis (if ISA declares one)
    irqData <- case isaInterruptBody isaDef of
        Nothing   -> return Nothing
        Just body -> do
            irqPendW <- freshWire
            emit $ NInput irqPendW "irq_pending" 1 domInfo
            irqVecW  <- freshWire
            emit $ NInput irqVecW "irq_vector" codeAddrBits domInfo
            r <- renderSynth renderCtx { rcIrqVector = Just irqVecW }
                             (Just irqPendW) (runISABuild aluRec body)
            return (Just (irqPendW, irqVecW, r))

    let allResults0 = results ++ maybe [] (\(_, _, r) -> [r]) irqData

    -- Route every data-memory read through the alias mux: a read whose address
    -- matches a memory-mapped register (SP at 0x5D, SREG at 0x5F, …) returns the
    -- live architectural register value instead of SRAM data.  For an address
    -- that matches nothing — or an ISA with no aliases — this is the SRAM bus
    -- unchanged.  Done per read so each port compares its own address.
    allResults <- forM allResults0 $ \r -> do
        mrs <- forM (srMemReads r) $ \rr -> do
            busW <- scReadMemFnAlias (mrAddrWire rr)
            return rr { mrBusWire = busW }
        return r { srMemReads = mrs }

    -- Execution sequencer: drives read-result wires, supplies the per-access
    -- cycle gate ('esGate') and the architectural-commit enable ('esCommit').
    sq <- buildExecSequencer domInfo wordBits stallWireId allResults
    let nAccOf = seqNAcc   -- total reads+writes: reads then writes on distinct cycles

    -- -----------------------------------------------------------------------
    -- Register files — a block of registers in cpu_state
    -- -----------------------------------------------------------------------

    let allRegWrites = concatMap srRegWrites allResults
    let allMemWrites = concatMap srMemWrites allResults

    let regWritesByRf :: Map String [RegWriteReq]
        regWritesByRf = foldl' (\m r -> Map.insertWith (++) (rwRfName r) [r] m)
                                Map.empty allRegWrites

    -- Collect the set of rf names that appear in reads (without flattening order).
    let readRfNames :: [String]
        readRfNames = nub [ rrRfName rr | r <- allResults, rr <- srRegReads r ]

    let involvedRfs = Map.keys regWritesByRf
                   ++ filter (`Map.notMember` regWritesByRf) readRfNames

    -- A register file is just a block of registers (an array field of cpu_state,
    -- e.g. @cpu_state.GPR@) — the "file" is userspace convenience.  EVERY write
    -- to it is an independent, enable-gated indexed assignment
    -- @GPR(idx) <= data@; there is no bank/port arbiter.  VHDL applies distinct
    -- indices independently, and since instruction matches are exclusive at most
    -- one instruction's writes fire per cycle (MUL's two hit distinct entries).
    -- Data-space stores in the file's alias window are simply more writers.
    forM_ involvedRfs $ \rfname -> do
        let (rfCount, rfWidth) = case Map.lookup rfname rfInfoMap of
                Just p  -> p
                Nothing -> (1, wordBits)
            aBits = addrBitsFor rfCount

        -- Instruction writes: one indexed write per RegWriteReq, gated by its
        -- match AND the architectural commit.
        instrWrites <- forM [ w | w <- allRegWrites, rwRfName w == rfname ] $ \w -> do
            enW <- andGate (rwMatchWire w) (esCommit sq)
            return (rwIdxWire w, rwDataWire w, enW)

        -- Data-space alias writes: a store whose address falls in [base,base+count)
        -- writes GPR[addr-base].
        let fileBases = [ base | (fn, base) <- schAliasFiles schema, fn == rfname ]
        aliasWrites <- fmap concat $ forM fileBases $ \base ->
            forM allMemWrites $ \mw -> do
                inR  <- inFileRange (mwAddrWire mw) base rfCount
                idxW <- fileIndexW  (mwAddrWire mw) base rfCount
                en0  <- andGate (mwMatchWire mw) inR
                enW  <- andGate en0 (esCommit sq)
                return (idxW, mwDataWire mw, enW)

        -- The bank is a clocked array register in cpu_state; each write port is
        -- an independent indexed assignment.  Emitted via the typed 'regBank'
        -- Hdl primitive (wires wrapped as 'SWire'; entry value type is erased).
        regBank "cpu_state" rfname rfCount rfWidth
            ([ (SWire a, SWire d, SWire e) | (a, d, e) <- instrWrites ++ aliasWrites ]
               :: [(Sig dom (), Sig dom (), Sig dom Bool)])

        -- Per-instruction reads for this rf, preserving slot order within each
        -- instruction.  Slot k = the k-th readReg call on rfname; each slot is an
        -- indexed combinational read @cpu_state.<rfname>(addr)@.
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
            hintWire rdAddrW (rfname ++ "_rd" ++ show slot ++ "_addr")
            rdOutW <- materialize =<<
                (regBankRead "cpu_state" rfname rfCount (SWire rdAddrW)
                   :: NetM (Sig dom ()))
            hintWire rdOutW (rfname ++ "_rd" ++ show slot)
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

    -- A data write to an aliased register byte updates just that byte (the other
    -- bytes keep their value); which byte each address writes follows endianness.
    aliasScalarWriteReqs <- fmap concat $ forM (schAliasRegs schema) $
        \(regName, aliasAddr) ->
            case Map.lookup regName scalarRegMap of
                Nothing -> return []
                Just (regW, regOutW, _, _) -> do
                    let nBytes = nBytesOf regW
                    fmap concat $ forM allMemWrites $ \mw ->
                        forM [0 .. nBytes - 1] $ \off -> do
                            addrLitW <- litWire (aliasAddr + fromIntegral off) addrBits
                            cmpW  <- freshWire; emit $ NComb cmpW N.PEq [mwAddrWire mw, addrLitW]
                            gated <- freshWire; emit $ NComb gated N.PAnd [mwMatchWire mw, cmpW]
                            fullW <- replaceByteWire regOutW regW (byteAtOffset nBytes off) (mwDataWire mw)
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
                               ; hintWire o "pc_inc"
                               ; return o }
                pcNxtW   <- buildMuxTree
                                [(swMatchWire r, swDataWire r) | r <- scWrites]
                                pcIncW
                -- The PC advances/jumps only on the instruction's final cycle.
                driveWire nxtW pcNxtW
                driveWire enW  (esCommit sq)

        else case Map.lookup name statusRegMap of
            Just (_, flagNames) -> do
                -- Combined arbiter: per-bit mux merging flag writes and
                -- whole-register scalar writes.  Flag writes take priority
                -- (they appear first in buildMuxTree).
                let bitAssigns = zip (reverse [0 .. w - 1]) flagNames
                bitNextWires <- forM bitAssigns $ \(bitPos, _) -> do
                    curBitW <- freshWire
                    emit $ NComb curBitW (N.PSlice bitPos bitPos) [regOutW]
                    -- Flag writes at this bit position.
                    -- Normalize value wires to 1-bit (MonadALU callers may pass
                    -- a wider Word wire, e.g. 8-bit litC 0; bit 0 is what matters).
                    fwPairs <- forM [ fw | fw <- allFlagWrites
                                        , fwRegName fw == name
                                        , fwBitPos  fw == bitPos ] $ \fw -> do
                        normW <- freshWire
                        emit $ NComb normW (N.PSlice 0 0) [fwValueWire fw]
                        return (fwMatchWire fw, normW)
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
                enOrW   <- buildOrTree allMatchWires
                enGated <- andGate enOrW (esCommit sq)
                driveWire enW  enGated
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
                        enOrW   <- buildOrTree (map swMatchWire ws)
                        enGated <- andGate enOrW (esCommit sq)
                        defW    <- litWire 0 w
                        nxtMux  <- buildMuxTree
                                      [(swMatchWire r, swDataWire r) | r <- ws]
                                      defW
                        driveWire enW  enGated
                        driveWire nxtW nxtMux

    -- -----------------------------------------------------------------------
    -- Data memory read address mux
    -- -----------------------------------------------------------------------

    -- Each access is gated by its cycle within the instruction (esGate), so the
    -- i-th read drives the read-address port only while exec_cycle = i.
    let readsIndexed  = [ (nAccOf r, idx, rr)
                        | r <- allResults, (idx, rr) <- zip [0 ..] (srMemReads r) ]
    gatedReads <- forM readsIndexed $ \(nAcc, idx, rr) -> do
        sel <- esGate sq nAcc (mrMatchWire rr) idx
        return (sel, mrAddrWire rr)
    dmemRdAddrW <- case gatedReads of
        [] -> litWire 0 addrBits
        rs -> buildMuxTree rs =<< litWire 0 addrBits

    -- -----------------------------------------------------------------------
    -- Data memory write arbiter (each write gated by its cycle)
    -- -----------------------------------------------------------------------

    -- Writes are sequenced after the instruction's reads: write j runs on exec
    -- cycle @nReads + j@ (so an SBI/CBI write follows its read, never coincides).
    let writesIndexed = [ (nAccOf r, length (srMemReads r) + idx, mw)
                        | r <- allResults, (idx, mw) <- zip [0 ..] (srMemWrites r) ]
    gatedWrites <- forM writesIndexed $ \(nAcc, cyc, mw) -> do
        sel <- esGate sq nAcc (mwMatchWire mw) cyc
        return (sel, mw)
    (dmemWrEnW, dmemWrAddrW, dmemWrDatW) <- case gatedWrites of
        [] -> (,,) <$> litWire 0 1
                   <*> litWire 0 addrBits
                   <*> litWire 0 wordBits
        ws -> do
            enW  <- buildOrTree (map fst ws)
            defA <- litWire 0 addrBits
            defD <- litWire 0 wordBits
            adW  <- buildMuxTree [(sel, mwAddrWire mw) | (sel, mw) <- ws] defA
            daW  <- buildMuxTree [(sel, mwDataWire mw) | (sel, mw) <- ws] defD
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
synthHarvardCPU :: forall (dom :: Type) wordW addrW codeWordW codeAddrW alu.
                   ( KnownDom dom
                   , KnownNat wordW, KnownNat addrW
                   , KnownNat codeWordW, KnownNat codeAddrW )
                => CPUDef alu
                -> ISADef (ISABuild alu wordW addrW codeWordW codeAddrW)
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
    stallWireId <- freshWire
    emit $ NInput stallWireId "stall" 1 domInfo

    cmi <- synthHarvardCPU' @dom @wordW @addrW @codeWordW @codeAddrW
               cpuDef isaDef instrWireId dmemRdDataW cmemRdDataW stallWireId

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
              -> ISADef (ISABuild alu wordW addrW codeWordW codeAddrW)
              -> NetM String
extractPcName aluRec isaDef =
    let SomeCPURegister (CPURegister n) = evalISABuild aluRec (isaPc isaDef)
    in return n

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

-- ---------------------------------------------------------------------------
-- Execution sequencer
-- ---------------------------------------------------------------------------

-- | Cycles an instruction needs on its busiest single-ported resource: the max
-- of (data reads, data writes).  Register files are register banks with
-- independent per-entry writes (see the bank lowering in 'synthHarvardCPU'), so
-- multiple register-file writes in one instruction (MUL → R0 and R1) commit in
-- the same cycle and do not extend the instruction.
-- An instruction's reads and writes are sequenced onto distinct cycles —
-- reads on cycles @0 .. nReads-1@, writes on @nReads .. nReads+nWrites-1@ — so
-- a read-modify-write of one address (AVR SBI/CBI) reads on an earlier cycle
-- than it writes, never both at once (which would loop the bus combinationally).
-- When an instruction only reads or only writes this equals @max@, so pure-read
-- / pure-write instructions are unaffected.
seqNAcc :: SynthResult -> Int
seqNAcc r = length (srMemReads r) + length (srMemWrites r)

-- | Per-instruction multi-cycle execution sequencer.
--
-- An instruction takes one cycle per access to a contended port: with a single
-- data read port and a single data write port, an instruction needs as many
-- cycles as its largest of (number of reads, number of writes).  The @stall@
-- input extends each individual transaction (bus latency / backpressure): the
-- cycle @stall = 0@ is when the read data is valid / the write is accepted.
data ExecSeq = ExecSeq
    { esCommit :: WireId
      -- ^ 1-bit: gate every architectural-state write enable (registers,
      --   register-file, flags, PC) with this.  Asserted only on the
      --   instruction's final cycle, and only when the bus is not stalling.
    , esGate   :: Int -> WireId -> Int -> NetM WireId
      -- ^ @esGate nAcc matchW i@ — the select for the @i@-th access of an
      --   instruction whose match is @matchW@ with @nAcc@ accesses to one port.
      --   Single-access instructions return @matchW@ unchanged (their access
      --   only ever fires while @exec_cycle = 0@, since matches are exclusive).
    }

-- | 1-bit AND / NOT helpers.
andGate :: WireId -> WireId -> NetM WireId
andGate a b = do { o <- freshWire; emit $ NComb o N.PAnd [a, b]; return o }

notGate :: WireId -> NetM WireId
notGate a = do { o <- freshWire; emit $ NComb o N.PNot [a]; return o }

-- | Build the execution sequencer from all per-instruction results and the
-- stall input, and drive every read's result wire.
--
-- A read's value is taken straight from the bus on the cycle it completes; if
-- the value must survive to a later cycle (only @retFromStack@'s @lo@ byte in
-- the AVR set), it is captured into a holding latch — a sequencer-internal
-- register, deliberately outside the architectural @cpu_state@.
buildExecSequencer
    :: DomId
    -> Int        -- ^ data width (bits) for read latches
    -> WireId     -- ^ stall input
    -> [SynthResult]
    -> NetM ExecSeq
buildExecSequencer domInfo dataW stallW results = do
    let nAccOf      = seqNAcc
        -- An instruction is "active" on the shared transaction sequencer if it
        -- touches data memory; pure register-file multi-writes (MUL) advance the
        -- exec cycle but never stall, so they are excluded from mem_active.
        memMatches  = [ m | r <- results
                          , max (length (srMemReads r)) (length (srMemWrites r)) >= 1
                          , Just m <- [srMatchWire r] ]
        maxAcc      = maximum (1 : map nAccOf results)

    comment "execution sequencer: stall + multi-cycle memory transactions"
    memActiveW <- buildOrTree memMatches
    hintWire memActiveW "mem_active"

    if maxAcc < 2
      then do
        -- No multi-cycle instruction: commit whenever the single transaction
        -- (if any) is not stalled.  Read results come straight off the bus.
        waitW   <- andGate memActiveW stallW;  hintWire waitW   "mem_wait"
        commitW <- notGate waitW;              hintWire commitW "commit"
        forM_ results $ \r ->
            forM_ (srMemReads r) $ \rr ->
                driveWire (mrResultWire rr) (mrBusWire rr)
        return ExecSeq { esCommit = commitW, esGate = \_ m _ -> return m }
      else do
        let cw = addrBitsFor maxAcc
        execW    <- freshWire
        execNxtW <- freshWire
        defer $ emit $ NReg execW execNxtW Nothing (SomeBits 0 cw) domInfo
        hintWire execW    "exec_cycle"
        hintWire execNxtW "exec_cycle_nxt"
        -- cycles_needed = (nAcc - 1) of the selected multi-cycle instruction.
        cyclesNeededW <- do
            pairs <- sequence
                [ do { lw <- litWire (fromIntegral (nAccOf r - 1)) cw; return (m, lw) }
                | r <- results, nAccOf r >= 2, Just m <- [srMatchWire r] ]
            buildMuxTree pairs =<< litWire 0 cw
        hintWire cyclesNeededW "cycles_needed"
        isLastW <- freshWire
        emit $ NComb isLastW N.PEq [execW, cyclesNeededW]
        hintWire isLastW "is_last_cycle"
        waitW    <- andGate memActiveW stallW;  hintWire waitW "mem_wait"
        notWaitW <- notGate waitW;              hintWire notWaitW "not_stall"
        commitW  <- andGate isLastW notWaitW;   hintWire commitW "commit"
        -- exec_cycle_nxt = wait ? exec_cycle : (is_last ? 0 : exec_cycle + 1)
        oneW   <- litWire 1 cw
        incW   <- freshWire; emit $ NComb incW   N.PAdd [execW, oneW]
        zeroW  <- litWire 0 cw
        afterW <- freshWire; emit $ NComb afterW N.PMux [isLastW, zeroW, incW]
        nxtW   <- freshWire; emit $ NComb nxtW   N.PMux [waitW, execW, afterW]
        driveWire execNxtW nxtW

        let gate nAcc m i
                | nAcc < 2  = return m
                | otherwise = do
                    iLit <- litWire (fromIntegral i) cw
                    eqW  <- freshWire; emit $ NComb eqW N.PEq [execW, iLit]
                    andGate m eqW

        -- Drive read result wires (latch reads whose value outlives their cycle).
        -- A read on cycle @i@ may be consumed by a later access of the same
        -- instruction (a subsequent read, or — for SBI/CBI — the write that
        -- follows it).  It can be taken straight off the bus only when it is the
        -- instruction's final access (@i == nAcc - 1@); otherwise it is latched
        -- and replayed on later cycles.  Gating uses the total access count.
        forM_ results $ \r -> do
            let rds  = srMemReads r
                nAcc = nAccOf r
            forM_ (zip [0 ..] rds) $ \(i, rr) ->
                if i == nAcc - 1
                    then driveWire (mrResultWire rr) (mrBusWire rr)
                    else do
                        sel    <- gate nAcc (mrMatchWire rr) i
                        capEnW <- andGate sel notWaitW
                        latchW <- freshWire
                        defer $ emit $ NReg latchW (mrBusWire rr) (Just capEnW)
                                            (SomeBits 0 dataW) domInfo
                        -- Unique per read (index alone collides when several
                        -- instructions latch a read at the same slot, e.g.
                        -- RET and RETI both latching retFromStack's lo byte).
                        hintWire latchW ("rd_latch_" ++ show i ++ "_" ++ show (mrResultWire rr))
                        iLit <- litWire (fromIntegral i) cw
                        eqW  <- freshWire; emit $ NComb eqW  N.PEq  [execW, iLit]
                        muxW <- freshWire; emit $ NComb muxW N.PMux [eqW, mrBusWire rr, latchW]
                        driveWire (mrResultWire rr) muxW

        return ExecSeq { esCommit = commitW, esGate = gate }
