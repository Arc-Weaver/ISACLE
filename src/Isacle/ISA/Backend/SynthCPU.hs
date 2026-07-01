{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecursiveDo         #-}
-- | CPU-level synthesis: adjoins a 'CPUDef' with an 'ISADef' to produce a
-- complete Harvard decode-execute circuit — written entirely against the
-- 'Hdl'/'Signal' interface (no 'NetM', no 'WireId').
--
-- State is 'registerW' (scalar registers, sequencer counters/latches) and
-- 'regBank' (register files); combinational logic is pure 'Signal' composition;
-- the @mdo@ block ties the feedback loops (register output → write arbiter →
-- next; data-read address → execution sequencer → read result).  CPU memory
-- inputs are signal arguments; outputs are the returned 'CpuMemIface'.
module Isacle.ISA.Backend.SynthCPU
    ( synthHarvardCPU'
    , synthHarvardCPU
    , CpuMemIface(..)
    , extractPcName
    , addrBitsFor
    , seqNAcc
      -- * Pure Signal vocabulary reused by the VN pass
    , litS, andS, orS, notS, toBool, eqS, muxS, sliceS, resizeS, addS, bwAndS, bwOrS
    , orReduce, priorityMux
    ) where

import Prelude hiding (Word)
import Data.Kind (Type)
import Data.List (foldl', nub, find)
import Data.Proxy (Proxy(..))
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Control.Monad (forM, forM_)
import GHC.TypeLits (natVal, KnownNat)

import Hdl.Bits (Unsigned(..), Bit(..))
import Hdl.Net (NetM, WireId, freshWire, emit, NetNode(..))
import qualified Hdl.Net as N
import Hdl.Types (KnownDom(..), Signal(..), Sig(..), materialize)
import Hdl.Monad (Hdl(..))
import Isacle.ISA.Types
import Isacle.ISA.CPUDef
import Isacle.ISA.Def
import Isacle.ISA.IR (ReadTok(..))
import Isacle.ISA.Build (ISABuild, runISABuild, evalISABuild)
import Isacle.ISA.Backend.Synth
import Data.Bits ((.|.), shiftL)

-- ---------------------------------------------------------------------------
-- Pure Signal vocabulary (the Signal operators the backend reaches for)
-- ---------------------------------------------------------------------------

-- | A literal (phantom-free, so it serves as data or a Bool condition).
litS :: Signal s => Integer -> Int -> s dom a
litS = sigLitW

-- Logical ops (sigPrim's output phantom is free) — yield a Bool condition.
andS, orS, eqS, ltS :: Signal s => s dom x -> s dom y -> s dom Bool
andS = sigPrim2 N.PAnd
orS  = sigPrim2 N.POr
eqS  = sigPrim2 N.PEq
ltS  = sigPrim2 N.PLt

notS :: Signal s => s dom x -> s dom Bool
notS = sigPrim1 N.PNot

-- | A 1-bit signal as a Bool condition: @x = '1'@.
toBool :: Signal s => s dom x -> s dom Bool
toBool x = sigPrim2 N.PEq x (sigLitW 1 1)

-- Data ops — yield a (type-erased) data signal.
addS, bwAndS, bwOrS :: Signal s => s dom x -> s dom y -> s dom ()
addS   = sigPrim2 N.PAdd
bwAndS = sigPrim2 N.PAnd
bwOrS  = sigPrim2 N.POr

muxS :: Signal s => s dom Bool -> s dom () -> s dom () -> s dom ()
muxS = sigPrim3 N.PMux

sliceS :: Signal s => Int -> Int -> s dom x -> s dom ()
sliceS hi lo = sigPrim1 (N.PSlice hi lo)

resizeS :: Signal s => Int -> s dom x -> s dom ()
resizeS w = sigPrim1 (N.PResize w)

-- | OR-reduce a list of conditions; constant-0 when empty.
orReduce :: Signal s => [s dom Bool] -> s dom Bool
orReduce []     = litS 0 1
orReduce (x:xs) = foldl' orS x xs

-- | Right-to-left priority mux: the /first/ matching pair wins, else @def@.
priorityMux :: Signal s => [(s dom Bool, s dom ())] -> s dom () -> s dom ()
priorityMux pairs def = foldr (\(sel, v) acc -> muxS sel v acc) def pairs

-- ---------------------------------------------------------------------------
-- Entry point
-- ---------------------------------------------------------------------------

-- | The CPU's memory-port /outputs/ (the CPU drives all of these).  Inputs
-- (instruction word, read data, stall, irq) are passed to 'synthHarvardCPU'' as
-- signal arguments.
data CpuMemIface s dom = CpuMemIface
    { cmiCodeRdAddr :: s dom ()    -- ^ PC value driving code memory address
    , cmiDataRdAddr :: s dom ()    -- ^ data read address
    , cmiDataWrEn   :: s dom Bool  -- ^ data write enable (1-bit)
    , cmiDataWrAddr :: s dom ()    -- ^ data write address
    , cmiDataWrData :: s dom ()   -- ^ data write data
    , cmiCodeAddrW  :: Int
    , cmiDataAddrW  :: Int
    , cmiWordW      :: Int
    , cmiCodeWordW  :: Int
    }

-- | Synthesise a complete Harvard CPU from a 'CPUDef' and 'ISADef' as 'Hdl'.
--
-- Inputs (all signals): instruction word, data read data, code read data (LPM),
-- stall (1 = data transaction not complete), and irq pending / vector (tie to 0
-- when the ISA has no interrupt body / no controller is wired).
synthHarvardCPU'
    :: forall s m dom wordW addrW codeWordW codeAddrW alu.
       ( Hdl s m, Signal s, KnownDom dom
       , KnownNat wordW, KnownNat addrW, KnownNat codeWordW, KnownNat codeAddrW )
    => CPUDef alu
    -> ISADef (ISABuild alu wordW addrW codeWordW codeAddrW)
    -> s dom ()   -- ^ instr_word
    -> s dom ()   -- ^ data_rd_data
    -> s dom ()   -- ^ code_rd_data (LPM)
    -> s dom ()   -- ^ stall
    -> s dom ()   -- ^ irq_pending
    -> s dom ()   -- ^ irq_vector
    -> m (CpuMemIface s dom)
synthHarvardCPU' cpuDef isaDef instrSig dmemRdData cmemRdData stallSig irqPendSig irqVecSig = do
    let (aluRec, schema) = runCPUDef cpuDef
        wordBits     = fromIntegral (natVal (Proxy @wordW))     :: Int
        addrBits     = fromIntegral (natVal (Proxy @addrW))     :: Int
        codeBits     = fromIntegral (natVal (Proxy @codeWordW)) :: Int
        codeAddrBits = fromIntegral (natVal (Proxy @codeAddrW)) :: Int

        -- Reset value maps from isaReset.
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

        -- Endianness-aware byte addressing for multi-byte register aliases.
        nBytesOf bits = (bits + wordBits - 1) `div` wordBits
        byteAtOffset nBytes off = case schEndianness schema of
            LittleEndian -> off
            BigEndian    -> nBytes - 1 - off

    mdo
        -- Scalar registers: output ← arbiter-computed (enable, next).
        scalarOuts <- forM regList $ \decl ->
            let name = rdName decl
            in  named name =<< registerW (rdWidth decl) (initOf name) (enOf name) (nxtOf name)
        let scalarMap  = Map.fromList (zip (map rdName regList) scalarOuts)
            readScalar n = Map.findWithDefault (litS 0 wordBits) n scalarMap
            getFlag rn bp = maybe (litS 0 1) (sliceS bp bp) (Map.lookup rn scalarMap)

            -- byte @b@ of a register signal, as a wordBits value
            regByteSig srcSig srcBits b =
                let lo = wordBits * b; hi = min (wordBits * (b + 1)) srcBits
                in resizeS wordBits (sliceS (hi - 1) lo srcSig)
            -- replace byte @b@ of @regSig@ with @dat@ (keeping the other bytes)
            replaceByteSig regSig regW b dat =
                let lo = wordBits * b; hi = min (wordBits * (b + 1)) regW
                    clearVal = (2 :: Integer) ^ hi - 2 ^ lo
                    maskVal  = (2 ^ regW - 1) - clearVal
                    cleared  = bwAndS regSig (litS maskVal regW)
                    shifted  = sigPrim2 N.PShiftL (resizeS regW dat) (litS (fromIntegral lo) regW)
                in bwOrS cleared shifted

            -- @addr@ in @[base, base+count)@ ?
            inFileRange addr base count =
                andS (notS (sigPrim2 N.PLt addr (litS base addrBits)))
                     (sigPrim2 N.PLt addr (litS (base + fromIntegral count) addrBits))
            fileIndexW addr base count =
                resizeS (addrBitsFor count) (sigPrim2 N.PSub addr (litS base addrBits))

        -- Alias-aware data read: a read whose address matches a memory-mapped
        -- register (or a register-file alias window) returns the live state.
        let aliasReadOf addr = do
                rfAcc <- foldr (\(fileName, base) accM -> do
                            acc <- accM
                            case Map.lookup fileName rfInfoMap of
                              Nothing -> pure acc
                              Just (count, _) -> do
                                rd <- regBankRead "cpu_state" fileName count (fileIndexW addr base count)
                                pure (muxS (inFileRange addr base count) rd acc))
                          (pure dmemRdData) (schAliasFiles schema)
                pure $ foldr (\(regName, aliasAddr) acc ->
                          case Map.lookup regName scalarMap of
                            Nothing -> acc
                            Just src ->
                              let srcBits = widthOf regName
                                  nBytes  = nBytesOf srcBits
                              in foldr (\off a ->
                                   let cmp   = eqS addr (litS (aliasAddr + fromIntegral off) addrBits)
                                       byteV = regByteSig src srcBits (byteAtOffset nBytes off)
                                   in muxS cmp byteV a) acc [0 .. nBytes - 1])
                       rfAcc (schAliasRegs schema)

        -- Per-instruction render context.  @rcReadRes@ is supplied by the
        -- sequencer (mdo); register reads come from the scalar map / regBankRead.
        let mkCtx rcReadResFn rcIrq = RenderCtx
                { rcInstrWire  = instrSig
                , rcReadScalar = readScalar
                , rcDataBus    = dmemRdData
                , rcCodeBus    = cmemRdData
                , rcReadRes    = rcReadResFn
                , rcGetFlag    = getFlag
                , rcRegCount   = regCount
                , rcIrqVector  = rcIrq
                , rcWordW      = wordBits
                }

        -- Render every instruction body (and the interrupt body, if any).
        results <- forM (zip [0 ..] (isaInstrs isaDef)) $ \(i, instr) ->
            renderSynth (mkCtx (seqReadResOf i) Nothing) Nothing (runISABuild aluRec instr)
        irqResult <- case isaInterruptBody isaDef of
            Nothing   -> pure Nothing
            Just body -> Just <$>
                renderSynth (mkCtx (seqReadResOf (length (isaInstrs isaDef))) (Just irqVecSig))
                            (Just (toBool irqPendSig)) (runISABuild aluRec body)
        let allResults0 = results ++ maybe [] (: []) irqResult

        -- Override each read's bus value with its alias-decoded read.
        allResults <- forM allResults0 $ \r -> do
            mrs <- forM (srMemReads r) $ \rr -> do
                bus <- aliasReadOf (mrAddrWire rr)
                pure rr { mrBusWire = bus }
            pure r { srMemReads = mrs }

        -- Execution sequencer: commit enable, per-access cycle gate, and the
        -- per-(instruction,read) result signal the bodies consumed.  Bound via
        -- lazy field accessors — a strict @ExecSeq _ _ _@ pattern would force the
        -- result before @mdo@'s mfix ties it (the bodies reference @seqReadResOf@).
        execSeq <- buildExecSequencer wordBits stallSig allResults
        let commit       = esCommit execSeq
            gate         = esGate execSeq
            seqReadResOf = esReadRes execSeq

        -- Register files: one indexed write port per RegWriteReq (gated by match
        -- AND commit), plus data-space alias-window stores.
        let allRegWrites = concatMap srRegWrites allResults
            allMemWrites = concatMap srMemWrites allResults
            regWritesByRf = foldl' (\mp r -> Map.insertWith (++) (rwRfName r) [r] mp)
                                   Map.empty allRegWrites
            readRfNames  = nub [ rrRfName rr | r <- allResults, rr <- srRegReads r ]
            involvedRfs  = Map.keys regWritesByRf
                        ++ filter (`Map.notMember` regWritesByRf) readRfNames
        forM_ involvedRfs $ \rfname -> do
            let (rfCount, rfWidth) = maybe (1, wordBits) id (Map.lookup rfname rfInfoMap)
                instrWrites = [ (rwIdxWire w, rwDataWire w, andS (rwMatchWire w) commit)
                              | w <- allRegWrites, rwRfName w == rfname ]
                fileBases   = [ base | (fn, base) <- schAliasFiles schema, fn == rfname ]
                aliasWrites = [ ( fileIndexW (mwAddrWire mw) base rfCount
                                , mwDataWire mw
                                , andS (andS (mwMatchWire mw) (inFileRange (mwAddrWire mw) base rfCount)) commit )
                              | base <- fileBases, mw <- allMemWrites ]
            regBank "cpu_state" rfname rfCount rfWidth (instrWrites ++ aliasWrites)

        -- Data writes to an aliased register byte route to a ScalarWriteReq.
        let aliasScalarWrites =
                [ ScalarWriteReq gated regName full
                | (regName, aliasAddr) <- schAliasRegs schema
                , Map.member regName scalarMap
                , let regW   = widthOf regName
                      regSig  = scalarMap Map.! regName
                      nBytes = nBytesOf regW
                , mw <- allMemWrites
                , off <- [0 .. nBytes - 1]
                , let cmp   = eqS (mwAddrWire mw) (litS (aliasAddr + fromIntegral off) addrBits)
                      gated = andS (mwMatchWire mw) cmp
                      full  = replaceByteSig regSig regW (byteAtOffset nBytes off) (mwDataWire mw)
                ]

        -- Scalar / status write arbiters → (enable, next) per register.
        let allScalarWrites = concatMap srScalarWrites allResults ++ aliasScalarWrites
            allFlagWrites   = concatMap srFlagWrites allResults
            scalarWritesByReg = foldl' (\mp r -> Map.insertWith (++) (swRegName r) [r] mp)
                                       Map.empty allScalarWrites
            arbiterOf decl =
                let name        = rdName decl
                    w           = rdWidth decl
                    scWrites    = Map.findWithDefault [] name scalarWritesByReg
                    writePairs  = [ (swMatchWire r, swDataWire r) | r <- scWrites ]
                    regSig      = scalarMap Map.! name
                in if name == pcName
                   then let pcInc = addS regSig (litS 1 w)
                        in (name, commit, priorityMux writePairs pcInc)
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
                       in (name, andS commit (orReduce matches), nxt)
                     Nothing -> case scWrites of
                       [] -> (name, litS 0 1, litS 0 w)
                       ws -> ( name
                             , andS commit (orReduce (map swMatchWire ws))
                             , priorityMux writePairs (litS 0 w) )
            arbiters = map arbiterOf regList
            enMap  = Map.fromList [ (n, e) | (n, e, _) <- arbiters ]
            nxtMap = Map.fromList [ (n, x) | (n, _, x) <- arbiters ]
            enOf  n = Map.findWithDefault (litS 0 1)        n enMap
            nxtOf n = Map.findWithDefault (litS 0 (widthOf n)) n nxtMap

        -- Data-memory read address mux (each read gated by its exec cycle).
        let readsIndexed  = [ (seqNAcc r, idx, rr)
                            | r <- allResults, (idx, rr) <- zip [0 ..] (srMemReads r) ]
            gatedReads    = [ (gate nAcc (mrMatchWire rr) idx, mrAddrWire rr)
                            | (nAcc, idx, rr) <- readsIndexed ]
            dmemRdAddr    = priorityMux gatedReads (litS 0 addrBits)
        -- Data-memory write arbiter (write j on exec cycle nReads + j).
        let writesIndexed = [ (seqNAcc r, length (srMemReads r) + idx, mw)
                            | r <- allResults, (idx, mw) <- zip [0 ..] (srMemWrites r) ]
            gatedWrites   = [ (gate nAcc (mwMatchWire mw) cyc, mw) | (nAcc, cyc, mw) <- writesIndexed ]
            dmemWrEn      = orReduce (map fst gatedWrites)
            dmemWrAddr    = priorityMux [ (g, mwAddrWire mw) | (g, mw) <- gatedWrites ] (litS 0 addrBits)
            dmemWrData    = priorityMux [ (g, mwDataWire mw) | (g, mw) <- gatedWrites ] (litS 0 wordBits)

        -- PC → code address (resize if PC width ≠ codeAddrW).
        let pcSig     = readScalar pcName
            pcRegW    = widthOf pcName
            codeRdAddr = if codeAddrBits == pcRegW then pcSig
                         else sliceS (codeAddrBits - 1) 0 pcSig

        pure CpuMemIface
            { cmiCodeRdAddr = codeRdAddr
            , cmiDataRdAddr = dmemRdAddr
            , cmiDataWrEn   = dmemWrEn
            , cmiDataWrAddr = dmemWrAddr
            , cmiDataWrData = dmemWrData
            , cmiCodeAddrW  = codeAddrBits
            , cmiDataAddrW  = addrBits
            , cmiWordW      = wordBits
            , cmiCodeWordW  = codeBits
            }

-- ---------------------------------------------------------------------------
-- Execution sequencer
-- ---------------------------------------------------------------------------

-- | Total accesses (reads + writes) on the single data port, sequenced onto
-- distinct cycles: reads on @0..nReads-1@, writes on @nReads..@.  A read-modify-
-- write of one address reads before it writes, never both at once.
seqNAcc :: SynthResult s dom -> Int
seqNAcc r = length (srMemReads r) + length (srMemWrites r)

-- | The execution sequencer's outputs.
data ExecSeq s dom = ExecSeq
    { esCommit :: s dom Bool
      -- ^ gate every architectural-state write enable; high on the instruction's
      --   final cycle and only when the bus is not stalling.
    , esGate   :: Int -> s dom Bool -> Int -> s dom Bool
      -- ^ @esGate nAcc match i@ — select for the @i@-th access of an instruction
      --   with @nAcc@ accesses and match signal @match@.
    , esReadRes :: Int -> ReadTok -> s dom ()
      -- ^ @esReadRes instrIdx tok@ — the read-result signal a body consumed.
    }

-- | Build the execution sequencer.  Single-access instructions commit whenever
-- their transaction isn't stalled; multi-access ones advance an @exec_cycle@
-- counter, latching read values that outlive their cycle.
buildExecSequencer
    :: forall s m dom. (Hdl s m, Signal s, KnownDom dom)
    => Int -> s dom () -> [SynthResult s dom] -> m (ExecSeq s dom)
buildExecSequencer dataW stallW results = do
    let memMatches = [ m | r <- results
                         , max (length (srMemReads r)) (length (srMemWrites r)) >= 1
                         , Just m <- [srMatchWire r] ]
        memActive  = orReduce memMatches
        maxAcc     = maximum (1 : map seqNAcc results)

    if maxAcc < 2
      then pure (ExecSeq (notS (andS memActive stallW)) (\_ m _ -> m) busOfReadTok)
      else mdo
        let cw = addrBitsFor maxAcc
        execCyc <- named "exec_cycle" =<< registerW cw 0 (litS 1 1) execNxt
        let cyclesNeeded = priorityMux
                [ (m, litS (fromIntegral (seqNAcc r - 1)) cw)
                | r <- results, seqNAcc r >= 2, Just m <- [srMatchWire r] ]
                (litS 0 cw)
            isLast   = eqS execCyc cyclesNeeded
            notWait  = notS (andS memActive stallW)
            commit   = andS isLast notWait
            execNxt  = muxS (notS notWait) execCyc
                            (muxS isLast (litS 0 cw) (addS execCyc (litS 1 cw)))
            gate nAcc m i
                | nAcc < 2  = m
                | otherwise = andS m (eqS execCyc (litS (fromIntegral i) cw))
        -- Read results: a read taken straight off the (alias-decoded) bus when it
        -- is the instruction's final access, else latched and replayed.
        latched <- buildReadLatches dataW cw execCyc notWait gate results
        let readRes i tok =
                Map.findWithDefault (busOfReadTok i tok) (i, tokIndex tok) latched
        pure (ExecSeq commit gate readRes)
  where
    -- the (alias-decoded) bus value the i-th instruction's read @tok@ sees.
    -- Resolved by the read's 'ReadTok' (NOT its position — code reads also
    -- consume tokens, so a data read's token need not equal its index).
    busOfReadTok i tok =
        case [ mrBusWire rr | rr <- srMemReads (results !! i), mrTok rr == tokIndex tok ] of
            (b : _) -> b
            []      -> litS 0 dataW
    tokIndex (ReadTok t) = t

-- | Holding latches for reads whose value outlives their cycle: returns a map
-- @(instrIdx, readIdx) -> replay signal@ for the non-final reads only.
buildReadLatches
    :: forall s m dom. (Hdl s m, Signal s, KnownDom dom)
    => Int -> Int -> s dom () -> s dom Bool
    -> (Int -> s dom Bool -> Int -> s dom Bool)
    -> [SynthResult s dom] -> m (Map (Int, Int) (s dom ()))
buildReadLatches dataW cw execCyc notWait gate results =
    fmap (Map.fromList . concat) $ forM (zip [0 ..] results) $ \(i, r) -> do
        let rds  = srMemReads r
            nAcc = seqNAcc r
        fmap concat $ forM (zip [0 ..] rds) $ \(j, rr) ->
            if j == nAcc - 1
                then pure []   -- final access: straight off the bus, no latch
                else do
                    let capEn = andS (gate nAcc (mrMatchWire rr) j) notWait
                    latch <- registerW dataW 0 capEn (mrBusWire rr)
                    let onCyc  = eqS execCyc (litS (fromIntegral j) cw)
                        replay = muxS onCyc (mrBusWire rr) latch
                    pure [((i, mrTok rr), replay)]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | The PC register name, from 'isaPc' (reads only the ALU record).
extractPcName :: alu -> ISADef (ISABuild alu wordW addrW codeWordW codeAddrW) -> String
extractPcName aluRec isaDef =
    let SomeCPURegister (CPURegister n) = evalISABuild aluRec (isaPc isaDef) in n

-- | Number of bits needed to address @count@ entries.
addrBitsFor :: Int -> Int
addrBitsFor n
    | n <= 1    = 1
    | otherwise = ceiling (logBase 2 (fromIntegral n) :: Double)

-- ---------------------------------------------------------------------------
-- Concrete instantiation boundary (NetM): exposes the CPU's memory ports as
-- top-level inputs/outputs, for unit-testing the CPU in isolation.  This is the
-- only place 'NetM' appears — it runs the abstract 'synthHarvardCPU'' at the
-- netlist backend (@s = Sig@, @m = NetM@), creating the ports and materialising
-- the outputs.
-- ---------------------------------------------------------------------------

synthHarvardCPU :: forall (dom :: Type) wordW addrW codeWordW codeAddrW alu.
                   ( KnownDom dom, KnownNat wordW, KnownNat addrW
                   , KnownNat codeWordW, KnownNat codeAddrW )
                => CPUDef alu
                -> ISADef (ISABuild alu wordW addrW codeWordW codeAddrW)
                -> NetM ()
synthHarvardCPU cpuDef isaDef = do
    let domInfo      = domId (Proxy @dom)
        wordBits     = fromIntegral (natVal (Proxy @wordW))     :: Int
        codeBits     = fromIntegral (natVal (Proxy @codeWordW)) :: Int
        codeAddrBits = fromIntegral (natVal (Proxy @codeAddrW)) :: Int
        inPort name w = do { wid <- freshWire; emit (NInput wid name w domInfo)
                           ; pure (SWire wid :: Sig dom ()) }
        outPort :: forall a. String -> Int -> Sig dom a -> NetM ()
        outPort name w sig = do { wid <- materialize sig; emit (NOutput wid name w domInfo) }
    instrW <- inPort "instr_word"   codeBits
    dmemRd <- inPort "data_rd_data" wordBits
    cmemRd <- inPort "code_rd_data" codeBits
    stall  <- inPort "stall"        1
    irqP   <- inPort "irq_pending"  1
    irqV   <- inPort "irq_vector"   codeAddrBits
    cmi <- synthHarvardCPU' @Sig @NetM @dom @wordW @addrW @codeWordW @codeAddrW
               cpuDef isaDef instrW dmemRd cmemRd stall irqP irqV
    outPort "code_rd_addr" (cmiCodeAddrW cmi) (cmiCodeRdAddr cmi)
    outPort "data_rd_addr" (cmiDataAddrW cmi) (cmiDataRdAddr cmi)
    outPort "data_wr_en"   1                  (cmiDataWrEn   cmi)
    outPort "data_wr_addr" (cmiDataAddrW cmi) (cmiDataWrAddr cmi)
    outPort "data_wr_data" (cmiWordW     cmi) (cmiDataWrData cmi)
