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
    , CpuMemIface(..)
    , extractPcName
    , addrBitsFor
    , seqNAcc
      -- * Pure Signal vocabulary reused by the VN pass
    , litS, andS, orS, notS, toBool, eqS, muxS, sliceS, resizeS, addS, bwAndS, bwOrS
    , orReduce, priorityMux
    ) where

import Prelude hiding (Word)
import Data.List (foldl', nub, find)
import Data.Proxy (Proxy(..))
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Control.Monad (forM, forM_)
import GHC.TypeLits (natVal, KnownNat, someNatVal, SomeNat(..))

import Hdl.Bits (Unsigned(..), Bit(..))
import qualified Hdl.Net as N
import GHC.Generics (Generic, Rep)
import Hdl.Types (KnownDom(..), Signal(..), HdlType, Width, fromBits, GFields, recordFields, projectField, updateField)
import Hdl.Monad (Hdl(..))
import Isacle.ISA.Types
import Isacle.ISA.CPUDef
import Isacle.ISA.Def
import Isacle.ISA.IR (ReadTok(..), IStmt(..), iirStmts)
import Isacle.ISA.Build (ISABuild, runISABuild, evalISABuild)
import Isacle.ISA.Backend.Synth
import Data.Bits ((.|.), shiftL)

-- ---------------------------------------------------------------------------
-- Pure Signal vocabulary (the Signal operators the backend reaches for)
-- ---------------------------------------------------------------------------

-- | A literal (width from the 'Int', type from context — must be an 'HdlType').
litS :: (Signal s, HdlType a) => Integer -> Int -> s dom a
litS = sigLitW

-- Logical ops (sigPrim's output phantom is free) — yield a Bool condition.
andS, orS, eqS, ltS :: (Signal s, HdlType x, HdlType y) => s dom x -> s dom y -> s dom Bool
andS = sigPrim2 N.PAnd
orS  = sigPrim2 N.POr
eqS  = sigPrim2 N.PEq
ltS  = sigPrim2 N.PLt

notS :: (Signal s, HdlType x) => s dom x -> s dom Bool
notS = sigPrim1 N.PNot

-- | A 1-bit signal as a Bool condition: @x = '1'@.
toBool :: forall s dom x. (Signal s, HdlType x) => s dom x -> s dom Bool
toBool x = sigPrim2 N.PEq x (sigLitW 1 1 :: s dom x)

-- Data ops — width-generic: same-width binary ops preserve the operand type,
-- so typed signals (e.g. @Unsigned addrW@) flow through unchanged.
addS, bwAndS, bwOrS :: (Signal s, HdlType a) => s dom a -> s dom a -> s dom a
addS   = sigPrim2 N.PAdd
bwAndS = sigPrim2 N.PAnd
bwOrS  = sigPrim2 N.POr

-- | A mux preserving its branch type (both arms and the result share @a@).
muxS :: (Signal s, HdlType a) => s dom Bool -> s dom a -> s dom a -> s dom a
muxS = sigPrim3 N.PMux

-- Width-changing ops: free output type (annotate at the use site).
sliceS :: (Signal s, HdlType x, HdlType a) => Int -> Int -> s dom x -> s dom a
sliceS hi lo = sigPrim1 (N.PSlice hi lo)

resizeS :: (Signal s, HdlType x, HdlType a) => Int -> s dom x -> s dom a
resizeS w = sigPrim1 (N.PResize w)

-- | OR-reduce a list of conditions; constant-0 when empty.
orReduce :: Signal s => [s dom Bool] -> s dom Bool
orReduce []     = litS 0 1
orReduce (x:xs) = foldl' orS x xs

-- | Right-to-left priority mux: the /first/ matching pair wins, else @def@.
-- Preserves the branch type @a@ across all pairs and the default.
priorityMux :: (Signal s, HdlType a) => [(s dom Bool, s dom a)] -> s dom a -> s dom a
priorityMux pairs def = foldr (\(sel, v) acc -> muxS sel v acc) def pairs

-- ---------------------------------------------------------------------------
-- Entry point
-- ---------------------------------------------------------------------------

-- | The CPU's memory-port /outputs/ (the CPU drives all of these).  Inputs
-- (instruction word, read data, stall, irq) are passed to 'synthHarvardCPU'' as
-- signal arguments.
data CpuMemIface s dom codeAddrW addrW wordW = CpuMemIface
    { cmiCodeRdAddr :: s dom (Unsigned codeAddrW) -- ^ single code read address: PC (opcode), then PC+1, PC+2, … (operands)
    , cmiDataRdAddr :: s dom (Unsigned addrW)     -- ^ data read address
    , cmiDataWrEn   :: s dom Bool                 -- ^ data write enable (1-bit)
    , cmiDataWrAddr :: s dom (Unsigned addrW)     -- ^ data write address
    , cmiDataWrData :: s dom (Unsigned wordW)     -- ^ data write data
    , cmiIrqAck     :: s dom Bool                 -- ^ 1 the cycle the interrupt handler commits (takes the IRQ)
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
    :: forall core s m dom wordW addrW codeWordW codeAddrW alu.
       ( Hdl s m, Signal s, KnownDom dom
       , HdlType core, Generic core, GFields (Rep core)
       , KnownNat wordW, KnownNat addrW, KnownNat codeWordW, KnownNat codeAddrW )
    => CPUDef alu
    -> ISADef (ISABuild alu wordW addrW codeWordW codeAddrW)
    -> s dom (Unsigned codeWordW)   -- ^ code_data: the single code read port's data
                                    --   (opcode on the fetch cycle, operand words after)
    -> s dom (Unsigned wordW)       -- ^ data_rd_data
    -> s dom Bool                   -- ^ stall
    -> s dom Bool                   -- ^ irq_pending
    -> s dom (Unsigned codeAddrW)   -- ^ irq_vector
    -> m (CpuMemIface s dom codeAddrW addrW wordW)
synthHarvardCPU' cpuDef isaDef codeData dmemRdData stallSig irqPendSig irqVecSig = do
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

        -- PASS 1 — static ISA analysis (pure, no elaboration monad).  Every size
        -- the hardware needs is counted from each instruction body's IR, so it is
        -- independent of the render results.  That independence is load-bearing:
        -- if a size depended on @allResults@, forcing the thing it sizes (e.g.
        -- @opWords@' spine) during render would knot the mfix.
        --   • @maxCode@  sizes the operand-word register array.
        --   • @maxCyc@   sizes the exec-cycle counter (opened as a type in pass 2).
        allBodies = isaInstrs isaDef ++ maybe [] (: []) (isaInterruptBody isaDef)
        stmtsOf body   = iirStmts (runISABuild aluRec body)
        codeReadsOf body = length [ () | SReadCode  _ _ <- stmtsOf body ]
        dataAccOf  body  = length [ () | SReadMem   _ _ <- stmtsOf body ]
                         + length [ () | SWriteMem  _ _ <- stmtsOf body ]
        -- commit cycle = a fetch cycle (opcode read, for any instruction with code
        -- operands) then code-operand reads (0..nCode-1) then data accesses, with
        -- one settle cycle for a pure code-read instruction.  MUST mirror
        -- 'commitCycleOf' (pass 2) exactly, or the counter is mis-sized — the
        -- @fetchOffset@ term is what makes an operanded instruction take its extra
        -- opcode-fetch cycle, so it must be counted here too.
        fetchOffStatic body = if codeReadsOf body > 0 then 1 else 0
        commitCycStatic body = fetchOffStatic body + codeReadsOf body + max (dataAccOf body) 1 - 1
        maxCode = maximum (0 : map codeReadsOf allBodies)
        maxCyc  = maximum (1 : map (\b -> commitCycStatic b + 1) allBodies)

        -- The Core record's fields (MSB-first) and its packed reset value: each
        -- @coreXXX@ field's reset is @initOf "XXX"@ (strip the @core@ prefix).
        coreFields = recordFields (Proxy @core)
        coreReset  = fromBits
            (foldl' (\acc (fn, w) -> (acc `shiftL` w) .|. initOf (drop 4 fn)) 0 coreFields) :: core

        -- A register read: project the named field of the core signal at the
        -- demanded type.  Kept /outside/ the @mdo@ knot and applied to
        -- @coreState@ inline at each use, so it stays polymorphic (a binding
        -- inside the recursive group would be monomorphised to a single type).
        readScalarOf :: forall a. HdlType a => s dom core -> String -> s dom a
        readScalarOf cst n = projectField ("core" ++ n) cst

        -- Endianness-aware byte addressing for multi-byte register aliases.
        nBytesOf bits = (bits + wordBits - 1) `div` wordBits
        byteAtOffset nBytes off = case schEndianness schema of
            LittleEndian -> off
            BigEndian    -> nBytes - 1 - off

    -- PASS 2 — elaboration, in a CPS prelude that opens the exec-cycle counter
    -- width @cyW@ as a real type from the pass-1 size.  'reflectNat' is a pure
    -- @case@ on 'someNatVal' that wraps (does not enter) the @mdo@, so the whole
    -- recursive body runs with @cyW@ in scope — the counter is @Unsigned cyW@ and
    -- its @execNxt@ feedback stays inside one @mdo@, no fixed width, no boundary.
    reflectNat (addrBitsFor maxCyc) $ \(Proxy :: Proxy cyW) -> mdo
        -- The scalar core (SP/PC/SREG) is one clocked register; the arbiter below
        -- ties its next value.  A register read projects the field at the demanded
        -- type — no erased signal, no per-name map.
        coreState <- named "cpu_core" =<< register coreReset nextCore
        let -- read status-register bit @bp@: project the status reg (reflect its
            -- runtime width to a type) and slice the bit.
            getFlag :: String -> Int -> s dom Bool
            getFlag rn bp =
                case someNatVal (fromIntegral (maybe wordBits fst (Map.lookup rn statusRegMap))) of
                    Just (SomeNat (_ :: Proxy n)) -> sliceS bp bp (readScalarOf coreState rn :: s dom (Unsigned n))
                    Nothing                       -> litS 0 1

            -- byte @b@ of a register signal, as a wordBits value
            regByteSig :: forall x. HdlType x => s dom x -> Int -> Int -> s dom (Unsigned wordW)
            regByteSig srcSig srcBits b =
                let lo = wordBits * b; hi = min (wordBits * (b + 1)) srcBits
                in resizeS wordBits (sliceS (hi - 1) lo srcSig :: s dom (Unsigned wordW))
            -- replace byte @b@ of @regSig@ with @dat@ (keeping the other bytes)
            replaceByteSig :: forall a b. (HdlType a, HdlType b)
                           => s dom a -> Int -> Int -> s dom b -> s dom a
            replaceByteSig regSig regW b dat =
                let lo = wordBits * b; hi = min (wordBits * (b + 1)) regW
                    clearVal = (2 :: Integer) ^ hi - 2 ^ lo
                    maskVal  = (2 ^ regW - 1) - clearVal
                    cleared  = bwAndS regSig (litS maskVal regW)
                    shifted  = sigPrim2 N.PShiftL (resizeS regW dat :: s dom a) (litS (fromIntegral lo) regW :: s dom a) :: s dom a
                in bwOrS cleared shifted

            -- @addr@ in @[base, base+count)@ ?
            inFileRange :: forall a. HdlType a => s dom a -> Integer -> Int -> s dom Bool
            inFileRange addr base count =
                andS (notS (sigPrim2 N.PLt addr (litS base addrBits :: s dom a) :: s dom Bool))
                     (sigPrim2 N.PLt addr (litS (base + fromIntegral count) addrBits :: s dom a) :: s dom Bool)
            fileIndexW :: forall a. HdlType a => s dom a -> Integer -> Int -> s dom (Unsigned 8)
            fileIndexW addr base count =
                resizeS (addrBitsFor count) (sigPrim2 N.PSub addr (litS base addrBits :: s dom a) :: s dom a)

        -- Alias-aware data read: a read whose address matches a memory-mapped
        -- register (or a register-file alias window) returns the live state.
        -- Polymorphic in the address type so the override can pass the read's
        -- address WITHOUT forcing it — a data read whose address is a (sequenced)
        -- code-read result must not force the sequencer while it is being built.
        let aliasReadOf :: forall aw. HdlType aw => s dom aw -> m (s dom (Unsigned wordW))
            aliasReadOf addr = do
                rfAcc <- foldr (\(fileName, base) accM -> do
                            acc <- accM
                            case Map.lookup fileName rfInfoMap of
                              Nothing -> pure acc
                              Just (count, _) -> do
                                rd <- regBankRead "cpu_state" fileName count (fileIndexW addr base count)
                                pure (muxS (inFileRange addr base count) rd acc))
                          (pure dmemRdData) (schAliasFiles schema)
                pure $ foldr (\(regName, aliasAddr) acc ->
                          let srcBits = widthOf regName
                              nBytes  = nBytesOf srcBits
                          in case someNatVal (fromIntegral srcBits) of
                               Just (SomeNat (_ :: Proxy n)) ->
                                 let src = readScalarOf coreState regName :: s dom (Unsigned n)
                                 in foldr (\off a ->
                                      let cmp   = eqS addr (litS (aliasAddr + fromIntegral off) addrBits :: s dom (Unsigned addrW))
                                          byteV = regByteSig src srcBits (byteAtOffset nBytes off)
                                                    :: s dom (Unsigned wordW)
                                      in muxS cmp byteV a) acc [0 .. nBytes - 1]
                               Nothing -> acc)
                       rfAcc (schAliasRegs schema)

        -- Per-instruction render context.  @rcReadRes@ is supplied by the
        -- sequencer (mdo); register reads come from the scalar map / regBankRead.
        let mkCtx i rcIrq = RenderCtx
                { rcInstrWire  = sigRetype decodeOpcode
                , rcReadScalar = readScalarOf coreState
                , rcDataBus    = sigRetype dmemRdData
                , rcCodeBus    = sigRetype codeData
                  -- the @j@-th sequentially-fetched operand word (a latched
                  -- register, see @opWords@ below) — forcing it forces a register,
                  -- not the exec sequencer, so no mfix knot.
                , rcCodeWord   = \j -> sigRetype (opWords !! j)
                , rcReadRes    = runSeqReadRes seqReadRes i
                , rcGetFlag    = getFlag
                , rcRegCount   = regCount
                , rcIrqVector  = fmap sigRetype rcIrq
                , rcWordW      = wordBits
                }

        -- Render every instruction body (and the interrupt body, if any).
        results <- forM (zip [0 ..] (isaInstrs isaDef)) $ \(i, instr) ->
            renderSynth (mkCtx i Nothing) Nothing (runISABuild aluRec instr)
        irqResult <- case isaInterruptBody isaDef of
            Nothing   -> pure Nothing
            Just body -> Just <$>
                renderSynth (mkCtx (length (isaInstrs isaDef)) (Just irqVecSig))
                            (Just (toBool irqPendSig)) (runISABuild aluRec body)
        -- Interrupt preemption: when the interrupt is TAKEN this cycle (its match =
        -- @irq_pending@ AND the ISA's gate, e.g. the I/EA flag), the fetched
        -- instruction must not commit — otherwise it races the handler (its PC
        -- write beats the vector, its writes corrupt the pushes).  Suppress every
        -- instruction's effects with @not irqActive@ so only the handler runs, the
        -- clean single-body semantics the reference sim's 'runIrq' already models.
        -- With no interrupt body (or @irq_pending = 0@) @irqActive = 0@, so this is
        -- a no-op — normal instruction execution is unchanged.
        let irqActive    = maybe (litS 0 1) id (irqResult >>= srMatchWire)
            notIrqActive = notS irqActive
            allResults0  = map (suppressWhen notIrqActive) results
                        ++ maybe [] (: []) irqResult

        -- Execution-cycle counter — from @allResults0@ (it depends only on
        -- per-instruction access COUNTS and match wires, which the alias override
        -- below does not change).  It must precede the override, because the
        -- override materialises code-operand addresses (an aliased @LDS@ reads
        -- register-file state at a code-fetched address), which forces @opWords@,
        -- which thread through this counter's @exec_cycle@ effect.
        --
        -- Built INLINE in this outer @mdo@ (not a helper with its own @mdo@): a
        -- nested @mfix@ forced mid-tie would black-hole.  The counter register is
        -- @Unsigned cyW@ — the exact width from pass 1 (@cyW@ opened by the CPS
        -- prelude) — and its @execNxt@ feedback stays inside this one @mdo@.
        -- Downstream cycle comparisons use @execCyc16@ (a resize) so nothing else
        -- needs the narrow width.
        let memActive = orReduce [ m | r <- allResults0, seqNAcc r >= 1, Just m <- [srMatchWire r] ]
            notWait   = notS (andS memActive stallSig)
            cw        = addrBitsFor maxCyc          -- == natVal @cyW
        execCycN <- if maxCyc < 2
            then pure (litS 0 cw :: s dom (Unsigned cyW))
            else named "exec_cycle" =<< registerW cw 0 (litS 1 1) execNxt
        let execCyc16 = resizeS 16 execCycN :: s dom (Unsigned 16)
            cyclesNeeded = priorityMux
                [ (m, litS (fromIntegral (commitCycleOf r)) cw)
                | r <- allResults0, commitCycleOf r >= 1, Just m <- [srMatchWire r] ]
                (litS 0 cw) :: s dom (Unsigned cyW)
            isLast  = eqS execCycN cyclesNeeded
            commit  = if maxCyc < 2 then notWait else andS isLast notWait
            execNxt = muxS (notS notWait) execCycN
                           (muxS isLast (litS 0 cw) (addS execCycN (litS 1 cw)))
                    :: s dom (Unsigned cyW)
            gate :: Int -> s dom Bool -> Int -> s dom Bool
            gate _ m i
                | maxCyc < 2 = m
                | otherwise  = andS m (eqS execCyc16 (litS (fromIntegral i) 16 :: s dom (Unsigned 16)))
            execCtr = ExecCounter commit gate execCyc16 notWait

        -- Sequentially-fetched code operand words.  The CPU walks the code bus:
        -- operand word @j@ is at @PC + 1 + j@, read on exec cycle @j@ and latched.
        -- A body reads back the /register/ (not a mux with the live bus), so the
        -- read path is a plain wire: forcing/materialising a code-operand value
        -- (e.g. as an @LDS@ address in the alias override) touches only the
        -- register, never @exec_cycle@ — that is what keeps the mfix from knotting.
        -- The cost is that the final word must be latched before the instruction
        -- commits, so a pure code-read instruction gets one settle cycle (see
        -- @commitCycleOf@).  @maxCode@ is a static ISA count, so the list spine is
        -- render-independent.
        -- The code memory is a SINGLE read port.  The opcode is the first read
        -- (exec cycle 0, address = PC), latched so the decoder sees it stable while
        -- operand words are fetched at PC+1, PC+2, … on later cycles.  The decode
        -- signal is the live bus on the fetch cycle — so a single-cycle instruction
        -- (no operands) decodes and commits at cycle 0 — and the latch afterwards.
        opcodeReg <- registerW codeBits 0
                       (eqS execCyc16 (litS 0 16 :: s dom (Unsigned 16))) codeData
                     :: m (s dom (Unsigned codeWordW))
        let decodeOpcode = muxS (eqS execCyc16 (litS 0 16 :: s dom (Unsigned 16)))
                                codeData opcodeReg :: s dom (Unsigned codeWordW)
        -- Operand word @j@ is at @PC+1+j@, read on exec cycle @j+1@ (cycle 0 is the
        -- opcode fetch) and latched.  Read back as the register, so forcing it forces
        -- a register, not the sequencer — no mfix knot.
        opWords <- forM [0 .. maxCode - 1] $ \j -> do
            let onCyc = eqS execCyc16 (litS (fromIntegral (j + 1)) 16 :: s dom (Unsigned 16))
            registerW codeBits 0 onCyc codeData :: m (s dom (Unsigned codeWordW))

        -- Override each data read's bus value with its alias-decoded read.  This
        -- materialises the read address; for a code-fetched address that is one of
        -- the @opWords@ registers, whose deferred read (see 'regBankRead') keeps the
        -- surrounding @mfix@ from being forced here.
        allResults <- forM allResults0 $ \r -> do
            mrs <- forM (srMemReads r) $ \(MemReadReq match tok adr _) -> do
                bus <- aliasReadOf adr
                pure (MemReadReq match tok adr bus)
            pure r { srMemReads = mrs }

        -- Data-read result latches (from the overridden results): the per-(instr,
        -- read) value each body consumed, sequenced onto the counter's cycles.
        seqReadRes <- buildDataLatches wordBits execCtr allResults

        -- Register files: one indexed write port per RegWriteReq (match AND
        -- commit), plus data-space alias-window stores.  The entry width is
        -- reflected to a type so the ports are typed.
        let allRegWrites = concatMap srRegWrites allResults
            allMemWrites = concatMap srMemWrites allResults
            involvedRfs  = nub ([ nm | RegWriteReq _ nm _ _ <- allRegWrites ]
                                ++ map (\(n,_,_) -> n) (schRegFiles schema))
        forM_ involvedRfs $ \rfname -> do
            let (rfCount, rfWidth) = maybe (1, wordBits) id (Map.lookup rfname rfInfoMap)
                fileBases          = [ base | (fn, base) <- schAliasFiles schema, fn == rfname ]
            case someNatVal (fromIntegral rfWidth) of
              Just (SomeNat (_ :: Proxy ew)) -> do
                let instrWrites =
                      [ ( sigRetype idx :: s dom (Unsigned 8)
                        , sigRetype dat :: s dom (Unsigned ew)
                        , andS match commit )
                      | RegWriteReq match nm idx dat <- allRegWrites, nm == rfname ]
                    aliasWrites =
                      [ ( fileIndexW addr base rfCount
                        , sigRetype dt :: s dom (Unsigned ew)
                        , andS (andS match (inFileRange addr base rfCount)) commit )
                      | base <- fileBases, MemWriteReq match adr dt <- allMemWrites
                      , let addr = sigRetype adr :: s dom (Unsigned addrW) ]
                regBank "cpu_state" rfname rfCount rfWidth (instrWrites ++ aliasWrites)
              Nothing -> pure ()

        -- Data writes to a memory-mapped scalar register byte → a ScalarWriteReq.
        -- The register width is reflected to a type so the (typed) full-register
        -- next-value can be built from its current value and the written byte.
        let aliasScalarWrites =
                [ ScalarWriteReq gated regName full
                | (regName, aliasAddr) <- schAliasRegs schema
                , let regW = widthOf regName; nBytes = nBytesOf regW
                , MemWriteReq match adr dt <- allMemWrites
                , off <- [0 .. nBytes - 1]
                , Just (SomeNat (_ :: Proxy rw)) <- [someNatVal (fromIntegral (max 1 regW))]
                , let addr   = sigRetype adr :: s dom (Unsigned addrW)
                      cmp    = eqS addr (litS (aliasAddr + fromIntegral off) addrBits :: s dom (Unsigned addrW))
                      gated  = andS match cmp
                      regSig = readScalarOf coreState regName :: s dom (Unsigned rw)
                      full   = replaceByteSig regSig regW (byteAtOffset nBytes off) (sigRetype dt :: s dom (Unsigned wordW)) ]

        -- Scalar / status write arbiter → the next Core value: fold each register's
        -- committed next-value into the core record with 'updateField'.  Each
        -- register's writes are re-tagged to /its/ type (they all target the same
        -- register, so the widths coincide) as they are collected.
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
                                  (resizeS w (b :: s dom Bool)     :: s dom t)
                                  (litS (fromIntegral i) w         :: s dom t) :: s dom t
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
                    en  | name == pcName = commit
                        | otherwise      = andS commit (orReduce matches)
                in updateField ("core" ++ name) (muxS en nxt cur) acc
            nextCore = foldl' updateReg coreState regList

        -- Single code read address: PC on the opcode-fetch cycle, then PC+1, PC+2,
        -- … as the exec cycle advances (operand words follow the opcode on the same
        -- one port — the code memory is a simple single-word-read bus).
        let pcRegW = widthOf pcName
            pcAddr = (case someNatVal (fromIntegral (max 1 pcRegW)) of
                Just (SomeNat (_ :: Proxy pw)) ->
                    resizeS codeAddrBits (readScalarOf coreState pcName :: s dom (Unsigned pw))
                Nothing -> litS 0 codeAddrBits) :: s dom (Unsigned codeAddrW)
            codeAddr = addS pcAddr (resizeS codeAddrBits execCyc16)
                     :: s dom (Unsigned codeAddrW)

        -- Data-memory read address mux (each read gated by its exec cycle).  Data
        -- reads follow the opcode fetch and operand reads, so read @j@ is on cycle
        -- @fetchOffset + nCode + j@.
        let readsIndexed = [ (seqNAcc r, fetchOffset r + length (srCodeReads r) + idx, rr)
                           | r <- allResults, (idx, rr) <- zip [0 ..] (srMemReads r) ]
            gatedReads   = [ (gate nAcc match cyc, sigRetype adr :: s dom (Unsigned addrW))
                           | (nAcc, cyc, MemReadReq match _ adr _) <- readsIndexed ]
            dmemRdAddr   = priorityMux gatedReads (litS 0 addrBits) :: s dom (Unsigned addrW)
        -- Data-memory write arbiter (write j on cycle fetchOffset + nCode + nReads + j).
        let writesIndexed = [ (seqNAcc r, fetchOffset r + length (srCodeReads r) + length (srMemReads r) + idx, mw)
                            | r <- allResults, (idx, mw) <- zip [0 ..] (srMemWrites r) ]
            gatedWrites   = [ ( gate nAcc match cyc
                              , sigRetype adr :: s dom (Unsigned addrW)
                              , sigRetype dt  :: s dom (Unsigned wordW) )
                            | (nAcc, cyc, MemWriteReq match adr dt) <- writesIndexed ]
            dmemWrEn      = orReduce [ g | (g, _, _) <- gatedWrites ]
            dmemWrAddr    = priorityMux [ (g, a) | (g, a, _) <- gatedWrites ] (litS 0 addrBits) :: s dom (Unsigned addrW)
            dmemWrData    = priorityMux [ (g, d) | (g, _, d) <- gatedWrites ] (litS 0 wordBits) :: s dom (Unsigned wordW)

        pure CpuMemIface
            { cmiCodeRdAddr = codeAddr
            , cmiDataRdAddr = dmemRdAddr
            , cmiDataWrEn   = dmemWrEn
            , cmiDataWrAddr = dmemWrAddr
            , cmiDataWrData = dmemWrData
            , cmiIrqAck     = andS irqActive commit  -- handler commits this cycle
            , cmiCodeAddrW  = codeAddrBits
            , cmiDataAddrW  = addrBits
            , cmiWordW      = wordBits
            , cmiCodeWordW  = codeBits
            }

-- ---------------------------------------------------------------------------
-- Execution sequencer
-- ---------------------------------------------------------------------------

-- | Qualify every match wire of a rendered instruction with @off@ (AND it in), so
-- the instruction's effects are suppressed whenever @off@ is low.  Used to hold
-- the fetched instruction off the architectural state while an interrupt is taken.
suppressWhen :: forall s dom. Signal s => s dom Bool -> SynthResult s dom -> SynthResult s dom
suppressWhen off r = r
    { srMatchWire    = fmap (andS off) (srMatchWire r)
    , srRegWrites    = [ w { rwMatchWire = andS off (rwMatchWire w) } | w <- srRegWrites r ]
    , srScalarWrites = [ w { swMatchWire = andS off (swMatchWire w) } | w <- srScalarWrites r ]
    , srMemWrites    = [ w { mwMatchWire = andS off (mwMatchWire w) } | w <- srMemWrites r ]
    , srMemReads     = [ w { mrMatchWire = andS off (mrMatchWire w) } | w <- srMemReads r ]
    , srCodeReads    = [ w { crMatchWire = andS off (crMatchWire w) } | w <- srCodeReads r ]
    , srFlagWrites   = [ w { fwMatchWire = andS off (fwMatchWire w) } | w <- srFlagWrites r ]
    }

-- | Total accesses sequenced onto distinct cycles: code operand reads on
-- @0..nCode-1@ (the CPU walks the code bus fetching operand words), then data
-- reads, then data writes.  A read-modify-write of one address reads before it
-- writes, never both at once.
seqNAcc :: SynthResult s dom -> Int
seqNAcc r = length (srCodeReads r) + length (srMemReads r) + length (srMemWrites r)

-- | The fetch offset: an instruction that reads operand words spends exec cycle 0
-- fetching its opcode on the single code port (the decoder latches it), then reads
-- operands on cycles @1..nCode@ — so all its data cycles shift by one.  An
-- instruction with no code operand reads decodes combinationally on cycle 0 and
-- needs no separate fetch cycle, so it is offset 0 (single-word timing unchanged).
fetchOffset :: SynthResult s dom -> Int
fetchOffset r = if not (null (srCodeReads r)) then 1 else 0

-- | The exec cycle on which an instruction commits.  With the single code port,
-- operand word @j@ is read on cycle @fetchOffset + j@ and latched, so all operand
-- words are readable from cycle @fetchOffset + nCode@; data reads\/writes then run
-- on cycles @fetchOffset + nCode .. fetchOffset + nCode + nData - 1@.  Commit is
-- the last of those, with one settle cycle for a pure code-read instruction.
commitCycleOf :: SynthResult s dom -> Int
commitCycleOf r =
    let nCode = length (srCodeReads r)
        nData = length (srMemReads r) + length (srMemWrites r)
    in fetchOffset r + nCode + max nData 1 - 1

-- | The execution-cycle counter.  Split out from the read latches so it can be
-- built /before/ the alias override (which forces the operand words that hang off
-- @ecExecCyc@) — see 'synthHarvardCPU''.  It depends only on per-instruction
-- access counts and match wires, both unchanged by the override.
data ExecCounter s dom = ExecCounter
    { ecCommit  :: s dom Bool
      -- ^ gate every architectural-state write enable; high on the instruction's
      --   final cycle and only when the bus is not stalling.
    , ecGate    :: Int -> s dom Bool -> Int -> s dom Bool
      -- ^ @ecGate nAcc match i@ — select for the @i@-th access of an instruction
      --   with @nAcc@ accesses and match signal @match@.
    , ecExecCyc :: s dom (Unsigned 16)
      -- ^ the current exec cycle (0 for single-access instructions).  Code operand
      --   word @j@ is fetched from @PC + 1 + j@, so the code operand address is
      --   @PC + 1 + ecExecCyc@ — no dependency on which instruction is live.
    , ecNotWait :: s dom Bool
      -- ^ bus not stalling this cycle (the latch capture-enable qualifier).
    }

-- | Run a continuation with a value-level width reflected to a type.
reflectNat :: Int -> (forall n. KnownNat n => Proxy n -> r) -> r
reflectNat n k = case someNatVal (fromIntegral (max 1 n)) of
    Just (SomeNat p) -> k p
    Nothing          -> error "reflectNat: impossible"

-- | The data-read result resolver (wrapped so the polymorphic read type can be
-- returned from @m@): @runSeqReadRes r instrIdx tok@ is the value a body consumed.
newtype SeqReadRes s dom =
    SeqReadRes { runSeqReadRes :: Int -> (forall a. HdlType a => ReadTok -> s dom a) }

-- | Build the data-read result latches: the per-(instruction, read) value each
-- body consumed, sequenced onto the counter's cycles.  Data reads follow the code
-- operand reads, so data read @j@ of an instruction is on cycle @nCode + j@.  Runs
-- on the OVERRIDDEN results (their bus values are the alias-decoded reads), after
-- the counter is already built.
buildDataLatches
    :: forall s m dom. (Hdl s m, Signal s, KnownDom dom)
    => Int                                  -- ^ data-read width (wordW)
    -> ExecCounter s dom
    -> [SynthResult s dom]
    -> m (SeqReadRes s dom)
buildDataLatches dataW ec results =
    reflectNat dataW $ \(Proxy :: Proxy wW) -> do
      let dataBusOf :: Int -> ReadTok -> s dom (Unsigned wW)
          dataBusOf i tok =
              case [ sigRetype bus | MemReadReq _ t _ bus <- srMemReads (results !! i), t == tokIndex tok ] of
                  (b : _) -> b
                  []      -> litS 0 dataW
          -- The second tuple element is the instruction's LAST sequenced cycle + 1
          -- (so @latchReads@' @cyc == nAcc-1@ test means "this read is on the final
          -- cycle"): it MUST include @fetchOffset@, because @cyc@ does.  Omitting it
          -- mis-classifies a non-final read (e.g. POP's stack read, consumed one
          -- cycle later by its aliased write) as final, so it goes UNLATCHED and the
          -- external bus is re-sampled on the write cycle — when its address is no
          -- longer driven.  (An aliased read survives regardless, being combinational.)
          dataReads = [ (i, fetchOffset r + seqNAcc r, fetchOffset r + length (srCodeReads r) + j, t, match, sigRetype bus :: s dom (Unsigned wW))
                      | (i, r) <- zip [0 ..] results
                      , (j, MemReadReq match t _ bus) <- zip [0 ..] (srMemReads r) ]
      dLatched <- latchReads dataW (ecExecCyc ec) 16 (ecNotWait ec) (ecGate ec) dataReads
      let readRes :: forall a. HdlType a => Int -> ReadTok -> s dom a
          readRes i tok = sigRetype (Map.findWithDefault (dataBusOf i tok) (i, tokIndex tok) dLatched)
      pure (SeqReadRes readRes)
  where
    tokIndex (ReadTok t) = t

-- | Holding latches for sequenced reads whose value outlives their cycle (all but
-- the final access).  Keyed @(instrIdx, token)@.
latchReads :: forall s m dom w cy. (Hdl s m, Signal s, KnownDom dom, HdlType w, HdlType cy)
           => Int -> s dom cy -> Int -> s dom Bool
           -> (Int -> s dom Bool -> Int -> s dom Bool)
           -> [(Int, Int, Int, Int, s dom Bool, s dom w)]   -- (i, nAcc, cycle, tok, match, bus)
           -> m (Map (Int, Int) (s dom w))
latchReads width execCyc cwBits notWait gate reads =
    fmap Map.fromList $ mapM latch1 [ r | r@(_, nAcc, cyc, _, _, _) <- reads, cyc /= nAcc - 1 ]
  where
    latch1 (i, nAcc, cyc, tok, match, bus) = do
        let capEn = andS (gate nAcc match cyc) notWait
        l <- registerW width 0 capEn bus
        let onCyc = eqS execCyc (litS (fromIntegral cyc) cwBits :: s dom cy)
        pure ((i, tok), muxS onCyc bus l)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | The PC register name, from 'isaPc' (reads only the ALU record).
extractPcName :: alu -> ISADef (ISABuild alu wordW addrW codeWordW codeAddrW) -> String
extractPcName aluRec isaDef =
    let SomeCPURegister (CPURegister n _ _) = evalISABuild aluRec (isaPc isaDef) in n

-- | Number of bits needed to address @count@ entries.
addrBitsFor :: Int -> Int
addrBitsFor n
    | n <= 1    = 1
    | otherwise = ceiling (logBase 2 (fromIntegral n) :: Double)

