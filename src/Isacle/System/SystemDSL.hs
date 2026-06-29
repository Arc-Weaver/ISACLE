-- | System-level DSL for the isacle-hdl backend.
--
-- Provides an API close to the ExampleAPI target:
--
-- @
-- mySystem (uart0Rx, gpio0In) = runSystemDSL $ do
--     uart0 <- createUart "uart0" uart0Rx
--     gpio0 <- createGpio "gpio0" gpio0In
--
--     bh <- createHarvardCPU \@16 \@16 \@16 "cpu" myCPUDef myISA romWords
--     (uart0Tx, gpio0Port, gpio0Ddr) <- createBus "databus" bh $ do
--         uart0' <- attachPeripheral 0x100 uart0
--         gpio0' <- attachPeripheral 0x300 gpio0
--         return (uartTxLine uart0', gpioPort gpio0', gpioDdr gpio0')
--
--     return (uart0Tx, gpio0Port, gpio0Ddr)
-- @
--
-- 'attachPeripheral' is generic: the output type is determined by the token
-- type, so GHC infers the right thing without any cast.
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Isacle.System.SystemDSL
    ( -- * System-level monad
      SysDSL
    , runSystemDSL
      -- * Peripheral tokens (opaque; carry name + PeriphDef)
    , PeriphToken
      -- * Bus sub-monad
    , BusDSL
    , BusHandle(..)    -- re-exported from Isacle.System.BusHandle
    , attachPeripheral
      -- * System-level operations
    , createBus
    , orphanBusMaster
    , createSimpleVectorIrq
    , createHarvardCPU
    , createL1Cache
    , createCachedCPU
      -- * Design runner
    , execSystemDSL
      -- * Peripheral constructors
    , createUart
    , createGpio
    , createTimer
    , createRamp
    , createRam
    , createRom
      -- * Utilities
    , sigFalse
    , sigTrue
    , sysOutput
      -- * System documentation
    , SysDoc(..)
    , BusSection(..)
    , PeriphEntry(..)
    ) where

import Prelude
import Data.Word (Word32)
import Control.Monad (forM, forM_, replicateM, zipWithM_)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class (lift)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (natVal, KnownNat, type (<=))

import Hdl.Net
import Hdl.Types
import Hdl.Prim (Unsigned)
import Hdl.Class (outputS)
import Isacle.System.Periph
import Isacle.System.BusHandle (BusHandle(..))
import Isacle.System.BusArch (BusArch(..), SimpleBus(..), BusPort(..))
import Isacle.System.HdlCircuit
    ( hdlOps, hdlBusIface, busPortIface, HdlPhys(..)
    , GpioPhys(..), UartPhys(..), TimerPhys(..)
    )
import Isacle.Periph.GPIO  (gpioDef, GPIO)
import Isacle.Periph.UART  (uartDefWithFSM, UART)
import Isacle.Periph.Timer (timerDefWithFSM, Timer)
import Isacle.Periph.Ramp  (rampDefWithFSM, Ramp)
import Isacle.ISA.CPUDef (CPUDef)
import Isacle.ISA.Def    (ISADef, isaInterruptBody)
import Isacle.ISA.Build (ISABuild)
import Isacle.ISA.Backend.SynthCPU (synthHarvardCPU', CpuMemIface(..))
import Isacle.ISA.Backend.SynthVnCPU (synthVonNeumannCPU', VnMemIface(..))
import Isacle.Cache.Config (CacheConfig)
import Isacle.Cache.L1 (CacheHandle(..), synthL1Cache)

-- ---------------------------------------------------------------------------
-- System documentation types
-- ---------------------------------------------------------------------------

-- | One peripheral in a bus section.
data PeriphEntry = PeriphEntry
    { peName :: String
    , peBase :: Word32
    , peSpec :: PeriphSpec
    } deriving (Show)

-- | A named memory bus and its attached peripherals.
data BusSection = BusSection
    { bsName    :: String
    , bsEntries :: [PeriphEntry]
    } deriving (Show)

-- | Top-level system documentation produced by 'runSystemDSL'.
data SysDoc = SysDoc
    { sdBuses :: [BusSection]
    , sdCPUs  :: [String]
    } deriving (Show)

emptySysDoc :: SysDoc
emptySysDoc = SysDoc [] []

-- ---------------------------------------------------------------------------
-- PeriphToken
-- ---------------------------------------------------------------------------

-- | Opaque handle produced by 'createUart' / 'createGpio' / 'createTimer'.
--
-- @a@ is the concrete physical-output type specific to the peripheral:
--
--   * UART  → @(Sig dom Bool, Sig dom Bool, Sig dom Bool)@ (TX, rxIrq, txIrq)
--   * GPIO  → @(Sig dom dat, Sig dom dat)@                 (PORT, DDR)
--   * Timer → @(Sig dom Bool, Sig dom Bool)@               (ovf, cmp)
--
-- 'attachPeripheral' is generic over @a@, so the right output type is
-- inferred from the token without any explicit annotation.
--
-- 'ptAddrSize' is the peripheral's address window in bytes.  For
-- register-mapped peripherals set it to 0 and the window is derived
-- automatically from 'specSize'.  Memory peripherals (RAM, ROM) must
-- provide an explicit size.
data PeriphToken p dom dat a = PeriphToken
    { ptName     :: String
    , ptDef      :: PeriphDef p (Sig dom) dat a
    , ptAddrSize :: Word32
    }

-- ---------------------------------------------------------------------------
-- SysDSL monad
-- ---------------------------------------------------------------------------

-- | System-level monad.  Wraps 'NetM' with accumulated 'SysDoc'.
newtype SysDSL dom dat a = SysDSL (StateT SysDoc NetM a)
    deriving newtype (Functor, Applicative, Monad)

-- | Run a system description, returning the user result, the flat 'NetNode'
-- list (for VHDL emission), and the system documentation.
runSystemDSL :: SysDSL dom dat a -> (a, [NetNode], SysDoc)
runSystemDSL (SysDSL st) = (a, nodes, doc)
  where
    ((a, doc), nodes, _design) = runNetM (runStateT st emptySysDoc)

-- ---------------------------------------------------------------------------
-- BusDSL monad
-- ---------------------------------------------------------------------------

-- | Internal record for one peripheral slot inside a bus.
-- The 'psRun' closure captures everything needed to instantiate the peripheral
-- entity; it receives the bus-entity output wire for the gated write enable.
data PeriphSlot dom dat = PeriphSlot
    { psName      :: String
    , psBase      :: Word32
    , psSize      :: Word32      -- address window in bytes
    , psRdData    :: WireId      -- pre-allocated system-level wire; peripheral drives this
    , psPhysWires :: [WireId]    -- pre-allocated system-level wires for physical outputs
    , psPhysMeta  :: [PortSpec]  -- metadata (name, width, dom) for each physical output
    , psSpec      :: PeriphSpec
    , psRun       :: (WireId, WireId, WireId, WireId) -> NetM ()
      -- ^ Instantiates the peripheral sub-entity at the system level.
      -- Argument: this child's @(req, we, addr, wdata)@ wires, driven by the
      -- bus interconnect ('synthBus').  The peripheral presents a
      -- protocol-agnostic slave 'BusPort' — it carries no bus protocol.
      -- Side-effect: aliases psRdData ← peripheral rd_data output,
      --              aliases psPhysWires[i] ← peripheral physical output i.
    }

data BusDSLState dom dat = BusDSLState
    { bdsPeriph  :: [PeriphEntry]
    , bdsSlots   :: [PeriphSlot dom dat]
    }

-- | Bus sub-monad; execute 'attachPeripheral' calls inside 'createBus'.
newtype BusDSL dom dat a = BusDSL (StateT (BusDSLState dom dat) NetM a)
    deriving newtype (Functor, Applicative, Monad)

-- | Attach a peripheral at @base@.
--
-- Synthesises the peripheral register file as a named sub-entity (its own
-- VHDL file), wires it to the enclosing bus, accumulates its read-data into
-- the bus mux, records it in the bus map, and returns the peripheral's
-- physical outputs.  The output type @a@ is determined by the token:
--
-- @
-- (tx, rxIrq, txIrq) <- attachPeripheral 0x100 uart0   -- UART token
-- (port, ddr)         <- attachPeripheral 0x300 gpio0   -- GPIO token
-- @
attachPeripheral
    :: forall p dom dat a.
       (KnownDom dom, HdlType dat, Num dat, Num (Sig dom dat), HdlPhys a, HdlPorts a)
    => Word32
    -> PeriphToken p dom dat a
    -> BusDSL dom dat a
attachPeripheral base token = BusDSL $ do
    st <- get
    -- Spec-only pass (no NetM nodes emitted).
    let specBus = hdlBusIface (SWire 0 :: Sig dom (Unsigned 32))
                               (SWire 0 :: Sig dom dat)
                               (SWire 0 :: Sig dom Bool)
                               (SWire 0 :: Sig dom (Unsigned 32)) base
        (_, _, spec) = runPeriphDef hdlOps specBus (ptDef token)
        entry = PeriphEntry { peName = ptName token, peBase = base, peSpec = spec }
        size  = case specSize spec of { 0 -> ptAddrSize token; n -> n }
        nm    = ptName token

    -- Pre-allocate system-level wires for this peripheral's rd_data and physical
    -- outputs.  These wires are driven later (via alias) when the peripheral
    -- sub-entity is instantiated at the system level by createBus.
    let n = physOutCount (Proxy @a)
    (rdDataW, physWires) <- lift $ do
        rd   <- freshWire
        phys <- replicateM n freshWire
        pure (rd, phys)

    -- Closure run by createBus at the system level after the bus interconnect
    -- has been built.  Receives this child's protocol-agnostic slave-port
    -- request wires (req, we, addr, wdata), driven by 'synthBus'.  The
    -- peripheral entity carries no bus protocol — it only sees register-level
    -- reads/writes via 'busPortIface'.
    let runPeriph (reqW, weW, addrW, wdataW) = do
            let parentIns = [reqW, weW, addrW, wdataW]
            (_, outPorts) <- inBlock nm nm parentIns $ do
                reqIn  <- freshWire; emit $ NInput reqIn  "req"   1        domInfo
                weIn   <- freshWire; emit $ NInput weIn   "we"    1        domInfo
                addrIn <- freshWire; emit $ NInput addrIn "addr"  busAddrW domInfo
                wdIn   <- freshWire; emit $ NInput wdIn   "wdata" datW     domInfo
                let bus = busPortIface (SWire reqIn) (SWire weIn)
                                       (SWire addrIn) (SWire wdIn) base
                let (phys, rd, _) = runPeriphDef hdlOps bus (ptDef token)
                rdWid <- materialize rd
                emit $ NOutput rdWid "rd_data" datW domInfo
                emitPhysOuts domInfo datW phys
            case outPorts of
                [] -> error $ "attachPeripheral: " ++ nm ++ " returned no ports"
                (_, rdActW, _) : physPorts -> do
                    alias rdDataW rdActW
                    zipWithM_ (\preW (_, actW, _) -> alias preW actW) physWires physPorts

    let slot = PeriphSlot
            { psName      = nm
            , psBase      = base
            , psSize      = size
            , psRdData    = rdDataW
            , psPhysWires = physWires
            , psPhysMeta  = portSpecs (Proxy @a)
            , psSpec      = spec
            , psRun       = runPeriph
            }
    put st { bdsPeriph = bdsPeriph st ++ [entry]
           , bdsSlots  = bdsSlots  st ++ [slot]
           }
    pure (fromPhysWires physWires :: a)

  where
    domInfo  = domId   (Proxy @dom)
    datW     = fromIntegral (natVal (Proxy @(Width dat)))
    busAddrW = 32 :: Int

-- ---------------------------------------------------------------------------
-- createBus
-- ---------------------------------------------------------------------------

-- | Build a named bus interconnect, driven by the master wires in @bh@.
--
-- Wire the 'BusDSL' peripherals to the bus and connect the aggregated
-- read-data and stall signals back into the 'BusHandle'.  Obtain @bh@ from
-- 'createHarvardCPU' (or 'orphanBusMaster' for test-only systems).
--
-- @
-- bh <- createHarvardCPU \@16 \@16 \@16 "cpu" myCPUDef myISA romWords
-- (tx, port) <- createBus "databus" bh $ do
--     tx'   <- attachPeripheral 0x100 uartTok
--     port' <- attachPeripheral 0x300 gpioTok
--     return (tx', port')
-- @
createBus
    :: forall dom dat a.
       (KnownDom dom, HdlType dat, Num dat, Num (Sig dom dat))
    => String
    -> BusHandle 32 (Width dat)
    -> BusDSL dom dat a
    -> SysDSL dom dat a
createBus busName bh (BusDSL busSt) = SysDSL $ do
    let wrAddr = bhWrAddr bh
        wrData = bhWrData bh
        wrEn   = bhWrEn   bh
        rdAddr = bhRdAddr bh

    -- Run the BusDSL: attachPeripheral pre-allocates rd_data/phys wires and
    -- builds psRun closures; no peripheral IR is emitted yet.
    let initSt = BusDSLState { bdsPeriph = [], bdsSlots = [] }
    (userA, finalSt) <- lift $ runStateT busSt initSt
    let slots  = bdsSlots  finalSt
        periph = bdsPeriph finalSt

    -- Build the interconnect as its own sub-entity, so the bus structure is
    -- preserved (each bus is its own decoder; attaching a bus to a bus nests
    -- decoders).  The protocol — address decode, read mux, stall — comes from
    -- the BusArch instance via 'synthBus', driven by the per-child base/size
    -- layout.  The entity exposes, per child, a protocol-agnostic slave port
    -- (req/we/addr/wdata) and the aggregated rd_data.
    let busParentIns = [wrAddr, wrData, wrEn, rdAddr] ++ map psRdData slots
    (childPortParWires, rdParentWire) <- lift $ do
        ((), outPorts) <- inBlock busName busName busParentIns $ do
            waIn <- freshWire; emit $ NInput waIn "wr_addr" addrW domInfo
            wdIn <- freshWire; emit $ NInput wdIn "wr_data" datW  domInfo
            weIn <- freshWire; emit $ NInput weIn "wr_en"   1     domInfo
            raIn <- freshWire; emit $ NInput raIn "rd_addr" addrW domInfo
            rdIns <- forM slots $ \slot -> do
                w <- freshWire
                emit $ NInput w (psName slot ++ "_rd_data") datW domInfo
                pure w
            -- Upstream master port: one transaction/cycle.  A combinational
            -- SimpleBus is always requesting; the address is the write address
            -- on writes, the read address otherwise.
            reqU   <- litOne
            addrU  <- do { o <- freshWire; emit $ NComb o PMux [weIn, waIn, raIn]; pure o }
            rdataU <- freshWire
            stallU <- freshWire
            let up = BusPort { bpReq = reqU, bpWe = weIn, bpAddr = addrU
                             , bpWData = wdIn, bpRData = rdataU, bpStall = stallU
                             , bpAddrW = addrW, bpDataW = datW }
            -- One downstream child port per peripheral slot.
            children <- forM (zip slots rdIns) $ \(slot, rdIn) -> do
                reqC   <- freshWire; weC    <- freshWire
                addrC  <- freshWire; wdataC <- freshWire
                stallC <- litZero1
                let child = BusPort { bpReq = reqC, bpWe = weC, bpAddr = addrC
                                    , bpWData = wdataC, bpRData = rdIn, bpStall = stallC
                                    , bpAddrW = addrW, bpDataW = datW }
                pure ( fromIntegral (psBase slot)
                     , fromIntegral (psBase slot + psSize slot) - fromIntegral (psBase slot)
                     , child )
            -- Protocol interconnect: drives child req/we/addr/wdata and the
            -- upstream rdata/stall.
            synthBus SimpleBus domInfo up children
            -- Expose each child's slave port and the aggregated read data.
            forM_ (zip slots children) $ \(slot, (_, _, child)) -> do
                emit $ NOutput (bpReq   child) (psName slot ++ "_req")   1     domInfo
                emit $ NOutput (bpWe    child) (psName slot ++ "_we")    1     domInfo
                emit $ NOutput (bpAddr  child) (psName slot ++ "_addr")  addrW domInfo
                emit $ NOutput (bpWData child) (psName slot ++ "_wdata") datW  domInfo
            emit $ NOutput rdataU "rd_data" datW domInfo

        -- Emission order: 4 ports per child (req,we,addr,wdata), then rd_data.
        case reverse outPorts of
            ((_, rdW, _) : revFront) ->
                pure (chunk4 (map (\(_, w, _) -> w) (reverse revFront)), rdW)
            [] -> error "createBus: bus inBlock returned no output ports"

    -- Instantiate peripheral entities (siblings of the bus), each wired to its
    -- child port, then promote their physical outputs to top-level ports.
    lift $ zipWithM_ psRun slots childPortParWires
    lift $ mapM_ promotePhysOuts slots

    -- SimpleBus never stalls its master: drive the handle's stall wire low.
    busStallW <- lift $ do
        w <- freshWire
        emit $ NComb w (PLit 0 1) []
        hintWire w (busName ++ "_stall")
        pure w

    -- Feed the aggregated read data and stall back into the master handle.
    lift $ alias (bhRdData bh) rdParentWire
    lift $ alias (bhStall  bh) busStallW
    modify $ \doc -> doc { sdBuses = sdBuses doc ++ [BusSection busName periph] }
    pure userA
  where
    domInfo = domId (Proxy @dom)
    datW    = fromIntegral (natVal (Proxy @(Width dat)))
    addrW   = 32 :: Int
    litOne :: NetM WireId
    litOne   = do { o <- freshWire; emit $ NComb o (PLit 1 1) []; pure o }
    litZero1 :: NetM WireId
    litZero1 = do { o <- freshWire; emit $ NComb o (PLit 0 1) []; pure o }
    chunk4 :: [WireId] -> [(WireId, WireId, WireId, WireId)]
    chunk4 (a:b:c:d:rest) = (a, b, c, d) : chunk4 rest
    chunk4 _              = []
    promotePhysOuts slot =
        zipWithM_ (\wid ps ->
            emit $ NOutput wid (psName slot ++ "_" ++ portName ps) (portWidth ps) (portDom ps)
        ) (psPhysWires slot) (psPhysMeta slot)

-- | Allocate a 'BusHandle' with fresh, undriven master wires.
--
-- Use in test-only systems and spec passes that have no CPU:
--
-- @
-- bh <- orphanBusMaster \@32 \@8
-- _ <- createBus "databus" bh $ attachPeripheral 0x60 gpio
-- @
orphanBusMaster
    :: forall addrW dataW dom dat.
       (KnownNat addrW, KnownNat dataW)
    => SysDSL dom dat (BusHandle addrW dataW)
orphanBusMaster = SysDSL $ lift $ do
    wa <- freshWire; wd <- freshWire; we <- freshWire; ra <- freshWire
    rd <- freshWire; st <- freshWire
    pure BusHandle
        { bhWrAddr = wa, bhWrData = wd, bhWrEn = we, bhRdAddr = ra
        , bhRdData = rd, bhStall  = st
        , bhAddrW  = fromIntegral (natVal (Proxy @addrW))
        , bhDataW  = fromIntegral (natVal (Proxy @dataW))
        }

-- ---------------------------------------------------------------------------
-- IRQ controller
-- ---------------------------------------------------------------------------

-- | Build a simple priority-encoder IRQ vector signal.
-- Sources are in priority order (head = highest priority).
createSimpleVectorIrq
    :: [(Sig dom Bool, Word32)]
    -> SysDSL dom dat (Sig dom (Maybe (Unsigned 32)))
createSimpleVectorIrq _sources = pure $
    -- Stub: permanent Nothing (no interrupt).  Full implementation
    -- requires an IRQ combiner expressed as a NetNode sub-circuit.
    SExpr $ do
        out <- freshWire
        emit $ NComb out (PLit 0 33) []
        pure out

-- ---------------------------------------------------------------------------
-- CPU
-- ---------------------------------------------------------------------------

-- | Synthesise a Harvard CPU into the system, connect it to a data bus, and
-- instantiate an internal code ROM.
--
-- * @addrW@, @codeWordW@, @codeAddrW@ — type-level widths via TypeApplications.
-- * @dataBus@   — handle returned by 'createBus'; the CPU's load/store
--   signals are wired directly to the bus master wires.
-- * @romWords@  — initial instruction words for the code ROM (padded with 0).
--
-- The CPU address width (@addrW@) may differ from the bus address width
-- (always 32-bit); a zero-extend resize is inserted automatically.
createHarvardCPU :: forall addrW codeWordW codeAddrW dom dat alu.
              ( KnownDom dom
              , KnownNat addrW, KnownNat codeWordW, KnownNat codeAddrW
              , addrW <= 32
              , HdlType dat
              )
           => String
           -> CPUDef alu
           -> ISADef (ISABuild alu (Width dat) addrW codeWordW codeAddrW)
           -> [Integer]
           -> SysDSL dom dat (BusHandle 32 (Width dat))
createHarvardCPU instName cpuDef isaDef romWords = SysDSL $ do
    let codeWordB    = fromIntegral (natVal (Proxy @codeWordW))  :: Int
    let wordBits     = fromIntegral (natVal (Proxy @(Width dat))) :: Int
    let addrBits     = fromIntegral (natVal (Proxy @addrW))       :: Int
    let codeAddrBits = fromIntegral (natVal (Proxy @codeAddrW))   :: Int
    let domInfo      = domId (Proxy @dom)

    -- Pre-allocate parent-side wires that map to the CPU's input ports.
    -- These are driven in the parent context after the sub-entity is built.
    instrWordParW  <- lift freshWire
    dmemRdDataParW <- lift freshWire
    cmemRdDataParW <- lift freshWire
    stallParW      <- lift freshWire

    -- The CPU exposes irq_pending/irq_vector input ports iff the ISA declares an
    -- interrupt body.  With no interrupt controller wired up, tie them off (0).
    let hasIrq = maybe False (const True) (isaInterruptBody isaDef)
    irqPendParW <- lift freshWire
    irqVecParW  <- lift freshWire
    let irqParWs = if hasIrq then [irqPendParW, irqVecParW] else []

    -- Synthesise the CPU inside a named sub-entity so it appears as a
    -- distinct entity in the VHDL hierarchy and is independently testable.
    ((), cpuPorts) <- lift $ inBlock instName instName
        ([instrWordParW, dmemRdDataParW, cmemRdDataParW, stallParW] ++ irqParWs) $ do
            -- Input ports (order must match the parent wire list above).
            instrWordW  <- freshWire
            dmemRdDataW <- freshWire
            cmemRdDataW <- freshWire
            stallW      <- freshWire
            emit $ NInput instrWordW  "instr_word"   codeWordB domInfo
            emit $ NInput dmemRdDataW "data_rd_data" wordBits  domInfo
            emit $ NInput cmemRdDataW "code_rd_data" codeWordB domInfo
            emit $ NInput stallW      "stall"        1         domInfo

            cmi <- synthHarvardCPU'
                       @dom @(Width dat) @addrW @codeWordW @codeAddrW
                       cpuDef isaDef instrWordW dmemRdDataW cmemRdDataW stallW

            hintWire (cmiCodeRdAddr cmi) "code_addr"
            hintWire (cmiDataRdAddr cmi) "data_rd_addr"
            hintWire (cmiDataWrEn   cmi) "data_wr_en"
            hintWire (cmiDataWrAddr cmi) "data_wr_addr"
            hintWire (cmiDataWrData cmi) "data_wr_data"

            -- Output ports — order determines index in cpuPorts list.
            emit $ NOutput (cmiCodeRdAddr cmi) "code_addr"    codeAddrBits domInfo
            emit $ NOutput (cmiDataRdAddr cmi) "data_rd_addr" addrBits     domInfo
            emit $ NOutput (cmiDataWrEn   cmi) "data_wr_en"  1             domInfo
            emit $ NOutput (cmiDataWrAddr cmi) "data_wr_addr" addrBits     domInfo
            emit $ NOutput (cmiDataWrData cmi) "data_wr_data" wordBits     domInfo

    -- Resolve parent wires for each CPU output port by name.
    let findPort name = case [ w | (n, w, _) <- cpuPorts, n == name ] of
            (w:_) -> w
            []    -> error ("createHarvardCPU: missing output port " ++ name)
        codeAddrParW   = findPort "code_addr"
        dataRdAddrParW = findPort "data_rd_addr"
        dataWrEnParW   = findPort "data_wr_en"
        dataWrAddrParW = findPort "data_wr_addr"
        dataWrDataParW = findPort "data_wr_data"

    -- Code ROMs live in the parent context, addressed by the CPU's PC output.
    let romCapacity = 2 ^ codeAddrBits
        romData     = take romCapacity (romWords ++ repeat 0)
    lift $ emit $ NRom instrWordParW codeAddrParW romCapacity codeWordB romData

    -- Second ROM port at PC+1 for 2-word instructions.
    lift $ do
        lit1W    <- freshWire
        pcPlus1W <- freshWire
        emit $ NComb lit1W    (PLit 1 codeAddrBits) []
        emit $ NComb pcPlus1W PAdd [codeAddrParW, lit1W]
        emit $ NRom cmemRdDataParW pcPlus1W romCapacity codeWordB romData

    -- Allocate wires the bus will drive back into the CPU (filled by createBus).
    rdDataFromBus <- lift freshWire
    stallFromBus  <- lift freshWire

    -- Resize CPU address outputs to 32-bit bus width and build the BusHandle.
    (wrAddrR, rdAddrR) <- lift $ do
        wr <- resizeTo addrBits 32 dataWrAddrParW
        rd <- resizeTo addrBits 32 dataRdAddrParW
        -- Bus response wires → CPU input wires.
        alias dmemRdDataParW rdDataFromBus
        alias stallParW      stallFromBus
        -- Tie off interrupt inputs (no IRQ controller wired yet).
        if hasIrq
            then do
                z1 <- do { w <- freshWire; emit $ NComb w (PLit 0 1) []; pure w }
                zv <- do { w <- freshWire; emit $ NComb w (PLit 0 codeAddrBits) []; pure w }
                alias irqPendParW z1
                alias irqVecParW  zv
            else pure ()
        pure (wr, rd)

    let dataBus = BusHandle
                    { bhWrAddr = wrAddrR
                    , bhWrData = dataWrDataParW
                    , bhWrEn   = dataWrEnParW
                    , bhRdAddr = rdAddrR
                    , bhRdData = rdDataFromBus
                    , bhStall  = stallFromBus
                    , bhAddrW  = 32
                    , bhDataW  = wordBits
                    }
    modify $ \doc -> doc { sdCPUs = sdCPUs doc ++ [instName] }
    pure dataBus

-- | Synthesise an L1 cache that bridges the CPU two-port interface to the
-- system bus.
--
-- Returns a 'CacheHandle' that carries the pre-allocated CPU-facing wire IDs.
-- Pass the handle to 'createCachedCPU' to attach the CPU.
--
-- The cache acts as the sole master of @dataBus@: it forwards CPU addresses
-- to the bus and returns bus read data to the CPU.  In the current stub
-- implementation, stall is always 0 (pass-through, no actual caching).
createL1Cache
    :: forall dom wordW addrW busAddrW busDataW dat.
       ( KnownDom dom, KnownNat wordW, KnownNat addrW, KnownNat busAddrW, HdlType dat )
    => CacheConfig
    -> BusHandle busAddrW busDataW
    -> SysDSL dom dat CacheHandle
createL1Cache cfg busH = SysDSL $ lift $
    synthL1Cache @dom @wordW @addrW cfg busH

-- | Synthesise a cached von Neumann CPU and connect it to a 'CacheHandle'.
--
-- The CPU's instruction fetch and data access both go through the L1 cache.
-- ROM, RAM, and peripherals all reside in the same flat address space on the
-- system bus; the CPU does not address them directly.
--
-- For a non-cached design (e.g. tightly-coupled SRAM), call 'createL1Cache'
-- with the stub implementation — the stall wire is permanently 0 and the
-- cache is a transparent pass-through.
createCachedCPU
    :: forall addrW dom dat alu.
       ( KnownDom dom, KnownNat addrW, HdlType dat )
    => String
    -> CPUDef alu
    -> ISADef (ISABuild alu (Width dat) addrW (Width dat) addrW)
    -> CacheHandle
    -> SysDSL dom dat ()
createCachedCPU instName cpuDef isaDef cacheH = SysDSL $ do
    let wordBits  = fromIntegral (natVal (Proxy @(Width dat))) :: Int
    let addrBits  = fromIntegral (natVal (Proxy @addrW))       :: Int
    let domInfo   = domId (Proxy @dom)
    let vmi       = chVnIface cacheH

    -- Synthesise the CPU inside a named sub-entity.
    ((), cpuPorts) <- lift $ inBlock instName instName
        [ vniInstrWord  vmi
        , vniDataRdData vmi
        , vniStall      vmi
        ] $ do
            instrW  <- freshWire
            rdDataW <- freshWire
            stallW  <- freshWire
            emit $ NInput instrW  "instr_word"   wordBits domInfo
            emit $ NInput rdDataW "data_rd_data" wordBits domInfo
            emit $ NInput stallW  "stall"        1        domInfo

            vnm <- synthVonNeumannCPU'
                       @dom @(Width dat) @addrW
                       cpuDef isaDef instrW rdDataW stallW

            emit $ NOutput (vniFetchAddr  vnm) "fetch_addr"    addrBits domInfo
            emit $ NOutput (vniDataRdAddr vnm) "data_rd_addr"  addrBits domInfo
            emit $ NOutput (vniDataWrEn   vnm) "data_wr_en"    1        domInfo
            emit $ NOutput (vniDataWrAddr vnm) "data_wr_addr"  addrBits domInfo
            emit $ NOutput (vniDataWrData vnm) "data_wr_data"  wordBits domInfo

    let findPort nm = case [ w | (n, w, _) <- cpuPorts, n == nm ] of
            (w:_) -> w
            []    -> error ("createCachedCPU: missing port " ++ nm)

    -- Wire CPU outputs → cache inputs.
    lift $ do
        alias (vniFetchAddr  vmi) (findPort "fetch_addr")
        alias (vniDataRdAddr vmi) (findPort "data_rd_addr")
        alias (vniDataWrEn   vmi) (findPort "data_wr_en")
        alias (vniDataWrAddr vmi) (findPort "data_wr_addr")
        alias (vniDataWrData vmi) (findPort "data_wr_data")

    modify $ \doc -> doc { sdCPUs = sdCPUs doc ++ [instName] }

-- | Emit a zero-extending resize node (identity when widths match).
resizeTo :: Int -> Int -> WireId -> NetM WireId
resizeTo srcW dstW src
    | srcW == dstW = return src
    | otherwise    = do { w <- freshWire; emit $ NComb w (PResize dstW) [src]; return w }

-- | Drive @dst@ from @src@ via a single-input OR (identity in synthesis).
alias :: WireId -> WireId -> NetM ()
alias dst src = emit $ NComb dst POr [src, src]

-- | Run a system description, returning the full 'Design' (top entity plus
-- any sub-entities such as RAM blocks) ready for 'emitVhdlDesignFiles'.
-- Apply @dom@ and @dat@ as visible type arguments to disambiguate:
-- @execSystemDSL \@Sys \@(Unsigned 8) "top" mySystem@
execSystemDSL :: forall dom dat a. String -> SysDSL dom dat a -> Design
execSystemDSL name (SysDSL st) =
    execDesign name (fmap fst (runStateT st emptySysDoc))

-- ---------------------------------------------------------------------------
-- Peripheral constructors
-- ---------------------------------------------------------------------------

-- | Create a UART peripheral token.
-- Register interface (UDR, USR, UBRR) is fully wired.
-- Physical outputs (TX, rxIrq, txIrq) are stubs until a serial-FSM
-- sub-component is implemented.
createUart
    :: (KnownDom dom, HdlType dat, Num dat, Num (Sig dom dat))
    => String
    -> Sig dom Bool                  -- ^ RX serial line
    -> SysDSL dom dat (PeriphToken UART dom dat (UartPhys dom))
createUart name rxPin = pure $ PeriphToken
    { ptName     = name
    , ptDef      = do
          (txLine, rxIrq, txIrq) <- uartDefWithFSM rxPin
          return UartPhys { uartTxLine = txLine, uartRxIrq = rxIrq, uartTxIrq = txIrq }
    , ptAddrSize = 0
    }

-- | Create a GPIO peripheral token.
-- Fully implemented: DDR, PORT, and PIN registers all synthesize correctly.
createGpio
    :: (Num dat, Num (Sig dom dat))
    => String
    -> Sig dom dat                   -- ^ input pin bus
    -> SysDSL dom dat (PeriphToken GPIO dom dat (GpioPhys dom dat))
createGpio name pins = pure $ PeriphToken
    { ptName     = name
    , ptDef      = gpioDef pins >>= \(port, ddr) ->
                       return GpioPhys { gpioPort = port, gpioDdr = ddr }
    , ptAddrSize = 0
    }

-- | Create a Timer peripheral token.
-- Physical outputs (overflow IRQ, compare-match IRQ) are stubs until a
-- counter-FSM sub-component is implemented.
createTimer
    :: (KnownDom dom, HdlType dat, Num dat, Num (Sig dom dat))
    => String
    -> Sig dom Bool                  -- ^ tick / count enable
    -> SysDSL dom dat (PeriphToken Timer dom dat (TimerPhys dom))
createTimer name tick = pure $ PeriphToken
    { ptName     = name
    , ptDef      = do
          (ovf, cmp) <- timerDefWithFSM tick
          return TimerPhys { timerOvfIrq = ovf, timerCmpIrq = cmp }
    , ptAddrSize = 0
    }

-- | Create a signed-ramp peripheral token (PLAN_TYPED_HDL #3 demonstrator).
-- The bus carries @Unsigned 8@ but the internal datapath is @Signed 8@; the
-- ramp moves its CURRENT value toward SETPOINT by STEP each tick.  Register
-- map: 0 SETPOINT (RW), 1 STEP (RW), 2 CURRENT (RO).
createRamp
    :: KnownDom dom
    => String
    -> Sig dom Bool                  -- ^ tick / advance enable
    -> SysDSL dom (Unsigned 8) (PeriphToken Ramp dom (Unsigned 8) ())
createRamp name tick = pure $ PeriphToken
    { ptName     = name
    , ptDef      = rampDefWithFSM tick
    , ptAddrSize = 0
    }

-- | Create a synchronous block RAM peripheral token.
-- Attach with @attachPeripheral base ram0@; the RAM occupies @size@ entries
-- starting at @base@.
createRam
    :: (Num dat, Num (Sig dom dat))
    => Int          -- ^ number of addressable entries
    -> [Integer]    -- ^ initial contents (padded to @size@ with 0)
    -> String       -- ^ instance name
    -> SysDSL dom dat (PeriphToken RAM dom dat ())
createRam size initVals name = pure $ PeriphToken
    { ptName     = name
    , ptDef      = blockRamDef size initVals
    , ptAddrSize = fromIntegral size
    }

-- | Create a read-only ROM peripheral token.
-- Attach with @attachPeripheral base rom0@; the ROM occupies @size@ entries
-- starting at @base@.
createRom
    :: (Num dat, Num (Sig dom dat))
    => Int
    -> [Integer]
    -> String
    -> SysDSL dom dat (PeriphToken ROM dom dat ())
createRom size initVals name = pure $ PeriphToken
    { ptName     = name
    , ptDef      = blockRomDef size initVals
    , ptAddrSize = fromIntegral size
    }

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Emit a top-level output port from within 'SysDSL'.
-- The signal type @a@ may differ from the bus data type @dat@
-- (e.g. expose a 'Bool' TX pin alongside 8-bit GPIO).
sysOutput :: forall a dom dat.
             (KnownDom dom, HdlType a)
          => String -> Sig dom a -> SysDSL dom dat ()
sysOutput name sig = SysDSL $ lift $ outputS @dom @a name sig

-- | Constant-false Bool signal (1-bit zero literal).
sigFalse :: Sig dom Bool
sigFalse = SExpr $ do
    out <- freshWire
    emit $ NComb out (PLit 0 1) []
    pure out

-- | Constant-true Bool signal (1-bit one literal).
sigTrue :: Sig dom Bool
sigTrue = SExpr $ do
    out <- freshWire
    emit $ NComb out (PLit 1 1) []
    pure out
