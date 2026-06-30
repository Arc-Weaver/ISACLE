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
import Data.Kind (Type)
import Data.Word (Word32)
import Control.Monad (forM, forM_, replicateM, zipWithM_)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class (lift)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (natVal, KnownNat, type (<=))

import Hdl.Net
import Hdl.Types
import Hdl.Prim (Unsigned)
import Hdl.Class (outputS, connectSig, freshSig)
import Isacle.System.Periph
import Isacle.System.BusHandle (BusHandle(..))
import Isacle.System.BusArch
    (BusArch(..), SimpleBus(..), MasterReq(..), SlaveResp(..), BusChild)
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

-- | Internal record for one peripheral slot inside a bus.  The peripheral logic
-- is built purely (typed 'Sig' signals) at 'attachPeripheral' time from the bus
-- request supplied via the knot ('bdsReqFeed'); 'psRun' only /materialises/ it.
data PeriphSlot dom dat = PeriphSlot
    { psName :: String
    , psBase :: Word32
    , psSize :: Word32      -- address window in bytes
    , psSpec :: PeriphSpec
    , psRun  :: NetM (SlaveResp Sig dom dat)
      -- ^ Materialise the peripheral at the system level: promote its physical
      -- outputs to top-level ports and return its slave response (read data +
      -- stall) for the bus read mux.  The peripheral's bus request was already
      -- captured (knot-tied) when the slot was built — 'psRun' takes no argument.
    }

data BusDSLState dom dat = BusDSLState
    { bdsPeriph  :: [PeriphEntry]
    , bdsSlots   :: [PeriphSlot dom dat]
    , bdsReqFeed :: [MasterReq Sig dom (Unsigned 32) dat]
      -- ^ Per-slot bus requests, supplied lazily by 'createBus' via the knot;
      -- 'attachPeripheral' indexes this by slot position.
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
    -- This slot's bus request (req/we/addr/wdata), supplied by createBus's knot.
    let idx   = length (bdsSlots st)
        myReq = bdsReqFeed st !! idx
        nm    = ptName token
        -- Build the peripheral purely from its (knot-tied) bus request.  The
        -- protocol-agnostic slave port becomes a register-level 'BusIface' via
        -- 'busPortIface'; 'runPeriphDef' returns the typed physical outputs, the
        -- read-data signal, and the register spec — all as deferred 'Sig's.
        bus = busPortIface (mqReq myReq) (mqWe myReq)
                           (mqAddr myReq) (mqWData myReq) base
        (phys, rd, spec) = runPeriphDef hdlOps bus (ptDef token)
        size  = case specSize spec of { 0 -> ptAddrSize token; n -> n }
        entry = PeriphEntry { peName = nm, peBase = base, peSpec = spec }
        -- Top-level names for this peripheral's physical outputs:
        -- @<instance>_<port>@ (e.g. @gpio_GpioPhys_gpioPort@).
        physNames = [ nm ++ "_" ++ portName ps | ps <- portSpecs (Proxy @a) ]
        slot  = PeriphSlot
            { psName = nm
            , psBase = base
            , psSize = size
            , psSpec = spec
            , psRun  = do
                  -- Promote the physical outputs to top-level ports, then hand
                  -- the slave response back for the bus read mux.
                  emitPhysOuts physNames domInfo datW phys
                  pure SlaveResp { srRData = rd, srStall = sigFalse }
            }
    put st { bdsPeriph = bdsPeriph st ++ [entry]
           , bdsSlots  = bdsSlots  st ++ [slot]
           }
    pure phys

  where
    domInfo  = domId   (Proxy @dom)
    datW     = fromIntegral (natVal (Proxy @(Width dat)))

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
    -> BusHandle dom (Unsigned 32) dat
    -> BusDSL dom dat a
    -> SysDSL dom dat a
createBus busName bh (BusDSL busSt) = SysDSL $ do
    -- The master's single-channel request: a combinational SimpleBus is always
    -- requesting; the address is the write address on writes, else the read
    -- address.  These are typed 'Sig' signals straight off the 'BusHandle'.
    let master = MasterReq
            { mqReq   = sigTrue
            , mqWe    = bhWrEn bh
            , mqAddr  = mux (bhWrEn bh) (bhWrAddr bh) (bhRdAddr bh)
            , mqWData = bhWrData bh
            }

    -- Two passes, no feedback knot.  The request routing depends only on the
    -- master and the static base/size layout (never on the responses), and the
    -- layout doesn't depend on the request at all — so we discover the layout
    -- first, route the per-child requests purely, then build the peripherals.
    (userA, masterResp, periph) <- lift $ do
        -- Pass 1: layout discovery.  'attachPeripheral' has no netlist effects,
        -- so building peripherals with a neutral request feed and reading off
        -- their base/size is pure and side-effect-free.
        (_, layoutSt) <- runStateT busSt
            BusDSLState { bdsPeriph = [], bdsSlots = [], bdsReqFeed = repeat master }
        let layout    = [ (toInteger (psBase s), toInteger (psSize s))
                        | s <- bdsSlots layoutSt ]
            -- Route requests via the architecture (responses are never forced by
            -- request routing — 'synthBus's child request ignores the response).
            childReqs = snd $ synthBus SimpleBus master
                            [ (b, s, errResp) | (b, s) <- layout ]
            errResp   = error "createBus: response unused in request routing"
        -- Pass 2: real build with the routed requests.
        (userA, finalSt) <- runStateT busSt
            BusDSLState { bdsPeriph = [], bdsSlots = [], bdsReqFeed = childReqs }
        let slots = bdsSlots finalSt
        -- Materialise each peripheral (promotes phys outputs, yields its response).
        resps <- mapM psRun slots
        let children :: [BusChild Sig dom dat]
            children   = zipWith (\(b, s) r -> (b, s, r)) layout resps
            masterResp = fst $ synthBus SimpleBus master children
        pure (userA, masterResp, bdsPeriph finalSt)

    -- Feed the aggregated read data and stall back into the master handle.
    lift $ connectSig (bhRdData bh) (srRData masterResp)
    lift $ connectSig (bhStall  bh) (srStall masterResp)
    modify $ \doc -> doc { sdBuses = sdBuses doc ++ [BusSection busName periph] }
    pure userA

-- | Allocate a 'BusHandle' with fresh, undriven master wires.
--
-- Use in test-only systems and spec passes that have no CPU:
--
-- @
-- bh <- orphanBusMaster \@32 \@8
-- _ <- createBus "databus" bh $ attachPeripheral 0x60 gpio
-- @
orphanBusMaster
    :: forall dom dat. SysDSL dom dat (BusHandle dom (Unsigned 32) dat)
orphanBusMaster = SysDSL $ lift $ do
    wa <- freshSig; wd <- freshSig; we <- freshSig; ra <- freshSig
    rd <- freshSig; st <- freshSig
    pure BusHandle
        { bhWrAddr = wa, bhWrData = wd, bhWrEn = we, bhRdAddr = ra
        , bhRdData = rd, bhStall  = st
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
createHarvardCPU :: forall addrW codeWordW codeAddrW (dom :: Type) dat alu.
              ( KnownDom dom
              , KnownNat addrW, KnownNat codeWordW, KnownNat codeAddrW
              , addrW <= 32
              , HdlType dat
              )
           => String
           -> CPUDef alu
           -> ISADef (ISABuild alu (Width dat) addrW codeWordW codeAddrW)
           -> [Integer]
           -> SysDSL dom dat (BusHandle dom (Unsigned 32) dat)
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

    -- Build the typed master handle: the CPU's load/store outputs become the
    -- master→fabric signals (wrapped from their netlist wires); the bus drives
    -- the read-data/stall placeholders back (via 'connectSig' in 'createBus').
    let dataBus = BusHandle
                    { bhWrAddr = SWire wrAddrR        :: Sig dom (Unsigned 32)
                    , bhWrData = SWire dataWrDataParW :: Sig dom dat
                    , bhWrEn   = SWire dataWrEnParW   :: Sig dom Bool
                    , bhRdAddr = SWire rdAddrR        :: Sig dom (Unsigned 32)
                    , bhRdData = SWire rdDataFromBus  :: Sig dom dat
                    , bhStall  = SWire stallFromBus   :: Sig dom Bool
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
    :: forall dom wordW addrW dat.
       ( KnownDom dom, KnownNat wordW, KnownNat addrW, HdlType dat )
    => CacheConfig
    -> BusHandle dom (Unsigned 32) dat
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
