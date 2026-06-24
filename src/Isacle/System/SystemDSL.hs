-- | System-level DSL for the isacle-hdl backend.
--
-- Provides an API close to the ExampleAPI target:
--
-- @
-- mySystem (uart0Rx, gpio0In) = runSystemDSL $ do
--     uart0 <- createUart "uart0" uart0Rx
--     gpio0 <- createGpio "gpio0" gpio0In
--
--     ((uart0Tx, gpio0Port, gpio0Ddr), _rdData) <- createBus "databus" $ do
--         (tx, _rxIrq, _txIrq) <- attachPeripheral 0x100 uart0
--         (port, ddr)           <- attachPeripheral 0x300 gpio0
--         return (tx, port, ddr)
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
    , BusHandle(..)
    , attachPeripheral
      -- * System-level operations
    , createBus
    , createSimpleVectorIrq
    , createHarvardCPU
      -- * Design runner
    , execSystemDSL
      -- * Peripheral constructors
    , createUart
    , createGpio
    , createTimer
    , createRam
    , createRom
      -- * Utilities
    , sigFalse
    , sysOutput
      -- * System documentation
    , SysDoc(..)
    , BusSection(..)
    , PeriphEntry(..)
    ) where

import Prelude
import Data.Word (Word32)
import Control.Monad (forM, foldM, replicateM, zipWithM_)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class (lift)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (natVal, KnownNat)

import Hdl.Net
import Hdl.Types
import Hdl.Prim (Unsigned)
import Hdl.Class (outputS)
import Isacle.System.Periph
import Isacle.System.HdlCircuit
    ( hdlOps, hdlBusIface, HdlPhys(..)
    , GpioPhys(..), UartPhys(..), TimerPhys(..)
    )
import Isacle.Periph.GPIO  (gpioDef, GPIO)
import Isacle.Periph.UART  (uartDef, UART)
import Isacle.Periph.Timer (timerDef, Timer)
import Isacle.ISA.CPUDef (CPUDef)
import Isacle.ISA.Def    (ISADef)
import Isacle.ISA.Backend.Synth    (SynthM)
import Isacle.ISA.Backend.SynthCPU (synthHarvardCPU', CpuMemIface(..))

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

-- | Wire handles for one bus, returned by 'createBus'.
-- Pass to 'createHarvardCPU' to connect the CPU to this bus.
data BusHandle = BusHandle
    { bhWrAddr :: WireId  -- ^ driven by CPU data write address
    , bhWrData :: WireId  -- ^ driven by CPU data write data
    , bhWrEn   :: WireId  -- ^ driven by CPU data write enable
    , bhRdAddr :: WireId  -- ^ driven by CPU data read address
    , bhRdData :: WireId  -- ^ aggregated peripheral read data → CPU
    , bhAddrW  :: Int     -- ^ address width in bits (always 32)
    , bhDataW  :: Int     -- ^ data width in bits (= Width dat)
    }

-- | Internal record for one peripheral slot inside a bus.
-- The 'psRun' closure captures everything needed to instantiate the peripheral
-- entity; it receives the bus-entity output wire for the gated write enable.
data PeriphSlot dom dat = PeriphSlot
    { psName      :: String
    , psBase      :: Word32
    , psSize      :: Word32      -- address window in bytes
    , psRdData    :: WireId      -- pre-allocated system-level wire; peripheral drives this
    , psPhysWires :: [WireId]    -- pre-allocated system-level wires for physical outputs
    , psSpec      :: PeriphSpec
    , psRun       :: WireId -> NetM ()
      -- ^ Instantiates the peripheral sub-entity at the system level.
      -- Argument: the bus entity's gated-wr_en output wire for this peripheral.
      -- Side-effect: aliases psRdData ← peripheral rd_data output,
      --              aliases psPhysWires[i] ← peripheral physical output i.
    }

data BusDSLState dom dat = BusDSLState
    { bdsWrAddr  :: WireId        -- system-level wire driven by CPU
    , bdsWrData  :: WireId
    , bdsWrEn    :: WireId
    , bdsRdAddr  :: WireId
    , bdsPeriph  :: [PeriphEntry]
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
       (KnownDom dom, HdlType dat, Num dat, Num (Sig dom dat), HdlPhys a)
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

    -- Closure run by createBus at the system level after the bus entity is built.
    -- Receives the bus entity's gated-wr_en output wire for this peripheral.
    let wrAddrW = bdsWrAddr st
        wrDataW = bdsWrData st
        rdAddrW = bdsRdAddr st
        runPeriph weGatedW = do
            let parentIns = [wrAddrW, wrDataW, weGatedW, rdAddrW]
            (_, outPorts) <- inBlock nm nm parentIns $ do
                waIn <- freshWire; emit $ NInput waIn "wr_addr" busAddrW domInfo
                wdIn <- freshWire; emit $ NInput wdIn "wr_data" datW     domInfo
                weIn <- freshWire; emit $ NInput weIn "wr_en"   1        domInfo
                raIn <- freshWire; emit $ NInput raIn "rd_addr" busAddrW domInfo
                let bus = hdlBusIface (SWire waIn) (SWire wdIn) (SWire weIn) (SWire raIn) base
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

-- | Build a named bus.
--
-- Allocates four internal wires for the bus master interface (write address,
-- write data, write enable, read address) and threads them through the
-- 'BusDSL' sub-block.  Returns the user result and a 'BusHandle' carrying
-- the raw wire IDs.  Pass the handle to 'createHarvardCPU' to connect the
-- CPU to this bus.
--
-- @
-- ((tx, port), bh) <- createBus "databus" $ do
--     (tx, _, _) <- attachPeripheral 0x100 uartTok
--     (port, _)  <- attachPeripheral 0x300 gpioTok
--     return (tx, port)
-- createHarvardCPU ... bh romContents
-- @
createBus
    :: forall dom dat a.
       (KnownDom dom, HdlType dat, Num dat, Num (Sig dom dat))
    => String
    -> BusDSL dom dat a
    -> SysDSL dom dat (a, BusHandle)
createBus busName (BusDSL busSt) = SysDSL $ do
    -- Allocate system-level wires for the bus master interface.
    (wrAddr, wrData, wrEn, rdAddr) <- lift $ do
        wa <- freshWire; wd <- freshWire; we <- freshWire; ra <- freshWire
        pure (wa, wd, we, ra)

    -- Run the BusDSL at the system level (not inside a block).
    -- attachPeripheral pre-allocates peripheral rd_data/phys wires and builds
    -- psRun closures; no NetNode IR is emitted yet for peripheral entities.
    let initSt = BusDSLState
            { bdsWrAddr = wrAddr
            , bdsWrData = wrData
            , bdsWrEn   = wrEn
            , bdsRdAddr = rdAddr
            , bdsPeriph = []
            , bdsSlots  = []
            }
    (userA, finalSt) <- lift $ runStateT busSt initSt
    let slots  = bdsSlots  finalSt
        periph = bdsPeriph finalSt

    -- Build the bus entity: decode logic + rd_data mux only.
    -- parentIns: bus master wires + one pre-allocated rd_data wire per peripheral.
    -- These rd_data wires are driven later by the peripheral entities.
    let busParentIns = [wrAddr, wrData, wrEn, rdAddr] ++ map psRdData slots
    (rdParentWire, weGatedParentWires) <- lift $ do
        ((), outPorts) <- inBlock busName busName busParentIns $ do
            waIn <- freshWire; emit $ NInput waIn "wr_addr" addrW domInfo
            wdIn <- freshWire; emit $ NInput wdIn "wr_data" datW  domInfo
            weIn <- freshWire; emit $ NInput weIn "wr_en"   1     domInfo
            raIn <- freshWire; emit $ NInput raIn "rd_addr" addrW domInfo
            rdIns <- forM slots $ \slot -> do
                w <- freshWire
                emit $ NInput w (psName slot ++ "_rd_data") datW domInfo
                pure (slot, w)
            let wrAddrS = SWire waIn :: Sig dom (Unsigned 32)
                rdAddrS = SWire raIn :: Sig dom (Unsigned 32)
                weS     = SWire weIn :: Sig dom Bool
            -- Per-peripheral: emit gated wr_en output; collect (rdIn, csRd) for mux.
            csRdPairs <- forM rdIns $ \(slot, rdIn) -> do
                let base  = fromIntegral (psBase slot) :: Integer
                    limit = fromIntegral (psBase slot + psSize slot) :: Integer
                    inRange s = sigNot (s .<. fromInteger base)
                                  .&&. (s .<. fromInteger limit)
                weGatedWid <- materialize (weS .&&. inRange wrAddrS)
                emit $ NOutput weGatedWid ("wr_en_" ++ psName slot) 1 domInfo
                csRdWid <- materialize (inRange rdAddrS)
                pure (rdIn, csRdWid)
            -- rd_data mux chain: fold over peripherals from zero baseline.
            zeroWid <- freshWire
            emit $ NComb zeroWid (PLit 0 datW) []
            rdMuxWid <- foldM (\acc (rdIn, csRdWid) -> do
                    out <- freshWire
                    emit $ NComb out PMux [csRdWid, rdIn, acc]
                    pure out
                ) zeroWid csRdPairs
            emit $ NOutput rdMuxWid "rd_data" datW domInfo

        -- outPorts: [wr_en_p0, wr_en_p1, ..., rd_data]  (emission order)
        case outPorts of
            [] -> error "createBus: bus inBlock returned no output ports"
            ports -> pure ( (\(_, w, _) -> w) (last ports)
                          , map (\(_, w, _) -> w) (init ports) )

    -- Instantiate each peripheral entity at the system level (siblings of bus).
    lift $ zipWithM_ psRun slots weGatedParentWires

    let bh = BusHandle
                { bhWrAddr = wrAddr
                , bhWrData = wrData
                , bhWrEn   = wrEn
                , bhRdAddr = rdAddr
                , bhRdData = rdParentWire
                , bhAddrW  = addrW
                , bhDataW  = datW
                }
    modify $ \doc -> doc { sdBuses = sdBuses doc ++ [BusSection busName periph] }
    pure (userA, bh)
  where
    domInfo = domId (Proxy @dom)
    datW    = fromIntegral (natVal (Proxy @(Width dat)))
    addrW   = 32 :: Int

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
              , HdlType dat
              )
           => String
           -> CPUDef alu
           -> ISADef (SynthM alu (Width dat) addrW codeWordW codeAddrW)
           -> BusHandle
           -> [Integer]
           -> SysDSL dom dat ()
createHarvardCPU instName cpuDef isaDef dataBus romWords = SysDSL $ do
    let codeWordB = fromIntegral (natVal (Proxy @codeWordW)) :: Int

    -- Pre-allocate CPU input wires (driven after synthesis)
    instrWordW  <- lift freshWire
    dmemRdDataW <- lift freshWire
    cmemRdDataW <- lift freshWire
    lift $ hintWire instrWordW  "instr_word"
    lift $ hintWire dmemRdDataW "data_rd_data"

    -- Synthesise CPU core; get memory interface wire IDs
    cmi <- lift $ synthHarvardCPU'
               @dom @(Width dat) @addrW @codeWordW @codeAddrW
               cpuDef isaDef instrWordW dmemRdDataW cmemRdDataW

    -- Name key interface signals
    lift $ do
        hintWire (cmiCodeRdAddr cmi) "code_addr"
        hintWire (cmiDataRdAddr cmi) "data_rd_addr"
        hintWire (cmiDataWrEn   cmi) "data_wr_en"
        hintWire (cmiDataWrAddr cmi) "data_wr_addr"
        hintWire (cmiDataWrData cmi) "data_wr_data"

    -- Code ROM: combinational lookup, addressed by PC
    let romSize = max 1 (length romWords)
    lift $ emit $ NRom instrWordW (cmiCodeRdAddr cmi) romSize codeWordB romWords

    -- Second ROM port at PC+1 for 2-word instructions (STS, LDS, CALL, JMP)
    lift $ do
        lit1W    <- freshWire
        pcPlus1W <- freshWire
        emit $ NComb lit1W    (PLit 1 (cmiCodeAddrW cmi)) []
        emit $ NComb pcPlus1W PAdd [cmiCodeRdAddr cmi, lit1W]
        emit $ NRom cmemRdDataW pcPlus1W romSize codeWordB romWords

    -- Wire CPU data outputs → bus master wires, resizing addresses 16→32
    lift $ do
        wrAddrR <- resizeTo (cmiDataAddrW cmi) (bhAddrW dataBus) (cmiDataWrAddr cmi)
        rdAddrR <- resizeTo (cmiDataAddrW cmi) (bhAddrW dataBus) (cmiDataRdAddr cmi)
        alias (bhWrAddr dataBus) wrAddrR
        alias (bhWrData dataBus) (cmiDataWrData cmi)
        alias (bhWrEn   dataBus) (cmiDataWrEn   cmi)
        alias (bhRdAddr dataBus) rdAddrR
        -- Wire bus aggregated read data → CPU data input
        alias dmemRdDataW (bhRdData dataBus)

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
    :: (Num dat, Num (Sig dom dat))
    => String
    -> Sig dom Bool                  -- ^ RX serial line (reserved for FSM)
    -> SysDSL dom dat (PeriphToken UART dom dat (UartPhys dom))
createUart name _rxPin = pure $ PeriphToken
    { ptName     = name
    , ptDef      = uartDef 0 0 >> return UartPhys
                       { uartTxLine = sigFalse
                       , uartRxIrq  = sigFalse
                       , uartTxIrq  = sigFalse
                       }
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
    :: (Num dat, Num (Sig dom dat))
    => String
    -> Sig dom Bool                  -- ^ tick / count-enable (reserved for FSM)
    -> SysDSL dom dat (PeriphToken Timer dom dat (TimerPhys dom))
createTimer name _tick = pure $ PeriphToken
    { ptName     = name
    , ptDef      = timerDef 0 >> return TimerPhys
                       { timerOvfIrq = sigFalse
                       , timerCmpIrq = sigFalse
                       }
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
