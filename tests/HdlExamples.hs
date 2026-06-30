{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
import Prelude
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)

import Hdl.Net
import Hdl.Types
import Hdl.Prim
import Hdl.Class
import Hdl.Entity
import Hdl.Emit.Vhdl

-- ---------------------------------------------------------------------------
-- System clock
-- ---------------------------------------------------------------------------

data SysClk
instance KnownDom SysClk where
    domId _ = DomId "clk" 100_000_000 Rising ActiveHigh "rst"

-- ---------------------------------------------------------------------------
-- 1. PWM generator
-- 8-bit free-running counter; output high while counter < duty.
-- ---------------------------------------------------------------------------

pwmGen :: Entity (Sig SysClk (Unsigned 8)) (Sig SysClk Bool)
pwmGen = entity "pwm_gen" (hdl go)
  where
    go duty = mdo
        count <- regS 0 (count + 1) >>= named "count"
        named "cmp" (count .<. duty)

-- ---------------------------------------------------------------------------
-- 2. Rising / falling edge detector
-- ---------------------------------------------------------------------------

edgeDetect :: Entity (Sig SysClk Bool) (Sig SysClk Bool, Sig SysClk Bool)
edgeDetect = entity "edge_detect" (hdl go)
  where
    go sig_in = do
        delayed <- regS False sig_in >>= named "delayed"
        rise    <- named "rising"  (sig_in .&&. sigNot delayed)
        fall    <- named "falling" (sigNot sig_in .&&. delayed)
        return (rise, fall)

-- ---------------------------------------------------------------------------
-- 3. Simple 4-operation ALU
-- op[1:0]: 00 = add, 01 = sub, 10 = and, 11 = or
-- ---------------------------------------------------------------------------

simpleAlu :: Entity ( Sig SysClk (Unsigned 8)
                    , Sig SysClk (Unsigned 8)
                    , Sig SysClk (Unsigned 2) )
                    ( Sig SysClk (Unsigned 8) )
simpleAlu = entity "simple_alu" (hdl go)
  where
    go (a, b, op) =
        named "result" $
            mux (sigBit 1 op)
                (mux (sigBit 0 op) (sigBwOr  a b) (sigBwAnd a b))
                (mux (sigBit 0 op) (a - b)        (a + b))

-- ---------------------------------------------------------------------------
-- 4. Accumulator with saturating add
-- ---------------------------------------------------------------------------

saturatingAccum :: Entity ( Sig SysClk (Unsigned 16)
                          , Sig SysClk Bool
                          , Sig SysClk (Unsigned 16) )
                          ( Sig SysClk (Unsigned 16) )
saturatingAccum = entity "sat_accum" (hdl go)
  where
    go (din, en, max_val) = mdo
        let added   = acc + din
            clamped = mux (max_val .<. added) max_val added
            next    = mux en clamped acc
        acc <- regS 0 next >>= named "acc"
        return acc

-- ---------------------------------------------------------------------------
-- 5. Register file: 32 × 8 bits, 1 read port, 1 write port
-- Record ports — derived automatically.
-- ---------------------------------------------------------------------------

data RamPorts = RamPorts
    { rdAddr :: Sig SysClk (Unsigned 5)
    , wrAddr :: Sig SysClk (Unsigned 5)
    , wrData :: Sig SysClk (Unsigned 8)
    , wrEn   :: Sig SysClk Bool
    } deriving (Generic, HdlPorts, PortRef)

regFile32x8 :: Entity RamPorts (Sig SysClk (Unsigned 8))
regFile32x8 = entity "reg_file_32x8" (hdl go)
  where
    go RamPorts{..} = ram 32 [] rdAddr wrAddr wrData wrEn

-- ---------------------------------------------------------------------------
-- 6. ROM lookup table: 4-bit address → 8-bit sine approximation
-- ---------------------------------------------------------------------------

sineLut :: Entity (Sig SysClk (Unsigned 4)) (Sig SysClk (Unsigned 8))
sineLut = entity "sine_lut" (hdl go)
  where
    go phase = rom 16 table phase
    table = [0, 50, 98, 142, 180, 210, 233, 246, 255,
             246, 233, 210, 180, 142, 98, 50]

-- ---------------------------------------------------------------------------
-- 7. Hierarchical design: two adder instances feeding a pipeline register
-- ---------------------------------------------------------------------------

adder8 :: Entity (Sig SysClk (Unsigned 8), Sig SysClk (Unsigned 8))
                 (Sig SysClk (Unsigned 8))
adder8 = entity "adder8" $ hdl $ \(a, b) -> return (a + b)

data PipeIn = PipeIn
    { pX :: Sig SysClk (Unsigned 8)
    , pY :: Sig SysClk (Unsigned 8)
    , pZ :: Sig SysClk (Unsigned 8)
    } deriving (Generic, HdlPorts, PortRef)

pipelineAdder :: Entity PipeIn (Sig SysClk (Unsigned 8))
pipelineAdder = entity "pipeline_top" (hdl go)
  where
    go PipeIn{..} = do
        s0 <- instEntity adder8 "u_add0" (pX, pY)
        s1 <- instEntity adder8 "u_add1" (s0, pZ)
        regS 0 s1 >>= named "result_r"

-- ---------------------------------------------------------------------------
-- Unnamed variants — names are optional annotations; these are fully valid
-- synthesis targets and are simpler to write when readability isn't a priority.
-- ---------------------------------------------------------------------------

pwmGenBare :: Entity (Sig SysClk (Unsigned 8)) (Sig SysClk Bool)
pwmGenBare = entity "pwm_gen_bare" (hdl go)
  where
    go duty = mdo
        count <- regS 0 (count + 1)
        return (count .<. duty)

edgeDetectBare :: Entity (Sig SysClk Bool) (Sig SysClk Bool, Sig SysClk Bool)
edgeDetectBare = entity "edge_detect_bare" (hdl go)
  where
    go sig_in = do
        delayed <- regS False sig_in
        return (sig_in .&&. sigNot delayed, sigNot sig_in .&&. delayed)

saturatingAccumBare :: Entity ( Sig SysClk (Unsigned 16)
                              , Sig SysClk Bool
                              , Sig SysClk (Unsigned 16) )
                              ( Sig SysClk (Unsigned 16) )
saturatingAccumBare = entity "sat_accum_bare" (hdl go)
  where
    go (din, en, max_val) = mdo
        let added   = acc + din
            clamped = mux (max_val .<. added) max_val added
        acc <- regS 0 (mux en clamped acc)
        return acc

-- ---------------------------------------------------------------------------
-- 8. ADT port bundles — sum type encodes as tag + union of all constructor fields.
--
-- 2-constructor, 1-bit tag.
-- BusWrite is first so fromWireIds surfaces both fields to consumers.
-- ---------------------------------------------------------------------------

data BusReq
    = BusWrite { reqAddr :: Sig SysClk (Unsigned 16)
               , reqData :: Sig SysClk (Unsigned 8) }
    | BusRead  { reqAddr :: Sig SysClk (Unsigned 16) }
    deriving (Generic, HdlPorts, PortRef)

-- Produces a write transaction: tag=0, both fields driven.
genWrite :: Entity (Sig SysClk (Unsigned 16), Sig SysClk (Unsigned 8)) BusReq
genWrite = entity "gen_write" (hdl go)
  where go (addr, dat) = return BusWrite { reqAddr = addr, reqData = dat }

-- Produces a read transaction: tag=1, reqData zero-padded.
genRead :: Entity (Sig SysClk (Unsigned 16)) BusReq
genRead = entity "gen_read" (hdl go)
  where go addr = return BusRead { reqAddr = addr }

-- Latches write address and data one cycle.
-- BusWrite is the first constructor so fromWireIds wires up both reqAddr and
-- reqData; for a BusRead the reqData field is zero (padding from genRead).
writeLatch :: Entity BusReq (Sig SysClk (Unsigned 16), Sig SysClk (Unsigned 8))
writeLatch = entity "write_latch" (hdl go)
  where
    go req = do
        addrR <- regS 0 (reqAddr req) >>= named "addr_r"
        dataR <- regS 0 (reqData req) >>= named "data_r"
        return (addrR, dataR)

-- ---------------------------------------------------------------------------
-- 4-constructor, 2-bit tag.
-- UartCmd has heterogeneous field widths so the port layout is clearly
-- different per constructor and the 2-bit tag is visibly necessary.
-- ---------------------------------------------------------------------------

data UartCmd
    = UartSend    { txByte   :: Sig SysClk (Unsigned 8) }
    | UartSetBaud { baudDiv  :: Sig SysClk (Unsigned 16) }
    | UartDiscard { discardN :: Sig SysClk (Unsigned 4) }
    | UartReset   { delayUs  :: Sig SysClk (Unsigned 8) }
    deriving (Generic, HdlPorts, PortRef)

-- Pipeline-registers the outgoing byte before packaging it as a send command.
uartSendCmd :: Entity (Sig SysClk (Unsigned 8)) UartCmd
uartSendCmd = entity "uart_send_cmd" (hdl go)
  where
    go byte = do
        byteR <- regS 0 byte >>= named "byte_r"
        return UartSend { txByte = byteR }

-- Halves the baud divisor (doubles the baud rate) then issues a set-baud command.
uartDoubleBaud :: Entity (Sig SysClk (Unsigned 16)) UartCmd
uartDoubleBaud = entity "uart_double_baud" (hdl go)
  where
    go div = return UartSetBaud { baudDiv = sigShiftR 1 div }

-- ---------------------------------------------------------------------------
-- Printing helpers
-- ---------------------------------------------------------------------------

banner :: String -> IO ()
banner name = do
    putStrLn ""
    putStrLn (replicate 72 '-')
    putStrLn ("-- " ++ name)
    putStrLn (replicate 72 '-')

-- | Elaborate and emit a single entity (flat, no sub-instances).
printEntity :: (PortRef i, PortRef o) => Entity i o -> IO ()
printEntity ent = do
    banner (entityName ent)
    putStrLn (emitEntity (elaborate ent))

-- | Elaborate and emit a hierarchical design (entity + all sub-entities).
printDesign :: (PortRef i, PortRef o) => Entity i o -> IO ()
printDesign ent = do
    let (_, design) = elaborateDesign ent
    mapM_ emit' (Map.toAscList design)
  where
    emit' (name, nodes) = do
        banner ("entity: " ++ name)
        putStrLn (emitVhdl Map.empty name nodes)

main :: IO ()
main = do
    putStrLn "=== HDL Examples — generated VHDL ==="
    printEntity pwmGen
    printEntity edgeDetect
    printEntity simpleAlu
    printEntity saturatingAccum
    printEntity regFile32x8
    printEntity sineLut
    printDesign pipelineAdder
    putStrLn "\n=== ADT port bundles (sum type → tag + union of fields) ==="
    printEntity genWrite
    printEntity genRead
    printEntity writeLatch
    printEntity uartSendCmd
    printEntity uartDoubleBaud
    putStrLn "\n=== Unnamed variants (named is optional) ==="
    printEntity pwmGenBare
    printEntity edgeDetectBare
    printEntity saturatingAccumBare
