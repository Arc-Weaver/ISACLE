{-# LANGUAGE RecursiveDo #-}
module Isacle.Periph.UART
    ( -- * Peripheral kind tag
      UART
      -- * PeriphDef description (single source of truth)
    , uartDef
      -- * Serial state machine
    , serialFSM
      -- * Combined PeriphDef with FSM wired in
    , uartDefWithFSM
      -- * Standalone circuit wrapper
    , uartUnit
    ) where

import Prelude
import Data.Word (Word32)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (natVal)

import Hdl.Net
import Hdl.Types
import Hdl.Prim (Unsigned)
import Isacle.System.Periph
import Isacle.System.HdlCircuit (hdlOps, hdlBusIface)

-- ---------------------------------------------------------------------------
-- Peripheral kind tag
-- ---------------------------------------------------------------------------

data UART

-- ---------------------------------------------------------------------------
-- Register map — single source of truth
-- ---------------------------------------------------------------------------

-- | UART register map.
--
--   offset 0  UDR   read/write  TX write / RX read (reads return Rx buffer)
--   offset 1  USR   read-only   status (bit 0 = UDRE, bit 1 = RXC)
--   offset 2  UBRR  read/write  baud rate divisor
--
-- @stat@ and @rxData@ are driven by the serial state machine.
-- Returns @(txData, txStrobe, baud)@ for the serial FSM.
-- @txStrobe@ pulses True on the cycle the CPU writes to UDR.
uartDef
    :: (Num dat)
    => sig dat    -- ^ status register value (driven by serial FSM)
    -> sig dat    -- ^ RX buffer (read side of UDR, driven by serial FSM)
    -> PeriphDef UART sig dat (sig dat, sig Bool, sig dat)
uartDef stat rxData = do
    -- UDR: typed metadata, but the read side returns the FSM's RX buffer (not the
    -- written value) and the write side needs a strobe, so the logic stays
    -- explicit (onWriteStrobe/onRead) rather than the fused regField.
    fieldOf @(Unsigned 8) ReadWrite 0 "UDR" "TX write / RX read (reads return Rx buffer)"
    (txData, txStrobe) <- onWriteStrobe "udr" 0 0
    onRead 0 rxData

    -- USR: sparse read-only status bits driven by the FSM (explicit bitF).
    register RW8 1 "USR" "Status"
        [ bitF ReadOnly 0 "UDRE" "TX data register empty (safe to write)"
        , bitF ReadOnly 1 "RXC"  "RX complete (received byte ready)"
        ]
    onRead 1 stat

    -- UBRR: a plain read/write register — the fused typed PE2 combinator fits.
    baud <- regField @(Unsigned 8) 2 "UBRR" "Baud rate divisor (system clocks per baud period)" 0

    return (txData, txStrobe, baud)

-- ---------------------------------------------------------------------------
-- Serial state machine
-- ---------------------------------------------------------------------------
--
-- TX states (2-bit): 0=Idle  1=Start  2=Bit  3=Stop
-- RX states (2-bit): 0=Idle  1=Start  2=Bit  3=Done
--
-- Internal registers (all Sig dom (Unsigned N), sized to match):
--   txSt     2-bit state
--   txBitN   4-bit current bit index 0-7
--   txCtr   16-bit baud counter
--   txBuf    dat-bit TX buffer  (txBufVld 1-bit valid flag)
--   rxSt     2-bit state
--   rxBitN   4-bit current bit index 0-7
--   rxCtr   16-bit baud counter
--   rxAcc    dat-bit accumulator
--   rxBuf    dat-bit RX buffer  (rxBufVld 1-bit valid flag)

-- | Logical-AND two Bool signals; short alias.
(.&.) :: Sig dom Bool -> Sig dom Bool -> Sig dom Bool
(.&.) = (.&&.)
infixr 3 .&.

-- | 'mux' with the branches swapped for readability (if cond then t else f).
ifSig :: Sig dom Bool -> Sig dom a -> Sig dom a -> Sig dom a
ifSig = mux

-- | @sigEqLit n s@ — True when signal @s@ (treated as unsigned integer) equals
--   the compile-time integer @n@.
sigEqLit :: Int -> Sig dom a -> Sig dom Bool
sigEqLit n s = SExpr $ do
    ws  <- materialize s
    wl  <- freshWire
    emit $ NComb wl (PLit (fromIntegral n) 16) []
    out <- freshWire
    emit $ NComb out PEq [ws, wl]
    pure out

-- | @sigGeLit n s@ — True when signal @s@ >= compile-time @n@ (unsigned).
--   Implemented as NOT (s < n).
sigGeLit :: Int -> Sig dom a -> Sig dom Bool
sigGeLit n s = sigNot (sigLtLit n s)
  where
    sigLtLit v x = SExpr $ do
        wx <- materialize x
        wl <- freshWire
        emit $ NComb wl (PLit (fromIntegral v) 16) []
        out <- freshWire
        emit $ NComb out PLt [wx, wl]
        pure out
-- | Resize a signal to @bw@ bits using a term-level width.
sigResizeN :: Int -> Sig dom a -> Sig dom b
sigResizeN bw s = SExpr $ do
    ws  <- materialize s
    out <- freshWire
    emit $ NComb out (PResize bw) [ws]
    pure out

-- | Extract a single data bit at a dynamic index: bit 0 of (val >> idx).
sigBitDyn' :: Sig dom a -> Sig dom b -> Sig dom Bool
sigBitDyn' val idx = sigBit 0 (sigShiftRDyn val idx)

-- | OR two signals of the same type.
sigOrV :: Sig dom a -> Sig dom a -> Sig dom a
sigOrV a b = SExpr $ do
    wa <- materialize a
    wb <- materialize b
    out <- freshWire
    emit $ NComb out POr [wa, wb]
    pure out

-- | Set bit @idx@ in a value: val .|. (1 << idx).
sigSetBit :: Sig dom a -> Sig dom b -> Sig dom a
sigSetBit val idx =
    let one = SExpr $ do
            out <- freshWire
            emit $ NComb out (PLit 1 8) []
            pure out
        mask = sigShiftLDyn (one :: Sig dom a) idx
    in sigOrV val mask

-- | Literal signal of a given width.
litW :: Integer -> Int -> Sig dom a
litW v bw = SExpr $ do
    out <- freshWire
    emit $ NComb out (PLit v bw) []
    pure out

-- | 1-bit True literal.
sigTrue :: Sig dom Bool
sigTrue = SExpr $ do
    out <- freshWire
    emit $ NComb out (PLit 1 1) []
    pure out

-- | 1-bit False literal.
sigFalse :: Sig dom Bool
sigFalse = SExpr $ do
    out <- freshWire
    emit $ NComb out (PLit 0 1) []
    pure out

-- | Serial 8N1 state machine.
serialFSM
    :: forall dom dat.
       (KnownDom dom, HdlType dat, Num dat, Num (Sig dom dat))
    => Sig dom dat    -- ^ baud divisor (from uartDef UBRR)
    -> Sig dom dat    -- ^ txData byte to transmit (from uartDef UDR write)
    -> Sig dom Bool   -- ^ txStrobe: True when CPU writes UDR
    -> Sig dom Bool   -- ^ RX serial line
    -> ( Sig dom Bool  -- ^ TX serial line
       , Sig dom dat   -- ^ status register (bit0=UDRE, bit1=RXC)
       , Sig dom dat   -- ^ RX buffer
       , Sig dom Bool  -- ^ RX complete IRQ
       , Sig dom Bool  -- ^ TX empty (UDRE) IRQ
       )
serialFSM baud txDataIn txStrobe rxLine = (txLine, status, rxBuf, rxIrq, udreIrq)
  where
    dom    = domId (Proxy @dom)
    datW   = fromIntegral (natVal (Proxy @(Width dat)))
    brr16  = sigResizeN 16 baud :: Sig dom (Unsigned 16)

    -- -----------------------------------------------------------------------
    -- TX state machine
    -- -----------------------------------------------------------------------

    -- Registers: pre-allocate output wires, defer NReg emission.
    txSt :: Sig dom (Unsigned 2)
    txSt = SExpr $ do
        outWid <- freshWire
        let stSig    = SWire outWid :: Sig dom (Unsigned 2)
            isIdle   = sigEqLit 0 stSig
            isStart  = sigEqLit 1 stSig
            isBit    = sigEqLit 2 stSig
            isStop   = sigEqLit 3 stSig
            ctrDone  = sigGeLit 1 (txCtr - brr16)  -- ctr+1 >= brr  ↔  ctr >= brr-1
            bit7Done = sigGeLit 7 txBitN
            startFSM = isIdle .&. txBufVld
            stNext   = ifSig startFSM            (litW 1 2)
                     $ ifSig (isStart .&. ctrDone) (litW 2 2)
                     $ ifSig (isBit   .&. ctrDone .&. bit7Done) (litW 3 2)
                     $ ifSig (isStop  .&. ctrDone) (litW 0 2)
                       stSig
        defer $ do
            nextWid <- materialize stNext
            emit $ NReg outWid nextWid Nothing (SomeBits 0 2) dom
        pure outWid

    txCtr :: Sig dom (Unsigned 16)
    txCtr = SExpr $ do
        outWid <- freshWire
        let ctrSig   = SWire outWid :: Sig dom (Unsigned 16)
            isIdle   = sigEqLit 0 (txSt :: Sig dom (Unsigned 2))
            isStart  = sigEqLit 1 (txSt :: Sig dom (Unsigned 2))
            isBit    = sigEqLit 2 (txSt :: Sig dom (Unsigned 2))
            isStop   = sigEqLit 3 (txSt :: Sig dom (Unsigned 2))
            ctrDone  = sigGeLit 1 (ctrSig - brr16)
            reset    =  (isIdle  .&. txBufVld)
                    .||. (isStart .&. ctrDone)
                    .||. (isBit   .&. ctrDone)
                    .||. (isStop  .&. ctrDone)
            ctrNext  = ifSig reset 0 (ctrSig + 1)
        defer $ do
            nextWid <- materialize ctrNext
            emit $ NReg outWid nextWid Nothing (SomeBits 0 16) dom
        pure outWid

    txBitN :: Sig dom (Unsigned 4)
    txBitN = SExpr $ do
        outWid <- freshWire
        let bitSig  = SWire outWid :: Sig dom (Unsigned 4)
            isBit   = sigEqLit 2 (txSt :: Sig dom (Unsigned 2))
            ctrDone = sigGeLit 1 ((txCtr :: Sig dom (Unsigned 16)) - brr16)
            advance = isBit .&. ctrDone
            bitNext = ifSig advance (bitSig + 1) bitSig
            -- Reset to 0 when leaving Bit state
            leaving = isBit .&. ctrDone .&. sigGeLit 7 bitSig
            bitFin  = ifSig leaving 0 bitNext
        defer $ do
            nextWid <- materialize bitFin
            emit $ NReg outWid nextWid Nothing (SomeBits 0 4) dom
        pure outWid

    -- TX buffer
    txBufVld :: Sig dom Bool
    txBufVld = SExpr $ do
        outWid <- freshWire
        let vldSig  = SWire outWid :: Sig dom Bool
            consume = sigEqLit 0 (txSt :: Sig dom (Unsigned 2)) .&. vldSig
            vldNext = ifSig txStrobe sigTrue
                    $ ifSig consume  sigFalse
                      vldSig
        defer $ do
            nextWid <- materialize vldNext
            emit $ NReg outWid nextWid Nothing (SomeBits 0 1) dom
        pure outWid

    txBufData :: Sig dom dat
    txBufData = SExpr $ do
        outWid <- freshWire
        let datSig  = SWire outWid :: Sig dom dat
            datNext = ifSig txStrobe txDataIn datSig
        defer $ do
            nextWid <- materialize datNext
            emit $ NReg outWid nextWid Nothing (SomeBits 0 datW) dom
        pure outWid

    -- TX current shift byte (loaded from buffer when entering Start)
    txShift :: Sig dom dat
    txShift = SExpr $ do
        outWid <- freshWire
        let shSig   = SWire outWid :: Sig dom dat
            loadIt  = sigEqLit 0 (txSt :: Sig dom (Unsigned 2)) .&. txBufVld
            shNext  = ifSig loadIt txBufData shSig
        defer $ do
            nextWid <- materialize shNext
            emit $ NReg outWid nextWid Nothing (SomeBits 0 datW) dom
        pure outWid

    -- TX output: idle/stop = '1', start = '0', bit = bit[bitN] of shift reg
    txBitOut :: Sig dom Bool
    txBitOut =
        let isStart = sigEqLit 1 (txSt :: Sig dom (Unsigned 2))
            isBit   = sigEqLit 2 (txSt :: Sig dom (Unsigned 2))
            datBit  = sigBitDyn' txShift (sigResizeN 4 txBitN :: Sig dom dat)
        in ifSig isStart sigFalse
         $ ifSig isBit   datBit
           sigTrue

    txLine = txBitOut

    -- -----------------------------------------------------------------------
    -- RX state machine
    -- -----------------------------------------------------------------------

    rxSt :: Sig dom (Unsigned 2)
    rxSt = SExpr $ do
        outWid <- freshWire
        let stSig    = SWire outWid :: Sig dom (Unsigned 2)
            isIdle   = sigEqLit 0 stSig
            isStart  = sigEqLit 1 stSig
            isBit    = sigEqLit 2 stSig
            ctrDone  = sigGeLit 1 ((rxCtr :: Sig dom (Unsigned 16)) - brr16)
            halfDone = sigGeLit 1 ((rxCtr :: Sig dom (Unsigned 16)) - (sigResizeN 16 (baud `div'` 2)))
            bit7Done = sigGeLit 7 (rxBitN :: Sig dom (Unsigned 4))
            startRx  = isIdle .&. sigNot rxLine
            stNext   = ifSig startRx            (litW 1 2)
                     $ ifSig (isStart .&. halfDone) (litW 2 2)
                     $ ifSig (isBit .&. ctrDone .&. bit7Done) (litW 0 2)
                     $ ifSig (isBit .&. ctrDone) stSig
                       stSig
        defer $ do
            nextWid <- materialize stNext
            emit $ NReg outWid nextWid Nothing (SomeBits 0 2) dom
        pure outWid

    -- baud/2 via PShiftR 1
    div' :: Sig dom dat -> Integer -> Sig dom dat
    div' s _ = sigShiftR 1 s

    rxCtr :: Sig dom (Unsigned 16)
    rxCtr = SExpr $ do
        outWid <- freshWire
        let ctrSig   = SWire outWid :: Sig dom (Unsigned 16)
            isIdle   = sigEqLit 0 (rxSt :: Sig dom (Unsigned 2))
            isStart  = sigEqLit 1 (rxSt :: Sig dom (Unsigned 2))
            isBit    = sigEqLit 2 (rxSt :: Sig dom (Unsigned 2))
            halfDone = sigGeLit 1 (ctrSig - sigResizeN 16 (baud `div'` 2))
            ctrDone  = sigGeLit 1 (ctrSig - brr16)
            reset    =  (isIdle  .&. sigNot rxLine)
                    .||. (isStart .&. halfDone)
                    .||. (isBit   .&. ctrDone)
            ctrNext  = ifSig reset 0 (ctrSig + 1)
        defer $ do
            nextWid <- materialize ctrNext
            emit $ NReg outWid nextWid Nothing (SomeBits 0 16) dom
        pure outWid

    rxBitN :: Sig dom (Unsigned 4)
    rxBitN = SExpr $ do
        outWid <- freshWire
        let bitSig  = SWire outWid :: Sig dom (Unsigned 4)
            isBit   = sigEqLit 2 (rxSt :: Sig dom (Unsigned 2))
            ctrDone = sigGeLit 1 ((rxCtr :: Sig dom (Unsigned 16)) - brr16)
            advance = isBit .&. ctrDone
            bitNext = ifSig advance (bitSig + 1) bitSig
            leaving = isBit .&. ctrDone .&. sigGeLit 7 bitSig
            bitFin  = ifSig leaving 0 bitNext
        defer $ do
            nextWid <- materialize bitFin
            emit $ NReg outWid nextWid Nothing (SomeBits 0 4) dom
        pure outWid

    rxAcc :: Sig dom dat
    rxAcc = SExpr $ do
        outWid <- freshWire
        let accSig  = SWire outWid :: Sig dom dat
            isBit   = sigEqLit 2 (rxSt :: Sig dom (Unsigned 2))
            ctrDone = sigGeLit 1 ((rxCtr :: Sig dom (Unsigned 16)) - brr16)
            sample  = isBit .&. ctrDone
            rxBitIdx = sigResizeN datW rxBitN :: Sig dom dat
            accSet  = sigSetBit accSig rxBitIdx
            accNext = ifSig sample
                          (ifSig rxLine accSet accSig)
                          accSig
            -- Clear accumulator when starting a new byte (entering new reception)
            leaving = isBit .&. ctrDone .&. sigGeLit 7 rxBitN
            accFin  = ifSig leaving (litW 0 datW) accNext
        defer $ do
            nextWid <- materialize accFin
            emit $ NReg outWid nextWid Nothing (SomeBits 0 datW) dom
        pure outWid

    -- RX buffer: latched when last bit received
    rxBufVld :: Sig dom Bool
    rxBufVld = SExpr $ do
        outWid <- freshWire
        let vldSig  = SWire outWid :: Sig dom Bool
            isBit   = sigEqLit 2 (rxSt :: Sig dom (Unsigned 2))
            ctrDone = sigGeLit 1 ((rxCtr :: Sig dom (Unsigned 16)) - brr16)
            done    = isBit .&. ctrDone .&. sigGeLit 7 rxBitN
            -- Buffer stays valid until a new byte arrives
            vldNext = ifSig done sigTrue vldSig
        defer $ do
            nextWid <- materialize vldNext
            emit $ NReg outWid nextWid Nothing (SomeBits 0 1) dom
        pure outWid

    rxBuf :: Sig dom dat
    rxBuf = SExpr $ do
        outWid <- freshWire
        let bufSig  = SWire outWid :: Sig dom dat
            isBit   = sigEqLit 2 (rxSt :: Sig dom (Unsigned 2))
            ctrDone = sigGeLit 1 ((rxCtr :: Sig dom (Unsigned 16)) - brr16)
            done    = isBit .&. ctrDone .&. sigGeLit 7 rxBitN
            bufNext = ifSig done rxAcc bufSig
        defer $ do
            nextWid <- materialize bufNext
            emit $ NReg outWid nextWid Nothing (SomeBits 0 datW) dom
        pure outWid

    -- -----------------------------------------------------------------------
    -- Status / IRQ outputs
    -- -----------------------------------------------------------------------

    udreIrq = sigNot txBufVld  -- UDRE: TX buffer empty
    rxIrq   = rxBufVld

    -- status byte: bit 0 = UDRE, bit 1 = RXC
    udreB :: Sig dom dat
    udreB = ifSig udreIrq (litW 1 datW) (litW 0 datW)
    rxcB  :: Sig dom dat
    rxcB  = ifSig rxIrq   (litW 2 datW) (litW 0 datW)
    status = sigOrV udreB rxcB

-- ---------------------------------------------------------------------------
-- Combined PeriphDef with serial FSM
-- ---------------------------------------------------------------------------

-- | 'uartDef' with 'serialFSM' wired in via a recursive binding.
-- Returns @(txLine, rxIrq, txIrq)@.  Use this as the @ptDef@ in a
-- 'PeriphToken' so that 'attachPeripheral' gets real serial outputs.
uartDefWithFSM
    :: (KnownDom dom, HdlType dat, Num dat, Num (Sig dom dat))
    => Sig dom Bool   -- ^ RX serial line
    -> PeriphDef UART (Sig dom) dat (Sig dom Bool, Sig dom Bool, Sig dom Bool)
                      -- ^ (txLine, rxIrq, txIrq)
uartDefWithFSM rxLine = mdo
    (txData, txStrobe, baud) <- uartDef stat rxBuf
    let (txLine, stat, rxBuf, rxIrq, txIrq) = serialFSM baud txData txStrobe rxLine
    return (txLine, rxIrq, txIrq)

-- ---------------------------------------------------------------------------
-- Standalone circuit wrapper
-- ---------------------------------------------------------------------------

-- | Memory-mapped 8N1 UART built from 'uartDef' + 'serialFSM'.
--
--   Register layout:
--     base + 0  UDR   data (TX on write, RX on read)
--     base + 1  USR   status (read-only)
--     base + 2  UBRR  baud rate divisor
uartUnit
    :: (KnownDom dom, HdlType dat, Num dat, Num (Sig dom dat))
    => Word32                          -- ^ peripheral base address
    -> Sig dom Bool                    -- ^ RX serial line
    -> Sig dom (Unsigned 32)           -- ^ bus write address
    -> Sig dom dat                     -- ^ bus write data
    -> Sig dom Bool                    -- ^ bus write enable
    -> Sig dom (Unsigned 32)           -- ^ bus read address
    -> (Sig dom dat, Sig dom Bool, Sig dom Bool, Sig dom Bool)
       -- ^ (rdData, txLine, rxIrq, txIrq)
uartUnit base rxLine wrAddr wrData wrEn rdAddr = (rdData, txLine, rxIrq, txIrq)
  where
    bus = hdlBusIface wrAddr wrData wrEn rdAddr base
    ((txDataIn, txStrobe, baud), rdData, _spec) =
        runPeriphDef hdlOps bus (uartDef stat rxBuf)
    (txLine, stat, rxBuf, rxIrq, txIrq) =
        serialFSM baud txDataIn txStrobe rxLine
