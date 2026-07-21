{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds           #-}
module Isacle.Periph.UART
    ( -- * Peripheral kind tag
      UART
      -- * PeriphDef description (single source of truth)
    , uartDef
      -- * Serial state machine
    , serialFSM
      -- * Combined PeriphDef with FSM wired in
    , uartDefWithFSM
    ) where

import Prelude
import Control.Monad.Fix (MonadFix)
import Data.Kind (Type)

import Hdl.Monad (Hdl)
import qualified Hdl.Monad as HM
import Hdl.Sig
import Hdl.Prim (Unsigned)
import Hdl.Reduce (sigTrue, sigFalse)
import Isacle.System.Periph

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
    :: (Num dat, MonadFix m)
    => sig dat    -- ^ status register value (driven by serial FSM)
    -> sig dat    -- ^ RX buffer (read side of UDR, driven by serial FSM)
    -> PeriphDef UART sig m dat (sig dat, sig Bool, sig dat)
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
-- RX states (2-bit): 0=Idle  1=Start  2=Bit  (returns to Idle when done)

-- | 'mux' with the branches swapped for readability (if cond then t else f).
ifSig :: HdlType a => Sig dom Bool -> Sig dom a -> Sig dom a -> Sig dom a
ifSig = mux

-- | @eqL n s@ — True when @s@ equals the literal @n@ (at @s@'s own width).
eqL :: (HdlType a, Num (Sig dom a)) => Integer -> Sig dom a -> Sig dom Bool
eqL n s = s .==. fromInteger n

-- | @geL n s@ — True when @s >= n@ (unsigned), i.e. @not (s < n)@.
geL :: (HdlType a, Num (Sig dom a)) => Integer -> Sig dom a -> Sig dom Bool
geL n s = sigNot (s .<. fromInteger n)

-- | Extract a single data bit at a dynamic index: bit 0 of (val >> idx).
sigBitDyn' :: (HdlType a, HdlType b) => Sig dom a -> Sig dom b -> Sig dom Bool
sigBitDyn' val idx = sigBit 0 (sigShiftRDyn val idx)

-- | Serial 8N1 state machine.  One clocked register per state element (the
-- abstract 'Hdl' 'register'); @mdo@ ties each register to the next value it
-- computes from itself and the others.
serialFSM
    :: forall (dom :: Type) dat m.
       (Hdl Sig m, KnownDom dom, HdlType dat, Num dat)
    => Sig dom dat    -- ^ baud divisor (from uartDef UBRR)
    -> Sig dom dat    -- ^ txData byte to transmit (from uartDef UDR write)
    -> Sig dom Bool   -- ^ txStrobe: True when CPU writes UDR
    -> Sig dom Bool   -- ^ RX serial line
    -> m ( Sig dom Bool  -- ^ TX serial line
         , Sig dom dat   -- ^ status register (bit0=UDRE, bit1=RXC)
         , Sig dom dat   -- ^ RX buffer
         , Sig dom Bool  -- ^ RX complete IRQ
         , Sig dom Bool  -- ^ TX empty (UDRE) IRQ
         )
serialFSM baud txDataIn txStrobe rxLine = mdo
    -- TX state registers
    txSt      <- HM.register (0 :: Unsigned 2)  txStNext
    txCtr     <- HM.register (0 :: Unsigned 16) txCtrNext
    txBitN    <- HM.register (0 :: Unsigned 4)  txBitNNext
    txBufVld  <- HM.register False              txBufVldNext
    txBufData <- HM.register (0 :: dat)         txBufDataNext
    txShift   <- HM.register (0 :: dat)         txShiftNext
    -- RX state registers
    rxSt      <- HM.register (0 :: Unsigned 2)  rxStNext
    rxCtr     <- HM.register (0 :: Unsigned 16) rxCtrNext
    rxBitN    <- HM.register (0 :: Unsigned 4)  rxBitNNext
    rxAcc     <- HM.register (0 :: dat)         rxAccNext
    rxBufVld  <- HM.register False              rxBufVldNext
    rxBuf     <- HM.register (0 :: dat)         rxBufNext

    let brr16    = sigResize @16 baud               :: Sig dom (Unsigned 16)
        halfBaud = sigResize @16 (sigShiftR 1 baud) :: Sig dom (Unsigned 16)

        -- ── TX ────────────────────────────────────────────────────────────────
        txIdle    = eqL 0 txSt
        txStart   = eqL 1 txSt
        txBit     = eqL 2 txSt
        txStop    = eqL 3 txSt
        txCtrDone = geL 1 (txCtr - brr16)   -- ctr+1 >= brr  ↔  ctr >= brr-1
        txBit7    = geL 7 txBitN
        txBegin   = txIdle .&&. txBufVld

        txStNext = ifSig txBegin                        1
                 $ ifSig (txStart .&&. txCtrDone)       2
                 $ ifSig (txBit .&&. txCtrDone .&&. txBit7) 3
                 $ ifSig (txStop .&&. txCtrDone)        0
                   txSt

        txCtrReset =  txBegin
                 .||. (txStart .&&. txCtrDone)
                 .||. (txBit   .&&. txCtrDone)
                 .||. (txStop  .&&. txCtrDone)
        txCtrNext  = ifSig txCtrReset 0 (txCtr + 1)

        txAdvance  = txBit .&&. txCtrDone
        txBitNNext = ifSig (txAdvance .&&. txBit7) 0
                   $ ifSig txAdvance (txBitN + 1) txBitN

        txBufVldNext  = ifSig txStrobe sigTrue
                      $ ifSig (txIdle .&&. txBufVld) sigFalse
                        txBufVld
        txBufDataNext = ifSig txStrobe txDataIn txBufData
        txShiftNext   = ifSig (txIdle .&&. txBufVld) txBufData txShift

        -- TX output: idle/stop = '1', start = '0', bit = bit[bitN] of shift reg
        txLine = ifSig txStart sigFalse
               $ ifSig txBit   (sigBitDyn' txShift txBitN)
                 sigTrue

        -- ── RX ────────────────────────────────────────────────────────────────
        rxIdle     = eqL 0 rxSt
        rxStart    = eqL 1 rxSt
        rxBit      = eqL 2 rxSt
        rxCtrDone  = geL 1 (rxCtr - brr16)
        rxHalfDone = geL 1 (rxCtr - halfBaud)
        rxBit7     = geL 7 rxBitN
        rxBegin    = rxIdle .&&. sigNot rxLine

        rxStNext = ifSig rxBegin                        1
                 $ ifSig (rxStart .&&. rxHalfDone)      2
                 $ ifSig (rxBit .&&. rxCtrDone .&&. rxBit7) 0
                   rxSt

        rxCtrReset =  rxBegin
                 .||. (rxStart .&&. rxHalfDone)
                 .||. (rxBit   .&&. rxCtrDone)
        rxCtrNext  = ifSig rxCtrReset 0 (rxCtr + 1)

        rxAdvance  = rxBit .&&. rxCtrDone
        rxBitNNext = ifSig (rxAdvance .&&. rxBit7) 0
                   $ ifSig rxAdvance (rxBitN + 1) rxBitN

        rxSample  = rxBit .&&. rxCtrDone
        rxAccSet  = rxAcc .|. sigShiftLDyn (1 :: Sig dom dat) rxBitN
        rxAccNext = ifSig (rxSample .&&. rxBit7) 0     -- clear as the byte completes
                  $ ifSig rxSample (ifSig rxLine rxAccSet rxAcc)
                    rxAcc

        rxDone       = rxBit .&&. rxCtrDone .&&. rxBit7
        rxBufVldNext = ifSig rxDone sigTrue rxBufVld
        rxBufNext    = ifSig rxDone rxAcc rxBuf

        -- ── Status / IRQ ───────────────────────────────────────────────────────
        udreIrq = sigNot txBufVld              -- UDRE: TX buffer empty
        rxIrq   = rxBufVld                     -- RXC:  received byte ready
        status  = ifSig udreIrq 1 0 .|. ifSig rxIrq 2 0   -- bit0=UDRE, bit1=RXC

    pure (txLine, status, rxBuf, rxIrq, udreIrq)

-- ---------------------------------------------------------------------------
-- Combined PeriphDef with serial FSM
-- ---------------------------------------------------------------------------

-- | 'uartDef' with 'serialFSM' wired in via a recursive binding.
-- Returns @(txLine, rxIrq, txIrq)@.  Use this as the @ptDef@ in a
-- 'PeriphToken' so that 'attachPeripheral' gets real serial outputs.
uartDefWithFSM
    :: forall (dom :: Type) dat m.
       (Hdl Sig m, KnownDom dom, HdlType dat, Num dat)
    => Sig dom Bool   -- ^ RX serial line
    -> PeriphDef UART (Sig dom) m dat (Sig dom Bool, Sig dom Bool, Sig dom Bool)
                      -- ^ (txLine, rxIrq, txIrq)
uartDefWithFSM rxLine = mdo
    (txData, txStrobe, baud) <- uartDef stat rxBuf
    (txLine, stat, rxBuf, rxIrq, txIrq) <- liftHdl (serialFSM baud txData txStrobe rxLine)
    return (txLine, rxIrq, txIrq)
