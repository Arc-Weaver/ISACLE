-- NB: NoImplicitPrelude is active from cabal common-options.
-- | Bus architectures (protocols): the Layer-2 definition of how a bus behaves.
--
-- A bus architecture knows how to /synthesise an interconnect/: given the
-- bus's upstream connection (the port its master drives) and the set of
-- children placed on it (peripherals, memories, or whole sub-buses), it emits
-- the address-decode, read-data muxing and stall/handshake logic for its
-- protocol.
--
-- The address /layout/ (where each child sits) is supplied by
-- "Isacle.System.BusDef" — the single source of truth.  This module owns only
-- the protocol behaviour, so the same layout can be realised under different
-- protocols (SimpleBus, Wishbone, AXI-lite …) by swapping the architecture.
module Isacle.System.BusArch
    ( -- * Architectures
      BusArch(..)
    , SimpleBus(..)
    , BurstBus(..)
      -- * The protocol-agnostic connection point
    , BusPort(..)
    , BusChild
      -- * Connecting ports
    , connectPort
    ) where

import Prelude
import Control.Monad (forM, foldM)
import Hdl.Net
    ( WireId, NetM, DomId, NetNode(..), PrimOp(..)
    , freshWire, emit, hintWire, comment )

-- ---------------------------------------------------------------------------
-- BusPort — the single connection object shared by master and slave ends
-- ---------------------------------------------------------------------------

-- | One bus connection point, as a bundle of wire handles.
--
-- A /master/ end drives @bpReq\/bpWe\/bpAddr\/bpWData@ and reads
-- @bpRData\/bpStall@.  A /slave/ end does the opposite.  The two ends of a
-- connection share the same 'BusPort' (same wires), so connecting a master to
-- a slave is just sharing the value — no aliasing needed.
--
-- A bus is /both/: it is a slave to its parent (its upstream 'BusPort') and a
-- master to each of its children (one downstream 'BusPort' per child).  This is
-- what lets buses bridge to buses: a child port's far end can itself be another
-- bus's upstream port, recursively, so each bus level becomes its own decoder
-- (the structure is preserved, never flattened).
--
-- The transaction protocol is single-outstanding:
--
--   * @bpReq@   — 1 = a transaction is requested this cycle (address valid)
--   * @bpWe@    — 1 = write, 0 = read
--   * @bpAddr@  — transaction address
--   * @bpWData@ — write data (meaningful when @bpWe = 1@)
--   * @bpRData@ — read data (valid when the transaction completes)
--   * @bpStall@ — 1 = transaction not yet complete; the master must hold.
data BusPort = BusPort
    { bpReq   :: WireId  -- ^ master → slave: transaction valid this cycle
    , bpWe    :: WireId  -- ^ master → slave: 1 = write, 0 = read
    , bpAddr  :: WireId  -- ^ master → slave: address
    , bpWData :: WireId  -- ^ master → slave: write data
    , bpRData :: WireId  -- ^ slave → master: read data
    , bpStall :: WireId  -- ^ slave → master: 1 = hold (not yet complete)
    , bpAddrW :: Int     -- ^ address width in bits
    , bpDataW :: Int     -- ^ data width in bits
    }

-- | A child placed on a bus: its base address, window size (bytes), and the
-- downstream 'BusPort' the interconnect drives.  Base/size come from the
-- 'Isacle.System.BusDef' layout.
type BusChild = (Integer, Integer, BusPort)

-- ---------------------------------------------------------------------------
-- BusArch — the protocol
-- ---------------------------------------------------------------------------

-- | A bus architecture (protocol).  Implementations synthesise the
-- interconnect for their protocol.
--
-- A system may contain several buses with different architectures; the
-- architecture is a phantom type on 'Isacle.System.BusDef.Bus'.
class BusArch arch where
    -- | Synthesise this bus's interconnect.
    --
    -- @synthBus arch dom up children@ wires the upstream port @up@ (this bus
    -- as a slave to its master) to the @children@ (this bus as a master to
    -- each), emitting the protocol's address decode, read mux and stall logic.
    --
    -- The default errors; a protocol must override it to be synthesisable.
    synthBus :: arch -> DomId -> BusPort -> [BusChild] -> NetM ()
    synthBus _ _ _ _ = error "BusArch.synthBus: not implemented for this architecture"

-- ---------------------------------------------------------------------------
-- SimpleBus — combinational, single-master, no stall
-- ---------------------------------------------------------------------------

-- | A simple synchronous memory-mapped bus.
--
-- Single master, combinational address decode, no stalling, no bursts.
-- Suitable for small AVR-style SoC designs.
data SimpleBus = SimpleBus

instance BusArch SimpleBus where
    synthBus _ _dom up children = do
        comment "SimpleBus interconnect: combinational decode, no stall"
        let aw = bpAddrW up
        -- Decode each child and route the master transaction to it.
        decoded <- forM children $ \(base, size, child) -> do
            cs <- chipSelect aw (bpAddr up) base size
            hintWire cs ("cs_0x" ++ showHex base)
            -- Child sees a request only when its window is selected; the rest
            -- of the transaction signals are shared (the child gates its own
            -- write on @bpReq && bpWe@).
            reqSel <- gate2 (bpReq up) cs
            drive (bpReq   child) reqSel
            drive (bpWe    child) (bpWe   up)
            drive (bpAddr  child) (bpAddr up)
            drive (bpWData child) (bpWData up)
            return (cs, child)
        -- Read data: the selected child's response, else zero.
        zero <- litW (bpDataW up) 0
        rd   <- foldM (\acc (cs, child) -> mux2 cs (bpRData child) acc) zero decoded
        drive (bpRData up) rd
        -- SimpleBus has no stall path: the upstream stall is hardwired low.
        -- A *stalling* child (e.g. a Wishbone sub-bus) is therefore unsupported
        -- here — its stall would be dropped.  Driving a stalling bus from a
        -- SimpleBus is broken by construction (the reverse bridge works fine).
        s0 <- litW 1 0
        drive (bpStall up) s0

-- ---------------------------------------------------------------------------
-- BurstBus — burst-capable system bus (interconnect not yet implemented)
-- ---------------------------------------------------------------------------

-- | A burst-capable synchronous bus for cache-line refill.  Used as the system
-- bus when an L1 cache bridges a von Neumann CPU to the memory fabric.
data BurstBus = BurstBus

instance BusArch BurstBus
    -- synthBus uses the default (error) until the burst interconnect lands.

-- ---------------------------------------------------------------------------
-- Connecting ports
-- ---------------------------------------------------------------------------

-- | Connect a master port to a slave port that were allocated separately:
-- alias the master's request signals onto the slave and the slave's responses
-- back onto the master.  Not needed when the two ends already share one
-- 'BusPort' (the common case); provided for bridging independently-built ends.
connectPort :: BusPort  -- ^ master end
            -> BusPort  -- ^ slave end
            -> NetM ()
connectPort m s = do
    drive (bpReq   s) (bpReq   m)
    drive (bpWe    s) (bpWe    m)
    drive (bpAddr  s) (bpAddr  m)
    drive (bpWData s) (bpWData m)
    drive (bpRData m) (bpRData s)
    drive (bpStall m) (bpStall s)

-- ---------------------------------------------------------------------------
-- Local netlist helpers
-- ---------------------------------------------------------------------------

-- | Emit a literal constant wire of the given width.
litW :: Int -> Integer -> NetM WireId
litW w v = do
    o <- freshWire
    emit (NComb o (PLit v w) [])
    return o

-- | Drive a pre-allocated wire from another (identity buffer; the emitter
-- renders it as a direct assignment).
drive :: WireId -> WireId -> NetM ()
drive dst src
    | dst == src = return ()
    | otherwise  = emit (NComb dst POr [src, src])

-- | 1-bit AND of two wires.
gate2 :: WireId -> WireId -> NetM WireId
gate2 a b = do
    o <- freshWire
    emit (NComb o PAnd [a, b])
    return o

-- | 2:1 mux: @mux2 sel a b = if sel then a else b@.
mux2 :: WireId -> WireId -> WireId -> NetM WireId
mux2 sel a b = do
    o <- freshWire
    emit (NComb o PMux [sel, a, b])
    return o

-- | Combinational chip-select: @base <= addr < base + size@.
chipSelect :: Int -> WireId -> Integer -> Integer -> NetM WireId
chipSelect aw addr base size = do
    ltLimit <- do
        l <- litW aw (base + size)
        o <- freshWire
        emit (NComb o PLt [addr, l])
        return o
    if base == 0
        then return ltLimit          -- addr >= 0 always holds
        else do
            b      <- litW aw base
            ltBase <- do { o <- freshWire; emit (NComb o PLt [addr, b]); return o }
            geBase <- do { o <- freshWire; emit (NComb o PNot [ltBase]); return o }
            o <- freshWire
            emit (NComb o PAnd [geBase, ltLimit])
            return o

-- | Minimal lowercase hex for naming chip-select wires.
showHex :: Integer -> String
showHex n
    | n < 0     = '-' : showHex (negate n)
    | n < 16    = [digit n]
    | otherwise = showHex (n `div` 16) ++ [digit (n `mod` 16)]
  where
    digit d | d < 10    = toEnum (fromEnum '0' + fromIntegral d)
            | otherwise = toEnum (fromEnum 'a' + fromIntegral (d - 10))
