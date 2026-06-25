-- NB: NoImplicitPrelude is active from cabal common-options.
module Isacle.System.BusArch
    ( BusArch
    , SimpleBus(..)
    , BurstBus(..)
    ) where

-- | Marks a type as a bus architecture.
--
-- This class is intentionally empty: it acts as a constraint that ensures
-- only declared architectures are used with 'Isacle.System.BusDef.Bus'.
-- Protocol-specific methods (interconnect construction, stall handling,
-- arbitration) will be added here when concrete bus implementations
-- are introduced.
--
-- A system may have multiple buses with different architectures; the
-- architecture is a phantom type parameter on 'Isacle.System.BusDef.Bus'
-- rather than a constraint on 'Isacle.System.Builder.SystemDSL'.
class BusArch arch

-- | A simple synchronous memory-mapped bus.
--
-- Single master, combinational address decode, no stalling, no burst support.
-- Suitable for small AVR-style SoC designs.  Data width is the @dat@ type
-- parameter on 'Bus', not an architecture property.
data SimpleBus = SimpleBus

instance BusArch SimpleBus

-- | A burst-capable synchronous bus.
--
-- Supports fixed-length burst transfers for cache-line refill.  Used as the
-- system bus architecture when an L1 cache bridges a von Neumann CPU to the
-- memory fabric.  The burst length is a type parameter on the bus binding,
-- not encoded here.
data BurstBus = BurstBus

instance BusArch BurstBus
