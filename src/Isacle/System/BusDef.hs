-- NB: NoImplicitPrelude is active from cabal common-options.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
module Isacle.System.BusDef
    ( -- * Bus topology monad
      BusDef
    , runBusDef
      -- * Named bus with explicit architecture
    , Bus
    , mkBus
    , getBusDef
      -- * Opaque peripheral handle
    , BusPeriph(..)
      -- * Bus master handle
    , BusMaster(..)
      -- * Placement
    , attach
    , periph
    , ramSegment
    , romBlock
      -- * Metadata
    , label
    , annotate
    ) where

import Prelude
import Data.Proxy (Proxy(..))
import Data.Word (Word32)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Isacle.Hdl.Prim (Unsigned)

import Isacle.System.Spec (ComponentSpec(..))
import Isacle.System.Periph (PeriphSpec)

-- ---------------------------------------------------------------------------
-- BusPeriph — opaque wired peripheral handle
-- ---------------------------------------------------------------------------

-- | An opaque handle representing a peripheral that has been instantiated
-- and wired to its physical signals (by 'Isacle.System.Builder.SystemDSL').
data BusPeriph p = BusPeriph
    { bpId   :: Int         -- ^ sequential ID assigned at 'mkPeriph' time
    , bpSpec :: PeriphSpec  -- ^ register definitions (analysis path)
    , bpSize :: Word32      -- ^ address window size in bytes
    }

-- ---------------------------------------------------------------------------
-- BusMaster — CPU / DMA bus master handle (symmetric with BusPeriph)
-- ---------------------------------------------------------------------------

-- | Signals driven by a bus master onto the shared data bus.
data BusMaster sig addrW dat = BusMaster
    { bmRdAddr   :: sig (Maybe (Unsigned addrW))            -- ^ data-bus read address
    , bmWrBus    :: sig (Maybe (Unsigned addrW, dat))       -- ^ data-bus write command
    , bmCodeAddr :: sig (Unsigned addrW)                    -- ^ code-bus address (PC)
    }

-- ---------------------------------------------------------------------------
-- BusDef — Writer-style product: pure spec list paired with result value.
-- ---------------------------------------------------------------------------

data BusDef a = BusDef [ComponentSpec] a

instance Functor BusDef where
    {-# INLINE fmap #-}
    fmap f (BusDef specs a) = BusDef specs (f a)

instance Applicative BusDef where
    {-# INLINE pure #-}
    pure a = BusDef [] a
    {-# INLINE (<*>) #-}
    BusDef s1 f <*> BusDef s2 x = BusDef (s1 ++ s2) (f x)

instance Monad BusDef where
    {-# INLINE return #-}
    return = pure
    {-# INLINE (>>=) #-}
    BusDef s1 a >>= k =
        let BusDef s2 b = k a
        in BusDef (s1 ++ s2) b

-- | Extract the specs accumulated by a 'BusDef' in declaration order.
{-# INLINE runBusDef #-}
runBusDef :: BusDef a -> (a, [ComponentSpec])
runBusDef (BusDef specs a) = (a, specs)

-- ---------------------------------------------------------------------------
-- Placement primitives
-- ---------------------------------------------------------------------------

-- | Place a sub-bus at @base@, shifting all its component addresses by that
-- offset.  Composes: @attach 0x100 (attach 0x10 x)@ = @attach 0x110 x@.
{-# INLINE attach #-}
attach :: Word32 -> BusDef a -> BusDef a
attach base (BusDef specs a) = BusDef (map shift specs) a
  where
    shift (SpecROM    b sz n)   = SpecROM    (b + base) sz n
    shift (SpecRAM    b sz n i) = SpecRAM    (b + base) sz n i
    shift (SpecPeriph b sz n i) = SpecPeriph (b + base) sz n i
    shift c                     = c

-- | Lift a pre-wired peripheral handle into a 'BusDef' at offset zero.
-- Always used with 'attach': @attach 0x100 (periph gpioBP)@.
{-# INLINE periph #-}
periph :: BusPeriph p -> BusDef ()
periph bp = BusDef [SpecPeriph 0 (bpSize bp) "" (bpId bp)] ()

-- | RAM region token produced by 'mkRam'.
{-# INLINE ramSegment #-}
ramSegment :: Word32 -> Int -> BusDef ()
ramSegment sz sid = BusDef [SpecRAM 0 sz "" sid] ()

-- | Read-only ROM region token.  Use with 'attach': @attach 0x0000 (romBlock 0x2000)@.
{-# INLINE romBlock #-}
romBlock :: Word32 -> BusDef ()
romBlock sz = BusDef [SpecROM 0 sz ""] ()

-- ---------------------------------------------------------------------------
-- Bus — named bus with explicit architecture
--
-- Wraps a 'BusDef ()' with a phantom architecture type.  The architecture
-- governs how the bus interconnect circuit is synthesised (address decode,
-- stall handling, arbitration, etc.).  A single SoC may contain multiple
-- 'Bus' values with different architecture types.
-- ---------------------------------------------------------------------------

-- | A bus with a specific architecture and address layout.
--
-- @arch@ is a phantom type from "Isacle.System.BusArch" (e.g. 'SimpleBus').
-- Use 'mkBus' to construct and 'getBusDef' to extract the layout.
newtype Bus arch = Bus { getBusDef :: BusDef () }

-- | Attach an architecture to an address layout, producing a named 'Bus'.
--
-- > myDataBus :: Bus SimpleBus
-- > myDataBus = mkBus SimpleBus $ do
-- >   label @"gpio" $ attach 0x60 gpioBusDef
-- >   label @"uart" $ attach 0x40 uartBusDef
{-# INLINE mkBus #-}
mkBus :: arch -> BusDef () -> Bus arch
mkBus _ = Bus

-- ---------------------------------------------------------------------------
-- Metadata combinators
-- ---------------------------------------------------------------------------

-- | Tag all components in the inner 'BusDef' with a type-level name.
-- Inner labels (set by nested 'label' calls) take priority over outer ones.
{-# INLINE label #-}
label :: forall name a. KnownSymbol name
      => BusDef a
      -> BusDef a
label (BusDef specs a) = BusDef (map tag specs) a
  where
    n = symbolVal (Proxy @name)
    tag (SpecROM    b sz "")   = SpecROM    b sz n
    tag (SpecRAM    b sz "" i) = SpecRAM    b sz n i
    tag (SpecPeriph b sz "" i) = SpecPeriph b sz n i
    tag c                      = c

-- | Alias for 'label'; conventionally used for linker section annotations.
{-# INLINE annotate #-}
annotate :: forall name a. KnownSymbol name
         => BusDef a
         -> BusDef a
annotate = label @name
