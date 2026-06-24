{-# LANGUAGE AllowAmbiguousTypes #-}
module Hdl.Class
    ( -- * Circuit typeclass
      Circuit(..)
      -- * NetBuilder: the synthesis interpreter
    , NetBuilder(..)
      -- * Entity instantiation
    , instEntity
      -- * Primitive circuit operations
    , regS
    , regEnS
    , ramS
    , romS
    , primS
    , inputS
    , outputS
      -- * Optional wire naming
    , named
    ) where

import Prelude
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (natVal)

import Hdl.Net
import Hdl.Types
import Hdl.Entity

-- ---------------------------------------------------------------------------
-- Circuit typeclass
-- ---------------------------------------------------------------------------

class Circuit (c :: Type -> Type -> Type) where
    reg :: forall dom a.
           (HdlType a, KnownDom dom)
        => a -> c (Sig dom a) (Sig dom a)

    regEn :: forall dom a.
             (HdlType a, KnownDom dom)
          => a -> c (Sig dom Bool, Sig dom a) (Sig dom a)

newtype NetBuilder i o = NetBuilder { runNetBuilder :: i -> NetM o }

instance Circuit NetBuilder where
    reg   initVal = NetBuilder $ regS   initVal
    regEn initVal = NetBuilder $ uncurry (regEnS initVal)

-- ---------------------------------------------------------------------------
-- Entity instantiation
-- ---------------------------------------------------------------------------

-- | Instantiate an entity as a named sub-instance in the current structural
-- context.  Input signals are connected positionally via 'PortRef'; output
-- wires are returned as a typed bundle.
instEntity :: forall i o.
              (PortRef i, PortRef o)
           => Entity i o
           -> String   -- ^ instance label (e.g. "u_alu")
           -> i        -- ^ parent-side input signals
           -> NetM o
instEntity ent instLabel inputs = do
    inWids <- toWireIds inputs
    let iSpecs = zipWith setN (portNames (Proxy @i)) (portSpecs (Proxy @i))
        oSpecs = zipWith setN (portNames (Proxy @o)) (portSpecs (Proxy @o))
        body   = do
            subIns  <- mapM allocIn iSpecs
            outputs <- runHDL (entityBody ent) (fromWireIds subIns)
            subOuts <- toWireIds outputs
            mapM_ (uncurry emitOut) (zip oSpecs subOuts)
    (_, outPorts) <- inBlock instLabel (entityName ent) inWids body
    return (fromWireIds [ w | (_, w, _) <- outPorts ])
  where
    setN n ps      = ps { portName = n }
    allocIn ps     = do { wid <- freshWire
                        ; emit (NInput wid (portName ps) (portWidth ps) (portDom ps))
                        ; return wid }
    emitOut ps wid = emit (NOutput wid (portName ps) (portWidth ps) (portDom ps))

-- ---------------------------------------------------------------------------
-- NetM primitives
-- ---------------------------------------------------------------------------

inputS :: forall dom a.
          (HdlType a, KnownDom dom)
       => String -> NetM (Sig dom a)
inputS name = do
    wid <- freshWire
    let w = fromIntegral (natVal (Proxy @(Width a)))
    emit $ NInput wid name w (domId (Proxy @dom))
    pure (SWire wid)

outputS :: forall dom a.
           (HdlType a, KnownDom dom)
        => String -> Sig dom a -> NetM ()
outputS name sig = do
    wid <- materialize sig
    let w = fromIntegral (natVal (Proxy @(Width a)))
    emit $ NOutput wid name w (domId (Proxy @dom))

-- | Register with deferred NReg emission so mdo feedback is safe.
regS :: forall dom a.
        (HdlType a, KnownDom dom)
     => a -> Sig dom a -> NetM (Sig dom a)
regS initVal inp = do
    outWid <- freshWire
    let domInfo  = domId (Proxy @dom)
        bitWidth = fromIntegral (natVal (Proxy @(Width a)))
        initBits = SomeBits (toBits initVal) bitWidth
    defer $ do
        inWid <- materialize inp
        emit $ NReg outWid inWid Nothing initBits domInfo
    pure (SWire outWid)

-- | Register with write-enable; same deferred strategy.
regEnS :: forall dom a.
          (HdlType a, KnownDom dom)
       => a -> Sig dom Bool -> Sig dom a -> NetM (Sig dom a)
regEnS initVal en inp = do
    outWid <- freshWire
    let domInfo  = domId (Proxy @dom)
        bitWidth = fromIntegral (natVal (Proxy @(Width a)))
        initBits = SomeBits (toBits initVal) bitWidth
    defer $ do
        enWid <- materialize en
        inWid <- materialize inp
        emit $ NReg outWid inWid (Just enWid) initBits domInfo
    pure (SWire outWid)

-- | Attach a human-readable name hint to a signal.  Optional: the emitter
-- falls back to @wN@ without it.  Use @(>>= named "x")@ for monadic sources
-- like 'regS'; apply directly to combinational expressions.
named :: String -> Sig dom a -> NetM (Sig dom a)
named hint sig = do
    wid <- materialize sig
    hintWire wid hint
    pure (SWire wid)

-- | Synchronous-write / asynchronous-read block RAM.
-- Emits a single 'NMem' node; all ports are materialized immediately.
ramS :: forall dom a addr.
        (HdlType a, KnownDom dom)
     => Int            -- ^ number of entries
     -> [Integer]      -- ^ initial contents (padded with 0)
     -> Sig dom addr   -- ^ read address
     -> Sig dom addr   -- ^ write address
     -> Sig dom a      -- ^ write data
     -> Sig dom Bool   -- ^ write enable
     -> NetM (Sig dom a)
ramS size initVals rdAddr wrAddr wrData wrEn = do
    outWid <- freshWire
    rdA    <- materialize rdAddr
    wrA    <- materialize wrAddr
    wrD    <- materialize wrData
    wrE    <- materialize wrEn
    let datW    = fromIntegral (natVal (Proxy @(Width a)))
        domInfo = domId (Proxy @dom)
    emit $ NMem outWid rdA wrA wrD wrE size datW initVals domInfo
    pure (SWire outWid)

-- | Purely combinational ROM lookup.
-- Emits a single 'NRom' node.
romS :: forall dom a addr.
        HdlType a
     => Int            -- ^ number of entries
     -> [Integer]      -- ^ ROM contents (padded with 0)
     -> Sig dom addr   -- ^ read address
     -> NetM (Sig dom a)
romS size initVals rdAddr = do
    outWid <- freshWire
    rdA    <- materialize rdAddr
    let datW = fromIntegral (natVal (Proxy @(Width a)))
    emit $ NRom outWid rdA size datW initVals
    pure (SWire outWid)

primS :: PrimOp -> [WireId] -> NetM WireId
primS op ins = do
    outWid <- freshWire
    emit $ NComb outWid op ins
    pure outWid
