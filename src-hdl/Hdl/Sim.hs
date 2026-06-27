-- | A simulation interpreter for the 'Signal' typeclass: a second instance
-- alongside the synthesis 'Sig', realizing the tagless-final design — the same
-- combinational signal program is either /synthesised/ (→ NetNode graph, 'Sig')
-- or /computed/ (→ a value, 'SimSig').
--
-- 'SimSig' carries the bit-pattern value, its width, and its representation
-- ('Repr'), so the 'Signal' operations compute exactly what the emitter would
-- produce (signed-vs-unsigned comparison via the representation, etc.).
module Hdl.Sim
    ( -- * Signal-level interpreter
      SimSig(..)
    , simLit
    , simResult
    , evalSimOp
      -- * Graph-level interpreter
    , simulateDesign
      -- * Whole-system: flatten a hierarchical design, then simulate
    , flattenDesign
    , simulateSystem
    ) where

import Prelude
import Data.Bits ((.&.), (.|.), xor, shiftL, shiftR)
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (natVal)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Control.Monad.State.Strict (State, evalState, get, put)

import Hdl.Net
    ( PrimOp(..), Repr(..), WireId, NetNode(..), SomeBits(..)
    , Design, localEntityName )
import Hdl.Types (Signal(..), HdlType(..))

-- | A simulated combinational signal: @SimSig value width repr@.
data SimSig (dom :: k) (a :: Type) = SimSig !Integer !Int !Repr
    deriving (Eq, Show)

-- | Inject an 'HdlType' value: bit pattern from 'toBits', width from 'Width',
-- representation from 'hdlRepr'.
simLit :: forall a dom. HdlType a => a -> SimSig dom a
simLit x = SimSig (toBits x) (fromIntegral (natVal (Proxy @(Width a)))) (hdlRepr (Proxy @a))

-- | Read a simulated signal back as its 'HdlType' value.
simResult :: HdlType a => SimSig dom a -> a
simResult (SimSig v _ _) = fromBits v

instance Signal SimSig where
    sigPrim1 op (SimSig a wa ra)                    = mk (evalSimOp op [(a, wa, ra)])
    sigPrim2 op (SimSig a wa ra) (SimSig b wb rb)   = mk (evalSimOp op [(a, wa, ra), (b, wb, rb)])
    sigPrim3 op (SimSig a wa ra) (SimSig b wb rb) (SimSig c wc rc) =
        mk (evalSimOp op [(a, wa, ra), (b, wb, rb), (c, wc, rc)])
    sigLitW v w = SimSig (v .&. mask w) w RUnsigned

-- | Numeric literals/arithmetic in simulation (mirrors the 'Num (Sig dom a)'
-- synthesis instance, on actual values).
instance (HdlType a, Num a) => Num (SimSig dom a) where
    a + b         = sigPrim2 PAdd a b
    a - b         = sigPrim2 PSub a b
    a * b         = sigPrim2 PMul a b
    negate a      = sigPrim1 PNot a
    abs           = id
    signum _      = fromInteger 1
    fromInteger n = simLit (fromInteger n :: a)

mk :: (Integer, Int, Repr) -> SimSig dom a
mk (v, w, r) = SimSig v w r

mask :: Int -> Integer
mask w = (1 `shiftL` w) - 1

-- ---------------------------------------------------------------------------
-- Graph-level interpreter: simulate a flat entity over clock cycles
-- ---------------------------------------------------------------------------

-- | Simulate one entity's flat 'NetNode' list for @nCycles@ cycles, given
-- constant input-port values (port name → value).  Returns the output-port
-- values for each cycle (index 0 = the reset/initial register state).  Handles
-- combinational nodes, registers (with enable), and representation tags;
-- memories, ROMs, and sub-instances are not yet evaluated.
simulateDesign :: [NetNode] -> Map String Integer -> Int -> [Map String Integer]
simulateDesign nodes inputs nCycles = go (initRegs, memInit) nCycles
  where
    reprOfW w  = Map.findWithDefault RUnsigned w (Map.fromList [ (rw, r) | NRepr rw r <- nodes ])
    regWidth   = Map.fromList [ (nOut n, sbW)   | n@NReg{} <- nodes, let SomeBits _ sbW = nInit n ]
    initRegs   = Map.fromList [ (nOut n, sbVal) | n@NReg{} <- nodes, let SomeBits sbVal _ = nInit n ]
    inputWires = [ (nPortName n, nOut n, nWidth n) | n@NInput{} <- nodes ]
    combs      = [ n | n@NComb{} <- nodes ]

    memInit = Map.fromList
        [ (nOut n, Map.fromList (zip [0 :: Integer ..] (nMemInit n))) | n@NMem{} <- nodes ]
    romData = Map.fromList
        [ (nOut n, Map.fromList (zip [0 :: Integer ..] (nRomInit n))) | n@NRom{} <- nodes ]

    go _              0 = []
    go (regs, mems) k = outs : go (regs', mems') (k - 1)
      where
        seed = Map.fromList $
            [ (wid, ((inputs Map.! nm) .&. mask wdt, wdt, reprOfW wid))
            | (nm, wid, wdt) <- inputWires, Map.member nm inputs ]
            ++
            [ (wid, (v, Map.findWithDefault 1 wid regWidth, reprOfW wid))
            | (wid, v) <- Map.toList regs ]
        full = solve mems seed
        outs = Map.fromList
            [ (nPortName n, v) | n@NOutput{} <- nodes
            , Just (v, _, _) <- [Map.lookup (nIn n) full] ]
        regs' = Map.fromList [ (nOut n, nextOf n) | n@NReg{} <- nodes ]
        nextOf n
            | not (enabled n full) = Map.findWithDefault 0 (nOut n) regs
            | otherwise = case Map.lookup (nIn n) full of
                Just (x, _, _) -> x
                Nothing        -> Map.findWithDefault 0 (nOut n) regs
        -- RAM writes: registered (rising edge) when the write enable is high.
        mems' = Map.mapWithKey stepMem mems
        stepMem memW st = case [ n | n@NMem{} <- nodes, nOut n == memW ] of
            (n:_) | Just (en,_,_) <- Map.lookup (nMemWrEn n) full, en /= 0
                  , Just (a,_,_)  <- Map.lookup (nMemWrA  n) full
                  , Just (d,_,_)  <- Map.lookup (nMemWrD  n) full
                      -> Map.insert a (d .&. mask (nMemDatW n)) st
            _         -> st

    enabled n full = case nEn n of
        Nothing  -> True
        Just enW -> maybe True (\(ev, _, _) -> ev /= 0) (Map.lookup enW full)

    roms     = [ n | n@NRom{} <- nodes ]
    memNodes = [ n | n@NMem{} <- nodes ]

    -- Fixpoint: evaluate combinational nodes, ROM lookups, and RAM reads until no
    -- new wire resolves.
    solve memState known =
        let kn1 = foldl tryComb known combs
            kn2 = foldl tryRom  kn1   roms
            kn3 = foldl tryMem  kn2   memNodes
            tryComb kn n
                | Map.member (nOut n) kn = kn
                | otherwise = maybe kn
                    (\ops -> Map.insert (nOut n) (evalSimOp (nOp n) ops) kn)
                    (mapM (`Map.lookup` kn) (nIns n))
            tryRom kn n
                | Map.member (nOut n) kn = kn
                | otherwise = case Map.lookup (nRomRdA n) kn of
                    Just (addr, _, _) ->
                        let v = Map.findWithDefault 0 addr
                                  (Map.findWithDefault Map.empty (nOut n) romData)
                        in Map.insert (nOut n) (v .&. mask (nRomDatW n), nRomDatW n, RUnsigned) kn
                    Nothing -> kn
            tryMem kn n
                | Map.member (nOut n) kn = kn
                | otherwise = case Map.lookup (nMemRdA n) kn of
                    Just (addr, _, _) ->
                        let v = Map.findWithDefault 0 addr
                                  (Map.findWithDefault Map.empty (nOut n) memState)
                        in Map.insert (nOut n) (v .&. mask (nMemDatW n), nMemDatW n, RUnsigned) kn
                    Nothing -> kn
        in if Map.size kn3 == Map.size known then known else solve memState kn3

asSigned :: Integer -> Int -> Bool -> Integer
asSigned v w True  | v >= 1 `shiftL` (w - 1) = v - (1 `shiftL` w)
asSigned v _ _     = v

-- | Evaluate a 'PrimOp' on @(value, width, repr)@ operands, matching the VHDL
-- emitter's semantics.  Returns @(value, width, repr)@.
evalSimOp :: PrimOp -> [(Integer, Int, Repr)] -> (Integer, Int, Repr)
evalSimOp op ins = case (op, ins) of
    (PLit v w, [])                          -> (v .&. mask w, w, RUnsigned)
    (PAdd, [(a, wa, ra), (b, wb, _)])       -> let w = max wa wb in ((a + b) .&. mask w, w, ra)
    (PSub, [(a, wa, ra), (b, wb, _)])       -> let w = max wa wb in ((a - b) .&. mask w, w, ra)
    (PMul, [(a, wa, ra), (b, wb, _)])       -> let w = max wa wb in ((a * b) .&. mask w, w, ra)
    (PAnd, [(a, wa, ra), (b, wb, _)])       -> let w = max wa wb in (a .&. b, w, ra)
    (POr,  [(a, wa, ra), (b, wb, _)])       -> let w = max wa wb in (a .|. b, w, ra)
    (PXor, [(a, wa, ra), (b, wb, _)])       -> let w = max wa wb in (a `xor` b, w, ra)
    (PNot, [(a, wa, ra)])                   -> (mask wa `xor` a, wa, ra)
    (PMux, [(s, _, _), (t, wt, rt), (f, wf, _)])
                                            -> (if s /= 0 then t else f, max wt wf, rt)
    (PEq,  [(a, _, _), (b, _, _)])          -> (if a == b then 1 else 0, 1, RUnsigned)
    (PLt,  [(a, wa, ra), (b, wb, rb)])      ->
        let signed = ra == RSigned || rb == RSigned
        in (if asSigned a wa signed < asSigned b wb signed then 1 else 0, 1, RUnsigned)
    (PSlice hi lo, [(a, _, _)])             -> ((a `shiftR` lo) .&. mask (hi - lo + 1), hi - lo + 1, RUnsigned)
    (PConcat, [(a, wa, _), (b, wb, _)])     -> ((a `shiftL` wb) .|. b, wa + wb, RUnsigned)
    (PResize w, [(a, wa, ra)])              -> (a .&. mask w, w, if w < wa then RUnsigned else ra)
    (PSignedResize w, [(a, wa, _)])         -> (asSigned a wa True .&. mask w, w, RUnsigned)
    (PReinterpret r, [(a, wa, _)])          -> (a, wa, r)
    (PShiftL, [(a, wa, ra), (n, _, _)])     -> ((a `shiftL` fromInteger n) .&. mask wa, wa, ra)
    (PShiftR, [(a, wa, ra), (n, _, _)])     -> (a `shiftR` fromInteger n, wa, ra)
    _                                       -> error ("evalSimOp: unhandled " ++ show op)

-- ---------------------------------------------------------------------------
-- Whole-system: flatten a hierarchical Design into one node list
-- ---------------------------------------------------------------------------

-- | Inline every sub-instance of the named top entity into a single flat
-- 'NetNode' list (wire IDs of each instance offset to avoid collision; ports
-- connected by aliasing).  The result can be fed to 'simulateDesign', so a whole
-- synthesized SoC is simulable in Haskell.
flattenDesign :: Design -> String -> [NetNode]
flattenDesign design top = evalState (go 0 top) 1
  where
    stride :: WireId
    stride = 1 + maximum (0 : concatMap (concatMap nodeWires) (Map.elems design))

    go :: WireId -> String -> State Int [NetNode]
    go base name =
        concat <$> mapM expand (map (mapWires (+ base)) (Map.findWithDefault [] name design))

    expand :: NetNode -> State Int [NetNode]
    expand (NSubInst _ ent ip op) = do
        i <- get; put (i + 1)
        case localEntityName ent of
            Nothing  -> pure []
            Just sub -> do
                subNodes <- go (i * stride) sub
                let inMap  = Map.fromList ip
                    outMap = Map.fromList [ (p, w) | (p, w, _) <- op ]
                pure (concatMap (connect inMap outMap) subNodes)
    expand n = pure [n]

    -- A sub-entity's input port becomes a wire driven by the parent; its output
    -- port drives the parent's wire.  (Single-input POr is an identity alias.)
    connect inMap outMap n = case n of
        NInput  w p _ _ | Just pw <- Map.lookup p inMap  -> [NComb w  POr [pw, pw]]
        NOutput s p _ _ | Just pw <- Map.lookup p outMap -> [NComb pw POr [s,  s ]]
        _                                                -> [n]

-- | Flatten a hierarchical design and simulate its top entity for @nCycles@.
simulateSystem :: Design -> String -> Map String Integer -> Int -> [Map String Integer]
simulateSystem design top = simulateDesign (flattenDesign design top)

-- | Re-map every wire id in a node.
mapWires :: (WireId -> WireId) -> NetNode -> NetNode
mapWires f n = case n of
    NReg o i e ini d            -> NReg (f o) (f i) (fmap f e) ini d
    NComb o op ins              -> NComb (f o) op (map f ins)
    NInput o nm w d             -> NInput (f o) nm w d
    NOutput i nm w d            -> NOutput (f i) nm w d
    NSubInst nm en ip op        -> NSubInst nm en [ (p, f w) | (p, w) <- ip ]
                                                  [ (p, f w, wd) | (p, w, wd) <- op ]
    NMem o ra wa wd we sz dw ix d -> NMem (f o) (f ra) (f wa) (f wd) (f we) sz dw ix d
    NRom o ra sz dw ix          -> NRom (f o) (f ra) sz dw ix
    NHint w nm                  -> NHint (f w) nm
    NRepr w r                   -> NRepr (f w) r
    NComment t                  -> NComment t
    NGroup nm fs                -> NGroup nm [ (fn, f w) | (fn, w) <- fs ]

-- | Every wire id referenced by a node (for computing the offset stride).
nodeWires :: NetNode -> [WireId]
nodeWires n = case n of
    NReg o i e _ _              -> o : i : maybe [] pure e
    NComb o _ ins               -> o : ins
    NInput o _ _ _              -> [o]
    NOutput i _ _ _             -> [i]
    NSubInst _ _ ip op          -> map snd ip ++ [ w | (_, w, _) <- op ]
    NMem o ra wa wd we _ _ _ _  -> [o, ra, wa, wd, we]
    NRom o ra _ _ _             -> [o, ra]
    NHint w _                   -> [w]
    NRepr w _                   -> [w]
    NComment _                  -> []
    NGroup _ fs                 -> map snd fs
