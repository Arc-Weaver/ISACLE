module Hdl.Emit.Vhdl
    ( emitVhdl
    , emitVhdlFile
    , emitVhdlDesign
    , emitVhdlDesignFiles
    , emitEntity
    ) where

import Prelude
import Data.Bits (testBit, xor, (.&.))
import Data.Char (toLower)
import Data.List (dropWhileEnd, elemIndex, foldl', intercalate, nub, partition, sort, tails)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.FilePath ((</>))
import Hdl.Net
import Hdl.Entity (ElabEntity(..))

-- | VHDL-2008 reserved words that must not be used as signal identifiers.
vhdlReserved :: Set.Set String
vhdlReserved = Set.fromList
    [ "abs","access","after","alias","all","and","architecture","array"
    , "assert","attribute","begin","block","body","buffer","bus","case"
    , "component","configuration","constant","context","cover","default"
    , "disconnect","downto","else","elsif","end","entity","exit","fairness"
    , "file","for","force","function","generate","generic","group","guarded"
    , "if","impure","in","inertial","inout","is","label","library","linkage"
    , "literal","loop","map","mod","nand","new","next","nor","not","null"
    , "of","on","open","or","others","out","package","parameter","port"
    , "postponed","procedure","process","property","protected","pure"
    , "range","record","register","reject","release","rem","report"
    , "restrict","return","rol","ror","select","sequence","severity"
    , "signal","shared","sla","sll","sra","srl","subtype","then","to"
    , "transport","type","unaffected","units","until","use","variable"
    , "vmode","vprop","vunit","wait","when","while","with","xnor","xor"
    ]

-- ---------------------------------------------------------------------------
-- Minimal VHDL type IR
-- ---------------------------------------------------------------------------

data VType
    = VStdLogic
    | VUnsigned  Int               -- unsigned(n-1 downto 0)
    | VSigned    Int               -- signed(n-1 downto 0)
    | VArrayOf   Int VType         -- array(0 to n-1) of elem
    | VRecord    [(String, VType)] -- record ... end record
    | VEnumRef   String            -- reference to a declared enumerated type
    | VTypeRef   String            -- reference to any declared named type
    | VEnum      [String]          -- (lit0, lit1, …)  — the enum type body

ppType :: VType -> String
ppType VStdLogic         = "std_logic"
ppType (VUnsigned n)     = "unsigned(" ++ show (n-1) ++ " downto 0)"
ppType (VSigned n)       = "signed(" ++ show (n-1) ++ " downto 0)"
ppType (VArrayOf n t)    = "array(0 to " ++ show (n-1) ++ ") of " ++ ppType t
ppType (VRecord fields)  = "record\n"
    ++ concatMap (\(n, t) -> "    " ++ n ++ " : " ++ ppType t ++ ";\n") fields
    ++ "  end record"
ppType (VEnumRef name)   = name
ppType (VTypeRef name)   = name
ppType (VEnum lits)      = "(" ++ intercalate ", " lits ++ ")"

-- | Stable VHDL type name for an enum, derived from its literals (so the same
-- enum used on many wires declares one shared type).
enumTypeName :: [String] -> String
enumTypeName lits = "enum_" ++ intercalate "_" lits ++ "_t"

-- | Convert a wire bit-width to the appropriate scalar VHDL type.
wireVType :: Int -> VType
wireVType 1 = VStdLogic
wireVType n = VUnsigned n

-- | Like 'wireVType' but honours a wire's representation tag: a signed wire of
-- width > 1 becomes @signed(..)@, so numeric_std overloading gives signed
-- arithmetic\/comparison\/resize for free.  Untagged (the default) and 1-bit
-- wires keep the plain 'wireVType' behaviour.
wireVTypeR :: Repr -> Int -> VType
wireVTypeR (REnum lits) _     = VEnumRef (enumTypeName lits)
wireVTypeR RSigned      n | n > 1 = VSigned n
wireVTypeR _            n         = wireVType n

-- | A wire's representation tag.  An explicit 'NRepr' tag wins; otherwise the
-- tag propagates from the operand a combinational op preserves the numeric
-- interpretation of (like 'inferWidth' for widths), bottoming out at untagged
-- leaves as 'RUnsigned'.  Only leaves (ports, registers) need explicit tags.
reprOf :: WireId -> [NetNode] -> Repr
reprOf wid nodes =
    case [ r | NRepr w r <- nodes, w == wid ] of
      (r:_) -> r
      []    -> case [ (op, ins) | NComb o op ins <- nodes, o == wid ] of
                 ((PReinterpret r, _):_) -> r   -- a cast wire's repr is its target
                 ((op, ins):_) -> maybe RUnsigned (`reprOf` nodes) (reprSource op ins)
                 []            -> RUnsigned
  where
    -- the operand a combinational op inherits its representation from
    reprSource :: PrimOp -> [WireId] -> Maybe WireId
    reprSource PAdd        (a:_)   = Just a
    reprSource PSub        (a:_)   = Just a
    reprSource PMul        (a:_)   = Just a
    reprSource (PResize _) (a:_)   = Just a
    reprSource PMux        (_:t:_) = Just t   -- skip the 1-bit selector
    reprSource PShiftL     (a:_)   = Just a
    reprSource PShiftR     (a:_)   = Just a
    reprSource _           _       = Nothing

-- ---------------------------------------------------------------------------
-- Architecture declarations
-- ---------------------------------------------------------------------------

-- | A VHDL architecture-region declaration.
-- The type field in 'VDConst' and 'VDSig' is a rendered type string so that
-- both structural types and named type aliases can be used uniformly.
data VDecl
    = VDType  String VType         -- type <name> is <type>;
    | VDConst String String String -- constant <name> : <type> := <value>;
    | VDSig   String String (Maybe String) -- signal <name> : <type> [:= <init>];

ppDecl :: VDecl -> String
ppDecl (VDType  n t)         = "  type " ++ n ++ " is " ++ ppType t ++ ";"
ppDecl (VDConst n t v)       = "  constant " ++ n ++ " : " ++ t ++ " := " ++ v ++ ";"
ppDecl (VDSig   n t Nothing) = "  signal " ++ n ++ " : " ++ t ++ ";"
ppDecl (VDSig   n t (Just v))= "  signal " ++ n ++ " : " ++ t ++ " := " ++ v ++ ";"

-- ---------------------------------------------------------------------------
-- Memory naming — type alias and signal/constant names per NMem/NRom node
-- ---------------------------------------------------------------------------

-- | Derive a human-readable base name for an NMem array from the hint on its
-- output wire (e.g. hint "GPR_rd0" → base "gpr"), falling back to "ram_<wid>".
-- The hint convention set by SynthCPU is "<RfName>_rd<slot>"; strip the suffix.
memBaseName :: WireId -> NameMap -> String
memBaseName wid nm =
    case Map.lookup wid nm of
        -- A semantic hint ("GPR_rd0" → "gpr"); but an internal "w<id>" name is
        -- the read wire's own name, so the array must get a distinct name or it
        -- collides with the wire (duplicate signal + self-reference).
        Just h | not (isInternalName h) -> map toLower (takeWhile (/= '_') h)
        _                               -> "ram_" ++ show wid
  where
    isInternalName ('w':rest) = not (null rest) && all (`elem` ['0'..'9']) rest
    isInternalName _          = False

memTypeName :: WireId -> NameMap -> String
memTypeName wid nm = memBaseName wid nm ++ "_t"

memSigName :: WireId -> NameMap -> String
memSigName wid nm = memBaseName wid nm

-- NRom: constant <entity>_rom : <entity>_rom_t := (...);
--
-- The ROM array/constant is named after the ENTITY (a peripheral instance, e.g.
-- @coderom0@), not the transient wire id — a stable, unique name a vendor tool
-- (Xilinx @updatemem@/@data2mem@, or a Tcl @INIT@ reload) can target to refresh
-- the memory contents post-synthesis.  An entity with more than one ROM
-- disambiguates by index (@<entity>_rom0@, @<entity>_rom1@, …).
romBaseName :: String -> [NetNode] -> WireId -> String
romBaseName name nodes wid =
    name ++ "_rom" ++ case roms of
        [_] -> ""
        _   -> maybe "" show (elemIndex wid roms)
  where
    roms = [ nOut n | n@NRom{} <- nodes ]

romSigName :: String -> [NetNode] -> WireId -> String
romSigName = romBaseName

romTypeName :: String -> [NetNode] -> WireId -> String
romTypeName name nodes wid = romBaseName name nodes wid ++ "_t"

-- | Build a VHDL aggregate initializer for an array of @sz@ elements.
initAggregate :: Int -> Int -> [Integer] -> String
initAggregate sz dw vs =
    "(" ++ intercalate ", " (map (ppLit . flip SomeBits dw) padded) ++ ")"
  where
    padded = take sz (vs ++ repeat 0)

-- ---------------------------------------------------------------------------
-- Port declarations
-- ---------------------------------------------------------------------------

data VPortDir = VIn | VOut

ppPortLine :: VPortDir -> String -> VType -> String
ppPortLine VIn  n t = n ++ " : in "  ++ ppType t
ppPortLine VOut n t = n ++ " : out " ++ ppType t

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Emit one VHDL file for the named entity.
emitVhdl :: Design -> String -> [NetNode] -> String
emitVhdl design name nodes = unlines (header ++ body)
  where
    nodes' = cse nodes
    nm     = buildNameMap nodes'
    pkgTypes = packageTypeDecls nodes'
    pkgName  = name ++ "_types"
    ieeeCtx  = [ "library ieee;"
               , "use ieee.std_logic_1164.all;"
               , "use ieee.numeric_std.all;" ]
    -- Structured types (records/enums) go in a per-file package so they are
    -- visible to the entity ports; the entity 'use' clause also covers the
    -- architecture.  Designs with no structured types emit byte-identically.
    header
      | null pkgTypes = ieeeCtx ++ [ "" ]
      | otherwise     = ieeeCtx ++ [ "" ]
          ++ [ "package " ++ pkgName ++ " is" ]
          ++ map ppDecl pkgTypes
          ++ [ "end package " ++ pkgName ++ ";", "" ]
          ++ ieeeCtx
          ++ [ "use work." ++ pkgName ++ ".all;", "" ]
    body =
      [ entityDecl design name nodes'
      , ""
      , architectureDecl design name nm nodes'
      ]

emitVhdlFile :: FilePath -> String -> [NetNode] -> IO ()
emitVhdlFile path name nodes = writeFile path (emitVhdl Map.empty name nodes)

-- | Emit every entity in a 'Design' to a map of entity-name → VHDL text.
-- Memory is now inlined behaviorally, so no separate RAM/ROM entity files.
emitVhdlDesign :: Design -> Map.Map String String
emitVhdlDesign design = Map.mapWithKey (emitVhdl design) design

-- | Write each entity in a 'Design' to @dir/<entityName>.vhd@.
emitVhdlDesignFiles :: FilePath -> Design -> IO ()
emitVhdlDesignFiles dir design =
    mapM_ (\(name, vhdl) -> writeFile (dir </> name ++ ".vhd") vhdl)
          (Map.toList (emitVhdlDesign design))

-- | Emit VHDL for a fully elaborated 'ElabEntity'.
-- Port declarations come from 'elabPorts'; clocks are derived from the nodes.
emitEntity :: ElabEntity -> String
emitEntity elab = emitVhdl Map.empty (elabName elab) (elabNodes elab)

-- | All distinct clock domains required by an entity (including sub-entities).
allClockDomains :: Design -> [NetNode] -> [DomId]
allClockDomains design nodes = nub (directClocks ++ subClocks)
  where
    directClocks = [ nDom n | n@NReg{} <- nodes ]
                ++ [ nDom n | n@NMem{} <- nodes ]
                ++ [ nrfDom n | n@NRegFile{} <- nodes ]
    subClocks    = concatMap lookupChild [ entRef | NSubInst _ entRef _ _ <- nodes ]
    lookupChild entRef = case localEntityName entRef >>= (`Map.lookup` design) of
        Nothing    -> []
        Just child -> allClockDomains design child

-- ---------------------------------------------------------------------------
-- Common subexpression elimination
-- ---------------------------------------------------------------------------

-- | Peephole identity rules: return the canonical wire when the operation
-- is a no-op (a or a, a and a), so the node can be dropped entirely.
identityWire :: PrimOp -> [WireId] -> Maybe WireId
identityWire POr  [a, b] | a == b = Just a
identityWire PAnd [a, b] | a == b = Just a
identityWire _    _               = Nothing

-- | Peephole constant-fold rules: rewrite the operation to a PLit constant.
-- Returns the new (op, ins) to substitute in-place so PLit CSE deduplicates it.
constantFold :: [NetNode] -> PrimOp -> [WireId] -> Maybe (PrimOp, [WireId])
constantFold allNodes PXor [a, b] | a == b =
    let w = inferWidth a allNodes
    in Just (PLit 0 w, [])
constantFold _ _ _ = Nothing

cse :: [NetNode] -> [NetNode]
cse nodes = map (rewrite finalSubst) kept
  where

    wireHints :: Map.Map WireId String
    wireHints = Map.fromList [ (nHintWire n, nHintName n) | n@NHint{} <- nodes ]

    (kept, finalSubst) = go Map.empty Map.empty Map.empty nodes

    go subst _ _ [] = ([], subst)
    go subst comb reg (n : rest) = case n of
        NComb out op ins ->
            let ins' = map (sub subst) ins
                isHinted = Map.member out wireHints
            in case identityWire op ins' of
                Just canon | not isHinted ->
                    -- a op a → a: drop node, substitute out → canon
                    go (Map.insert out canon subst) comb reg rest
                _ -> case constantFold nodes op ins' of
                    Just (op', ins'') ->
                        -- rewrite to constant, re-queue so PLit CSE deduplicates it
                        go subst comb reg (NComb out op' ins'' : rest)
                    Nothing ->
                        let key = (op, ins')
                        in case Map.lookup key comb of
                            Just canon | not isHinted ->
                                go (Map.insert out canon subst) comb reg rest
                            _ ->
                                let (kept', s) = go subst (Map.insert key out comb) reg rest
                                in (NComb out op (map (sub s) ins') : kept', s)
        NReg out inp en initV dom ->
            let inp' = sub subst inp
                en'  = fmap (sub subst) en
                hint = Map.lookup out wireHints
                key  = (inp', en', sbValue initV, sbWidth initV, domName dom, hint)
            in case Map.lookup key reg of
                Just canon ->
                    go (Map.insert out canon subst) comb reg rest
                Nothing    ->
                    let (kept', s) = go subst comb (Map.insert key out reg) rest
                    in (NReg out (sub s inp') (fmap (sub s) en') initV dom : kept', s)
        _ ->
            let (kept', s) = go subst comb reg rest
            in (rewrite s (rewrite subst n) : kept', s)

    sub subst w = fromMaybe w (Map.lookup w subst)

    rewrite subst n = case n of
        NComb out op ins ->
            NComb out op (map (sub subst) ins)
        NReg out inp en initV dom ->
            NReg out (sub subst inp) (fmap (sub subst) en) initV dom
        NOutput inp name w dom ->
            NOutput (sub subst inp) name w dom
        NSubInst inst entRef ins outs ->
            NSubInst inst entRef [(p, sub subst w) | (p, w) <- ins] outs
        NHint w name ->
            NHint (sub subst w) name
        NRepr w r ->
            NRepr (sub subst w) r
        NMem out rdA wrA wrD wrEn sz dw ini dom ->
            NMem out (sub subst rdA) (sub subst wrA)
                     (sub subst wrD) (sub subst wrEn) sz dw ini dom
        NRom out rdA sz dw ini ->
            NRom out (sub subst rdA) sz dw ini
        NGroup name fields ->
            NGroup name [(fn, sub subst w) | (fn, w) <- fields]
        NRegFile g fld c w wr dom ->
            NRegFile g fld c w [(sub subst a, sub subst d, sub subst e) | (a, d, e) <- wr] dom
        NRegFileRead out g fld a c ->
            NRegFileRead out g fld (sub subst a) c
        _ -> n

-- ---------------------------------------------------------------------------
-- Wire naming
-- ---------------------------------------------------------------------------

type NameMap = Map.Map WireId String

buildNameMap :: [NetNode] -> NameMap
buildNameMap nodes = Map.unions [inputMap, groupMap, hintMap, litMap, regMap, internalMap]
  where
    inputMap = Map.fromList [(nOut n, nPortName n) | n@NInput{} <- nodes]

    -- Group fields take precedence: wire → "groupName.fieldName"
    groupMap = Map.fromList
        [ (wid, grpName ++ "." ++ fldName)
        | NGroup grpName fields <- nodes
        , (fldName, wid) <- fields ]

    portNames = Set.fromList $
        [ nPortName n | n@NInput{}  <- nodes ] ++
        [ nPortName n | n@NOutput{} <- nodes ]
    groupedWires = Map.keysSet groupMap

    -- Per-wire chosen hint (last hint on a wire wins, as before).
    wireHint = Map.fromList
        [ (nHintWire n, safeHint (nHintWire n) (nHintName n))
        | n@NHint{} <- nodes
        , not (isLitWire (nHintWire n) nodes)
        , not (Set.member (nHintWire n) groupedWires) ]

    -- Names already claimed by ports, group records, literal constants and
    -- output-register aliases — these are fixed, so colliding hints defer.
    reservedNames = Set.unions
        [ portNames
        , Set.fromList (Map.elems groupMap)
        , Set.fromList (Map.elems litMap)
        , Set.fromList (Map.elems regMap)
        ]

    -- Two distinct wires must never share a VHDL signal name (it would emit a
    -- duplicate declaration / multiple drivers).  Many hints legitimately
    -- derive the same readable name across instructions (e.g. a per-instruction
    -- GPR read forward, or @GPR_d_sub_GPR_r@ in several ALU ops), so suffix
    -- collisions with _2, _3, … while keeping the first occurrence pristine.
    hintMap = fst $ foldl' assign (Map.empty, reservedNames) (Map.toAscList wireHint)
      where
        assign (m, used) (wid, base) =
            let nm = uniqueName base used
            in (Map.insert wid nm m, Set.insert nm used)
        uniqueName base used
            | not (Set.member base used) = base
            | otherwise                  = go (2 :: Int)
          where go i = let c = base ++ "_" ++ show i
                       in if Set.member c used then go (i + 1) else c

    -- VHDL identifiers are case-insensitive, so a reserved word must be matched
    -- regardless of case (e.g. "PORT" is as reserved as "port"); vhdlReserved is
    -- all-lowercase, so fold the hint before testing.
    safeHint wid h
        | Set.member h portNames               = h ++ if isReg wid then "_r" else "_s"
        | Set.member (map toLower h) vhdlReserved = h ++ "_s"
        | otherwise                            = h
    isReg wid = any (\case { NReg out _ _ _ _ -> out == wid; _ -> False }) nodes

    litMap = Map.fromList
        [ (nOut n, litConstName v bw)
        | n@(NComb _ (PLit v bw) []) <- nodes ]

    outputDrivers = Map.fromList [(nIn n, nPortName n) | n@NOutput{} <- nodes]
    regMap = Map.fromList
        [ (nOut n, "r_" ++ pname)
        | n@NReg{} <- nodes
        , Just pname <- [Map.lookup (nOut n) outputDrivers]
        ]

    realNames = Map.unions [inputMap, groupMap, hintMap, litMap, regMap]
    namedWids = Map.keysSet realNames
    internalWids = sort [ wid | wid <- nub (concatMap drivenWires nodes)
                              , not (Set.member wid namedWids) ]
    destBase = destinationNames nodes realNames (Set.fromList internalWids)
    internalMap = deriveInternalNames nodes realNames destBase
                    (Set.fromList (Map.elems realNames)) internalWids

    drivenWires (NReg    out _ _ _ _) = [out]
    drivenWires (NComb   out _ _)     = [out]
    drivenWires (NSubInst _ _ _ outs) = [w | (_, w, _) <- outs]
    drivenWires n@NMem{}              = [nOut n]
    drivenWires n@NRom{}              = [nOut n]
    drivenWires (NRegFileRead out _ _ _ _) = [out]
    drivenWires _                     = []

litConstName :: Integer -> Int -> String
litConstName v bw = "C_" ++ show v ++ "_" ++ show bw

isLitWire :: WireId -> [NetNode] -> Bool
isLitWire wid = any $ \case
    NComb out (PLit _ _) [] -> out == wid
    _                       -> False

lookupWire :: NameMap -> WireId -> String
lookupWire nm wid = fromMaybe ("w" ++ show wid) (Map.lookup wid nm)

-- ---------------------------------------------------------------------------
-- Readable names for internal wires (derived from op + operands)
-- ---------------------------------------------------------------------------

-- | Give every un-hinted internal wire a name built from its driver op and its
-- operands' names, rather than an opaque @wN@.  Processed in wire-id order (a
-- node's operands have smaller ids, so they are already named); de-duplicated
-- against real names and each other, length-capped, and reserved-word-safe.
deriveInternalNames :: [NetNode] -> Map.Map WireId String -> Map.Map WireId String
                    -> Set.Set String -> [WireId] -> Map.Map WireId String
deriveInternalNames nodes realNames destBase reserved0 wids =
    fst $ foldl' step (Map.empty, reserved0) wids
  where
    combMap = Map.fromList [ (o, (op, ins))         | NComb o op ins       <- nodes ]
    rfrMap  = Map.fromList [ (o, g ++ "_" ++ f ++ "_rd") | NRegFileRead o g f _ _ <- nodes ]
    subMap  = Map.fromList [ (w, inst ++ "_" ++ pn)
                           | NSubInst inst _ _ outs <- nodes, (pn, w, _) <- outs ]
    step (m, used) wid =
        let nameOf w = case Map.lookup w realNames of
                         Just s  -> s
                         Nothing -> Map.findWithDefault ("w" ++ show w) w m
            content = case Map.lookup wid combMap of
                        Just (op, ins) -> deriveOp nameOf op ins
                        Nothing -> case Map.lookup wid rfrMap of
                                     Just s  -> s
                                     Nothing -> Map.findWithDefault ("w" ++ show wid) wid subMap
            -- A content name built from a long chain (a wide mux head, an
            -- orReduce accumulator) reads worse than @wN@; fall back to the
            -- destination it feeds (e.g. @cpu_core_next@) when one is known.
            base | length content > 45, Just d <- Map.lookup wid destBase = d
                 | otherwise                                              = content
            nm'  = uniqueName (reservedSafe (capIdent base)) used
        in (Map.insert wid nm' m, Set.insert nm' used)
    uniqueName base used
        | not (Set.member base used) = base
        | otherwise                  = go (2 :: Int)
      where go i = let c = base ++ "_" ++ show i
                   in if Set.member c used then go (i + 1) else c

-- | Propagate a destination name (the register next-state, register-file port,
-- or output a wire ultimately feeds) backward through single-fanout
-- combinational cones.  Used only when a wire's own content name is unwieldy.
destinationNames :: [NetNode] -> Map.Map WireId String -> Set.Set WireId
                 -> Map.Map WireId String
destinationNames nodes realNames internal =
    foldl' step direct (reverse (sort (Set.toList internal)))
  where
    combMap = Map.fromList [ (o, ins) | NComb o _ ins <- nodes ]
    fanout  = Map.fromListWith (+) [ (w, 1 :: Int) | n <- nodes, (w, _) <- consumerRefs n ]
    single v = Map.findWithDefault 0 v fanout == 1
    direct = Map.fromList $ concat $
        [ regInputs r                                   | r@NReg{}    <- nodes ]
        ++ [ [(nIn o, nPortName o ++ "_o")]             | o@NOutput{} <- nodes ]
        ++ [ rfPorts r                                  | r@NRegFile{} <- nodes ]
    regInputs r = case Map.lookup (nOut r) realNames of
        Just rn -> (nIn r, rn ++ "_next") : maybe [] (\e -> [(e, rn ++ "_en")]) (nEn r)
        Nothing -> []
    rfPorts (NRegFile g f _ _ wr _) =
        concat [ [ (a, base ++ "_waddr"), (d, base ++ "_wdata"), (e, base ++ "_wen") ]
               | (a, d, e) <- wr ]
      where base = g ++ "_" ++ f
    rfPorts _ = []
    step m w = case Map.lookup w m of
        Nothing   -> m
        Just base -> foldl' (add base) m (Map.findWithDefault [] w combMap)
    add base m v
        | Set.member v internal, single v, not (Map.member v m) = Map.insert v base m
        | otherwise                                             = m

-- | Trim a derived name to a sane length (VHDL identifiers can't end in @_@).
capIdent :: String -> String
capIdent s
    | length s <= 64 = s
    | otherwise      = dropWhileEnd (== '_') (take 64 s)

reservedSafe :: String -> String
reservedSafe h | Set.member (map toLower h) vhdlReserved = h ++ "_s"
               | otherwise                               = h

-- | A readable name fragment for a combinational op applied to named operands.
deriveOp :: (WireId -> String) -> PrimOp -> [WireId] -> String
deriveOp nm op ins = case (op, ins) of
    (PNot, [a])            -> "not_" ++ nm a
    (PSlice hi lo, [a])    -> nm a ++ "_" ++ (if hi == lo then "b" ++ show hi
                                              else show hi ++ "_" ++ show lo)
    (PResize _, [a])       -> nm a                 -- resize/cast: transparent, keep operand name
    (PSignedResize _, [a]) -> nm a
    (PReinterpret _, [a])  -> nm a
    (PConcat, xs)          -> intercalate "_" (map nm xs)
    (PMux, [_, t, f])      -> nm t ++ "_mux_" ++ nm f
    (PLit v bw, [])        -> litConstName v bw
    (_, [a, b])            -> nm a ++ "_" ++ opTok op ++ "_" ++ nm b
    _                      -> opTok op

opTok :: PrimOp -> String
opTok op = case op of
    PAdd -> "add"; PSub -> "sub"; PMul -> "mul"
    PAnd -> "and"; POr -> "or";  PXor -> "xor"; PNot -> "not"
    PEq  -> "eq";  PLt -> "lt"
    PShiftL -> "shl"; PShiftR -> "shr"
    PResize _ -> "rsz"; PSignedResize _ -> "srsz"; PReinterpret _ -> "cast"
    PConcat -> "cat"; PSlice _ _ -> "bits"; PMux -> "mux"; PLit _ _ -> "c"

-- ---------------------------------------------------------------------------
-- Expression inlining — collapse one-off @wN@ nets into their consumer
-- ---------------------------------------------------------------------------

-- | How a wire is consumed (only 'NComb'/'NOutput' consumers render operands
-- through 'mkRender', so only they can absorb an inlined wire).
data ConsumerTag = CComb | COutput | COther deriving Eq

consumerRefs :: NetNode -> [(WireId, ConsumerTag)]
consumerRefs (NComb _ _ ins)             = [ (w, CComb)   | w <- ins ]
consumerRefs (NOutput inp _ _ _)         = [ (inp, COutput) ]
consumerRefs (NReg _ i en _ _)           = [ (w, COther) | w <- i : maybe [] pure en ]
consumerRefs (NMem _ a wa wd we _ _ _ _) = [ (w, COther) | w <- [a, wa, wd, we] ]
consumerRefs (NRom _ a _ _ _)            = [ (a, COther) ]
consumerRefs (NRegFileRead _ _ _ a _)    = [ (a, COther) ]
consumerRefs (NRegFile _ _ _ _ wr _)     = [ (w, COther) | (x, y, z) <- wr, w <- [x, y, z] ]
consumerRefs (NSubInst _ _ inp _)        = [ (w, COther) | (_, w) <- inp ]
consumerRefs _                            = []

-- | Wires safe to inline into their single consumer's expression: an internal
-- (auto-named @wN@) wire, driven by a pure /operator/ op (renders as a plain
-- sub-expression), used exactly once, by an 'NComb' or 'NOutput'.  This collapses
-- the sea of one-off @wN <= …@ signals into readable nested expressions.  Ops
-- that render as @… when … else …@ (mux/compare) are excluded — that form is a
-- statement RHS, not a pre-2008 sub-expression; shifts are excluded (their
-- @is_x@ guard lives in the statement wrapper, not the expression).
inlinableWires :: Set.Set WireId -> [NetNode] -> Set.Set WireId
inlinableWires named nodes = inlineSet
  where
    -- Base eligibility: a pure-operator, internal, single-use wire.
    base w op = inlinableOp op
             && not (Set.member w named)                  -- internal (not design-named)
             && not (Set.member w slicedOperands)         -- can't slice an expression
             && not (Set.member w shiftAmounts)           -- named in the shift's is_x guard
             && case Map.findWithDefault [] w consumers of
                  [CComb]   -> True
                  [COutput] -> True
                  _         -> False

    -- Bound the size of an inlined expression: inline a candidate only while its
    -- collapsed operator count stays within 'cap', so short datapath ops fold into
    -- one readable line but long reduction chains (orReduce, big priority muxes)
    -- keep named intermediates instead of a wall of nested parens.  Processed in
    -- wire-id order (defs precede uses) so operand sizes are known first.
    cap = 6 :: Int
    combMap = Map.fromList [ (o, (op, ins)) | NComb o op ins <- nodes ]
    (inlineSet, _) = foldl' step (Set.empty, Map.empty) (sort (Map.keys combMap))
    step acc@(inl, szMap) w = case Map.lookup w combMap of
        Just (op, ins) | base w op ->
            let s = 1 + sum [ Map.findWithDefault 0 i szMap | i <- ins, Set.member i inl ]
            in if s <= cap then (Set.insert w inl, Map.insert w s szMap)
                           else (inl, Map.insert w 0 szMap)
        _ -> acc

    consumers = Map.fromListWith (++) [ (w, [tag]) | n <- nodes, (w, tag) <- consumerRefs n ]
    -- VHDL slicing/indexing @X(hi downto lo)@ requires @X@ to be a name, not an
    -- expression — so a wire fed into a slice (or a /shrinking/ resize, which
    -- also slices) must keep its own signal.
    slicedOperands = Set.fromList $
        [ a | NComb _ (PSlice _ _) [a]  <- nodes, inferWidth a nodes > 1 ]
        ++ [ a | NComb _ (PResize tgt) [a] <- nodes, tgt < inferWidth a nodes ]
    -- The shift-amount operand is named again in the statement's @is_x@ guard.
    shiftAmounts = Set.fromList
        [ b | NComb _ op [_, b] <- nodes, op == PShiftL || op == PShiftR ]
    inlinableOp op = case op of
        PAdd -> True; PSub -> True; PMul -> True
        PAnd -> True; POr  -> True; PXor -> True; PNot -> True
        PSlice _ _ -> True; PConcat -> True
        PResize _ -> True; PSignedResize _ -> True; PReinterpret _ -> True
        _ -> False

-- | Priority-mux chains: a 'PMux' whose false branch is another /single-use/
-- 'PMux' folds into one cascaded @… when … else … when … else …@ conditional
-- signal assignment (legal VHDL — the cascade lives in the false branch; the
-- nested-expression form is not).  These are the false-branch mux wires to
-- absorb into their parent's cascade (dropping their own signal + statement).
muxTailWires :: Set.Set WireId -> [NetNode] -> Set.Set WireId
muxTailWires named nodes = Set.fromList
    [ f
    | NComb _ PMux [_, _, f] <- nodes
    , Just (PMux, _) <- [Map.lookup f combMap]
    , [CComb] == Map.findWithDefault [] f consumers   -- used only as this false branch
    , not (Set.member f named)                         -- internal (not design-named)
    ]
  where
    combMap   = Map.fromList [ (o, (op, ins)) | NComb o op ins <- nodes ]
    consumers = Map.fromListWith (++) [ (w, [tag]) | n <- nodes, (w, tag) <- consumerRefs n ]

-- | Wires the design names explicitly (ports, hints, group fields, literals,
-- output-register aliases) — everything else is an auto-named internal wire
-- eligible for inlining / mux-folding.  Mirrors the keysets 'buildNameMap' uses.
designNamedWids :: [NetNode] -> Set.Set WireId
designNamedWids nodes = Set.unions
    [ Set.fromList [ nOut n | n@NInput{} <- nodes ]
    , grouped
    , Set.fromList [ nHintWire n | n@NHint{} <- nodes
                   , not (isLitWire (nHintWire n) nodes)
                   , not (Set.member (nHintWire n) grouped) ]
    , Set.fromList [ nOut n | n@(NComb _ (PLit _ _) []) <- nodes ]
    , Set.fromList [ nOut n | n@NReg{} <- nodes, Set.member (nOut n) outDriven ] ]
  where
    grouped   = Set.fromList [ w | NGroup _ fs <- nodes, (_, w) <- fs ]
    outDriven = Set.fromList [ nIn n | n@NOutput{} <- nodes ]

-- ---------------------------------------------------------------------------
-- Instruction-decode recovery: @(sel and mask) = value@ → a VHDL-2008 case?
-- ---------------------------------------------------------------------------

-- | A group of mask/value matches on one common selector, recovered from the
-- @(sel and mask) = value@ netlist idiom that ISA decoders lower to.
data DecodeGroup = DecodeGroup
    { dgSel     :: WireId
    , dgWidth   :: Int
    , dgMatches :: [(WireId, Integer, Integer)]  -- (match output wire, mask, value)
    }

-- | Recover decode groups whose match patterns are pairwise non-overlapping —
-- the precondition for a VHDL @case?@ (matching case), whose choices may not
-- overlap.  Overlapping / non-idiom decoders are left as plain comparisons.
decodeGroups :: [NetNode] -> [DecodeGroup]
decodeGroups nodes =
    [ DecodeGroup sel (inferWidth sel nodes) ms
    | (sel, ms) <- Map.toList grouped
    , length ms >= 2
    , nonOverlapping ms
    ]
  where
    combMap = Map.fromList [ (o, (op, ins)) | NComb o op ins <- nodes ]
    litOf w = case Map.lookup w combMap of
                Just (PLit v _, []) -> Just v
                _                   -> Nothing
    hits =
        [ (sel, (out, mask, val))
        | (out, (PEq, ins2)) <- Map.toList combMap
        , (andW, val)        <- pickLit ins2
        , Just (PAnd, ins1)  <- [Map.lookup andW combMap]
        , (sel, mask)        <- pickLit ins1
        ]
    -- of two operands, if exactly one is a literal, yield (otherWire, litVal).
    pickLit [a, b] = case (litOf a, litOf b) of
                       (Nothing, Just v) -> [(a, v)]
                       (Just v, Nothing) -> [(b, v)]
                       _                 -> []
    pickLit _ = []
    grouped = Map.fromListWith (++) [ (sel, [m]) | (sel, m) <- hits ]
    -- two patterns overlap iff they agree on every commonly-fixed bit.
    nonOverlapping ms = and [ ((v1 `xor` v2) .&. (m1 .&. m2)) /= 0
                            | ((_,m1,v1):rest) <- tails ms, (_,m2,v2) <- rest ]

-- | Match-output wires absorbed into a decode process (their comparison
-- statement is replaced, but the signal itself is still declared/read).
decodeMatchOuts :: [DecodeGroup] -> Set.Set WireId
decodeMatchOuts dgs = Set.fromList [ o | DecodeGroup _ _ ms <- dgs, (o,_,_) <- ms ]

-- | The @sel and mask@ intermediate wires that become dead once the matches are
-- absorbed (every remaining reference is an absorbed match) — dropped entirely.
decodeDeadWires :: [NetNode] -> [DecodeGroup] -> Set.Set WireId
decodeDeadWires nodes dgs = Set.fromList
    [ andW
    | andW <- Set.toList candidateAnds
    , not (any (\n -> andW `elem` refWires n && not (isAbsorbed n)) nodes) ]
  where
    matchOuts = decodeMatchOuts dgs
    combMap   = Map.fromList [ (o, (op, ins)) | NComb o op ins <- nodes ]
    candidateAnds = Set.fromList
        [ andW | o <- Set.toList matchOuts
               , Just (PEq, ins) <- [Map.lookup o combMap]
               , andW <- ins, Just (PAnd, _) <- [Map.lookup andW combMap] ]
    refWires = map fst . consumerRefs
    isAbsorbed (NComb o PEq _) = Set.member o matchOuts
    isAbsorbed _               = False

-- | Render a decode group as one @case?@ process driving all its match outputs.
renderDecodeProcess :: NameMap -> DecodeGroup -> [String]
renderDecodeProcess nm (DecodeGroup sel w ms) =
    [ "", "  -- instruction decode (" ++ show (length ms) ++ " patterns)"
    , "  process(all)"
    , "  begin" ]
    ++ [ "    " ++ lookupWire nm o ++ " <= '0';" | (o,_,_) <- ms ]
    ++ [ "    case? " ++ lookupWire nm sel ++ " is" ]
    ++ [ "      when \"" ++ pattern mask val ++ "\" => " ++ lookupWire nm o ++ " <= '1';"
       | (o, mask, val) <- ms ]
    ++ [ "      when others => null;"
       , "    end case?;"
       , "  end process;" ]
  where
    pattern mask val =
        [ if testBit mask i then (if testBit val i then '1' else '0') else '-'
        | i <- [w - 1, w - 2 .. 0] ]

-- | An operand renderer that inlines 'inlinableWires' recursively (parenthesised)
-- and expands 'muxTailWires' as a bare cascade tail (no parens), otherwise
-- falling back to the wire's signal name.
mkRender :: NameMap -> [NetNode] -> Set.Set WireId -> Set.Set WireId -> (WireId -> String)
mkRender nm nodes inlineSet muxTails = render
  where
    combMap = Map.fromList [ (o, (op, ins)) | NComb o op ins <- nodes ]
    render w
        | Set.member w inlineSet
        , Just (op, ins) <- Map.lookup w combMap = "(" ++ combExpr render nodes op ins ++ ")"
        | Set.member w muxTails
        , Just (PMux, ins) <- Map.lookup w combMap = combExpr render nodes PMux ins
        | otherwise = lookupWire nm w

-- ---------------------------------------------------------------------------
-- Entity declaration
-- ---------------------------------------------------------------------------

entityDecl :: Design -> String -> [NetNode] -> String
entityDecl design name nodes
    | null allPorts = unlines
        [ "entity " ++ name ++ " is"
        , "end entity " ++ name ++ ";"
        ]
    | otherwise = unlines $
        [ "entity " ++ name ++ " is"
        , "  port ("
        ] ++
        portLines allPorts ++
        [ "  );"
        , "end entity " ++ name ++ ";"
        ]
  where
    allPorts =
        [ ppPortLine VIn  (domName d)      VStdLogic            | d <- doms ]
     ++ [ ppPortLine VIn  (domResetName d) VStdLogic            | d <- doms ]
     ++ [ ppPortLine VIn  (nPortName n) (wireVTypeR (reprOf (nOut n) nodes) (nWidth n)) | n@NInput{}  <- nodes ]
     ++ [ ppPortLine VOut (nPortName n) (wireVTypeR (reprOf (nIn n) nodes) (nWidth n))  | n@NOutput{} <- nodes ]
    doms = allClockDomains design nodes

-- | Render a port list with semicolons on all but the final entry.
portLines :: [String] -> [String]
portLines []         = []
portLines [p]        = ["    " ++ p]                 -- last port: no trailing ';'
portLines (p : rest) = ("    " ++ p ++ ";") : portLines rest

-- ---------------------------------------------------------------------------
-- Architecture
-- ---------------------------------------------------------------------------

architectureDecl :: Design -> String -> NameMap -> [NetNode] -> String
architectureDecl design name nm nodes = unlines $
    [ "architecture rtl of " ++ name ++ " is" ]
    ++ map ppDecl (archDecls name nm hidden nodes)
    ++ [ "begin" ]
    ++ concatMap (toStmt name design nm nodes render stmtSkip) nodes
    ++ concatMap (renderDecodeProcess nm) dgs
    ++ clockProcesses nm nodes
    ++ [ "end architecture rtl;" ]
  where
    named     = designNamedWids nodes
    inlineSet = inlinableWires named nodes
    muxTails  = muxTailWires named nodes
    dgs       = decodeGroups nodes
    matchOuts = decodeMatchOuts dgs             -- driven by a case? process; keep decl
    deadWires = decodeDeadWires nodes dgs        -- dead @sel and mask@ intermediates
    hidden    = Set.unions [inlineSet, muxTails, deadWires]  -- no own signal
    stmtSkip  = Set.union hidden matchOuts        -- + skip the replaced comparisons
    render    = mkRender nm nodes inlineSet muxTails

-- | Structured TYPE declarations that must be package-visible (so they can be
-- used in entity ports, not just internal signals): record types (from
-- 'NGroup') and enumerated types (from 'REnum' tags), deduplicated.  Emitted
-- into a per-file @<entity>_types@ package; the entity's @use@ clause makes them
-- visible to both the ports and the architecture.
packageTypeDecls :: [NetNode] -> [VDecl]
packageTypeDecls nodes =
    concatMap groupTypes (recordGroups nodes)
    ++
    Map.elems (Map.fromList
        [ (enumTypeName lits, VDType (enumTypeName lits) (VEnum lits))
        | NRepr _ (REnum lits) <- nodes ])
  where
    -- A register file is an array field of its record; declare the named array
    -- type before the record type that references it.
    groupTypes (grpName, fields, arrays) =
        [ VDType (groupArrayTypeName grpName fn) (VArrayOf cnt (wireVType wdt))
        | (fn, cnt, wdt) <- arrays ]
        ++
        [ VDType (grpName ++ "_t")
                 (VRecord ( [ (fn, wireVType (inferWidth w nodes)) | (fn, w) <- fields ]
                         ++ [ (fn, VTypeRef (groupArrayTypeName grpName fn)) | (fn, _, _) <- arrays ] )) ]

-- | One record per group name: its scalar fields (from 'NGroup') and its array
-- fields (register files, from 'NRegFile'), in first-seen order.
recordGroups :: [NetNode] -> [(String, [(String, WireId)], [(String, Int, Int)])]
recordGroups nodes =
    [ (nm, [ (fn, w) | NGroup n fs <- nodes, n == nm, (fn, w) <- fs ]
         , [ (nrfField r, nrfCount r, nrfWidth r) | r@NRegFile{} <- nodes, nrfGroup r == nm ])
    | nm <- nub ([ n | NGroup n _ <- nodes ] ++ [ nrfGroup r | r@NRegFile{} <- nodes ]) ]

groupArrayTypeName :: String -> String -> String
groupArrayTypeName grpName fldName = grpName ++ "_" ++ fldName ++ "_t"

-- | All architecture-region declarations: types, constants, signals.
archDecls :: String -> NameMap -> Set.Set WireId -> [NetNode] -> [VDecl]
archDecls name nm inlineSet nodes =
    -- NGroup record SIGNALS (the record TYPES are in the package, packageTypeDecls).
    concatMap groupDecls groups
    ++
    -- Named array types for inline RAM and ROM (deduplicated by base name).
    Map.elems (Map.fromList
        [ (memTypeName (nOut n) nm,
           VDType (memTypeName (nOut n) nm) (VArrayOf (nMemSize n) (wireVType (nMemDatW n))))
        | n@NMem{} <- nodes ])
    ++
    Map.elems (Map.fromList
        [ (romTypeName name nodes (nOut n),
           VDType (romTypeName name nodes (nOut n)) (VArrayOf (nRomSize n) (wireVType (nRomDatW n))))
        | n@NRom{} <- nodes ])
    ++
    -- PLit constants (deduplicated by value×width).
    [ VDConst (litConstName v bw) (ppType (wireVType bw)) (ppLit (SomeBits v bw))
    | ((v, bw), _) <- Map.toAscList litMap ]
    ++
    -- ROM contents as constants.
    [ VDConst (romSigName name nodes (nOut n)) (romTypeName name nodes (nOut n))
              (initAggregate (nRomSize n) (nRomDatW n) (nRomInit n))
    | n@NRom{} <- nodes ]
    ++
    -- RAM state as signals (deduplicated by base name so multi-port RFs share one array).
    Map.elems (Map.fromList
        [ (memSigName (nOut n) nm,
           VDSig (memSigName (nOut n) nm) (memTypeName (nOut n) nm)
                 (Just (initAggregate (nMemSize n) (nMemDatW n) (nMemInit n))))
        | n@NMem{} <- nodes ])
    ++
    -- All other driven wires (registers, combinational outputs).
    -- Skip wires that are declared as part of an NGroup record.
    [ VDSig (lookupWire nm wid) (ppType (wireVTypeR (reprOf wid nodes) (inferWidth wid nodes)))
            (fmap (ppLitR (reprOf wid nodes)) (regInit wid nodes))
    | wid <- sort (Map.keys nm)
    , not (isInputWire wid nodes)
    , not (isLitWire wid nodes)
    , not (Set.member wid groupedWires)
    , not (Set.member wid inlineSet)          -- inlined into its consumer
    , hasDriver wid nodes
    ]
  where
    litMap :: Map.Map (Integer, Int) ()
    litMap = Map.fromList [ ((v, bw), ()) | NComb _ (PLit v bw) [] <- nodes ]

    groups = recordGroups nodes

    groupedWires :: Set.Set WireId
    groupedWires = Set.fromList
        [ w | NGroup _ fields <- nodes, (_, w) <- fields ]

    -- The record TYPE is declared in the per-file package (see 'packageTypeDecls');
    -- here we only declare the record SIGNAL, which references that type.  Array
    -- fields (register files) initialise every entry to 0.
    groupDecls (grpName, fields, arrays) =
        let scalarInits =
                [ fn ++ " => " ++ ppLit (maybe (SomeBits 0 (inferWidth w nodes)) id
                                               (regInit w nodes))
                | (fn, w) <- fields ]
            arrayInits =
                [ fn ++ " => (others => " ++ ppLit (SomeBits 0 wdt) ++ ")"
                | (fn, _, wdt) <- arrays ]
            initStr = "(" ++ intercalate ", " (scalarInits ++ arrayInits) ++ ")"
        in [ VDSig grpName (grpName ++ "_t") (Just initStr) ]

    isInputWire wid ns = wid `elem` [ nOut n | n@NInput{} <- ns ]
    hasDriver w        = any (drivesWire w)
    drivesWire w (NReg    out _ _ _ _)  = out == w
    drivesWire w (NComb   out _ _)      = out == w
    drivesWire w (NSubInst _ _ _ outs)  = any (\(_, pw, _) -> pw == w) outs
    drivesWire w n@NMem{}               = nOut n == w
    drivesWire w n@NRom{}               = nOut n == w
    drivesWire w (NRegFileRead out _ _ _ _) = out == w
    drivesWire _ _                      = False

regInit :: WireId -> [NetNode] -> Maybe SomeBits
regInit wid nodes = case [ nInit n | n@NReg{} <- nodes, nOut n == wid ] of
    (b:_) -> Just b
    []    -> Nothing

-- ---------------------------------------------------------------------------
-- Concurrent statements
-- ---------------------------------------------------------------------------

-- | Emit the concurrent statement(s) for a node.  The leading entity @name@ is
-- only needed to name ROM arrays stably ('romSigName'); everything else ignores
-- it and delegates to 'toStmt''.
toStmt :: String -> Design -> NameMap -> [NetNode] -> (WireId -> String) -> Set.Set WireId -> NetNode -> [String]
toStmt name _ nm nodes _ _ (NRom out rdA _ _ _) =
    let addr = lookupWire nm rdA
    in [ "  " ++ lookupWire nm out ++ " <= "
             ++ romSigName name nodes out ++ "(to_integer(" ++ addr ++ "))"
             ++ " when not is_x(" ++ addr ++ ") else (others => '0');" ]
toStmt _ design nm nodes render skip node = toStmt' design nm nodes render skip node

toStmt' :: Design -> NameMap -> [NetNode] -> (WireId -> String) -> Set.Set WireId -> NetNode -> [String]
toStmt' _     _  _  _ _ NInput{}                = []
toStmt' _     _  _  _ _ NHint{}                 = []
toStmt' _     _  _  _ _ NRepr{}                 = []
toStmt' _     _  _  _ _ NGroup{}               = []
toStmt' _     _  _  _ _ (NComment txt)          = ["", "  -- " ++ txt]
toStmt' _     _  _  _ _ NReg{}                  = []  -- handled by clockProcesses
toStmt' _     _  _  _ _ NRegFile{}              = []  -- writes handled by clockProcesses
toStmt' _     nm _  _ _ (NRegFileRead out g fld rdA cnt) =
    -- Indexed combinational read of a register-file record field.
    let addr  = lookupWire nm rdA
        nbits = ceiling (logBase 2 (fromIntegral (max cnt 2) :: Double)) :: Int
        raddr = "resize(" ++ addr ++ ", " ++ show nbits ++ ")"
    in [ "  " ++ lookupWire nm out ++ " <= "
             ++ g ++ "." ++ fld ++ "(to_integer(" ++ raddr ++ "))"
             ++ " when not is_x(" ++ addr ++ ") else (others => '0');" ]
toStmt' _     nm _  _ _ (NMem out rdA _ _ _ sz _ _ _) =
    -- Truncate addr to the minimum bits that can index sz entries before to_integer
    -- so that out-of-range addresses (e.g. 0xFFFFFE00) don't overflow INTEGER.
    let addr  = lookupWire nm rdA
        nbits = ceiling (logBase 2 (fromIntegral (max sz 2) :: Double)) :: Int
        raddr = "resize(" ++ addr ++ ", " ++ show nbits ++ ")"
    in [ "  " ++ lookupWire nm out ++ " <= "
             ++ memSigName out nm ++ "(to_integer(" ++ raddr ++ "))"
             ++ " when not is_x(" ++ addr ++ ") else (others => '0');" ]
-- NRom is handled by the 'toStmt' wrapper (it needs the entity name).
toStmt' _     _  _  _ _ (NComb _ (PLit _ _) []) = []  -- handled by archDecls
toStmt' _     _  _  render _ (NOutput inp pname _ _) =
    [ "  " ++ pname ++ " <= " ++ render inp ++ ";" ]
toStmt' _     _  _  _ inlineSet (NComb out _ _) | Set.member out inlineSet = []  -- inlined into consumer
toStmt' _     nm ns render _ (NComb out op ins) =
    let expr = combExpr render ns op ins
        stmt = case (op, ins) of
            (PShiftL, [_, b]) -> expr ++ " when not is_x(" ++ lookupWire nm b ++ ") else (others => '0')"
            (PShiftR, [_, b]) -> expr ++ " when not is_x(" ++ lookupWire nm b ++ ") else (others => '0')"
            _                  -> expr
    in [ "  " ++ lookupWire nm out ++ " <= " ++ stmt ++ ";" ]
toStmt' design nm _  _ _ (NSubInst instNm entRef inPorts outPorts) =
    let (libName, entName) = case entRef of
            LocalEntity  e   -> ("work", e)
            ExternEntity l e -> (l, e)
        childDoms = case localEntityName entRef >>= (`Map.lookup` design) of
            Nothing    -> []
            Just child -> allClockDomains design child
        clkPorts  = [ (domName d,      domName d)      | d <- childDoms ]
                 ++ [ (domResetName d, domResetName d)  | d <- childDoms ]
        allPorts  = clkPorts
                 ++ [ (pn, lookupWire nm w)  | (pn, w)    <- inPorts  ]
                 ++ [ (pn, lookupWire nm w)  | (pn, w, _) <- outPorts ]
    in [ "  " ++ instNm ++ " : entity " ++ libName ++ "." ++ entName ++ " port map ("
       , "    " ++ intercalate ",\n    " [ pn ++ " => " ++ wn | (pn, wn) <- allPorts ]
       , "  );"
       ]

-- ---------------------------------------------------------------------------
-- Clock processes — registers and RAM write ports
-- ---------------------------------------------------------------------------

clockProcesses :: NameMap -> [NetNode] -> [String]
clockProcesses nm nodes = concatMap emitProc (Map.toAscList domGroups)
  where
    domGroups :: Map.Map String (DomId, [NetNode])
    domGroups = foldr addNode Map.empty (filter isClocked nodes)
    isClocked NReg{}     = True
    isClocked NMem{}     = True
    isClocked NRegFile{} = True
    isClocked _          = False
    addNode n = Map.insertWith (\(d, xs) (_, ys) -> (d, xs ++ ys))
                               (domName (clockDom n))
                               (clockDom n, [n])
    clockDom (NReg _ _ _ _ dom)         = dom
    clockDom (NMem _ _ _ _ _ _ _ _ dom) = dom
    clockDom n@NRegFile{}               = nrfDom n
    clockDom _                          = error "clockDom: not a clocked node"

    emitProc (_, (dom, domNodes)) =
        let clkName  = domName dom
            rstName  = domResetName dom
            rstCond  = case domReset dom of
                ActiveHigh -> rstName ++ " = '1'"
                ActiveLow  -> rstName ++ " = '0'"
            (regNodes, memNodes) = partition isReg (reverse domNodes)
            isReg NReg{} = True; isReg _ = False
        in
        [ "  process(" ++ clkName ++ ", " ++ rstName ++ ")"
        , "  begin"
        , "    if " ++ rstCond ++ " then"
        ]
        ++ concatMap resetBody regNodes
        ++ [ "    elsif rising_edge(" ++ clkName ++ ") then" ]
        ++ concatMap clockedBody memNodes
        ++ concatMap clockedBody regNodes
        ++ [ "    end if;"
           , "  end process;"
           ]

    resetBody (NReg out _ _ initV _) =
        [ "      " ++ lookupWire nm out ++ " <= " ++ ppLitR (reprOf out nodes) initV ++ ";" ]
    resetBody _ = []

    clockedBody (NReg out inp Nothing _ _) =
        [ "      " ++ lookupWire nm out ++ " <= " ++ lookupWire nm inp ++ ";" ]
    clockedBody (NReg out inp (Just enW) _ _) =
        [ "      if " ++ lookupWire nm enW ++ " = '1' then"
        , "        " ++ lookupWire nm out ++ " <= " ++ lookupWire nm inp ++ ";"
        , "      end if;"
        ]
    clockedBody (NMem out _ wrA wrD wrEn _ _ _ _) =
        [ "      if " ++ lookupWire nm wrEn ++ " = '1' then"
        , "        " ++ memSigName out nm ++ "(to_integer("
                     ++ lookupWire nm wrA ++ ")) <= " ++ lookupWire nm wrD ++ ";"
        , "      end if;"
        ]
    clockedBody (NRegFile g fld cnt _ wr _) =
        -- One indexed, enabled write per port: group.field(to_integer(addr)) <= data.
        let nbits = ceiling (logBase 2 (fromIntegral (max cnt 2) :: Double)) :: Int
        in concat
            [ [ "      if " ++ lookupWire nm enW ++ " = '1' then"
              , "        " ++ g ++ "." ++ fld ++ "(to_integer(resize("
                           ++ lookupWire nm aW ++ ", " ++ show nbits ++ "))) <= "
                           ++ lookupWire nm dW ++ ";"
              , "      end if;" ]
            | (aW, dW, enW) <- wr ]
    clockedBody _ = []

-- ---------------------------------------------------------------------------
-- Combinational expressions
-- ---------------------------------------------------------------------------

combExpr :: (WireId -> String) -> [NetNode] -> PrimOp -> [WireId] -> String
combExpr w ns op ins = case (op, ins) of
    (POr,   [a,b]) | a == b -> w a   -- identity: hinted a or a → a
    (PAnd,  [a,b]) | a == b -> w a   -- identity: hinted a and a → a
    -- 1-bit unsigned arithmetic is mod-2: a+b = a-b = a xor b, a*b = a and b.
    -- (A width-1 wire is std_logic, which has no numeric_std "+".)
    (PAdd,  [a,b]) -> arithOp (dataRepr [a,b]) " + " " xor " a b
    (PSub,  [a,b]) -> arithOp (dataRepr [a,b]) " - " " xor " a b
    (PMul,  [a,b])
        | max (inferWidth a ns) (inferWidth b ns) == 1 -> w a ++ " and " ++ w b
        | otherwise ->
            let r  = dataRepr [a,b]
                aw = inferWidth a ns
                bw = inferWidth b ns
                ow = max aw bw
                prod = widthPad r ow aw a ++ " * " ++ widthPad r ow bw b
            in if ow < aw + bw
               then "resize(" ++ prod ++ ", " ++ show ow ++ ")"
               else prod
    (PAnd,  [a,b]) -> binOp (dataRepr [a,b]) " and " a b
    (POr,   [a,b]) -> binOp (dataRepr [a,b]) " or "  a b
    (PXor,  [a,b]) -> binOp (dataRepr [a,b]) " xor " a b
    (PNot,  [a])   -> "not " ++ w a
    (PMux,  [s,t,f]) ->
        let r  = dataRepr [t,f]
            tw = inferWidth t ns
            fw = inferWidth f ns
            ow = max tw fw
            fit sw src
                | sw == ow  = castLit r src
                | sw == 1   = "resize(unsigned'(0 => " ++ w src ++ "), " ++ show ow ++ ")"
                | ow > 1    = "resize(" ++ castLit r src ++ ", " ++ show ow ++ ")"
                | otherwise = w src
        -- Break each branch onto its own line: a priority-mux chain (folded via
        -- 'muxTailWires') then reads as an aligned @… when … else …@ cascade.
        in fit tw t ++ " when " ++ w s ++ " = '1' else\n           " ++ fit fw f
    (PEq,   [a,b]) -> let r = dataRepr [a,b]
                      in "'1' when " ++ castLit r a ++ " = " ++ castLit r b ++ " else '0'"
    (PLt,   [a,b]) -> let r = dataRepr [a,b]
                      in "'1' when " ++ castLit r a ++ " < " ++ castLit r b ++ " else '0'"
    (PSlice hi lo, [a])
        | inferWidth a ns == 1 -> w a   -- 1-bit source is std_logic; its only bit is itself
        | hi == lo  -> w a ++ "(" ++ show hi ++ ")"
        | otherwise -> w a ++ "(" ++ show hi ++ " downto " ++ show lo ++ ")"
    (PConcat, ws)   -> intercalate " & " (map w ws)
    (PResize tgt, [a]) ->
        let aw = inferWidth a ns
        in if tgt >= aw
           then if aw == 1
                then "resize(unsigned'(0 => " ++ w a ++ "), " ++ show tgt ++ ")"
                else "resize(" ++ w a ++ ", " ++ show tgt ++ ")"
           else if tgt == 1
                then w a ++ "(0)"
                else w a ++ "(" ++ show (tgt-1) ++ " downto 0)"
    (PSignedResize tgt, [a]) ->
        "unsigned(resize(signed(" ++ w a ++ "), " ++ show tgt ++ "))"
    -- Same-width reinterpretation: a VHDL type cast (same bits).  1-bit wires are
    -- std_logic in either representation, so the cast is the identity there.
    (PReinterpret r, [a])
        | inferWidth a ns == 1 -> w a
        | otherwise            -> reprCast r ++ "(" ++ w a ++ ")"
    (PLit v bw, []) -> ppLit (SomeBits v bw)
    (PShiftL, [a,b]) -> "shift_left("  ++ w a ++ ", to_integer(" ++ w b ++ "))"
    (PShiftR, [a,b]) -> "shift_right(" ++ w a ++ ", to_integer(" ++ w b ++ "))"
    _ -> "/* unhandled " ++ show op ++ " */"
  where
    reprCast RSigned   = "signed"
    reprCast RUnsigned = "unsigned"
    reprCast (REnum _) = "unsigned"  -- enums are not numerically reinterpret-cast
    -- The representation a numeric op operates in: a literal operand carries no
    -- representation of its own (it is a shared @unsigned@ bit-pattern constant),
    -- so the op's repr is taken from its first non-literal data operand.  When
    -- that is signed, literal operands must be cast to keep numeric_std types
    -- consistent (and to pick the signed overload).  All-unsigned ops are
    -- unaffected — the output is byte-for-byte identical.
    -- The op's data representation: the first non-default repr among ALL
    -- operands (so a signed signal, or a tagged enum literal, drives it).
    dataRepr xs = case [ r | x <- xs, let r = reprOf x ns, r /= RUnsigned ] of
                    (r:_) -> r
                    []    -> RUnsigned
    castLit r src
        | REnum lits <- r
        , (v:_) <- [ vv | NComb o (PLit vv _) [] <- ns, o == src ]
        , fromInteger v < length lits = lits !! fromInteger v   -- enum literal → its name
        | r == RSigned, isLitWire src ns, inferWidth src ns > 1 = "signed(" ++ w src ++ ")"
        | otherwise                                             = w src
    widthPad r ow sw src
        | sw == ow  = castLit r src
        | sw == 1   = "resize(unsigned'(0 => " ++ w src ++ "), " ++ show ow ++ ")"
        | otherwise = "resize(" ++ castLit r src ++ ", " ++ show ow ++ ")"
    binOp r opr a b =
        let aw = inferWidth a ns
            bw = inferWidth b ns
            ow = max aw bw
        in widthPad r ow aw a ++ opr ++ widthPad r ow bw b
    -- Arithmetic that degrades to a boolean op at width 1 (std_logic has no "+").
    arithOp r arithOpr boolOpr a b
        | max (inferWidth a ns) (inferWidth b ns) == 1 = w a ++ boolOpr ++ w b
        | otherwise = binOp r arithOpr a b

-- ---------------------------------------------------------------------------
-- Width inference
-- ---------------------------------------------------------------------------

inferWidth :: WireId -> [NetNode] -> Int
inferWidth wid nodes = case filter (drives wid) nodes of
    (NInput  _ _ w _   : _) -> w
    (NOutput _ _ w _   : _) -> w
    (NReg    _ _ _ b _ : _) -> sbWidth b
    (NComb   _ op ins  : _) -> inferOpWidth op ins nodes
    (NSubInst _ _ _ outs : _) ->
        case [ w | (_, pw, w) <- outs, pw == wid ] of
            (w:_) -> w
            []    -> 1
    (n@NMem{} : _)           -> nMemDatW n
    (n@NRom{} : _)           -> nRomDatW n
    (NRegFileRead _ _ _ _ _ : _) -> regFileReadWidth wid nodes
    _                        -> 1
  where
    drives w (NInput  out _ _ _)   = out == w
    drives w (NReg    out _ _ _ _) = out == w
    drives w (NComb   out _ _)     = out == w
    drives w (NSubInst _ _ _ outs) = any (\(_, pw, _) -> pw == w) outs
    drives w n@NMem{}              = nOut n == w
    drives w n@NRom{}              = nOut n == w
    drives w (NRegFileRead out _ _ _ _) = out == w
    drives _ _                     = False

-- | A register-file read's width is the file's entry width, found via the
-- matching 'NRegFile' (same group+field).
regFileReadWidth :: WireId -> [NetNode] -> Int
regFileReadWidth wid nodes =
    case [ (nrfrGroup r, nrfrField r) | r@NRegFileRead{} <- nodes, nrfrOut r == wid ] of
        ((g, fld) : _) ->
            case [ nrfWidth f | f@NRegFile{} <- nodes, nrfGroup f == g, nrfField f == fld ] of
                (w : _) -> w
                []      -> 1
        [] -> 1

inferOpWidth :: PrimOp -> [WireId] -> [NetNode] -> Int
inferOpWidth (PLit _ w)     _         _ = w
inferOpWidth (PResize w)    _         _ = w
inferOpWidth (PSignedResize w) _      _ = w
inferOpWidth (PReinterpret _) (a:_)  ns = inferWidth a ns
inferOpWidth (PSlice hi lo) _         _ = hi - lo + 1
inferOpWidth PConcat        ins       ns = sum (map (`inferWidth` ns) ins)
inferOpWidth PEq            _         _  = 1
inferOpWidth PLt            _         _  = 1
inferOpWidth PMux        (_:t:e:_)   ns = max (inferWidth t ns) (inferWidth e ns)
inferOpWidth PShiftL        (i:_)    ns = inferWidth i ns
inferOpWidth PShiftR        (i:_)    ns = inferWidth i ns
inferOpWidth _           (i:j:_)    ns = max (inferWidth i ns) (inferWidth j ns)
inferOpWidth _              (i:_)   ns = inferWidth i ns
inferOpWidth _              []       _  = 1

-- ---------------------------------------------------------------------------
-- Literal rendering
-- ---------------------------------------------------------------------------

ppLit :: SomeBits -> String
ppLit (SomeBits v 1) = if v == 0 then "'0'" else "'1'"
ppLit (SomeBits v w)
    -- VHDL's @to_unsigned@ takes an @integer@ (32-bit), so a value wider than 31
    -- bits overflows it.  Emit a qualified bit-string literal for wide constants.
    | w > 31    = "unsigned'(\"" ++ bitString v w ++ "\")"
    | otherwise = "to_unsigned(" ++ show v ++ ", " ++ show w ++ ")"

-- | The low @w@ bits of @v@ as a VHDL bit-string (MSB first).
bitString :: Integer -> Int -> String
bitString v w = [ if odd (v `div` (2 ^ i)) then '1' else '0' | i <- [w - 1, w - 2 .. 0] ]

-- | Repr-aware literal: a signed wire's constant\/init must be @to_signed@ (with
-- the bit pattern reinterpreted as a signed value), not @to_unsigned@.
ppLitR :: Repr -> SomeBits -> String
ppLitR RSigned (SomeBits v w)
    | w > 31 = "signed'(\"" ++ bitString v w ++ "\")"   -- raw two's-complement bits
    | w > 1  = "to_signed(" ++ show signedVal ++ ", " ++ show w ++ ")"
  where signedVal = if v >= 2 ^ (w - 1) then v - 2 ^ w else v
ppLitR (REnum lits) (SomeBits v _)
    | fromInteger v < length lits = lits !! fromInteger v   -- enum value → literal
ppLitR _ sb = ppLit sb
