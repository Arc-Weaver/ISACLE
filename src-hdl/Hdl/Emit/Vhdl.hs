module Hdl.Emit.Vhdl
    ( emitVhdl
    , emitVhdlFile
    , emitVhdlDesign
    , emitVhdlDesignFiles
    , emitEntity
    ) where

import Prelude
import Data.Char (toLower)
import Data.List (foldl', intercalate, nub, partition, sort)
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

-- NRom: constant rom_<wid> : rom_<wid>_t := (...);
romTypeName :: WireId -> String
romTypeName wid = "rom_" ++ show wid ++ "_t"

romSigName :: WireId -> String
romSigName wid = "rom_" ++ show wid

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
            | otherwise = head [ c | i <- [(2 :: Int) ..]
                                   , let c = base ++ "_" ++ show i
                                   , not (Set.member c used) ]

    safeHint wid h
        | Set.member h portNames    = h ++ if isReg wid then "_r" else "_s"
        | Set.member h vhdlReserved = h ++ "_s"
        | otherwise                 = h
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

    namedWids = Map.keysSet (Map.unions [inputMap, groupMap, hintMap, litMap, regMap])
    internalMap = Map.fromList
        [ (wid, "w" ++ show wid)
        | wid <- nub $ concatMap drivenWires nodes
        , not (Set.member wid namedWids)
        ]

    drivenWires (NReg    out _ _ _ _) = [out]
    drivenWires (NComb   out _ _)     = [out]
    drivenWires (NSubInst _ _ _ outs) = [w | (_, w, _) <- outs]
    drivenWires n@NMem{}              = [nOut n]
    drivenWires n@NRom{}              = [nOut n]
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
portLines []  = []
portLines [p] = ["    " ++ p]
portLines ps  = ["    " ++ p ++ ";" | p <- init ps] ++ ["    " ++ last ps]

-- ---------------------------------------------------------------------------
-- Architecture
-- ---------------------------------------------------------------------------

architectureDecl :: Design -> String -> NameMap -> [NetNode] -> String
architectureDecl design name nm nodes = unlines $
    [ "architecture rtl of " ++ name ++ " is" ]
    ++ map ppDecl (archDecls nm nodes)
    ++ [ "begin" ]
    ++ concatMap (toStmt design nm nodes) nodes
    ++ clockProcesses nm nodes
    ++ [ "end architecture rtl;" ]

-- | Structured TYPE declarations that must be package-visible (so they can be
-- used in entity ports, not just internal signals): record types (from
-- 'NGroup') and enumerated types (from 'REnum' tags), deduplicated.  Emitted
-- into a per-file @<entity>_types@ package; the entity's @use@ clause makes them
-- visible to both the ports and the architecture.
packageTypeDecls :: [NetNode] -> [VDecl]
packageTypeDecls nodes =
    [ VDType (grpName ++ "_t")
             (VRecord [ (fn, wireVType (inferWidth w nodes)) | (fn, w) <- fields ])
    | NGroup grpName fields <- nodes ]
    ++
    Map.elems (Map.fromList
        [ (enumTypeName lits, VDType (enumTypeName lits) (VEnum lits))
        | NRepr _ (REnum lits) <- nodes ])

-- | All architecture-region declarations: types, constants, signals.
archDecls :: NameMap -> [NetNode] -> [VDecl]
archDecls nm nodes =
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
        [ (romTypeName (nOut n),
           VDType (romTypeName (nOut n)) (VArrayOf (nRomSize n) (wireVType (nRomDatW n))))
        | n@NRom{} <- nodes ])
    ++
    -- PLit constants (deduplicated by value×width).
    [ VDConst (litConstName v bw) (ppType (wireVType bw)) (ppLit (SomeBits v bw))
    | ((v, bw), _) <- Map.toAscList litMap ]
    ++
    -- ROM contents as constants.
    [ VDConst (romSigName (nOut n)) (romTypeName (nOut n))
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
    , hasDriver wid nodes
    ]
  where
    litMap :: Map.Map (Integer, Int) ()
    litMap = Map.fromList [ ((v, bw), ()) | NComb _ (PLit v bw) [] <- nodes ]

    groups = [ n | n@NGroup{} <- nodes ]

    groupedWires :: Set.Set WireId
    groupedWires = Set.fromList
        [ w | NGroup _ fields <- nodes, (_, w) <- fields ]

    -- The record TYPE is declared in the per-file package (see 'packageTypeDecls');
    -- here we only declare the record SIGNAL, which references that type.
    groupDecls (NGroup grpName fields) =
        let initStr = "("
                ++ intercalate ", "
                    [ fn ++ " => " ++ ppLit (maybe (SomeBits 0 (inferWidth w nodes)) id
                                                   (regInit w nodes))
                    | (fn, w) <- fields ]
                ++ ")"
        in [ VDSig grpName (grpName ++ "_t") (Just initStr) ]
    groupDecls _ = []

    isInputWire wid ns = wid `elem` [ nOut n | n@NInput{} <- ns ]
    hasDriver w        = any (drivesWire w)
    drivesWire w (NReg    out _ _ _ _)  = out == w
    drivesWire w (NComb   out _ _)      = out == w
    drivesWire w (NSubInst _ _ _ outs)  = any (\(_, pw, _) -> pw == w) outs
    drivesWire w n@NMem{}               = nOut n == w
    drivesWire w n@NRom{}               = nOut n == w
    drivesWire _ _                      = False

regInit :: WireId -> [NetNode] -> Maybe SomeBits
regInit wid nodes = case [ nInit n | n@NReg{} <- nodes, nOut n == wid ] of
    (b:_) -> Just b
    []    -> Nothing

-- ---------------------------------------------------------------------------
-- Concurrent statements
-- ---------------------------------------------------------------------------

toStmt :: Design -> NameMap -> [NetNode] -> NetNode -> [String]
toStmt _      _  _  NInput{}                = []
toStmt _      _  _  NHint{}                 = []
toStmt _      _  _  NRepr{}                 = []
toStmt _      _  _  NGroup{}               = []
toStmt _      _  _  (NComment txt)          = ["", "  -- " ++ txt]
toStmt _      _  _  NReg{}                  = []  -- handled by clockProcesses
toStmt _      nm _  (NMem out rdA _ _ _ sz _ _ _) =
    -- Truncate addr to the minimum bits that can index sz entries before to_integer
    -- so that out-of-range addresses (e.g. 0xFFFFFE00) don't overflow INTEGER.
    let addr  = lookupWire nm rdA
        nbits = ceiling (logBase 2 (fromIntegral (max sz 2) :: Double)) :: Int
        raddr = "resize(" ++ addr ++ ", " ++ show nbits ++ ")"
    in [ "  " ++ lookupWire nm out ++ " <= "
             ++ memSigName out nm ++ "(to_integer(" ++ raddr ++ "))"
             ++ " when not is_x(" ++ addr ++ ") else (others => '0');" ]
toStmt _      nm _  (NRom out rdA _ _ _) =
    let addr = lookupWire nm rdA
    in [ "  " ++ lookupWire nm out ++ " <= "
             ++ romSigName out ++ "(to_integer(" ++ addr ++ "))"
             ++ " when not is_x(" ++ addr ++ ") else (others => '0');" ]
toStmt _      _  _  (NComb _ (PLit _ _) []) = []  -- handled by archDecls
toStmt _      nm _  (NOutput inp pname _ _) =
    [ "  " ++ pname ++ " <= " ++ lookupWire nm inp ++ ";" ]
toStmt _      nm ns (NComb out op ins) =
    let expr = combExpr nm ns op ins
        stmt = case (op, ins) of
            (PShiftL, [_, b]) -> expr ++ " when not is_x(" ++ lookupWire nm b ++ ") else (others => '0')"
            (PShiftR, [_, b]) -> expr ++ " when not is_x(" ++ lookupWire nm b ++ ") else (others => '0')"
            _                  -> expr
    in [ "  " ++ lookupWire nm out ++ " <= " ++ stmt ++ ";" ]
toStmt design nm _  (NSubInst instNm entRef inPorts outPorts) =
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
    isClocked NReg{} = True
    isClocked NMem{} = True
    isClocked _      = False
    addNode n = Map.insertWith (\(d, xs) (_, ys) -> (d, xs ++ ys))
                               (domName (clockDom n))
                               (clockDom n, [n])
    clockDom (NReg _ _ _ _ dom)         = dom
    clockDom (NMem _ _ _ _ _ _ _ _ dom) = dom
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
    clockedBody _ = []

-- ---------------------------------------------------------------------------
-- Combinational expressions
-- ---------------------------------------------------------------------------

combExpr :: NameMap -> [NetNode] -> PrimOp -> [WireId] -> String
combExpr nm ns op ins = case (op, ins) of
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
        in fit tw t ++ " when " ++ w s ++ " = '1' else " ++ fit fw f
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
    w = lookupWire nm
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
    _                        -> 1
  where
    drives w (NInput  out _ _ _)   = out == w
    drives w (NReg    out _ _ _ _) = out == w
    drives w (NComb   out _ _)     = out == w
    drives w (NSubInst _ _ _ outs) = any (\(_, pw, _) -> pw == w) outs
    drives w n@NMem{}              = nOut n == w
    drives w n@NRom{}              = nOut n == w
    drives _ _                     = False

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
ppLit (SomeBits v w) = "to_unsigned(" ++ show v ++ ", " ++ show w ++ ")"

-- | Repr-aware literal: a signed wire's constant\/init must be @to_signed@ (with
-- the bit pattern reinterpreted as a signed value), not @to_unsigned@.
ppLitR :: Repr -> SomeBits -> String
ppLitR RSigned (SomeBits v w)
    | w > 1 = "to_signed(" ++ show signedVal ++ ", " ++ show w ++ ")"
  where signedVal = if v >= 2 ^ (w - 1) then v - 2 ^ w else v
ppLitR (REnum lits) (SomeBits v _)
    | fromInteger v < length lits = lits !! fromInteger v   -- enum value → literal
ppLitR _ sb = ppLit sb
