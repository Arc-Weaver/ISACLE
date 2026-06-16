module Isacle.Hdl.Emit.Vhdl
    ( emitVhdl
    , emitVhdlFile
    , emitVhdlDesign
    , emitVhdlDesignFiles
    ) where

import Prelude
import Data.List (intercalate, nub, sort)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.FilePath ((</>))

import Isacle.Hdl.Net

-- ---------------------------------------------------------------------------
-- RAM/ROM entity naming and address-width helpers
-- ---------------------------------------------------------------------------

-- | Bit width required to address @n@ entries (minimum 1).
addrWidth :: Int -> Int
addrWidth n = max 1 (length (takeWhile (< n) (iterate (*2) 1)))

memEntityName :: Int -> Int -> String
memEntityName sz dw = "isacle_ram_" ++ show sz ++ "x" ++ show dw

romEntityName :: Int -> Int -> String
romEntityName sz dw = "isacle_rom_" ++ show sz ++ "x" ++ show dw

-- | Emit a standalone synchronous block-RAM entity (sync write, async read).
emitRamEntity :: Int -> Int -> [Integer] -> String
emitRamEntity sz dw initVals = unlines
    [ "library ieee;"
    , "use ieee.std_logic_1164.all;"
    , "use ieee.numeric_std.all;"
    , ""
    , "entity " ++ eName ++ " is"
    , "  port ("
    , "    clk     : in  std_logic;"
    , "    rd_addr : in  unsigned(" ++ show (aw - 1) ++ " downto 0);"
    , "    wr_addr : in  unsigned(" ++ show (aw - 1) ++ " downto 0);"
    , "    wr_data : in  " ++ wireType dw ++ ";"
    , "    wr_en   : in  std_logic;"
    , "    rd_data : out " ++ wireType dw
    , "  );"
    , "end entity " ++ eName ++ ";"
    , ""
    , "architecture rtl of " ++ eName ++ " is"
    , "  type ram_t is array(0 to " ++ show (sz - 1) ++ ") of " ++ wireType dw ++ ";"
    , "  signal ram_r : ram_t := " ++ initAggregate sz dw initVals ++ ";"
    , "begin"
    , "  process(clk)"
    , "  begin"
    , "    if rising_edge(clk) then"
    , "      if wr_en = '1' then"
    , "        ram_r(to_integer(wr_addr)) <= wr_data;"
    , "      end if;"
    , "    end if;"
    , "  end process;"
    , "  rd_data <= ram_r(to_integer(rd_addr));"
    , "end architecture rtl;"
    ]
  where
    eName = memEntityName sz dw
    aw    = addrWidth sz

-- | Emit a standalone combinational ROM entity.
emitRomEntity :: Int -> Int -> [Integer] -> String
emitRomEntity sz dw initVals = unlines
    [ "library ieee;"
    , "use ieee.std_logic_1164.all;"
    , "use ieee.numeric_std.all;"
    , ""
    , "entity " ++ eName ++ " is"
    , "  port ("
    , "    rd_addr : in  unsigned(" ++ show (aw - 1) ++ " downto 0);"
    , "    rd_data : out " ++ wireType dw
    , "  );"
    , "end entity " ++ eName ++ ";"
    , ""
    , "architecture rtl of " ++ eName ++ " is"
    , "  type rom_t is array(0 to " ++ show (sz - 1) ++ ") of " ++ wireType dw ++ ";"
    , "  constant ROM : rom_t := " ++ initAggregate sz dw initVals ++ ";"
    , "begin"
    , "  rd_data <= ROM(to_integer(rd_addr));"
    , "end architecture rtl;"
    ]
  where
    eName = romEntityName sz dw
    aw    = addrWidth sz

-- | Build a VHDL aggregate initializer for an array of @sz@ elements.
initAggregate :: Int -> Int -> [Integer] -> String
initAggregate sz dw vs =
    "(" ++ intercalate ", " (map (numericLit . flip SomeBits dw) padded) ++ ")"
  where
    padded = take sz (vs ++ repeat 0)

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Emit a single entity.
emitVhdl :: String -> [NetNode] -> String
emitVhdl name nodes = unlines
    [ "library ieee;"
    , "use ieee.std_logic_1164.all;"
    , "use ieee.numeric_std.all;"
    , ""
    , entityDecl name nodes'
    , ""
    , architectureDecl name (buildNameMap nodes') nodes'
    ]
  where nodes' = cse nodes

emitVhdlFile :: FilePath -> String -> [NetNode] -> IO ()
emitVhdlFile path name nodes = writeFile path (emitVhdl name nodes)

-- | Emit every entity in a 'Design'; returns a map of entity name → VHDL text.
-- Also emits named RAM/ROM sub-entities for any 'NMem'/'NRom' nodes found.
emitVhdlDesign :: Design -> Map.Map String String
emitVhdlDesign design = Map.unions [mainMap, ramMap, romMap]
  where
    mainMap  = Map.mapWithKey emitVhdl design
    allNodes = concatMap snd (Map.toList design)
    ramMap   = Map.fromList
        [ (memEntityName (nMemSize n) (nMemDatW n),
           emitRamEntity  (nMemSize n) (nMemDatW n) (nMemInit n))
        | n@NMem{} <- allNodes ]
    romMap   = Map.fromList
        [ (romEntityName (nRomSize n) (nRomDatW n),
           emitRomEntity  (nRomSize n) (nRomDatW n) (nRomInit n))
        | n@NRom{} <- allNodes ]

-- | Write each entity in a 'Design' to @dir/<entityName>.vhd@.
emitVhdlDesignFiles :: FilePath -> Design -> IO ()
emitVhdlDesignFiles dir design =
    mapM_ (\(name, vhdl) -> writeFile (dir </> name ++ ".vhd") vhdl)
          (Map.toList (emitVhdlDesign design))

-- ---------------------------------------------------------------------------
-- Common subexpression elimination
-- ---------------------------------------------------------------------------

-- | Remove duplicate NComb and NReg nodes.
--
-- Scans in emission order; the first node with a given key is canonical;
-- later duplicates are dropped and downstream wire references are redirected
-- to the canonical wire.
--
-- NReg deduplication handles the case where 'SExpr' re-materialisation
-- inside 'hdlSigReg' emits two registers with identical (input, enable,
-- init, domain) — the second is merged into the first.
cse :: [NetNode] -> [NetNode]
cse = go Map.empty Map.empty Map.empty
  where
    go _ _ _ [] = []
    go subst comb reg (n : rest) = case n of
        NComb out op ins ->
            let ins' = map (sub subst) ins
                key  = (op, ins')
            in case Map.lookup key comb of
                Just canon -> go (Map.insert out canon subst) comb reg rest
                Nothing    -> NComb out op ins'
                               : go subst (Map.insert key out comb) reg rest
        NReg out inp en init dom ->
            let inp' = sub subst inp
                en'  = fmap (sub subst) en
                key  = (inp', en', sbValue init, sbWidth init, domName dom)
            in case Map.lookup key reg of
                Just canon -> go (Map.insert out canon subst) comb reg rest
                Nothing    -> NReg out inp' en' init dom
                               : go subst comb (Map.insert key out reg) rest
        _ -> rewrite subst n : go subst comb reg rest

    sub subst w = fromMaybe w (Map.lookup w subst)

    rewrite subst n = case n of
        NOutput inp name w dom ->
            NOutput (sub subst inp) name w dom
        NSubInst inst ent ins outs ->
            NSubInst inst ent [(p, sub subst w) | (p, w) <- ins] outs
        NHint w name ->
            NHint (sub subst w) name
        NMem out rdA wrA wrD wrEn sz dw ini dom ->
            NMem out (sub subst rdA) (sub subst wrA)
                     (sub subst wrD) (sub subst wrEn) sz dw ini dom
        NRom out rdA sz dw ini ->
            NRom out (sub subst rdA) sz dw ini
        _ -> n

-- ---------------------------------------------------------------------------
-- Wire naming
-- ---------------------------------------------------------------------------

type NameMap = Map.Map WireId String

buildNameMap :: [NetNode] -> NameMap
buildNameMap nodes = Map.unions [inputMap, hintMap, litMap, regMap, internalMap]
  where
    -- Input port names: highest priority — these are the entity interface.
    inputMap = Map.fromList [(nOut n, nPortName n) | n@NInput{} <- nodes]

    -- User-supplied hints via 'named': override auto names, but not port names.
    -- If a hint clashes with an existing port name, append "_r" (register) or
    -- "_s" (signal) to avoid the collision.
    portNames = Set.fromList $
        [ nPortName n | n@NInput{}  <- nodes ] ++
        [ nPortName n | n@NOutput{} <- nodes ]
    hintMap = Map.fromList
        [ (nHintWire n, safeHint (nHintWire n) (nHintName n))
        | n@NHint{} <- nodes ]
    safeHint wid h
        | Set.member h portNames = h ++ if isReg wid then "_r" else "_s"
        | otherwise              = h
    isReg wid = any (\n -> case n of NReg out _ _ _ _ -> out == wid; _ -> False) nodes

    -- PLit constants: deduplicated by (value, width).
    litMap = Map.fromList
        [ (nOut n, litConstName v bw)
        | n@(NComb _ (PLit v bw) []) <- nodes ]

    -- Register outputs that directly drive an output port get "r_<portname>".
    outputDrivers = Map.fromList [(nIn n, nPortName n) | n@NOutput{} <- nodes]
    regMap = Map.fromList
        [ (nOut n, "r_" ++ pname)
        | n@NReg{} <- nodes
        , Just pname <- [Map.lookup (nOut n) outputDrivers]
        ]

    -- Fallback: wN for everything else.
    namedWids = Map.keysSet (Map.unions [inputMap, hintMap, litMap, regMap])
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
isLitWire wid = any $ \n -> case n of
    NComb out (PLit _ _) [] -> out == wid
    _                       -> False

lookupWire :: NameMap -> WireId -> String
lookupWire nm wid = fromMaybe ("w" ++ show wid) (Map.lookup wid nm)

-- ---------------------------------------------------------------------------
-- Entity declaration
-- ---------------------------------------------------------------------------

entityDecl :: String -> [NetNode] -> String
entityDecl name nodes
    | null allPorts = unlines
        [ "entity " ++ name ++ " is"
        , "end entity " ++ name ++ ";"
        ]
    | otherwise = unlines $
        [ "entity " ++ name ++ " is"
        , "  port ("
        ] ++
        portList allPorts ++
        [ "  );"
        , "end entity " ++ name ++ ";"
        ]
  where
    clockPorts  = uniqueClockPorts nodes
    inputPorts  = [ "    " ++ nPortName n ++ " : in "  ++ wireType (nWidth n)
                  | n@NInput{} <- nodes ]
    outputPorts = [ "    " ++ nPortName n ++ " : out " ++ wireType (nWidth n)
                  | n@NOutput{} <- nodes ]
    allPorts = clockPorts ++ inputPorts ++ outputPorts

portList :: [String] -> [String]
portList []  = []
portList [p] = ["  " ++ p]
portList ps  = map (\p -> "  " ++ p ++ ";") (init ps) ++ ["  " ++ last ps]

uniqueClockPorts :: [NetNode] -> [String]
uniqueClockPorts nodes =
    [ "    " ++ domName d ++ " : in std_logic"
    | d <- nub ([ nDom n | n@NReg{} <- nodes ]
             ++ [ nDom n | n@NMem{} <- nodes ])
    ]

-- ---------------------------------------------------------------------------
-- Architecture
-- ---------------------------------------------------------------------------

architectureDecl :: String -> NameMap -> [NetNode] -> String
architectureDecl name nm nodes = unlines $
    [ "architecture rtl of " ++ name ++ " is" ]
    ++ constantDecls nodes
    ++ signalDecls nm nodes
    ++ [ "begin" ]
    ++ concatMap (toStmt nm nodes) nodes
    ++ clockProcesses nm nodes
    ++ [ "end architecture rtl;" ]

-- | Emit one 'constant' per unique (value, width) pair among PLit nodes.
constantDecls :: [NetNode] -> [String]
constantDecls nodes =
    [ "  constant " ++ litConstName v bw ++ " : "
      ++ wireType bw ++ " := " ++ numericLit (SomeBits v bw) ++ ";"
    | ((v, bw), _) <- Map.toAscList litMap ]
  where
    litMap :: Map.Map (Integer, Int) ()
    litMap = Map.fromList
        [ ((v, bw), ())
        | NComb _ (PLit v bw) [] <- nodes ]

signalDecls :: NameMap -> [NetNode] -> [String]
signalDecls nm nodes =
    [ "  signal " ++ lookupWire nm wid ++ " : "
      ++ wireType (inferWidth wid nodes) ++ initVal wid nodes ++ ";"
    | wid <- sort (Map.keys nm)
    , not (isInputWire wid nodes)
    , not (isLitWire wid nodes)
    ]
  where
    initVal wid ns = case [ n | n@NReg{} <- ns, nOut n == wid ] of
        (n:_) -> " := " ++ numericLit (nInit n)
        []    -> ""
    isInputWire wid ns = wid `elem` [ nOut n | n@NInput{} <- ns ]

-- ---------------------------------------------------------------------------
-- Statements
-- ---------------------------------------------------------------------------

toStmt :: NameMap -> [NetNode] -> NetNode -> [String]
toStmt _  _  NInput{}                = []
toStmt _  _  NHint{}                 = []
toStmt _  _  NReg{}                  = []   -- handled by clockProcesses
toStmt _  _  (NComb _ (PLit _ _) []) = []   -- handled by constantDecls
toStmt nm _  (NOutput inp pname _ _) =
    [ "  " ++ pname ++ " <= " ++ lookupWire nm inp ++ ";" ]
toStmt nm ns (NComb out op ins) =
    [ "  " ++ lookupWire nm out ++ " <= " ++ combExpr nm ns op ins ++ ";" ]
toStmt nm _ (NSubInst instNm entNm inPorts outPorts) =
    let allPorts = [ (pn, lookupWire nm w) | (pn, w)    <- inPorts  ]
               ++ [ (pn, lookupWire nm w)  | (pn, w, _) <- outPorts ]
    in [ "  " ++ instNm ++ " : entity work." ++ entNm ++ " port map ("
       , "    " ++ intercalate ",\n    " [ pn ++ " => " ++ wn | (pn, wn) <- allPorts ]
       , "  );"
       ]
toStmt nm _ (NMem out rdA wrA wrD wrEn sz dw _ dom) =
    let instNm = "mem_" ++ show out
        entNm  = memEntityName sz dw
        aw     = addrWidth sz
        ports  = [ ("clk",     domName dom)
                 , ("rd_addr", castAddr aw (lookupWire nm rdA))
                 , ("wr_addr", castAddr aw (lookupWire nm wrA))
                 , ("wr_data", lookupWire nm wrD)
                 , ("wr_en",   lookupWire nm wrEn)
                 , ("rd_data", lookupWire nm out)
                 ]
    in [ "  " ++ instNm ++ " : entity work." ++ entNm ++ " port map ("
       , "    " ++ intercalate ",\n    " [ pn ++ " => " ++ wn | (pn, wn) <- ports ]
       , "  );"
       ]
toStmt nm _ (NRom out rdA sz dw _) =
    let instNm = "rom_" ++ show out
        entNm  = romEntityName sz dw
        aw     = addrWidth sz
        ports  = [ ("rd_addr", castAddr aw (lookupWire nm rdA))
                 , ("rd_data", lookupWire nm out)
                 ]
    in [ "  " ++ instNm ++ " : entity work." ++ entNm ++ " port map ("
       , "    " ++ intercalate ",\n    " [ pn ++ " => " ++ wn | (pn, wn) <- ports ]
       , "  );"
       ]

-- | Emit a resize cast for address signals to match the RAM/ROM port width.
castAddr :: Int -> String -> String
castAddr aw sig = "resize(" ++ sig ++ ", " ++ show aw ++ ")"

-- | One clocked process per clock domain, containing all registers on that clock.
clockProcesses :: NameMap -> [NetNode] -> [String]
clockProcesses nm nodes = concatMap emitProc (Map.toAscList domGroups)
  where
    domGroups :: Map.Map String [NetNode]
    domGroups = foldr addReg Map.empty [ n | n@NReg{} <- nodes ]
    addReg n  = Map.insertWith (++) (domName (nDom n)) [n]

    emitProc (clkName, regs) =
        [ "  process(" ++ clkName ++ ")"
        , "  begin"
        , "    if rising_edge(" ++ clkName ++ ") then"
        ] ++
        (regBody . reverse) regs ++
        [ "    end if;"
        , "  end process;"
        ]

    regBody = concatMap regStmt
    regStmt (NReg out inp Nothing _ _) =
        [ "      " ++ lookupWire nm out ++ " <= " ++ lookupWire nm inp ++ ";" ]
    regStmt (NReg out inp (Just enW) _ _) =
        [ "      if " ++ lookupWire nm enW ++ " = '1' then"
        , "        " ++ lookupWire nm out ++ " <= " ++ lookupWire nm inp ++ ";"
        , "      end if;"
        ]
    regStmt _ = []

-- ---------------------------------------------------------------------------
-- Combinational expressions
-- ---------------------------------------------------------------------------

combExpr :: NameMap -> [NetNode] -> PrimOp -> [WireId] -> String
combExpr nm ns op ins = case (op, ins) of
    (PAdd,  [a,b]) -> w a ++ " + " ++ w b
    (PSub,  [a,b]) -> w a ++ " - " ++ w b
    (PMul,  [a,b]) -> w a ++ " * " ++ w b
    (PAnd,  [a,b]) -> w a ++ " and " ++ w b
    (POr,   [a,b]) -> w a ++ " or "  ++ w b
    (PXor,  [a,b]) -> w a ++ " xor " ++ w b
    (PNot,  [a])   -> "not " ++ w a
    (PMux,  [s,t,f]) ->
        w t ++ " when " ++ w s ++ " = '1' else " ++ w f
    (PEq,   [a,b]) ->
        "'1' when " ++ w a ++ " = " ++ w b ++ " else '0'"
    (PLt,   [a,b]) ->
        "'1' when " ++ w a ++ " < " ++ w b ++ " else '0'"
    (PSlice hi lo, [a]) ->
        w a ++ "(" ++ show hi ++ " downto " ++ show lo ++ ")"
    (PConcat, ws)  -> intercalate " & " (map w ws)
    (PResize tgt, [a]) ->
        let aw = inferWidth a ns
        in if tgt >= aw
           then "resize(" ++ w a ++ ", " ++ show tgt ++ ")"
           else w a ++ "(" ++ show (tgt-1) ++ " downto 0)"
    (PLit v bw, []) -> numericLit (SomeBits v bw)
    (PShiftL, [a,b]) -> "shift_left("  ++ w a ++ ", to_integer(" ++ w b ++ "))"
    (PShiftR, [a,b]) -> "shift_right(" ++ w a ++ ", to_integer(" ++ w b ++ "))"
    _ -> "/* unhandled " ++ show op ++ " */"
  where
    w = lookupWire nm

-- ---------------------------------------------------------------------------
-- Utilities
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
    _ -> 1
  where
    drives w (NInput  out _ _ _)   = out == w
    drives w (NReg    out _ _ _ _) = out == w
    drives w (NComb   out _ _)     = out == w
    drives w (NSubInst _ _ _ outs) = any (\(_, pw, _) -> pw == w) outs
    drives w n@NMem{}              = nOut n == w
    drives w n@NRom{}              = nOut n == w
    drives _ _                     = False

inferOpWidth :: PrimOp -> [WireId] -> [NetNode] -> Int
inferOpWidth (PLit _ w)     _       _ = w
inferOpWidth (PResize w)    _       _ = w
inferOpWidth (PSlice hi lo) _       _ = hi - lo + 1
inferOpWidth PConcat        ins     ns = sum (map (`inferWidth` ns) ins)
inferOpWidth PEq            _       _  = 1
inferOpWidth PLt            _       _  = 1
inferOpWidth PMux           (_:t:_) ns = inferWidth t ns
inferOpWidth PShiftL        (i:_)   ns = inferWidth i ns
inferOpWidth PShiftR        (i:_)   ns = inferWidth i ns
inferOpWidth _              (i:_)   ns = inferWidth i ns
inferOpWidth _              []      _  = 1

wireType :: Int -> String
wireType 1 = "std_logic"
wireType n = "unsigned(" ++ show (n-1) ++ " downto 0)"

numericLit :: SomeBits -> String
numericLit (SomeBits v 1) = if v == 0 then "'0'" else "'1'"
numericLit (SomeBits v w) = "to_unsigned(" ++ show v ++ ", " ++ show w ++ ")"
