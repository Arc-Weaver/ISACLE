# Running ISACLE systems

A system is a single `SysDSL` value. Every output — VHDL, memory map, C header,
linker script — is generated from it. You no longer write an `IO` `main`, an
`emitVhdlDesignFiles` call, or a per-system `cabal` stanza.

## The minimal system file

One import, one call. Build the system inline and hand it to `systemMain`:

```haskell
{-# LANGUAGE DataKinds, TypeApplications, ScopedTypeVariables, FlexibleContexts #-}
module Main where

import Isacle.System.CLI
import Isacle.ISA.Chip         (Chip(..))
import Isacle.ISA.Example.Tiny (TinyCore, TinyAlu, tinyCPUDef, tinyISA)

tinyChip :: Chip TinyCore TinyAlu 8 8 8 8
tinyChip = Chip tinyCPUDef tinyISA

main :: IO ()
main = systemMain "tiny_soc" $ do
    gpio    <- createGpio "gpio0" (0 :: Sig Sys (Unsigned 8))
    coderom <- createRom 256 (RomImage [0x46,0xC9] :: RomImage (Unsigned 8)) "coderom0"
    (codeBus, ()) <- createBus @SimpleBus "codebus" (attachPeripheral 0x0  coderom >> pure ())
    (dataBus, ()) <- createBus @SimpleBus "databus" (attachPeripheral 0x60 gpio    >> pure ())
    dormant <- noIrq
    createHarvardCPU "cpu0" tinyChip codeBus dataBus dormant
```

`Sys` is a ready-made 50 MHz clock domain provided by `Isacle.System.CLI`, so the
common case needs no `KnownDom` instance. Need different clocking? Declare your own
`data MyClk; instance KnownDom MyClk where …` and annotate signals with `Sig MyClk a`.

Working examples live in [`examples/`](examples/).

## Script / batch (runghc)

```sh
cabal build isacle                                              # once
cabal exec runghc examples/TinySoc.hs -- --out build/tiny_soc   # generate
```

`systemMain` parses flags:

| Flag          | Effect                                             |
|---------------|----------------------------------------------------|
| `--out DIR`   | output directory (default `build/<name>`)          |
| `--name NAME` | top-entity / file basename                          |
| `--guard NAME`| C-header include guard (default: name)             |
| `--vhdl-only` | emit only VHDL                                     |
| `--no-vhdl`   | skip VHDL                                           |
| `--no-memmap` | skip the memory map                                |
| `--no-header` | skip the C header                                  |
| `--no-linker` | skip the linker script                             |
| `--print`     | render to stdout instead of writing files          |
| `-h`,`--help` | usage                                              |

Output for `--out build/tiny_soc` (default flags):

```
build/tiny_soc/
  tiny_soc.vhd        top entity
  cpu0.vhd  gpio0.vhd  coderom0.vhd  …   sub-entities
  tiny_soc.memmap     human-readable memory map
  tiny_soc.h          C register/base macros
  tiny_soc.ld         GNU LD linker script
```

## Interactive (GHCI)

```sh
cabal build isacle    # once
cabal exec ghci       # loads the repo .ghci
```

Use `cabal exec ghci` (not `cabal repl isacle`): it uses the *built* package, so the
repo `.ghci` can expose `isacle` and pre-import the system surface. Bind a system to
a name and generate — no `main` needed:

```haskell
isacle> sys = do { g <- createGpio "gpio0" (0 :: Sig Sys (Unsigned 8)); createBus @SimpleBus "databus" (attachPeripheral 0x300 g >> pure ()) >> pure () }
isacle> writeSystem "build/repl" "demo" sys
wrote build/repl/demo.vhd
wrote build/repl/gpio0.vhd
wrote build/repl/demo.memmap
...
isacle> saMemMap (renderSystem defaultGenOptions "demo" sys)   -- inspect a string, no disk
```

## ROM images

A `createRom` takes a typed `RomImage`. Build one however suits you (all padded to
the ROM's `size` by `createRom` — no double-padding):

```haskell
-- inline literal
createRom 256 (RomImage [0x46, 0xC9] :: RomImage (Unsigned 8)) "coderom0"

-- from a list you computed
createRom 256 (romFromList xs) "coderom0"

-- from a file, read at GENERATION time (deferred; see below)
bytes <- loadFileBytes "prog.bin"
createRom 256 (romFromBytes bytes :: RomImage (Unsigned 8)) "coderom0"

-- from a file, embedded at COMPILE time (Template Haskell)
createRom 256 ($(romBin8 "prog.bin") :: RomImage (Unsigned 8)) "coderom0"
--            romBin16LE / romBin32LE for little-endian wider words
```

See [`examples/TinySoc.hs`](examples/TinySoc.hs) (runtime `loadFileBytes`) and
[`examples/TinySocTH.hs`](examples/TinySocTH.hs) (compile-time `$(romBin8 …)`).

## Deferred file loading

`loadFile :: FilePath -> SysDSL String` (and `loadFileBytes :: FilePath -> SysDSL
[Word8]`) let a system reference a file *from inside the DSL* without making the
whole thing `IO`. The read is **deferred**: the `SysDSL` value only records the
request. The interpreter — `writeSystem` / `systemMain` — runs a harvest pass to
collect requested paths, reads them, then re-runs with the contents available.

```haskell
main = systemMain "tiny_soc" $ do
    prog <- loadFileBytes "prog.bin"          -- no IO here; just recorded
    coderom <- createRom 256 (romFromBytes prog) "coderom0"
    ...
```

Constraint: the *set* of paths a system loads must not depend on any file's
*contents* (paths are gathered before contents are read). In practice paths are
literals, so this is not a real restriction. Pure runs (`renderSystem`,
`runSystemDSL`) see empty contents; use `writeSystem`/`systemMain` (or
`resolveFiles` + `renderSystemWith`) to resolve.

## Post-synthesis memory reload

Every ROM/RAM in the design is a **named bank** whose contents are emitted as
separate files, so you can refresh firmware **without re-running synthesis**:

- `<bank>.mem` — `$readmemh`-compatible hex, one word per line;
- `<bank>.coe` — Xilinx BRAM IP initialization vector;
- `<system>.membanks.json` — a manifest mapping bank → base address → depth×width →
  HDL signal name → data files.

The ROM array is named after the peripheral instance (`coderom0` →
`constant coderom0_rom`), a **stable** name a vendor tool can target.

The reload loop — rebuild firmware, regenerate only the memory files, patch:

```sh
# structure + contents (first time, feeds synthesis)
cabal exec runghc examples/TinySoc.hs -- --out build/tiny_soc

# ...edit prog.bin, then refresh ONLY the memory files (the .vhd is untouched):
cabal exec runghc examples/TinySoc.hs -- --mem-only --out build/tiny_soc

# patch the already-synthesized bitstream (Xilinx), no resynthesis:
updatemem -meminfo design.mmi -data build/tiny_soc/coderom0.mem \
          -bit design.bit -proc dummy -out design.new.bit
```

`--mem-only` writes just `.mem`/`.coe`/`.membanks.json`; `--no-mem` skips them.

**Honest caveat:** the real Vivado `.mmi` (which maps the logical memory to physical
BRAM INIT cells) is produced by Vivado *after* place-and-route — we can't emit it
from RTL alone. ISACLE gives you the stable bank name, the logical bank map
(`.membanks.json`), and the raw contents (`.mem`/`.coe`); your `updatemem`/Tcl step
binds them to the placed BRAMs. For a `.coe`-driven Xilinx BRAM IP, point the IP's
COE at `<bank>.coe` and re-generate the IP (no full resynthesis of your design).

## API

`Isacle.System.Emit` is the shared core (re-exported by `Isacle.System.CLI`):

- `writeSystem :: FilePath -> String -> SysDSL a -> IO ()` — emit everything.
- `writeSystemWith :: GenOptions -> FilePath -> String -> SysDSL a -> IO ()` — pick artifacts.
- `renderSystem :: GenOptions -> String -> SysDSL a -> SystemArtifacts` — pure; strings + the
  raw `Design` / `SysDoc` for inspection.
- `GenOptions` / `defaultGenOptions` — the flags (`goVhdl`, `goMemMap`, `goHeader`, `goLinker`,
  `goMemFiles`, `goGuard`). `Isacle.System.MemBanks` (`extractMemBanks`, `renderMem`,
  `renderCoe`, `renderManifest`) backs the bank artifacts.

The system is run **once** (`runSystemDesign`) and all artifacts share that pass.

## Future: data-driven system specs

Currently a system is Haskell code (a `SysDSL` do-block). A declarative,
value-level `SystemSpec` — a table of peripherals, base addresses, and ROM images
folded into `SysDSL` by a generic interpreter — is a planned extension. It would let
systems be described as pure data (and eventually loaded from an external file).
Reifying the type-level parameters (data width, address widths, clock domain, CPU
`Chip`) from such data needs `someNatVal` / existential plumbing and a named-chip
registry; that work is deferred.
