# ISACLE — System Definitions

> Status: **living document** (started 2026-06-26). The canonical definition of
> the ISACLE stack's vocabulary, layers, types, and contracts. Where a concept
> is still being *refined* (not yet built), it is marked **[target]**. Prose
> architecture lives in `ARCHITECTURE.html`; migration history in `PLAN_*.md`.

## 0. Purpose

ISACLE builds synthesisable CPU SoCs from three composable layers. This document
*defines* the nouns of that stack — each with its type, its role, where it
lives, and the invariants it must uphold — so that the layers can be discussed
and refined precisely. Read it as a glossary with structure, not a tutorial.

## 1. The central pattern: typed surface over untyped graph

Every layer is a **typed surface** over an **untyped graph core**, bridged at
lowering:

| Layer | Typed surface | Untyped core | Bridge |
|-------|---------------|--------------|--------|
| HDL | `Sig dom a` (representation + width in the type) | `NetNode` / `WireId` graph | `materialize` |
| ISA | `IExpr w` / `InstrIR` (width-typed bit-vector semantics) | synth read/write requests | `renderInstr` / `renderSynth` |
| System | `PeriphDef`, `BusHandle addrW dataW` (typed ports) | `WireId` interconnect | `runPeriphDef`, `createBus` |

The graph core stays untyped because it needs heterogeneous nodes in lists. The
type information that must survive lowering is carried as a small per-node tag
(e.g. a wire's `Repr`), read by the emitter. **Widths and representation are
compile-time facts; the graph is the runtime artefact.**

## 2. Layers

- **HDL layer** (`isacle-hdl`, modules `Hdl.*`): a Clash-free HDL meta-compiler.
  Per-signal typed values, an untyped `NetNode` IR, and a VHDL emitter.
- **ISA layer** (`Isacle.ISA.*`): width-typed instruction *semantics*. An
  instruction is a value (`InstrIR`) lowered to synth requests; the CPU core
  state is the *core definition*.
- **System layer** (`Isacle.System.*`): the SoC builder — buses, peripherals,
  CPUs, memory map — assembled in the `SysDSL` monad and lowered to one design.

---

## 3. HDL-layer definitions

- **`WireId`** — an integer handle naming one node's output in the graph. The
  atom of the untyped core.
- **`NetNode`** — one node of the graph IR: `NInput`/`NOutput` (ports), `NReg`
  (clocked register), `NComb op ins` (combinational op), `NRom`/`NMem`,
  `NSubInst` (entity instance), `NRepr` (a per-wire representation tag), … A
  design is a flat `[NetNode]`.
- **`PrimOp`** — the combinational operations a `NComb` can carry (`PAdd`,
  `PMux`, `PLt`, `PSlice`, `PResize`, `PReinterpret Repr`, `PLit`, …).
- **`Width a` :: Nat** — the bit width of a type, a type-level natural.
- **`Repr`** — how a wire's bits are *interpreted* by the emitter: `RUnsigned` |
  `RSigned` (extensible: fixed-point/enum/struct **[target]**). Drives the VHDL
  signal type and thus `numeric_std` overload resolution. A wire defaults to
  `RUnsigned`; signed leaves are tagged via `NRepr`.
- **`HdlType a`** — the class of types that erase to wires: carries
  `Width a :: Nat` (superclass `KnownNat (Width a)`), `toBits`/`fromBits`, and
  `hdlRepr :: Proxy a -> Repr` (default `RUnsigned`). Instances: `Bool`,
  `Unsigned n`, `Signed n` (`hdlRepr = RSigned`).
- **`Sig dom a`** — the typed signal surface: a value in clock domain `dom`
  representing a wire of HDL type `a`. Built with typed ops; `materialize`
  lowers it to a `WireId`. **Representation and width live in `a`, not in the
  graph** — `sigReinterpret` is the only same-width cross-representation seam
  (emits a real `signed()`/`unsigned()` cast).
- **emitter** (`Hdl.Emit.Vhdl`) — lowers a design to VHDL. Reads each wire's
  `Repr` (explicit tag, else propagated through ops) to pick its declared type;
  a literal operand is cast to `signed(..)` only when its sibling data operands
  are signed (`dataRepr`/`castLit`), keeping all-unsigned output byte-identical.

---

## 4. ISA-layer definitions

- **`IExpr w`** — a width-typed bit-vector expression: the instruction-semantics
  surface, parameterised by its bit width `w :: Nat`. Concern: **type-level
  width laws**.
- **Width adapters** — conversions between `IExpr` widths. *Loose* forms
  (`zeroExtend`/`signExtend`/`truncateB`/`slice`) place no constraint; the
  *checked* `*C` forms enforce the law in the signature (`zeroExtendC`,
  `signExtendC :: k <= w`; `truncateC :: w <= k`; `sliceC @hi @lo`). Migrating a
  body to `*C` turns an out-of-range or wrong-direction resize into a compile
  error. **A silent default-width extension is a bug source** (the RJMP
  zero-extend bug); the migration target is to make every extension explicit.
- **`InstrIR`** — one instruction as a value: an ordered sequence of effects
  (register/flag/memory reads and writes over `IExpr`). Lowered by `renderInstr`
  to synth read/write requests; also interpreted by the Sim and Doc backends.
- **`MonadALU` / `MonadHarvardALU`** — the class instruction bodies are written
  against. Provides `cpu`/`register`/`immediate`/`readReg`/`writeReg`/
  `readMem`/`writeMem`/`aluOp`/flags. `ISABuild` is the single instance.
- **`AluDef m`** — the associated type giving the **core definition** the monad
  `m` operates on (e.g. `AVRALU pcW`). All CPU state is reached through it.
- **`CPURegister (w :: Nat)`** — a length-typed handle to a register: a name
  plus its width in the type. `readReg :: CPURegister w -> m (IExpr w)`.
- **`CPURegFile count w`** — a length-typed register file (`count` registers of
  `w` bits), indexed by an instruction field.
- **`CPUFlag`** — a bit-view into a packed status register (a single SREG bit).
- **`CPUDef`** — the builder that *declares* the core: registers, register
  files, flags, endianness, and **memory aliases** (`aliasReg`/`aliasFile` map a
  register into the data address space, e.g. AVR SREG@0x5F, SP@0x5D).
- **`immediate "field"`** — extract an instruction-encoding field as an
  `IExpr n`. **Hazard:** today `n` is *inferred* by use, so a field used at a
  wider type is silently zero-extended (the RJMP bug). **[target]** make the
  field width the field's actual width and force explicit extension.
- **backends** — `Synth` (→ HDL graph), `Sim` (reference interpreter, exposes
  the instruction encoding on result state), `Doc` (memory map / disassembly).

### 4a. The ISA core definition — **[target]**

The directive (2026-06-26): the core def should be a **special structured
`HdlType`**, and instruction bodies should be **parametric on the core**, with
**length-by-default field access**.

- *Today:* `AVRALU pcW` is a plain record of `CPURegister w` fields,
  parameterised by a free `pcW`; bodies are written `AVR m pcW` and do width
  math that silently assumes `pcW = 16`.
- *Target:* the core type is a product of length-typed fields **plus** a memory
  alias map **plus** a flag/bit-field layer. It is an `HdlType` (total
  `Width` = Σ field widths; `toBits`/`fromBits` for state snapshots /
  state-as-memory). A field access is a **length-indexed projection**: the
  result width is the field's *declared* length — so the PC width *is* the width
  of the PC field in the core, never inferred or assumed. The free `pcW`
  parameter disappears; conversions between PC width and other quantities use
  the checked/lengthen (`*C`) adapters, sized from the core. This unifies the
  ISA layer with the HDL `HdlType`/`Width` system and structurally prevents the
  RJMP-class bug.
- *Note:* `Word m ~ IExpr 8`, `DataAddr m ~ IExpr 16`, `CodeWord m ~ IExpr 16`
  are already concrete in the `AVR m pcW` alias — `pcW` is the only free width.

---

## 5. System-layer definitions

- **`SysDSL dom dat`** — the system-builder monad (`StateT SysDoc` over `NetM`),
  parameterised by clock domain `dom` and bus data type `dat`. `execSystemDSL`
  lowers a description to one `Design`.
- **`PeriphDef p sig dat a`** — a peripheral description: register-map metadata
  *and* signal behaviour in one do-block, backend-agnostic via injected
  `PeriphOps`. `runPeriphDef` runs it (purely) to `(result, readData, spec)`.
- **`FieldSpec`** — one memory-mapped register's metadata: offset, width
  (`RegWidth`), access (`RegAccess`), name, description, **`fieldRepr :: Repr`**,
  and bit-fields. `field`/`field8` declare unsigned registers; `fieldOf @a`
  derives width *and* representation from an `HdlType` — the home for
  signed/unsigned interpretation (the bus carries none) and the source for
  C-header/doc generation.
- **`BusHandle (addrW :: Nat) (dataW :: Nat)`** — a bus master interface, typed
  by **wire counts only**. *A bus is just wires* (`std_logic_vector`);
  signedness is a peripheral's interpretation, never a bus property.
  `createBus → BusHandle 32 (Width dat)`; masters bind with `addrW <= busAddrW`
  and matching data width, both compile-time.
- **`BusArch` / `BusPort`** — the bus *protocol* (e.g. `SimpleBus`) and the
  protocol-agnostic per-connection wire bundle. A bus is a slave to its parent
  and a master to each child, so buses nest (each level its own decoder).
- **peripheral / CPU constructors** — `createUart`/`createGpio`/`createTimer`/
  `createRamp`/`createRam`/`createRom`; `createHarvardCPU`/`createCachedCPU`.
  `attachPeripheral base token` wires a peripheral onto a bus.

---

## 6. Invariants & laws

1. **Widths and representation are compile-time.** Runtime `Int`/`Repr` fields
   (e.g. `bhAddrW`) mirror the types and must be *populated from* them, never
   the source of truth.
2. **The bus carries no representation.** Only wire counts. Interpretation lives
   on peripheral registers (`fieldRepr`) and CPU datapaths.
3. **One representation per wire.** A value consumed at two representations must
   cross `sigReinterpret` (a distinct cast wire); a leaf is tagged once.
4. **All-unsigned emission is output-neutral.** Signed handling must not perturb
   the bytes of an unsigned design.
5. **Extensions are explicit. [target]** No width conversion should rely on an
   inferred-width default; sign-vs-zero extension is always a visible, checked
   choice (the RJMP lesson).
6. **Widths come from the core def. [target]** Register/PC widths are projected
   from the ISA core type, not threaded as free parameters or assumed.

---

## 7. Open / to refine

- Core-def-as-structured-`HdlType` (§4a) — the central redesign.
- `immediate`/`signExtendBits`/`resizeBits` gaining width laws.
- Struct/enum/fixed-point `Repr` cases.
- Fixture reproducibility: `.bin`s are gitignored with no assembly step.
- A monadic sequential-process DSL at the HDL layer (auto state/transitions).
