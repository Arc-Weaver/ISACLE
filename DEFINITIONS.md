# ISACLE — System Definitions

> Status: **living document** (started 2026-06-26). The canonical definition of
> the ISACLE stack's vocabulary, layers, types, and contracts. Where a concept
> is still being *refined* (not yet built), it is marked **[target]**. Prose
> architecture lives in `ARCHITECTURE.html`; migration history and live state in
> `PLAN_ALIGNMENT.md`; the working backlog in `ADJUSTMENTS.md`.
>
> **Foundation (Part I — DONE & committed):** the four HDL pillars are
> typeclasses, with synthesis as *one* interpreter among several —
> **`HdlType`** is structure-preserving (records → VHDL records, arrays → arrays,
> enums → enumerated types, package-scoped for ports), and is *recursive*: a
> record/array of `HdlType` is `HdlType` (so a CPU core, a peripheral register
> block, and a flag register are the same mechanism); **`Signal`** is the
> combinational typeclass (`Sig` = synth instance, `SimSig` = sim instance) with
> operations anchored on the value type (`HdlEq`/`HdlOrd`, signed-vs-unsigned via
> `hdlRepr`); **`Hdl`** is the stateful **arrow** (`Category`/`Arrow`/`register`;
> `NetBuilder` = synth instance); **`KnownDom`** is the clock-domain class.
> "One description, many interpreters" is concrete: synth (→VHDL→GHDL) and sim
> (→values) of the same `Signal` program agree (the signed-ramp trajectory
> matches GHDL exactly).
>
> **System (Part II — IN PROGRESS):** the contained items are landed (see each
> section); the large structural reframes still marked **[target]** are the
> whole-core-as-`HdlType` migration and the heterogeneous (multi-domain /
> multi-width / multi-CPU) SystemDSL.

## 0. Purpose

ISACLE builds synthesisable CPU SoCs from composable layers. This document
*defines* the nouns of that stack — each with its type, its role, where it
lives, and the invariants it upholds — so the layers can be discussed and
refined precisely. Read it as a glossary with structure, not a tutorial.

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

A second discipline runs orthogonal to layering: **a description is not its
output; it *reduces to* outputs.** A `Signal` program reduces to a netlist *or*
to simulated values; a system reduces to Hdl I/O *or* a memory map *or* a C
header. The reductions are distinct interpreters of one description (§II.4).

## 2. Layers

- **HDL layer** (`isacle-hdl`, modules `Hdl.*`): a Clash-free HDL meta-compiler.
  Per-signal typed values, an untyped `NetNode` IR, a VHDL emitter, and a graph
  simulator.
- **ISA layer** (`Isacle.ISA.*`): width-typed instruction *semantics*. An
  instruction is a value (`InstrIR`) lowered to synth requests; the CPU core
  state is the *core definition*.
- **System layer** (`Isacle.System.*`): the SoC builder — buses, peripherals,
  CPUs, address map — assembled in the `SysDSL` monad and *reduced* to a design
  (and to software-facing artefacts).
- **Address mapping** (`Isacle.Layout`): a small **shared** helper used by both
  cores and peripherals — under neither layer (§II.3).

---

# Part I — HDL core (foundation)

- **`WireId`** — an integer handle naming one node's output in the graph. The
  atom of the untyped core.
- **`NetNode`** — one node of the graph IR: `NInput`/`NOutput` (ports), `NReg`
  (clocked register), `NComb op ins` (combinational op), `NRom`/`NMem`,
  `NSubInst` (entity instance), `NRepr` (a per-wire representation tag), … A
  design is a flat `[NetNode]`; a hierarchical `Design` is a `Map` of named
  entities.
- **`PrimOp`** — the combinational operations a `NComb` can carry (`PAdd`,
  `PMux`, `PLt`, `PSlice`, `PResize`, `PSignedResize`, `PReinterpret Repr`,
  `PLit`, …).
- **`Width a` :: Nat** — the bit width of a type, a type-level natural.
- **`Repr`** — how a wire's bits are *interpreted* by the emitter: `RUnsigned` |
  `RSigned` | `REnum [String]` (extensible). Drives the VHDL signal type and thus
  `numeric_std` overload resolution. A wire defaults to `RUnsigned`; signed/enum
  leaves are tagged via `NRepr`.
- **`HdlType a`** — the class of types that erase to wires: carries
  `Width a :: Nat` (superclass `KnownNat (Width a)`), `toBits`/`fromBits`, and
  `hdlRepr :: Proxy a -> Repr` (default `RUnsigned`). Instances:
  - scalar: `Bool`, `Unsigned n`, `Signed n` (`hdlRepr = RSigned`);
  - **record** (`Generic` derivation): `Width = Σ field widths`, packed MSB-first
    (`genericToBits`/`genericFromBits`); structure-preserved to a VHDL record;
  - **array** (`Vec n a`, H4): `Width = n * Width a`, packed MSB-first (element 0
    in the high bits); a first-class field of a core/peripheral record;
  - the derivation is **recursive** — a record whose fields are `HdlType`
    (scalars, arrays, nested records) is itself `HdlType`. This is why a CPU core
    state, a peripheral register block, and a bit-map register are one mechanism.
- **`Signal sig`** — the combinational typeclass: `sigPrim1/2/3`, `sigLitW`, with
  operations (`+`, `.==.`, `.<.`, bitwise, slice, resize, reinterpret) defined
  once and anchored on the value type via `HdlEq`/`HdlOrd` (signed-vs-unsigned
  decided by `hdlRepr`). Instances: **`Sig dom a`** (synth — lowers to `WireId`)
  and **`SimSig`** (sim — computes values).
- **`Hdl c`** — the stateful **arrow**: `Category`/`Arrow`/`ArrowChoice` plus
  `register`/`registerEn`. `NetBuilder` (Kleisli over `NetM`) is the synth
  instance. State and feedback live here; pure combinational logic lives in
  `Signal`.
- **`KnownDom dom`** — the clock-domain class (`domId :: DomId` — name, frequency,
  edge, reset polarity). Per-signal domains let a design carry several.
- **emitter** (`Hdl.Emit.Vhdl`) — reduces a design to VHDL. Reads each wire's
  `Repr` to pick its declared type; per-entity package (`<entity>_types`) holds
  record/enum types so ports referencing them elaborate (GHDL-checked). A literal
  operand is cast to `signed(..)` only when its sibling data operands are signed
  (`dataRepr`/`castLit`), keeping all-unsigned output byte-identical.
- **simulator** (`Hdl.Sim`) — reduces a design to *values*: `SimSig` (Signal-level)
  and `simulateDesign`/`simulateSystem` (graph-level, over cycles: combinational +
  registers + ROM/RAM, sub-instance flattening). Signed-correct via each wire's
  `Repr`. Cross-validated against GHDL.

---

# Part II — system definition

The system layer has four topics: **CPU**, **Bus & peripherals**, **Address
mapping**, and the **SystemDSL** that combines them.

## II.1 CPU — core, ISA, instructions

- **`IExpr w`** — a width-typed bit-vector expression: the instruction-semantics
  surface, parameterised by bit width `w :: Nat`. Concern: **type-level width
  laws**.
- **Width adapters** — conversions between `IExpr` widths. *Loose* forms
  (`zeroExtend`/`signExtend`/`truncateB`/`slice`) place no constraint; the
  *checked* `*C` forms enforce the law in the signature (`zeroExtendC`,
  `signExtendC :: k <= w`; `truncateC :: w <= k`; `sliceC @hi @lo`). Migrating a
  body to `*C` turns an out-of-range or wrong-direction resize into a compile
  error. **A silent default-width extension is a bug source** (the RJMP
  zero-extend bug, fixed by `signExtendBits` on a `12`-bit field).
- **`InstrIR`** — one instruction as a value: an ordered sequence of effects
  (register/flag/memory reads and writes over `IExpr`). Lowered by `renderInstr`/
  `renderSynth` to synth requests; also interpreted by Sim and Doc backends.
  The **interrupt body** (`isaInterruptBody`) is uniformly an `InstrIR` built and
  lowered *identically* to a regular instruction (A3) — it differs only in a
  pending-gate and an IRQ-vector context, the semantics of an interrupt, not a
  separate mechanism.
- **`MonadALU` / `MonadHarvardALU`** — the class instruction bodies are written
  against (`cpu`/`register`/`immediate`/`readReg`/`writeReg`/`readMem`/
  `writeMem`/`aluOp`/flags). `ISABuild` is the single instance.
- **`AluDef m`** — the associated type giving the **core definition** the monad
  `m` operates on (e.g. `AVRALU pcW`). All CPU state is reached through it.
- **`CPURegister (w :: Nat)`** — a length-typed handle to a register (name + width
  in the type). `readReg :: CPURegister w -> m (IExpr w)`.
- **`CPURegFile count w`** — a length-typed register file (`count` registers of
  `w` bits), indexed by an instruction field.
- **`CPUFlag`** — a single status-register bit (containing register + bit index).
- **`CPUDef`** — the builder that *declares* the core: registers, register files,
  flags, endianness, and **memory aliases** (`aliasReg`/`aliasFile`).
  - **`flagPack name [names]`** — declare a status register from an explicit
    MSB-first name list.
  - **`flagRec @Record name`** — declare a status register from a **record
    `HdlType`**: its width and each flag's bit position derive from the record
    layout (`bitLayout`), so a CPU flag and a peripheral bit-field share one
    mechanism and "flag = bit N" needs no separate declaration. The register's
    type-level width is tied to `Width Record` (length-by-default). *In use:* the
    AVR SREG is the record `Sreg { sI..sC :: Bit }` (GHDL-verified).
- **`MemModel`** (`Isacle.ISA.WidthCheck`) — `VonNeumann w` (one unified bus) or
  `Harvard codeW dataW` (separate buses). `checkMemModel` enforces ISA↔width
  compatibility (A2): VN runs **one** check (encodings + values vs the one
  width), Harvard runs **two** (encodings vs code width, values vs data width).
  Value-level today (encoding widths are value-level); lifting to a type-level
  check needs type-level encoding strings **[target]**.
- **`EncodingInfo` / `parseEncoding`** — a parsed instruction encoding (field bit
  positions, fixed-bit mask/value, total width). `encodingErrors`/
  `isValidEncoding` (I3) reject malformed encodings (a stray character would
  otherwise silently corrupt the width); `checkInstrEncodings` runs
  well-formedness then the A2 width check on raw strings.
- **backends** — `Synth` (→ HDL graph), `Sim` (reference interpreter), `Doc`
  (memory map / disassembly).

### II.1a Core-as-`HdlType` — partially landed, full reframe **[target]**

The directive: the core def should be a structured **`HdlType`** and instruction
bodies should be **parametric on the core**, with **length-by-default** field
access.

- *Landed:* the AVR state is expressible as one `HdlType` record (`AvrState pcW`)
  whose every field is `HdlType` — `asGPR :: Vec 32 (Unsigned 8)` (array),
  `asPC :: Unsigned pcW` (PC width = the field's `Width`), `asSREG :: Sreg`
  (bit-map record). Core / registers / bit-maps are the same recursive
  `HdlType`. SREG already drives the real core via `flagRec`.
- *Remaining (**[target]**):* replace the handle record (`AVRALU` of
  `CPURegister String`) with the derived `HdlType` record as the *access* path —
  typed projection instead of name-handles (C6), register-level names
  single-sourced from fields (C3), and the free `pcW` thread eliminated (PC width
  projected from the `asPC` field). This is the large structural step; it touches
  the IR, both synth backends, and every instruction body, so it lands as one
  coordinated migration with GHDL re-verification.

## II.2 Bus & peripherals

A peripheral is what hangs off a bus, so they are one topic.

- **`PeriphDef p sig dat a`** — a peripheral description: register-map metadata
  *and* signal behaviour in one do-block, backend-agnostic via injected
  `PeriphOps`. `runPeriphDef` runs it (purely) to `(result, readData, spec)`.
- **`FieldSpec`** — one register's metadata: offset, width (`RegWidth`), access
  (`RegAccess`), name, description, **`fieldRepr :: Repr`**, and bit-fields.
  - `field`/`field8` — untyped (unsigned) register declarations.
  - **`fieldOf @a`** — derives width *and* representation from an `HdlType` (the
    home for signed/unsigned interpretation — the bus carries none — and the
    source for C-header types).
  - **`fieldRec @Record`** — derives width, representation, *and* bit-fields from
    a record `HdlType` (the peripheral mirror of `flagRec`; PE1).
  - **`regField @a` / `roField @a`** — declare a typed field **and** wire its
    write-register / read-mux in one call (PE2): name, offset, width and
    representation single-sourced, so metadata and logic agree by construction.
- **`onWrite`/`onWriteStrobe`/`onRead`** — the primitive logic combinators
  (raw offset); `regField`/`roField` are built on them.
- **`BusHandle (addrW :: Nat) (dataW :: Nat)`** — a bus master interface, typed by
  **wire counts only**. *A bus is just wires* (`std_logic_vector`); signedness is
  a peripheral's interpretation, never a bus property. Masters bind with
  `addrW <= busAddrW` and matching data width, both compile-time.
- **`BusArch` / `BusPort`** — the bus *protocol* (`SimpleBus` real; `BurstBus`
  capability-tagged, interconnect **[target]**) and the protocol-agnostic
  per-connection wire bundle. A bus is a slave to its parent and a master to each
  child, so buses nest. Each `BusArch` carries `type Cap arch :: Capability`.
- **bus capability hierarchy** (`Isacle.System.BusCap`, BU6) — a master may drive
  a child only when it is **at least as capable**, on two axes:
  - **stall** (`Capability`/`Subsumes`): a non-stalling master cannot drive a
    stalling child (its stall would be dropped) — that pair has *no `Subsumes`
    instance*, so the connection is a compile error. `canDrive` is the witness.
  - **width** (`canDriveWidth`): a wider-or-equal master may drive a narrower
    slave (`cW <= mW`); narrow-drives-wide is a compile error.
- **crossing adapters** (BU7) — `BusAdapter mcap ccap` with `widthAdapter`
  (data-width conversion at fixed capability) and `stallAdapter` (a
  handshake-inserting bridge that legalises a non-stalling master reaching a
  stalling child). Introspectable (faces + widths visible to a runner) — the one
  place protocol/width conversion is expressed.
- **constructors** — `createUart`/`createGpio`/`createTimer`/`createRamp`/
  `createRam`/`createRom`; `createHarvardCPU`/`createCachedCPU`.
  `attachPeripheral base token` wires a peripheral onto a bus.

## II.3 Address mapping (the shared helper)

`Isacle.Layout` — one mechanism for the three "place a flat view at a position in
a containing space" relationships, used by **both** cores and peripherals (hence
it sits under neither layer):

- **`Placement`** — a named flat view occupying `[pos, pos+span)` in a containing
  space (a bit in a register, a byte in a window, …; the index unit is the
  space's).
- **`Layout`** — the placements within a space of known size.
- **`bitLayout @Record`** — bit positions derived MSB-first from a record
  `HdlType` (the "flag = bit N, for free" case; C2/C5). `flagRec` and `fieldRec`
  both single-source through it.
- **`addrLayout`** — explicit base-relative byte windows (PE4).
- **`placeAt base`** — shift a window's flat view to an assigned base; the one
  operation buses use to turn offsets into absolute addresses.

## II.4 SystemDSL

- **`SysDSL dom dat`** — the system-builder monad (`StateT SysDoc` over `NetM`),
  parameterised today by **one** clock domain `dom` and **one** bus data type
  `dat`. The heterogeneous reframe — multiple domains (SY2), bus widths (SY3),
  and CPUs (SY4), with full topology tracking (SY5) — is **[target]**.
- **`SysDoc`** — the introspectable topology (buses + peripherals + bases).
- **system reductions** (`Isacle.System.Reduce`, SY6/SY7/BU5) — a system *is not*
  any one output; it **reduces to** several. `reduceSystem` runs a description
  once and bundles every reduction:
  - `reduceToHdl` — the Hdl netlist / system I/O (SY7).
  - `reduceToDesign` — the full `Design` (top + sub-entities) for VHDL.
  - `reduceToDoc` — the introspectable topology (BU5).
  - `reduceToMemoryMap` / `reduceToCHeader` / `reduceToLinkerScript` — the
    software-facing renders (SY6). The C header carries each register's C type
    (from its `Repr`+width) and a `_BIT` macro per bit-field.
  - *Remaining (**[target]**):* the *combined heterogeneous system type* these
    project from, and **per-CPU** maps/headers (each master sees its own reachable
    address space), depend on the SY2–SY5 reframe.

---

## 3. Invariants & laws

1. **Widths and representation are compile-time.** Runtime `Int`/`Repr` fields
   (e.g. `bhAddrW`) mirror the types and must be *populated from* them, never the
   source of truth.
2. **The bus carries no representation.** Only wire counts. Interpretation lives
   on peripheral registers (`fieldRepr`) and CPU datapaths.
3. **One representation per wire.** A value consumed at two representations must
   cross `sigReinterpret` (a distinct cast wire); a leaf is tagged once.
4. **All-unsigned emission is output-neutral.** Signed handling must not perturb
   the bytes of an unsigned design.
5. **Extensions are explicit.** No width conversion relies on an inferred-width
   default; sign-vs-zero extension is a visible, checked choice (the `*C` forms).
6. **Widths come from the core def. [target]** Register/PC widths are projected
   from the ISA core type, not threaded as free parameters or assumed.
7. **A description reduces to outputs; it is not them.** Synth, sim, memory map,
   C header are *interpreters* of one description, not the description itself.
8. **Compatibility is a type, not a comment.** Bus stall/width subsumption (BU6)
   and ISA↔memory-model widths (A2) are checked relations; the illegal case
   fails to compile (or is reported), never silently wrong.

---

## 4. Open / to refine

- **Whole-core-as-`HdlType`** (§II.1a) — the central remaining redesign: typed
  projection replacing handles, `pcW` elimination.
- **Heterogeneous SystemDSL** (§II.4) — multiple domains / widths / CPUs;
  per-CPU reductions.
- **Type-level A2** — encodings carrying their width as a `Nat`.
- **Protocol library** — a real `BurstBus` interconnect; Wishbone/AXI-lite.
- **Structure-preserving array emission** — `Vec`-typed *wires* → VHDL arrays
  (the value-level `HdlType (Vec n a)` is done; signal/emitter path remains).
- Whole-SoC simulation now handles multi-level local hierarchies and
  un-stimulated (master-less) subsystems (undriven wires tie off to 0); the
  remaining gap is routing data *through* an external primitive (e.g. an IP RAM
  block) that flattening cannot inline — that needs a behavioural model.
- A monadic sequential-process DSL at the HDL layer (auto state/transitions).
