# Alignment Plan — toward the typeclass-interpreter architecture

> Started 2026-06-26. Sequences the design backlog (`ADJUSTMENTS.md`, the
> per-item deltas) into dependency-ordered phases that move the code to the
> target model (`DEFINITIONS.md`). Read `ADJUSTMENTS.md` for the *what/why* of
> each item (P/H/S/D/C/I/A/PE/BU/SY); this plan is the *order* and *discipline*.

## 1. Target in one picture

Everything is **a type satisfying small composable typeclasses**, interpreted
many ways:

- **Four HDL pillars, all typeclasses** — `HdlType` (structure-preserving
  synthesizable types), `Signal` (combinational value, derived from a monad),
  `ClockDomain`, `Hdl` (the stateful **arrow** — `register` is its core; inputs
  expand). Concrete `Sig`/`NetM`/`NetBuilder` become *one* interpreter (synth)
  among several (sim, doc).
- **One description, many interpreters**, at every scale: `HdlType`→synth/sim/doc;
  `InstrIR`→stalling/pipeline/microcode; `PeriphDef`/`Bus`→Hdl/introspection;
  `SystemDSL`→Hdl I/O + per-CPU bus maps + C decls.
- **Recursive register blocks** — a CPU core and a peripheral are the *same*
  thing: a record of `HdlType` fields (one of which, e.g. `Sreg`, is itself a
  record of `HdlType` bits), placed at locations by a shared **address-mapping**
  helper.
- **Compatibility rule + type-injected adapter** for every crossing — clock
  domains (S2), bus protocol/width (BU6 rule / BU7 bridge).
- **is vs reduces** — a core *satisfies* `HdlType`; a system *reduces to*
  `Hdl I/O` (it is more than any one reduction exposes).

## 2. Discipline (proven by the typed-HDL work this session)

1. **Incremental** — land small, verifiable steps; never a big-bang rewrite.
2. **Builds green at every step** — `cabal build`; **clavr and cl51 build
   unchanged** at each step (additive-first; breaking changes gated + migrated in
   the same step).
3. **Output-neutral where possible** — structural changes must not perturb the
   bytes of existing synthesised VHDL unless intended (as the signed-literal fix
   was byte-identical for unsigned designs).
4. **GHDL-verify synthesis-path changes** — analyse/elaborate/simulate, not just
   `cabal build`.
5. **Additive surface, then migrate** — add the new class/type alongside the old,
   move call sites, then remove the old.

## 3. Phases (dependency-ordered)

### Phase A — Structure-preserving `HdlType` + emitter  (H1–H7)
*Why first:* it is the root, additive (new structured types alongside scalars),
and **unlocks the register-block/bit-map model** used by everything above.
- `HdlType` carries a **structural expression** (Prim | Record | Array; ADT→record
  with **enum** tag); `Generic` deriving for records (reuse `PortLayout`). Packing
  (`toBits`/`Width`) demoted to a derived *flatten* (H6, settled).
- **Emitter (H7)** gains VHDL records, arrays, enum types — the heavy lift; gates
  H1–H4.
- *Verify:* a derived record / array / enum round-trips to valid GHDL; existing
  scalar designs **byte-identical**.
- *Spike first:* one record → VHDL record end-to-end, to de-risk H1/H2/H7.

### Phase B — Pillars as typeclasses  (P1, S1, S4, D1–D4)
*Why second:* needs nothing from A, but is the big structural pivot; doing it
after A means the structured `HdlType` is already in hand.
- `Signal` → typeclass derived from a monad (combinational ops only, S4); `Hdl` →
  the `Circuit` class given **`Category`/`Arrow`** structure (`register`, input
  expansion, D2); naming (`Sig`/`Signal`, `KnownDom`/`ClockDomain`, S1).
- Make the synth path *one instance*; leave room for sim/doc instances.
- *Verify:* clavr/cl51 build through the rename + class indirection; emitted VHDL
  unchanged.

### Phase C — Signal/Hdl state model  (S2, S3)
- Move registers out of `Sig` (`SExpr`/`defer`) into explicit **monadic** `Hdl`
  ops so pure `Sig→Sig` is *strictly* combinational (S3). Add the **domain-crossing
  strategy class** (S2) — type-injected, structural, never a signal op.
- *Verify:* `rampFSM` re-expressed with monadic registers; same VHDL/sim.

### Phase D — Address-mapping helper + CPU layer  (address-mapping, C, I, A)
- The shared **address-mapping helper class** ("place a field's flat view at a
  position in a containing space") — used by cores *and* peripherals.
- CPU core **satisfies `HdlType`** (record of `HdlType` registers; bit-maps as
  records, C2); names single-sourced (C3); aliases via the helper (C5); the free
  `pcW` dies (C1/§4a). ISA **width typecheck** (A2: VN=1 width, Harvard=2);
  `immediate` typed to its field width (I3). *(Pipeline/microcode backends I2 are
  later/optional.)*
- *Verify:* AVR core re-expressed; clavr ISA bodies build; `test_*` green.

### Phase E — Bus & peripherals  (PE, BU)
- Peripheral register block **= the CPU register-block mechanism** reused (PE1),
  logic bound to typed fields (PE2). Bus **capability hierarchy** (BU6:
  stall+width subsumption) and **bridges/adapters** (BU7); **multiple-outstanding**
  interface (BU2); bus-as-peripheral in the DSL (BU4); **runner introspection**
  (BU5).
- *Verify:* ramp/uart/gpio/timer peripherals re-expressed; SoC sims green.

### Phase F — Heterogeneous System DSL  (SY)
- Drop the single `dom`/`dat`: **multiple domains** (SY2), **multiple bus widths**
  (SY3), arbitrary **topology** (SY4), full **tracking** (SY5). Multi-render with
  **per-CPU** bus maps + C decls (SY6); system **reduces to** `Hdl I/O` (SY7).
- *Verify:* a 2-domain / 2-width / 2-CPU example synthesises + per-CPU headers.

## 4. Dependencies (the critical edges)

- **Phase A gates everything structured**: C2 bit-maps, C1 core record, PE1
  peripheral block all need records/arrays in `HdlType` + emitter (H7).
- **Address-mapping helper precedes** the CPU (C5) and peripheral (PE4) reuse.
- **Adapters (S2, BU7) precede** the heterogeneous SystemDSL (SY2/SY3).
- **P1 (Phase B)** is independent of A but should land before the system-layer
  reworks so cores/peripherals/buses are already class-polymorphic.
- **Backends plurality (I2 pipeline/microcode, BU2 multi-outstanding)** are
  *value-optional* — schedule by need, not as blockers.

## 5. Big rocks / risk

- **H7 emitter** (records/arrays/enums) — large; the gating item of Phase A.
- **P1 pivot** (`Sig` concrete → class) — touches every call site; do behind an
  additive class with the synth instance, migrate, then remove concrete leaks.
- **CPU core reframe** (C1) — eliminates `pcW` threading; coordinate with clavr.
- Keep clavr/cl51 building at **every** step (the stack is already landed to
  main/master; new work branches off there).

## 6. First concrete step (recommendation)

Start Phase A with the **structured-`HdlType` spike**: a `data Foo = Foo { … }
deriving (Generic, HdlType)` emitting a VHDL **record**, GHDL-elaborated, with all
existing scalar output proven byte-identical. It de-risks the gating item (H7) and
the deriving mechanism (H2) before committing the root reframe (H1). Also a Phase-A
doc task: **rewrite `DEFINITIONS.md` to the target** (it currently mixes
current/`[target]`), organised per the `ADJUSTMENTS` "Target organization"
(CPU / Bus&peripherals / Address mapping / SystemDSL).
