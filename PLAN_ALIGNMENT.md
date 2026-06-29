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

## Progress log

- **Phase A — DONE** (commit `7949f3a`): structure-preserving `HdlType` (record
  `Generic` deriving, `mkGroup`, enums, package types). GHDL-verified;
  output-neutral.
- **Phase B — DONE** (commits `7ac39a3`, `b026883`): `Signal` typeclass (ops
  lifted, anchored on `HdlType` via `HdlEq`/`HdlOrd`); `Hdl` arrow
  (`Category`/`Arrow`/`register`). Naming settled — kept `KnownDom`, `Signal`
  (class) / `Sig` (synth instance). All four pillars are now typeclasses.
- **Next:** Phase C (sim interpreter — second `Signal`/`Hdl` instance; S2/S3) →
  D/E/F (system layer).
- **Phase C (sim) — DONE** (commits `2ee59f2`…`247c689`): `Hdl.Sim` — a second
  `Signal` interpreter (`SimSig`, computes values) **and** a graph-level
  `simulateDesign` (runs synthesized `NetNode` designs over cycles; combinational
  + registers + ROM). Signed-correct via the value type's `Repr`. Cross-validated:
  the sim's signed-ramp trajectory matches GHDL exactly (`0,-2,-4,-6`); unit
  tests added to `test-library`. **The "one description, many interpreters"
  principle is now concrete: synth (→VHDL→GHDL) and sim (→values) of the same
  `Signal` program agree.**
- **Phase D/E (system-layer increments) — IN PROGRESS:**
  - **C2 / PE1 bit-maps from records** (commit `7eb120e`): `GFields`/`recordFields`
    + `fieldRec @Record` — a register's bit-fields *are* its record `HdlType`
    structure; CPU flags and peripheral bit-fields share one mechanism.
  - **PE C-header consumer** (commit `03d3af2`): `sysGenCHeader` now carries each
    register's C type (from its `Repr`+width) and a `_BIT` macro per bit-field —
    the typed-peripheral → firmware-header loop closed end to end.
  - **C5 / PE4 address-mapping helper** (commit `bf9dc65`): `Isacle.System.Layout`
    — `Placement`/`Layout`, `bitLayout` (MSB-first from a record), `addrLayout`,
    `placeAt`. One "place a flat view at a position in a containing space"
    mechanism for flag-bits, register offsets and bus bases; `fieldRec`
    single-sources through it.
  - **BU6 / BU7 bus capability hierarchy** (commit `a6cee61`): `Isacle.System.BusCap`
    — `Capability`/`Subsumes` (forbidden non-stalling→stalling has no instance →
    compile error, verified), `canDrive` witness, `BusAdapter`/`widthAdapter`/
    `stallAdapter`. `BusArch` gains `type Cap arch`.
  - **A2 ISA↔memory-model width check** (commit `bc96af4`): `Isacle.ISA.WidthCheck`
    — `MemModel` (VonNeumann one width / Harvard code+data), `checkMemModel` runs
    one combined check for VN, two for Harvard. Same structural-compatibility
    shape as `BusCap`, over widths. Tested with AVR-Harvard + RV32I-VN shapes.
  - **C1/C2/C5 core flags from a record** (commit `8a6f9c3`): address-mapping
    helper moved to neutral `Isacle.Layout` (shared by ISA + System, under
    neither); `CPUDef.flagRec @Record` mirrors peripheral `fieldRec` — a status
    register's width + flag bit-positions derive from the record via `bitLayout`,
    register width tied to `Width a` (length-by-default). CPU flag ≡ peripheral
    bit-field, one mechanism.
  - **A2/I3 ISA width cluster** (commits `bc96af4`, `24b933d`): `Isacle.ISA.WidthCheck`
    (`MemModel` VN-one / Harvard-two, `checkMemModel`) + encoding well-formedness
    (`encodingErrors`/`checkInstrEncodings`).
  - **SY7/SY6/BU5 reduction layer** (commit `9645ab3`): `Isacle.System.Reduce` —
    a system *reduces to* (not *is*) Hdl I/O / doc / memory-map / C-header /
    linker-script; `reduceSystem` bundles all from one elaboration.
  - **C1 AVR core SREG → record** (isacle `add23e6`, clavr `16d6b5f`): the AVR
    status register is now a bit-map record `HdlType`, flags derived via
    `flagRec`. GHDL-verified (8/8 clavr sim tests).
- **Session 2 (overnight, additive + hardening):**
  - **H4 array `HdlType`** (`cbdeb4d`): `Vec n a` is `HdlType` (Width = n·Width a,
    MSB-first pack) — an array is a first-class record field.
  - **C1 core state as record** (clavr `2291e28`): `AvrState pcW` — the whole AVR
    state as one recursive `HdlType` (Vec gpr + Sreg + scalars), Width 344,
    round-trips. Additive (handle machinery still drives synthesis).
  - **A3 + BU6 width axis** (`5cd06cd`): interrupt-as-instruction uniformity
    confirmed; `canDriveWidth` (cW ≤ mW, narrow→wide is a compile error).
  - **PE2** (`a4f2da9`): `regField`/`roField` fuse typed-field declaration with
    write/read logic (name/offset/width/repr single-sourced).
  - **DEFINITIONS.md** (`a4159d4`): consolidated to the target organisation
    (Part I HDL / Part II CPU·Bus&periph·Address-mapping·SystemDSL).
  - **sim** (`a67d534`): 3-level hierarchy flatten+sim test; corrected the
    whole-SoC limitation to its real scope (external/primitive sub-instances are
    dropped, not "deep hierarchy").
  - **regsFromRecord** (`29cfd23`): single-source CPU registers from a record
    (reframe step 2 groundwork; H4 array field → width).
  - **PLAN_CORE_REFRAME.md** (`d08746d`): ordered, GHDL-verified-per-step plan for
    the supervised whole-core migration.
  - **cl51 8051 flag + state migration** (cl51 `f520403`, `17af2b2`): PSW and IE
    moved to record `HdlType`s via `flagRec` (the AVR SREG migration mirrored on
    the 8051 — GHDL-analyzed), and the whole 8051 state expressed as a recursive
    record `Mcs51State` (nested Psw/Ie, Width 80, round-trips). Both CPU cores now
    have record flags + record state at C1 parity.
  - **Whole-SoC sim fix** (`e530a2a`): diagnosed the "gpio unresolved" gap —
    flattening is clean; the cause was a master-less bus leaving its interface
    wires undriven, stalling the solver. Tie undriven operand wires to 0 → the
    gpio SoC now simulates (port/ddr = 0 at reset). Whole-SoC sim test added.
  - **Emitter reserved-word fix + peripheral PE2 adoption** (`316b41b`,
    `92a37cf`, `32d04b0`, `fada328`): migrating GPIO to `regField`/`roField`
    surfaced a case-sensitive reserved-word check (`PORT` → invalid VHDL);
    fixed to fold case (`PORT` → `PORT_s`). Then GPIO/UART/Timer/Ramp adopt the
    typed PE2 combinators where they fit (Ramp exercises the *signed* path).
    Each GHDL-verified — all 8 clavr benches PASS after every step.
  - *Verified:* full pass — ISACLE tests, cl51 21 + synth + GHDL-analyze, clavr
    unit + 8/8 GHDL — all green.
- **Remaining (large, supervised):** whole-`AVRALU`-as-`HdlType` access +
  `pcW` elimination (PLAN_CORE_REFRAME.md steps 3–4); Phase F heterogeneous
  SystemDSL (multi-domain/width/CPU + per-CPU reductions); type-level A2;
  BurstBus interconnect; structure-preserving `Vec` *wire* emission; whole-SoC
  sim of data routed *through* external primitives (the un-stimulated /
  master-less and multi-level-local cases are now handled — commit `e530a2a`).
