# Fundamentals — Adjustments Queue

Working queue from the type-by-type design review (started 2026-06-26). Each item:
what changes, where, and kind (additive / breaking / reframe / design-question).
Nothing here is executed yet — this is the agreed backlog.

## Cross-cutting principle (P) — four pillars, all typeclasses

**P1 (overarching).** The four core abstractions — **`HdlType`**, **`Signal`**,
**`ClockDomain`**, **`Hdl`** — must *all* be typeclasses, so multiple
**interpreters** can express them correctly (tagless-final): a synthesis
interpreter (→ NetNode graph / VHDL), a simulation interpreter (→ values), a
documentation interpreter (→ metadata), etc. each provides instances. Status:
`HdlType` ✅ class (content reframe pending), `ClockDomain` ✅ (`KnownDom`),
`Signal` ❌ concrete `Sig` (→ S4), `Hdl` ◑ `Circuit` class but bare (→ D-series).
Target: all four are interpreter-able classes; concrete `Sig`/`NetM`/`NetBuilder`
become *one* interpreter among several.

## HdlType (root type)

**User model (refined):** `HdlType` = the class of **synthesizable** types. It
carries **how the type is expressed** in HDL — structure *preserved* for
synthesis — **not a flat packing**. A width can be assigned, but
synthesizability/expression is primary. The universe is the closure of primitive
bus types under structure:

- **Primitive bus types** (leaves): `Bit` / `std_logic`, `std_logic_vector`,
  `unsigned`, `signed`, … → the actual VHDL scalar types.
- **Records** (products): all fields `HdlType` → a VHDL **record**.
- **ADTs** (sums): all constructor payloads `HdlType` → a structure-preserving
  **variant**.
- **Fixed arrays**: `Vec (n :: Nat) a`, `KnownNat n`, `a` an `HdlType` → a VHDL
  **array**.

Records/ADTs are promoted by **deriving** (when all subfields are `HdlType`).

**Current:** packing-centric, primitives-only. `toBits :: a -> Integer` /
`fromBits` (a *flat packing*), `Width :: Nat`, `hdlRepr :: Repr`
(`RUnsigned`/`RSigned`). Hand-written scalar instances (`Bool`, `Unsigned n`,
`Signed n`, `Bit`). No records / sums / arrays / deriving; **no structure
preservation** — everything erases to one flat wire. The emitter
(`Hdl.Emit.Vhdl`) only knows flat `unsigned`/`signed`/`std_logic` wires.

→ **Large reframe of the root type, not an add-on.** (H4 from the first pass —
"records pack to a single wire" — is **withdrawn**: we preserve structure.)

- [ ] **H1 (reframe — core)** — `HdlType` carries a **structural expression**
  (Prim | Record [field types] | Sum [ctor payloads] | Array n elem); synthesis
  **preserves** it (VHDL record/array/variant). The current
  `toBits`/`fromBits`/`Width`/`Repr` packing view is demoted (→ H6). Lands in
  `Hdl.Types` (class shape) + `Hdl.Net`/emitter.
- [x] **H2 (DONE — packing deriving)** — `Generic` deriving for records implemented
  in `Hdl.Types`: `GWidth` (type-family Σ of field widths) + `GHdlType` (value
  packing) + `genericToBits`/`genericFromBits`. A record derives `HdlType` with a
  one-line instance (`type Width Foo = GWidth (Rep Foo)`; `toBits = genericToBits`;
  needs `UndecidableInstances`, already project-wide). Verified: `Foo (Unsigned 4)
  (Signed 4)` → `Width 8`, `toBits (Foo 3 -1) = 0x3F` (MSB-first), round-trip ok.
  *Remaining:* the structural (VHDL-record) *emission* side is H7 (the packing is
  the flatten view, H6).
- [ ] **H3 (design → reuse records)** — **ADTs convert to a record.** A sum
  lowers to a record `{ tag, payload }`, riding the record path (H2) instead of a
  distinct VHDL variant construct. **The `tag` is a VHDL enumerated type**
  (`type t_<name>_tag is (Ctor1, Ctor2, …)`), not a `clog2` bit field — readable
  and type-safe; its packing/alias width is `clog2(#ctors)` (derived, H6).
  Special case: an **all-nullary ADT maps directly to a VHDL enum** (tag only, no
  payload record). Remaining sub-decision: payload layout — one field per
  constructor (all present, only the tagged one valid) vs a single payload sized
  to the widest constructor. Pin field/ctor bit order to match flatten (H6).
  - **PROGRESS (enum infra DONE, verified):** `Repr += REnum [String]` (`Hdl.Net`);
    emitter gains `VEnum`/`VEnumRef`, `enumTypeName`, deduped enum **type
    declarations** (from `NRepr` tags), `wireVTypeR` enum case, and enum-aware
    `ppLitR` (a value → its literal name). Verified: an internal enum register
    emits `type … is (Idle,Run,Done); signal state : … := Idle; state <= Idle`
    and GHDL analyzes + elaborates. **Enum-aware `combExpr` literals DONE**:
    `dataRepr` now picks the first non-`RUnsigned` repr among *all* operands
    (signed unchanged), and `castLit (REnum lits)` emits a literal's name. A full
    3-state enum FSM (compares `state = Idle`, muxes `Run when go else Idle`,
    enum reset) emits + GHDL-elaborates. **Output-neutral**: all 8 clavr ghdl-sim
    tests still pass (signed ramp reads −6). **Package types DONE**: structured
    type declarations (records from `NGroup`, enums from `REnum`) now emit into a
    per-file `<entity>_types` **package** (`packageTypeDecls` + restructured
    `emitVhdl`), with a `use work.<entity>_types.all` clause covering both the
    entity **ports** and the architecture. Verified: enum **ports** GHDL-elaborate;
    `cpu.vhd`'s `cpu_state` record moved to `cpu_types` package, all 8 clavr tests
    still green; type-free designs emit **byte-identical** (no package). Minor
    cosmetic: dead `to_unsigned` constants for inlined enum literals (could filter).
- [ ] **H4 (additive)** — **Fixed-size arrays** `Vec (n :: Nat)` of `HdlType`
  (`KnownNat n`) → VHDL array type. New structural form (was missing entirely).
- [ ] **H5 (design)** — **Primitive leaf set** mapped to VHDL bus types: `Bit`
  vs `std_logic` (2- vs 9-valued), `std_logic_vector` (raw bits), `unsigned`,
  `signed`, and **`enum`** (VHDL enumerated type — used for ADT tags and usable
  directly). Reconcile with current `Bool`/`Bit`/`Unsigned`/`Signed`; pick the
  canonical primitives + VHDL mapping. `Repr` (`RUnsigned`/`RSigned`, + a new
  `REnum`) is a property of a **primitive leaf**, not of structured types.
- [x] **H6 (reframe — SETTLED)** — Demote packing. Every `HdlType` *does* have an
  **alias / packing width** (a flat bit-vector view), always derivable (Σ for
  records, n·elem for arrays, tag+payload for sums) — but it is **not a design
  driver**; structure/expression is primary. So `toBits`/`fromBits`/`Width` stay
  as a derived *flatten* capability (for memory/bus where bits genuinely
  flatten), not the definition of `HdlType`.
- [ ] **H7 (emitter — SMALLER THAN FEARED)** — DISCOVERY: the emitter **already
  has** `VRecord`, `VArrayOf`, `VDType` and an **`NGroup`** node that declares a
  VHDL record type + signal — and it is **exercised in production**: `SynthCPU.hs`
  emits `NGroup "cpu_state"`, so **CPU state is already a VHDL record today**.
  Records ✅ and arrays ✅ (used for ROM/RAM) exist. So H7 reduces to: (a) **enums**
  (`VEnum`) for ADT tags (the real gap, H3); (b) wire the structural `HdlType`
  (H1) to `NGroup`/`VArrayOf` so *derived* records/arrays auto-emit (today
  `NGroup` is hand-emitted by `SynthCPU`). Much less risk than "flat-wire only."
  - **PROGRESS:** `mkGroup` added to `Hdl.Class` — a generic
    `HdlPorts a => String -> a -> NetM ()` that emits an `NGroup` from a record of
    signals (the generic form of the hand-rolled `NGroup "cpu_state"`). **Verified**:
    a `deriving (Generic, HdlPorts)` record → a VHDL `record` type with correct
    `state.<field>` mapping, GHDL analyzed + elaborated. (Note: identical
    registers CSE-merge — correct, but means distinct fields need distinct
    drivers.) Remaining: (a) enums (H3); (b) the `HdlType`-level structural
    projection (H1) so a `Sig dom <record>` materialises to an `NGroup`
    automatically (mkGroup currently takes the `HdlPorts` bundle explicitly).

### Resolved
- [x] **H8** — "generally fixed width": confirmed. Every `HdlType` has a fixed
  packing width (always present, derivable); not a concern, never unbounded.

## Signal / Sig — `(ClockDomain dom, HdlType a) => Signal dom a`

**User model:** an `HdlType a` living within a VHDL clock domain `dom` (clock +
reset). Bundling `(dom, a)` type-checks **domain crossing**. A pure
`Signal dom a -> Signal dom b` is **combinational**. Signals **can be monadic**,
so we can build/work with `HdlType`s within a domain.

**Current:** `Sig dom a = SWire WireId | SExpr (NetM WireId)`; ops constrained
`(KnownDom dom, HdlType a)`. `DomId` carries `domFreqHz`/`domEdge` (clock) +
`domReset`/`domResetName` (reset). Same-domain ops ⇒ cross-domain mixing is a
type error. Combinational ops (`mux`, `.==.`, arithmetic) are pure `Sig→Sig`.
**Largely matches** the model, incl. the monadic `SExpr` carrier.

- [ ] **S1 (naming)** — Canonical names: `Sig` vs `Signal`, `KnownDom` vs
  `ClockDomain`. Pick one; align the API surface + docs.
- [ ] **S2 (additive)** — Domain crossing is **provided**, with the *how*
  **type-injected from a separate crossing-strategy class** (2-FF sync / async
  FIFO / handshake …), selected by instance resolution. **NOT a signal
  operation** — signals carry no `cdc` combinator; crossing happens at the
  **structural** level (where domains connect / as a dedicated component), not as
  `signal dom1 a -> signal dom2 a`. (Today: crossing is merely prevented by the
  type; no strategy class exists.)
- [x] **S3 (DECIDED — signals are combinational)** — A `signal dom a` *value* is
  **combinational** (a wire); pure `signal → signal` is always combinational.
  **All state — registers AND domain crossings — is introduced monadically** (in
  the monad `Signal` derives from, S4). Rationale: a crossing requires state, and
  signals are combinational, so it cannot be a signal op (→ S2). Implementation
  consequence: the current pattern that hides a register *inside* a `Sig`
  (`rampFSM`'s `SExpr`/`defer`) must move to explicit **monadic** register ops, so
  pure `Sig→Sig` is *strictly* combinational. Refactor of `Sig`/`Hdl.Class`.
- [ ] **S4 (reframe — structural)** — Make **`Signal` a typeclass** *derived from
  a monad* (the signal *value* surface is abstract; multiple interpreters —
  synth, sim, …). Its methods are the **combinational** ops only (`mux`, `.==.`,
  arithmetic, slice/concat). **State is NOT here** — registers/crossings live in
  `Hdl` (S3/D-series). User leans yes ("which it should probably be"). NOTE:
  distinct from the existing `Circuit (c :: Type -> Type -> Type)` class, which
  abstracts the **builder arrow** (`reg`/`regEn` over `NetBuilder`) while keeping
  `Sig` concrete. Decide the relationship: does `Signal` subsume `Circuit`, sit
  beside it (Signal = value layer, Hdl = stateful arrow), or replace
  the concrete `Sig` with class-polymorphic code? Large surface change.

## Hdl — `(Signal i, Signal o) => Hdl i o`

**User model:** the typeclass capturing **stateful** hardware operations (input
signal-bundle `i` → output `o`). Fundamental member:
`register :: (ClockDomain dom, HdlType i) => i -> Signal dom i -> Hdl (Signal dom
i) (Signal dom i)`. It is **monadic**, *and* has **capabilities to expand its
input space** (arrow-like — more than a plain monad). `Signal` is *derived from*
this monad (state enters here; the value layer is combinational, S3/S4).

**Current:** exists as `Circuit (c :: Type -> Type -> Type)` with
`reg :: a -> c (Sig dom a) (Sig dom a)`, `regEn`, and one instance
`NetBuilder i o = i -> NetM o`. **Bare** — no `Category`/`Arrow`/`Monad`
instances, so no composition or input-space expansion; `Sig` is concrete in the
signatures. The fundamental member (`reg`) and the arrow shape are right; the
structure and the interpreter-plurality are missing.

- [ ] **D1 (naming)** — `Circuit → Hdl`, `reg → register`; constrain ends as
  signal bundles (`Signal i`, `Signal o`) instead of bare `Sig` in signatures.
- [ ] **D2 (structural — core)** — `Hdl` is **monadic + input-space-expanding =
  an Arrow.** Give it `Category`/`Arrow` (and likely `ArrowChoice` for muxing /
  ADT case) structure so circuits compose (`>>>`) and grow inputs
  (`first`/`***`/`&&&`). Today `Circuit` has none. This is the "expand its input
  space" capability.
- [ ] **D3 (register sig)** — Reconcile `register`'s signature
  (`i -> Signal dom i -> Hdl (Signal dom i) (Signal dom i)`) with current
  `reg :: a -> c (Sig dom a) (Sig dom a)` (the extra explicit input-signal arg).
- [ ] **D4 (interpreters)** — As a class, `Hdl` admits **multiple interpreters**
  (synth `NetBuilder` → graph/VHDL; sim → values; doc → metadata), per P1. Today
  only `NetBuilder`. The `Signal` monad (S4) is the same story one layer down.

---

# Part II — System-definition types (expanded layer)

The four pillars above (`HdlType`, `Signal`, `ClockDomain`, `Hdl`) **close the HDL
layer** — sufficient to express any logical design. Part II defines the
**expanded types that assist system definition**, built *on* the pillars and
composing with them (peripheral ports are `Signal`s; the ISA core def is an
`HdlType`; buses move `Signal`s between domains/components).

Candidate types to define (same describe → compare → queue loop):
- **ISA core definition** — `CPUDef`/`AluDef`/`CPURegister`/`CPUFlag` + memory
  aliases. (Carries the earlier directive: structured `HdlType` + aliases +
  flags; length-by-default field projection; PC width from the core, not a free
  `pcW`.)
- **Instruction definition** — `InstrIR`, `MonadALU` ops, `immediate` + the
  `*C` width adapters (target: `immediate` typed to its field width).
- **Peripheral** — `PeriphDef`, `FieldSpec`/register map (`fieldRepr`/`fieldOf`),
  `PeriphOps`.
- **Bus** — `BusHandle (addrW dataW)`, `BusArch`/`BusPort` (wires only; no repr).
- **Memory map / address space** — register aliases + bus layout → docs / C
  headers.
- **SoC builder** — `SysDSL`, `createBus`/`attachPeripheral`/`createHarvardCPU`.

(Definitions begin below as you describe each type.)

## ISA core definition — `CPUDef` / `AluDef` / `CPURegister` / `CPUFlag` (+ aliases)

**User model:** an easy way to define **all CPU fields** + **memory aliases** +
**register bit maps** (bit maps reusable — they appear in peripherals too).
The core **just satisfies the `HdlType` class** — so it is *accessed and
expressed as* an `HdlType` directly (state record, width, structure from the
instance). The core-specific extras (memory aliases, flags, bit maps, names,
semantics) are **additional composable layers — separate typeclasses on the same
type** — not crammed into `HdlType`, and not a separate parallel type system.
(Considered briefly: a distinct core type system that *reduces* to HDL; set aside
in favour of "satisfy `HdlType` + extra classes".) Naming matters: **define a
record with deriving** (names + widths from it), then **wire it up in a monad**.

**Current:** `CPUDef a = Writer CPUSchema a` — a *declaration* monad with
`reg`/`regFile`/`flagPack`/`aliasReg`/`aliasFile`. The returned core (`AVRALU
pcW`) is a record of **name-handles** (`CPURegister w = CPURegister String`,
`CPURegFile`, `CPUFlag`), **not** an `HdlType`. `flagPack`/`CPUFlag` (CPU bit
map) is a **separate** mechanism from the peripheral `BitField`. Names are
**duplicated** (Haskell field `avrPC` + string `reg "PC"`). The `CPUSchema` is
already multi-interpreter (Synth/Sim/Doc) — good (P1).

- [ ] **C1 (reframe — core)** — The core **satisfies `HdlType`**: the state is an
  `HdlType` record via `deriving (Generic, HdlType)` (structure-preserved, H1/H2),
  not a bundle of `CPURegister String` handles. **All its fields satisfy `HdlType`
  too** (recursive — exactly H2's "record is `HdlType` when every field is"): each
  register is a first-class `HdlType` value, e.g. `pc : Unsigned 16`,
  `sreg : Sreg` (a bit-map record, C2), `gpr : Vec 32 (Unsigned 8)` (an array,
  H4). So core / registers / bit-maps are the *same* `HdlType` mechanism,
  recursively (resolves C6 → typed fields, not name-handles). The alias / flag /
  name concerns ride *separate composable classes* on the same type (C2/C3/C5),
  not part of `HdlType`, not a separate type system. Field access is
  **length-by-default projection** — PC width = `Width` of the `pc` field, so the
  free `pcW` disappears (§4a / the PC-width directive).
- [ ] **C2 (unify — bit maps)** — A **bit map = an `HdlType` record of named
  bit-fields** (e.g. `data Sreg = Sreg { c,z,n,… :: Bit } deriving (Generic,
  HdlType)`). Collapse the CPU `flagPack`/`CPUFlag` mechanism *and* the peripheral
  `BitField` mechanism into this one reusable Part-I record concept. Reusable
  across CPU registers and peripheral registers (Part II).
- [ ] **C3 (naming)** — Register/signal/doc names come from the **record field
  names** (via deriving), single-sourced — not duplicated string args
  (`reg "PC"` beside Haskell `avrPC`).
- [ ] **C4 (wire-up monad — flexible)** — A monad wires the derived record into
  hardware (register per field via `Hdl`, plus the locations below). *Wiggle
  room OK* — one builder vs declare-then-wire is not pinned. Its real job is to
  make the two **location** relationships (C5) easy to express.
- [ ] **C5 (locations — addresses & flag-bits)** — One unifying idea: **place a
  field's flat view (H6) at a position within a containing space.** Two cases the
  core must express easily:
  - **Field → address**: a register/field located in the data **address space**
    (the memory alias). *Explicit* but easy declaration ("this field is at 0x5F");
    feeds the memory-map / C-header type (Part II).
  - **Flag → bit-in-register**: a flag located at a **bit position in a register**
    (C = bit 0 of SREG). **Derived for free** — if `Sreg` is a bit-map `HdlType`
    record (C2), the flag's bit offset *is* its position in `Sreg`'s flatten (H6).
    "flag = bit N of SREG" = field projection + the record layout; no separate
    declaration. The same byte is reachable both as the whole `Sreg` (via the
    address alias) and per-flag (via projection).
- [x] **C6 (field handles — RESOLVED by C1)** — Fields are **typed `HdlType`
  values** reached by record-field projection (length-by-default), not
  `CPURegister String` name-handles. The handle type is subsumed; remaining detail
  is only the projection mechanism (typed lens vs `Generic`), shared with the
  Part-I field-access question.

## ISA declaration — instruction definition + ISA definition

Two types. Both largely **match** the code; gaps are backend-plurality and a
width-compatibility typecheck.

### Instruction definition
**User model:** monadic; expresses an instruction's **name**, **bit encoding**,
and **CPU steps as compile-time microcode**. One definition is realizable as a
**stalling** CPU, a **pipeline** CPU, or converted to **microcode** for that kind
of CPU. It is the core of what each instruction does.

**Current:** `m ()` in `MonadALU` — body sets `mnemonic`/`encoding` + effect
steps; `runISABuild` lowers it to **`InstrIR`** (a value), interpreted by
Synth/Sim/Doc. The monadic body → `InstrIR` *is* the backend-agnostic
compile-time microcode.

- [ ] **I1 (match)** — Instruction = monadic body (`mnemonic` + `encoding` +
  effect steps) → `InstrIR`, the backend-agnostic "compile-time microcode."
  Largely matches the model.
- [ ] **I2 (additive — backends)** — One `InstrIR` → multiple CPU realizations:
  **stalling/multi-cycle** (EXISTS: `synthHarvardCPU'`/`SynthVnCPU`), **pipeline**
  (future), **microcode ROM** (future). `InstrIR` is the right factoring; add the
  pipeline + microcode interpreters (P1-style).
- [ ] **I3 (encoding width)** — `encoding` is an unchecked `String`; should carry
  / be checked for its bit width (feeds A2).

### ISA definition
**User model:** combines the **instruction set** + **interrupt behaviour** (also
an instruction) + **reset state** + any other data needed to define a CPU core.
Plus a **typecheck that instruction width and ISA width are compatible** (incl.
**Harvard vs VN**).

**Current:** `ISADef m = { isaInstrs :: [m ()], isaInterruptBody :: Maybe (m ()),
isaReset, isaPc }` — combines instruction set + interrupt-body + reset + pc.
Widths are `ISABuild`'s type params (`wordW/addrW/cwW/caW`); Harvard vs VN via
`MonadHarvardALU` vs the VN class.

- [ ] **A1 (match)** — `ISADef` combines instruction set + interrupt (a body,
  "also an instruction") + reset + pc. Matches.
- [ ] **A2 (additive — TYPECHECK)** — Enforce at the **type level** that
  instruction widths are compatible with ISA widths, *differently per memory
  model*:
  - **VN**: code and data are one memory ⇒ **a single width**; one check —
    instruction **encoding width ↔ the unified word width**.
  - **Harvard**: separate memories ⇒ **two widths**; two checks — encoding width ↔
    **code** word width (`cwW`), and data ops ↔ **data** word width (`wordW`).
  Maps onto `ISABuild`'s params (`wordW`/`addrW` = data, `cwW`/`caW` = code): VN
  collapses them to one pair, Harvard keeps both. Today `encoding` is an unchecked
  `String` and the widths are free params — no compile-time check. Same family as
  the RJMP/`immediate` typing gap.
- [ ] **A3 (uniformity)** — interrupt-as-instruction: confirm `isaInterruptBody`
  is treated uniformly with `isaInstrs` (same DSL + lowering). Mostly already so.

## Peripheral — `PeriphDef` (typed register block + bound HDL logic)

**User model:** describe a set of **typed registers with offsets** (like a CPU
core def), **including flags**; then an easy way to **bind the peripheral's HDL
logic to the type definitions**. The two can be **done together**.

**Current:** `PeriphDef p sig dat a` — one do-block holding register *metadata*
(`FieldSpec` via `field8`/`fieldOf @a`: offset, access, name, repr) + bit-fields
(`BitField`) + *behaviour* (`onWrite`/`onRead`, the FSM); `runPeriphDef` →
`(result, readData, spec)`; multi-backend via `PeriphOps` (good, P1). Does
"together" already, but registers are declared *imperatively* (not a typed
record) and logic binds by **offset integer**.

- [ ] **PE1 (unify with core def)** — A peripheral's register set **is** a
  core-def-style **typed `HdlType` record** (C1) with **bit-maps** (C2) and
  **offsets** (C5) — the *same* mechanism as the CPU core, reused. Replaces the
  imperative `field8`/`fieldOf`+`BitField` declaration with a derived record.
- [ ] **PE2 (bind logic to typed fields)** — Bind read/write logic to the
  **typed register fields** (field projection), not raw offset integers
  (`onWrite "setpoint" 0 0`); the offset comes from the record/location, the
  behaviour names the field.
- [x] **PE3 (done together — already so)** — Keep the one-do-block style
  (register decl + behaviour together); `PeriphDef` already matches "done
  together."
- [ ] **PE4 (bus-slave deltas from a CPU core)** — What's peripheral-specific vs
  core-shared: offsets are **base-relative** (the bus assigns the base; C5
  locations relative to a base), and the logic is **bus-slave read/write
  responses** (onWrite captures a bus write; onRead drives a bus read), not
  instruction execution. Everything else (typed record, bit-maps, offsets) is the
  shared core/peripheral register-block mechanism.

## Bus — `Bus arch` (single master + based children; protocol as a type param)

**User model:** a single-master bus + downstream peripherals, **each with a base
address**. A peripheral **can itself be another bus** (nesting). The **bus
protocol is a type parameter** (Wishbone vs AXI vs …). The interface must support
**stalling** and **multiple simultaneous transactions**; a concrete bus either
**arbitrates** (full) or acts as a **subset**. Both **peripherals and buses
reduce to `Hdl` via runners**, and **different runners allow bus introspection**
(memory map / structure) vs synthesis.

**Current:** `Bus arch = Bus (BusDef ())` — protocol `arch` is a **phantom type
param** ✅ (`SimpleBus`/`BurstBus`). `BusDef [ComponentSpec]` — master + children
with base addresses ✅. `BusArch.synthBus` allows **sub-bus children** ✅ (protocol
layer). `BusPort.bpStall` = stalling ✅, but the protocol is **single-outstanding**
❌. `SimpleBus` = no stall/bursts — a subset (notion exists). `runPeriphDef`
(`hdlOps` → Hdl vs `nullOps` → spec) + independent bus spec extraction already
hint at multi-runner introspection.

- [ ] **BU1 (match)** — protocol-as-type-param (`Bus arch`), single master +
  based children (`BusDef [ComponentSpec]`), sub-bus nesting (`BusArch`) — all
  present and matching.
- [ ] **BU2 (interface — multiple outstanding)** — The full bus interface should
  support **stalling** (have it) + **multiple simultaneous/outstanding
  transactions** (GAP — currently single-outstanding). A concrete bus either
  **arbitrates** (full) or is a capability **subset** (`SimpleBus` = single, no
  stall). The subset/superset *binding* rule is BU6.
- [ ] **BU6 (capability typeclass hierarchy — bind-time subsumption)** — A
  typeclass hierarchy encoding **capability subsumption**: a master may drive a
  child only when it is **at least as capable**. Two axes (same shape):
  - **Stall**: a **non-stalling master can never drive a stalling child**; a
    *stalling* master driving a *non-stalling* child **is** allowed. (One
    direction.)
  - **Width**: a **wider master can drive a narrower slave** (32-bit can read/
    write an 8-bit slave) but **not the reverse** (8-bit master ✗ 32-bit slave).
  Bind only type-checks when master ⊇ child on both axes. Today this is a comment
  ("driving a stalling bus from a SimpleBus is broken by construction"), not a
  type. (Relates to the BusHandle width-bind work, #3b.)
- [ ] **BU3 (protocol library)** — Only `SimpleBus` is real (`BurstBus` stub; no
  Wishbone/AXI). The type-param mechanism is ready; the protocol *library* is thin.
- [ ] **BU4 (bus-as-peripheral in the DSL)** — Nesting exists at the `BusArch`
  layer (children can be sub-buses) but the high-level DSL `attachPeripheral`
  takes `PeriphToken`s; expose attaching a **bus as a child** so "a peripheral can
  be another bus" is ergonomic end-to-end.
- [ ] **BU5 (runners — multi-interpreter at the system layer)** — Both
  `PeriphDef` and `Bus` **reduce to `Hdl` via runners**; **different runners give
  different views**: a synthesis runner (→ `Hdl` hardware) and an **introspection**
  runner (→ memory map / register layout / bus tree → docs/C-headers/linker
  script). This is P1 applied to the system layer. Partially present
  (`hdlOps`/`nullOps`, independent spec extraction); generalize the runner set.
- [ ] **BU7 (bus bridges/adapters — protocol & width conversion)** — A **place to
  express conversion** between mismatched bus endpoints, distinct from BU6 (which
  only says what's *permitted*). Two kinds, likely one adapter mechanism:
  - **Protocol conversion** — a **bridge** between `Bus arch`s (e.g. Wishbone ↔
    AXI): realize one protocol's master/slave from the other's.
  - **Width conversion** — a **gasket** for differing data widths (byte-lane
    steering / pack-split; e.g. 32-bit master ↔ 8-bit slave).
  Same shape as the S2 domain-crossing strategy: an explicit, **type-injected
  adapter** between endpoints. BU6 gates direct binds; BU7 supplies the
  conversion when you bridge across a protocol or width boundary. Home: a
  `BusBridge`/adapter type or class (new — none today).

## Memory map — ephemeral (NOT a persistent type)

The memory map exists only **ephemerally during construction**. It may be
necessary, but it is produced and **consumed by the individual bus runner
interpretations** (BU5 — introspection / docs / C-headers / linker script), not a
persistent first-class type to define. Folds into BU5; no separate type. (The
current `SysDoc` / `sysExtractMemoryMap` should be a *runner output*, not a stored
artifact.)

## System DSL — `SysDSL` (heterogeneous system description)

**User model:** a **system-level description** — instantiate **peripherals, buses,
CPUs**. A system may have **multiple buses of various widths**, **multiple CPUs**,
**multiple peripherals** connected in **various** configurations, and the DSL must
**track** all of it.

**Current:** `SysDSL dom dat a = StateT SysDoc NetM a` — `create*` constructors
for peripherals/buses/CPUs; tracks in `SysDoc`. **Monomorphic**: fixed to **one
clock domain** `dom` and **one bus data width** `dat`
(`createBus → BusHandle 32 (Width dat)`). Multiple CPUs/peripherals work via
repeated calls but share that single `dom`/`dat`; `createBus` is one linear bus.

- [ ] **SY1 (match)** — Instantiates peripherals/buses/CPUs and tracks them.
  Basic "system level description" matches.
- [ ] **SY2 (reframe — multiple clock domains)** — Drop the single `dom`: a
  system has **multiple domains**; each bus/CPU/peripheral lives in its domain,
  and crossings are explicit (S2 strategy). GAP — today one `dom` for everything.
- [ ] **SY3 (reframe — multiple bus widths)** — Drop the global `dat`: support
  **multiple buses of various widths**, each independently width-typed
  (`BusHandle` already is). Different-width buses connect via **bridges/width
  gaskets** (BU7); binds gated by subsumption (BU6). GAP — today one `dat`.
- [ ] **SY4 (heterogeneous topology)** — Support **multiple CPUs (masters),
  multiple buses, peripherals on various buses**, in arbitrary connectivity —
  not the single-`dom`/`dat`, single-linear-bus shape. Connections type-checked
  by BU6 (capability) + S2 (domain). A bus may be a child of another bus (BU4).
- [ ] **SY5 (tracking)** — The DSL **tracks the full heterogeneous topology**
  (domains, widths, instances, connections) for the runners to consume
  (synth + introspection, BU5). Today `SysDoc` tracks only buses+CPUs and should
  become a *runner output* (memory-map note), not the stored description.
- [ ] **SY6 (multi-render — per-CPU outputs)** — SystemDSL renders to **Hdl**
  (synth) *and* other flavours via runners (P1 at the top level): **per-CPU
  bus/memory maps** and **C declarations**. KEY: these are **per-CPU** — each CPU
  master sees its own address space (the buses/peripherals reachable from it
  through the topology), so maps/headers are computed *per CPU* by traversing that
  CPU's reachable bus tree (SY5 topology). Today `sysExtractMemoryMap`/
  `sysGenCHeader` produce a **single global** map from `SysDoc` — must become
  **per-CPU** *and* framed as runner outputs (BU5), not a stored artifact.
- [ ] **SY7 (system REDUCES to `Hdl I/O` — not identity)** — A system is **not**
  `Hdl SystemIn SystemOut`; it is its own **combined** type that can be **reduced
  to** one. The `Hdl SystemIn SystemOut` arrow (I/O-typed, D-series shape; external
  face = typed system pins, possibly multi-domain) is the system's **synthesis
  reduction** — one of several, alongside the per-CPU bus maps and C decls (SY6).
  The heterogeneous topology (domains, buses, CPUs) is internal to the combined
  type; reduction projects it out. (Distinction matters: the combined system type
  carries more than the Hdl arrow exposes — same is/reduces nuance as the core.)

## Target organization (for the DEFINITIONS.md consolidation)

Regroup the model (user, 2026-06-26) — supersedes the per-type section order:

- **Part I — HDL core (foundation):** the four pillars `HdlType` / `Signal` /
  `ClockDomain` / `Hdl` (P, H, S, D series). Unchanged.
- **Part II — system definition:**
  1. **CPU** — core definition + ISA + instruction definition (all CPU-specific):
     **C / I / A** series.
  2. **Bus & bus-peripherals** — the bus interconnect + peripherals as bus slaves:
     **BU + PE** series. (Bus and peripheral are one topic — peripherals are what
     hangs off a bus.)
  3. **Address mapping** — a **shared helper class**: register/field **locations**
     (C5), peripheral base-relative **offsets** (PE4), register **aliases**.
     Factor the "place a field's flat view at a position in a containing space"
     mechanism into one reusable helper used by both CPU cores and peripherals.
  4. **SystemDSL** — the **combined** type tying CPUs + buses + address mapping
     together (**SY** series); *reduces to* Hdl I/O **+** per-CPU bus maps **+** C
     decls (SY6/SY7). Not identical to any one reduction.
