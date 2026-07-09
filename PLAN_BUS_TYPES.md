# PLAN: bus/peripheral type architecture (types & API first)

Status: **design, iterating** (2026-06-29). Types and API first; stall, CDC, and
the data-driven front-end are later. This captures the model converged in
discussion.

## Core idea

There is **no peripheral-vs-bus distinction**. There is one recursive thing — an
**address-space node** — and combining nodes is what widens the address space. A
GPIO is just a 2-bit node (4 register slots); the system bus is a wide node built
by laying smaller nodes out in an address space; the CPU drives the root node.

Two layers, deliberately separated so the system can become **data-driven** later:

- **Types = the static contract.** Bus protocol + capabilities (class
  constraints), clock domain, data width, and each *leaf's* static size.
- **Value-level data = the address map.** A layout built from spacing objects;
  addresses are *derived* by a layout pass, never hand-written. Because it is
  ordinary data, a config/table/generator can produce it later.

> Invariants live in types; the address map lives in value-level layout data.

## The node type

```haskell
data Bus proto dom (addrW :: Nat) (dataW :: Nat)
--       ^protocol  ^clock domain  ^space = 2^addrW   ^data width
```

`proto` is a concrete bus protocol (`SimpleBus`, `Wishbone`, …). Capabilities are
**typeclasses on `proto`**, and the *physical handshake wiring is the instance
method* — not a phantom tag:

```haskell
class BusArch proto                       -- every protocol: decode + read mux
class BusArch proto => StallingBus proto  -- adds the wait-state handshake wiring
-- future: Burstable, Atomic, ByteEnable, …  (open: add a class, not a kind case)

instance BusArch SimpleBus                 -- combinational, no stall
instance BusArch Wishbone
instance StallingBus Wishbone              -- Wishbone can stall; SimpleBus cannot
```

A node's *need* is a constraint on `proto`; the illegal combination is just a
missing instance, surfaced at the connection site (no `Capability` kind, no
`Subsumes` lattice — both retired).

## Leaf constructors

Physical (off-bus) I/O is *just signals* — inputs in, outputs out — never in the
type. The node type carries only the bus contract.

```haskell
newGPIO :: Sig dom dat -> (GpioOuts dom dat, Bus proto dom 2 (Width dat))
--          ^pin inputs    ^pin outputs       ^a 4-slot node, any protocol

newRam  :: StallingBus proto => Word32 -> ((), Bus proto dom aw (Width dat))
--          ^needs wait-states (a constraint), nothing else special
```

The caller routes the returned outputs (to `sysOutput`, another block, …); the
node only ever appears to the address-space layer as `Bus proto dom aw dataW`.

**Constructors are pure** (`inputs -> (outputs, node)`, no `SysDSL`). `Sig dom a
= SWire WireId | SExpr (NetM WireId)` is a *deferred* signal: combinational ops
build `SExpr` thunks with no effect, and `materialize` runs them once (memoised by
stable name) at elaboration. So output signals and forward references compose as
plain values — wire allocation/emission happens later — which is also the right
substrate for building peripheral trees from data.

## Addressing — width-respecting binary combine (value-level)

No hand-written bases. The address space is a **binary trie**: the only
composition primitive combines two nodes of *equal* address width into one node
that is exactly one bit wider — the new MSB is the decode bit.

```haskell
(<+>) :: Bus proto dom w dataW -> Bus proto dom w dataW -> Bus proto dom (w + 1) dataW
empty :: KnownNat w => Bus proto dom w dataW                    -- a width-w hole (unmapped)
grow  :: Bus proto dom w dataW -> Bus proto dom (w + 1) dataW   -- = (<+> empty): reserve upper half
```

Everything is structural, by construction:

- **Alignment is free** — a width-`w` node is a whole subtree, always on a `2^w`
  boundary; a misaligned placement is inexpressible.
- **Sizes are powers of two; bases are derived** — a leaf's base *is* its path of
  select bits from the root. No address is written.
- **Non-overlap is structural** — two nodes cannot share a trie slot.
- **Unequal widths must be padded first** — `grow` lifts a node a bit at a time
  (reserve the upper half), so combining a 2-bit peripheral region with an 11-bit
  RAM means growing the region to 11 first; reserved space is explicit.

Composition is just `+1`, statically known when leaves are statically sized — no
`max`, no existential `addrW` in the common case. It is still a plain value tree,
so the data-driven front-end builds the same structure at runtime.

**Base-address helpers live on top** and compile to `empty`/`<+>`/`grow`:

```haskell
at     :: <base> -> Bus proto dom w dataW -> …          -- place at a base (pad+combine to that slot)
region :: [(<base>, node)] -> Bus proto dom w dataW      -- build a map; fit/alignment/overlap by construction
```

You can still think in base addresses where convenient; they are sugar over the
trie, and the trie guarantees the invariants. The derived memory map *is* the
documentation.

## What this retires / replaces

- `attachPeripheral` + `createBus` → one combinator (lay nodes out → wider node).
- `BusHandle` / `orphanBusMaster` / master-vs-slave split → gone; every node is
  the same shape, the CPU is just whatever drives the root.
- `Capability` data-kind + `Subsumes` + `cap`/`phys` type params → gone (classes
  + plain signals + the layout pass do the work).
- `PeriphToken … ptAddrSize` runtime size → size is `2^addrW` from the leaf type.

## Stall: a two-sided, must-implement surface

The framework provides the *surface*; behaviour is the implementer's. But an
ignorable wire can never prove the master honours stall, so the handshake is
**not** exposed to the master as a signal — it is owned by the transaction
primitive, and driving a stalling bus is gated by a master instance the device
**must implement**.

```haskell
-- SLAVE: a peripheral that may stall (provisions the handshake wire).
-- Calling assertStall carries `StallingBus proto`, infecting the subtree's proto.
class BusArch proto => StallingBus proto where
  assertStall :: Sig dom Bool -> SlaveM proto dom ()

-- MASTER: behaviour is expressed as transactions; the master never sees bpStall.
-- `transact` yields its Response only on completion, so the read data is
-- unreachable until the transaction finishes — the master cannot sample early.
class BusMaster proto m where
  transact :: m -> Request -> MasterM proto dom Response

-- A master that drives a STALLING bus must implement this; its `transact`
-- holds until stall clears, so the instance is unwritable without engaging the
-- handshake. "Must implement" is the obligation, not an optional method call.
class BusMaster proto m => StallingMaster proto m

driveRoot :: BusMaster proto m => m -> Bus proto dom aw dw -> SysDSL ()
-- driving a stalling subtree additionally demands `StallingMaster proto m`
```

Consequences:

- **No ignorable stall.** The master expresses `transact`s and never touches a
  stall wire; correct handling is not the master's to get wrong — the protocol's
  `transact` owns it, and the `Response` gates the result on completion.
- **Must-implement obligation.** A device with no `StallingMaster` instance
  cannot be wired to a stalling bus — compile error at the connect site.
- **Subsumption is free.** `StallingMaster ⊇ BusMaster` (superclass), so a
  stall-capable master drives both stalling and non-stalling buses; a master
  written against `transact` is protocol-polymorphic.
- **Master capability earns its keep only for the can't-pause case** — a fixed
  combinational master needs `transact` to complete combinationally, a constraint
  on the *protocol*, and the real subsumption (`Holdable ⊇ CombinationalOnly`).

## Cross-cutting nodes (later, but typed-for now)

Width bridge, stall bridge (`stallAdapter`), and **CDC** (`domA → domB`) are each
*also* just `Bus` nodes that present one face to the parent and another to the
child. Domain mismatch is a `dom ~ dom'` failure unless routed through a CDC node.
Bake the `dom` parameter + crossing-node signatures in now; implement later.

## The HDL layer (`Hdl` / `HdlIO` / backends)

The bus nodes above sit on a layered HDL substrate, all typeclasses; concrete
monads are only *instances*. Layering: `HdlType` (bits → VHDL/Verilog) → `Hdl m`
(monad) → `HdlIO` (entities) → `SysDSL` (buses/CPU/IRQ/clocks). In progress on
`docs/fundamentals-alignment`; core committed (`Hdl.Monad`, `06c543c`).

**`Hdl m` — the monad typeclass (minimal primitives only).**
```haskell
class (Monad m, MonadFix m) => Hdl m where   -- MonadFix = the backward/feedback wire
  register     :: (HdlType a, KnownDom dom) => a -> Sig dom a -> m (Sig dom a)   -- the ONE state primitive
  registerEn   :: (HdlType a, KnownDom dom) => a -> Sig dom Bool -> Sig dom a -> m (Sig dom a)
  forceConnect :: Sig d1 a -> m (Sig d2 a)                                       -- cross-domain escape (CDC only)
  caseOf       :: (HdlType sel, HdlType a, KnownDom dom)                         -- exact-match decode (mux today, real `case` later)
               => Sig dom sel -> m (Sig dom a) -> Map sel (m (Sig dom a)) -> m (Sig dom a)
  -- caseRange (ordered, (lo,hi) keys, overlap-checked at emit) — sibling of caseOf
```
Combinational logic stays *pure* `Signal`/`Sig` ops (not in the class). FSMs/CPU
ISAs are *lowerings onto* these primitives (`rec s <- register i0 (caseOf s …)`),
not new methods — `MonadALU`/`ISA` already proves this for CPUs.

**`Named` + naming** (kills bare `wN`): `Named a` is a representation-identical
marker on the *value* type. `name :: String -> Sig dom a -> Sig dom (Named a)`
(attach, outputs); `erase :: Hdl m => Sig dom (Named a) -> (Sig dom a -> m r) -> m r`
(scoped strip, inputs — the name seeds a scope for derived logic).

**`HdlIO` — the entity typeclass (methods, instanced per backend).**
```haskell
bind     :: (Named a, Named b) => String -> (a -> m b) -> h a b   -- we generate the body
foreign_ :: (Named a, Named b) => String -> [Generic] -> ForeignSrc -> h a b  -- body is external VHDL/Verilog
withSim  :: (a -> m b) -> h a b -> h a b                          -- optional Haskell sim model for a foreign block
entity   :: h i o -> i -> m o                                     -- instantiate any of them, uniformly
```
Interfaces are `Named` records (derived); `bind` erases the input bundle for the
plain-signal body and re-`name`s the outputs. Black-box/OEM primitives:

```haskell
data ForeignSrc = Referenced [Import]          -- emit these library/use clauses (user- or us-authored)
                | Vendored   FilePath [Import]  -- ship the file into the fileset + emit its clauses
```
Provenance: user-library refs → user supplies the imports; manufacturer prims →
we bake the imports into the shipped declaration; our-backend-library VHDL → we
vendor the file. A foreign block is **opaque in sim** (drives `X`) unless
`withSim` attaches a model. Generics ride as a `[Generic]` → the generic map.

**Backends are just instances.** Synthesis (the `NetM`-based `newtype HDL i o a`)
is one `Hdl`/`HdlIO` instance; sim, vhdl, verilog are siblings — none privileged.

**Helpers, not primitives.** `ram`/`rom`, `name`/`erase`, scope: free functions
over the monad (memory hides its IR inside the helper) — *not* `Hdl` methods.

**Output is a fileset/manifest**, not a single `.vhd`: generated entities + the
union of import clauses + any vendored files.

## Sequencing

0. **HDL layer** (underway): add `HdlIO` (`bind`/`foreign_`/`withSim`/`entity`) +
   `ram`/`rom` helpers; drop `ramS`/`romS` (→ helpers); convert `Hdl.Entity` onto
   the monad; ripple entity bodies + consumers; verify. Then ↓ builds on it.
1. **Types/API** (this doc): `Bus proto dom addrW dataW`, capability classes,
   `newX :: inputs -> (outputs, node)`, the `Layout` algebra + `layout` pass.
2. **Stall**: `StallingBus` instances whose method muxes the selected child's
   `bpStall` up; drive the CPU `stall` input from the root node's stall. (The CPU
   sequencer already splits same-address read/write across cycles — RMW stays
   correct, each access can additionally be *held*.)
3. **CDC + bridges**: implement the crossing nodes.
4. **Data-driven front-end**: build `Layout` (and protocol/width choices) from
   data; the type contract guards the generated system.

## Open decisions

- ~~Pure vs `SysDSL` leaf constructors~~ → **pure** (`Sig`'s `SExpr` defers
  netlist effects to `materialize`; outputs/forward-refs compose as values).
- Node name: `Bus` vs `Region` vs `Space` (bikeshed).
- `Layout`/trie existential over child `addrW` vs a fixed combinator shape.

---

## Conversion progress (2026-06-30): bus wiring landed, cache/CPU leg next

**Done & compiling (up to the cache leg):**
- `Hdl.Class` gained two typed feedback primitives (sanctioned, wrap netlist
  internals): `freshSig :: NetM (Sig dom a)` (forward-declared/undriven wire) and
  `connectSig :: Sig dom a -> Sig dom a -> NetM ()` (typed `alias` — drive a
  `freshSig` placeholder). These replace bare `freshWire`+`alias` at the system
  layer. `isacle-hdl` builds clean.
- `BusArch`: `synthBus` is a **pure** `Signal` function `MasterReq -> [BusChild]
  -> (SlaveResp, [MasterReq])` (no `NetM`). `BusHandle`: typed `Sig` fields,
  `BusHandle dom addr dat`.
- `SystemDSL` `createBus`/`attachPeripheral`/`PeriphSlot` fully converted —
  **no `WireId`/`inBlock`/`BusPort`/`alias`**:
  - `PeriphSlot` carries `psRun :: NetM (SlaveResp Sig dom dat)` (materialises the
    peripheral: promotes phys via `emitPhysOuts`, returns its response). The
    peripheral logic is built **purely** by `runPeriphDef hdlOps bus def`
    (returns deferred `Sig`s) from the bus request.
  - `BusDSLState` gains `bdsReqFeed :: [MasterReq Sig dom (Unsigned 32) dat]` —
    per-slot requests fed back via the **mdo knot** in `createBus`.
    `attachPeripheral` indexes it by slot position and returns the typed phys
    bundle directly (no pre-allocated placeholders).
  - `createBus` is one `mdo`: `synthBus` produces `childReqs` (→ `bdsReqFeed`) and
    `masterResp`; `mapM psRun slots` gives the responses (→ `children`). Cycle is
    well-founded: `childReqs`/`selOf` depend only on master + static base/size,
    never on responses. Master read-data/stall driven into the handle with
    `connectSig`. `RecursiveDo` pragma added.

**Next domino (the gate): cache/CPU leg.** `cabal build lib:isacle` now stops at
`Cache/L1.hs:64` — `synthL1Cache :: ... -> BusHandle busAddrW dat -> NetM
CacheHandle` still uses the **old Nat-param** `BusHandle`. Fix its signature to
`BusHandle dom (Unsigned 32) dat`, then `createHarvardCPU`/`createCachedCPU`
(which allocate the handle's `bhRdData`/`bhStall` as `freshSig` placeholders the
CPU reads, then `createBus` drives via `connectSig`) and the master-side wiring.
The CPU master ports come from the typed `BusHandle`/`MasterReq`.

**Then the bulk:** retarget `SynthCPU`/`SynthVnCPU`/`Lower`/`Synth`
(`synthHarvardCPU'`) from `InstrIR -> NetNode` to `InstrIR -> Hdl` (regfile banks
→ `register` over an array type; decode → `caseOf`; exec sequencer → clocked
`Hdl`). Needs a register-file/bank class primitive added to `Hdl`. This is the
multi-session piece. Finally: convert `HdlCircuit.emitPhysOuts` to `outputS`
(drop raw `emit`), then un-export `NetM`/`emit`/`freshWire`/`inBlock`.

---

## UPDATE (2026-06-30): bus + cache/CPU-boundary conversion DONE & GREEN

The typed-signal conversion of the whole system-layer wiring is complete and
verified end-to-end: **ISACLE `cabal test` 1/1 pass; clavr `cabal test` 658/658
pass; clavr `ghdl-sim` all instruction coverage (cov_flow/cov_arith/cov_io +
every fixture) passes through real VHDL synthesis.**

What landed:
- `freshSig`/`connectSig` typed primitives in `Hdl.Class`.
- `BusArch` pure `synthBus`; `BusHandle dom addr dat` typed.
- `createBus`: **two-pass, no `mfix`** (the earlier mdo-knot deadlocked with a
  `<<loop>>` because `mapM psRun` forced its own result's spine). Pass 1 runs the
  (effect-free) `BusDSL` with a neutral request feed to discover the static
  base/size layout; routes per-child requests purely via `synthBus` (the child
  request ignores the response, so a `error` placeholder response is safe). Pass
  2 re-runs the `BusDSL` with the routed requests, materialises peripherals
  (`mapM psRun`), and builds the read mux. No `WireId`/`inBlock`/`BusPort`/`alias`.
- `attachPeripheral`/`PeriphSlot`: peripheral built purely by `runPeriphDef`;
  `psRun :: NetM (SlaveResp Sig dom dat)` promotes phys outputs + returns the
  response. Phys output names threaded explicitly (`<inst>_<port>`), so
  `emitPhysOuts` now takes a `[String]` names list (the old per-entity `p0/p1` +
  `promotePhysOuts` rename step is gone).
- `createHarvardCPU` builds the typed `BusHandle` by wrapping the CPU's output
  wires as `SWire` (master→fabric) and `bhRdData`/`bhStall` as placeholders the
  bus drives via `connectSig`. `orphanBusMaster`/`createL1Cache`/`synthL1Cache`
  (VN pass-through stub) likewise typed; callers dropped `@32 @8`.

**Boundary note:** the CPU/cache *internals* (`synthHarvardCPU'`, `synthVnCPU'`,
`Lower`/`Synth`, the L1 stub) are still raw `NetM`/`WireId` — they are *bridged*
to the typed bus via `SWire` at the `BusHandle`. That retarget
(`InstrIR -> NetNode` ⇒ `InstrIR -> Hdl`, + a register-file/bank `Hdl` primitive)
is the remaining bulk (todo #5). Also still internal-`NetM`: `HdlCircuit.hdlOps`
+ `emitPhysOuts` (materialize/emit), and `createHarvardCPU`'s ROM/entity wiring
(`inBlock`/`NRom`). Final step after the retarget: un-export
`NetM`/`emit`/`freshWire`/`inBlock`.

---

## UPDATE (2026-06-30 cont.): ISA-compiler retarget started — register file on Hdl

First slice of the `InstrIR -> Hdl` retarget landed & green (commits 90c4a28,
3859861, 3142336):
- **`regBank` / `regBankRead`** added to the `Hdl` class (Monad.hs). `regBank`
  takes a runtime `Int` entry width (the bank is structural, type-erased bits —
  like `sigLitW`); netlist instance defers `NRegFile` emission for feedback
  safety. `regBankRead` is **eager** (fresh wire + emit per call) — an `SExpr`
  version gets memoised by `materialize`'s CSE and wrongly merges distinct
  indexed reads (caught as a cov_io GPIO regression).
- **SynthCPU** now emits the register file (write bank + both read sites) via
  `regBank`/`regBankRead` instead of constructing `NRegFile`/`NRegFileRead`
  nodes directly. `dom` pinned to `Type` (the `Hdl` class fixes
  `s :: Type -> Type -> Type`), propagated to `synthHarvardCPU'`/
  `synthHarvardCPU`/`createHarvardCPU`.
- Verified: clavr 658/658 + ghdl-sim full instruction coverage green.

**Remaining retarget bulk (the hard core):** scalar registers (`NReg` →
`register`/`registerEn`) require the **register-feedback restructure** — the
register output feeds the write arbiters which feed its next/enable, so the
scalar-register + arbiter section must become a `rec`/`MonadFix` block, and the
register outputs change `WireId -> Sig`, rippling through every reader
(`scReadRegFn`, `getFlagFn`, pc/code-addr, the exec sequencer). Combinational
logic (`emit NComb op` → `sigPrim*`), `buildMuxTree`/`buildOrTree` → `mux`/`.||.`
folds, and `Synth.hs`/`Lower.hs` (`SynthResult`/`RenderCtx` `WireId -> Sig`) all
ride along. That rippling `WireId -> Sig` conversion is the large atomic piece
still to do (then `SynthVnCPU`, then un-export `NetM`).

---

## UPDATE (2026-06-30): ISA compiler → real Hdl/Signal (NetM-free). In progress.

Correcting course: the `Backend.Wire` pass (commits 42ea7ed/09ba99d/7d2d710)
shrank the `freshWire/emit/NComb` boilerplate but kept the *wrong layer*
(WireId/NetM). Per the user, `SynthCPU` must be `(Hdl s m, Signal s) => … -> m
(…)` — combinational logic is pure `Signal` ops, state is `register`/`regBank`,
**no NetM/WireId/emit/materialize**. Signal *expressions stay monadic* only so
each new signal can be bound + named; **naming is an Hdl-layer op**, the new
`named :: String -> s dom a -> m (s dom a)` (committed). `Backend.Wire` deletes
itself — its combinators are the `Signal` operators (`comb2 PAnd` = `sigPrim2
PAnd`, `litW` = `sigLitW`, `sliceW`/`resizeW` = `sigPrim1 (PSlice/PResize)`,
`muxW` = `mux`).

**Done & validated:** `named` Hdl method (green). `Lower.hs` rewritten against
`(Hdl s m, Signal s)` — `LowerCtx s dom` with pure `s dom ()` leaf callbacks,
`Named s dom = {nSig, nName}`, `lowerExpr`/`renderInstr` monadic only for `named`;
`Rendered`/`RegWrite`/`Jump` carry `s dom ()`. Compiles clean standalone.
(Tree is RED until Synth/SynthCPU follow — they share these types.)

**Remaining (the intricate core) — confirmed design:**
- `Synth.hs`: parameterise `SynthResult`/req-records + `RenderCtx` over `s dom`.
  `renderSynth :: (Hdl s m, Signal s) => …  -> m (SynthResult s dom)`. Field
  decode/match/index = pure `Signal` + `named`. **Register-file reads inline via
  `regBankRead`** (drops the shared-read-port optimisation; one combinational
  port per read — correct, more VHDL) so there's no read forward-ref. Add
  `rcRegCount :: String -> Int` and `rcReadRes :: ReadTok -> s dom ()` to the ctx.
- `SynthCPU.hs`: the forward-refs resolve **two-pass + a small `rec`**:
  * Mem-read results: pass 1 renders with `rcReadRes = const rcDataBus` (discover
    read addresses); build the sequencer (exec_cycle `register`, latches
    `registerEn`) from them; pass 2 renders with `rcReadRes` = the sequencer's
    per-(instr,read) result signal. (Same two-pass shape as `createBus`.)
  * Scalar registers: `rec { out <- registerEn init en nxt; (en,nxt) <- arbiter
    out … }` (MonadFix). Reg outputs feed the arbiters which feed next/enable.
  * Decode/arbiters/alias-muxes: `mux` folds / `caseOf`. Reg file: `regBank`
    (already wired) for writes.
  * `synthHarvardCPU' :: (Hdl s m, Signal s) => CpuInputs s dom -> m
    (CpuMemIface s dom)`; inputs are signal args, outputs the returned record.
- `SynthVnCPU.hs`: same shape.
- Boundary: `createHarvardCPU`/`createCachedCPU` run it at `m = NetM`, `s = Sig`
  (the entity body), feeding `SWire` port inputs and `materialize`-ing outputs
  for `NOutput`. `NInput`/`NOutput`/ROM stay here (entity construction).
- Then delete `Backend.Wire`; un-export `NetM`/`emit`/`freshWire`.
