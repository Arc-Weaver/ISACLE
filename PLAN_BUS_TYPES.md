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
