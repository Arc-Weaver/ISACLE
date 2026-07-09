# PLAN: Shared Functional Cores

## Goal

Replace the current "every instruction grows its own datapath, a write arbiter
picks the winner" model with **shared functional cores**: fixed, reusable logic
blocks (ALU, multiplier, dividers, crypto, …) that are instantiated **once** per
CPU, emitted as their **own entities/files**, and driven by whichever instruction
is live.

Today, ADD synthesises its own adder (`GPR_b_add_GPR_a`), SUB its own subtractor,
MUL its own multiplier, and a big priority mux selects the committing result.
That is a *sea of function units*. Sharing replaces N adders with **one** adder +
operand muxes on its inputs — a large area win for wide units (dramatic for
multipliers and crypto), at the cost of input-side muxing. Standard resource
sharing.

## Two-layer principle (fixed cores + ISA wiring)

This mirrors the ISACLE ⇄ ISA boundary (`project_isacle_architecture`):

- **Cores are FIXED logic, defined once in ISACLE.** `Isacle.Core.ALU`,
  `Isacle.Core.Mul`, `Isacle.Core.Div`, `Isacle.Core.Crypto.*`. Width-parametric,
  ISA-agnostic, individually verifiable, emitted as standalone entities. They do
  not change per ISA.
- **The ISA WIRES UP to them.** A CPU's instruction bodies *invoke* a core
  (`alu Add a b`, `mul a b`); they never describe the arithmetic themselves.
- **The synthesis binds the wiring:** instantiate each core once, mux operands by
  the live instruction, drive op-select, route results/flags back, and (for
  multi-cycle cores) gate commit on `done`.

Chosen dispatch model: **explicit invocation** (not automatic `+`-recognition,
which can never generalise to crypto; not bus-attached coprocessors, which are
overkill for a single-cycle ALU — though see §7 for long-latency cores).

## 1. Core interface

Two shapes, both fixed ISACLE logic emitted as sub-entities.

### Combinational core (ALU)

```haskell
-- Isacle.Core.ALU  (FIXED)
data ALUOp = Add | Adc | Sub | Sbc | And | Or | Xor | Com | Neg
           | ShL | ShR | AShR | Rol | Ror | Swap | PassA | PassB
  deriving (Show, Eq, Enum, Bounded)          -- HdlType via REnum → VHDL enum

data ALUFlags = ALUFlags { fC, fZ, fN, fV, fH :: Bit }   -- generic, standard defs

-- port interface (one instance per CPU, emitted as alu.vhd):
--   in:  op : ALUOp, a : word, b : word, carryIn : Bit
--   out: result : word, flags : ALUFlags
aluCore :: (Hdl s m, KnownNat w) => ALUCoreIn s dom w -> m (ALUCoreOut s dom w)
```

The ALU produces a **generic, standard-definition** flag set (C/Z/N/V/H). ISAs map
these to their status register; AVR's flags follow the standard definitions, so
the mapping is direct. (If an ISA needs an exotic flag, it derives it in its own
wiring from the ALU result — cores stay generic.)

### Sequential core (MUL / DIV / crypto)

Multi-cycle, registered, with a `start`/`done` handshake and a fixed or
data-dependent latency:

```haskell
-- port interface (emitted as mul.vhd, etc.):
--   in:  start : Bit, a : word, b : word, ctrl : MulOp
--   out: done : Bit, result : dword
```

`done` integrates with the existing exec-cycle sequencer (§6).

## 2. Invocation in the ISA (MonadALU)

New `MonadALU` operations — the ISA's only contact with cores. They *record* an
invocation; they do not emit arithmetic.

```haskell
alu  :: ALUOp -> IExpr (Unsigned w) -> IExpr (Unsigned w)
     -> m (IExpr (Unsigned w), IExpr ALUFlags)          -- combinational
mul  :: IExpr (Unsigned w) -> IExpr (Unsigned w) -> m (IExpr (Unsigned (w+w)))  -- multi-cycle
```

Example — AVR ADD body becomes wiring, not arithmetic:

```haskell
addDef = do
  encoding "000011dddddrrrr"
  rd <- register gpr "dd"; rs <- register gpr "rr"
  a  <- readReg rd; b <- readReg rs
  (r, fl) <- alu Add a b            -- invoke the shared ALU
  writeReg rd r
  setFlags [C,Z,N,V,H] fl           -- commit this instruction's flag subset
```

`setFlags mask fl` commits only the flags this instruction affects (ADD → all;
CP → all but no reg write; MOV → none). Flag masking is per-instruction wiring.

## 3. IR representation

One new node carrying (core, op, operands); the result is an ordinary `IExpr`
the body consumes.

```haskell
-- an invocation of a shared core, result-typed
ICoreOp :: CoreId -> CoreOp -> [SomeIExpr] -> IExpr result
```

- `CoreId` identifies which fixed core (ALU / MUL / …).
- `CoreOp` is the op-select value for that core (an enum).
- Flags/second outputs: either a paired result node or a projection
  (`ICoreFlags`), TBD in §10.

## 4. Lowering & binding (synthesis)

The heart of the change, in `SynthCPU`/`Backend.Lower`:

1. **Collect** every `ICoreOp` across all instruction bodies (like `srRegWrites`
   etc. today), tagged with the invoking instruction's match/live signal.
2. **Instantiate each core once** as a sub-entity.
3. **Operand muxing:** each core input = priority mux over its invocations,
   selected by the live-instruction signal (exactly the arbiter pattern already
   used for register/scalar writes). Op-select = mux of each invocation's op.
4. **Result routing:** the core output is a single wire; because the operand mux
   already selected the live instruction's operands, the result *is* that
   instruction's result. It feeds the normal write arbiter / flag arbiter,
   gated per-instruction by match (unchanged downstream).

Only one instruction commits per cycle, so operand-side muxing is sound and
needs no per-op tags beyond the existing match signals.

## 5. Arbitration reuse

This reuses machinery that already exists:

- The **operand mux** is `priorityMux` over `(liveSignal, operand)` — identical to
  the scalar/register write arbiter.
- The **result** flows into the existing `RegWriteReq`/`ScalarWriteReq` path.
- The **flags** flow into the existing `FlagWriteReq` path (§2's `setFlags`).

So the synthesis diff is: add a "core-invocation" collection + one arbiter per
core input, and drop the per-instruction private datapaths.

## 6. Multi-cycle integration

Sequential cores (MUL/DIV/crypto) plug into the **exec-cycle sequencer** that
already sequences multi-access instructions:

- An instruction invoking a multi-cycle core asserts the core's `start` on its
  first exec cycle.
- The sequencer holds `commit` until the core's `done` (like `stall` today).
- The registered `result` is latched and consumed on commit.

No new global mechanism — `done` becomes another input to the commit gate.

## 7. Coupling follows the ISA: instruction vs. data bus

Whether a capability is a directly-invoked functional core or a bus-attached
coprocessor is **not a property of the logic** — it is decided by whether the
target ISA has a **dedicated instruction** for it:

- **A dedicated instruction exists** (an AES-round opcode, a MAC instruction, a
  hardware `MUL`) → wire it as a functional core, invoked from that instruction's
  body (§2). Tightly coupled, part of execute, operands straight from registers.
- **No dedicated instruction** → expose the *same* fixed logic as a memory-mapped
  peripheral on the data bus (reuse `PeriphDef`/bus machinery); software drives it
  with ordinary load/store. The "instruction" is just an `LD`/`ST` to its address.

The **fixed logic block is identical either way** — `Isacle.Core.Crypto.AES` is
one module; only the *wiring* differs. The same core can be a tightly-coupled
execution unit on a CPU whose ISA has crypto opcodes, and a bus peripheral on a
CPU whose ISA does not — chosen entirely by how the ISA/SoC wires it. This is the
two-layer principle (fixed cores, ISA-decided wiring) applied to coupling.

In practice: the ALU and a short multiplier are always instruction-coupled (every
ISA has arithmetic opcodes). Long-latency crypto and slow dividers are usually
bus-attached — *unless* the ISA adds dedicated opcodes, at which point the exact
same core moves inline. Both couplings can coexist in one SoC.

## 8. Emission as entities

Each core instance is a **sub-entity** (`alu.vhd`, `mul.vhd`, …). This needs the
sub-entity boundary, which today is `NetM`-concrete (`Hdl.IO.entity`) while the
CPU synth is abstract. Options (decide in §10):

- (a) Add an abstract `block :: String -> (i -> m o) -> i -> m o` primitive to the
  `Hdl` class that lowers to a NetM sub-entity and is transparent in sim.
- (b) Emitter-level hoisting: the emitter lifts a core's wire-cone into its own
  entity by its crossing signals (like the `case?` decoder recovery — analysis,
  no synth change).

The ALU core has a clean fixed interface, so (a) is natural and reusable; (b)
avoids touching the abstract layer. Leaning (a).

## 9. Phasing

1. **ALU core** — `Isacle.Core.ALU`, `alu`/`setFlags` in `MonadALU`, `ICoreOp`
   lowering, one shared instance, emit `alu.vhd`. Convert AVR arithmetic/logic/
   shift instructions (ADD/ADC/SUB/SBC/AND/OR/EOR/COM/NEG/LSR/ROR/ASR/SWAP/…).
   Verify ghdl 15/15 + 658/658.
2. **MUL/DIV core** — sequential handshake, sequencer integration; convert
   MUL/MULS/FMUL.
3. **Crypto/custom cores** — coprocessor or invocation per §7.

## 10. Open decisions

- **Flag delivery**: paired result+flags node vs a `setFlags` that reads the ALU's
  last-invocation flags. Prefer explicit `(result, flags) <- alu …` (types make
  the pairing clear).
- **Sub-entity mechanism**: abstract `block` primitive (§8a) vs emitter hoisting
  (§8b). Leaning `block`.
- **ALU op coverage**: which ops are core-native vs derived (e.g. NEG = 0 − a,
  COM = a xor 0xFF) — keep the op enum small, derive the rest in wiring.
- **Carry/immediate operands**: ADC/SBC need carry-in; immediate-form instructions
  (SUBI/ANDI/…) route an immediate as operand B. Both are just operand wiring.
- **Backward compat**: convert instructions incrementally — un-converted ones keep
  the current private-datapath path until migrated. No flag-day.

## Non-goals (for now)

- Automatic operator-level resource sharing (rejected: no path to crypto).
- Pipelining / multi-issue (single commit per cycle assumed, as today).
- Changing the register file or decoder (already clean; `case?` decoder stays).
