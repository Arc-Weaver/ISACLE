# Typed HDL Signals — status & remaining work

Branch: **`feat/typed-hdl-signals`** (ISACLE), based on `feat/ir-bus-cpu-foundation`.
All work below is additive and verified; clavr and cl51 build unchanged at every
step, and `avr_cpu`/`avr_soc` regenerate output-neutral (zero `signed`/`to_signed`
unless a wire opts in).

## Goal

Maximise compile-time checking by making signal **types** (not just widths)
first-class, kept as two separate levels that must not be mixed:

- **ISA level** — `Term` / `IExpr w` (`Isacle.ISA.IR`): width-typed bit-vector
  *semantics* of instructions. Concern: type-level **width** laws.
- **HDL level** — `Sig dom a`, `PrimOp`, `NetNode`, ports (`Hdl.*`,
  `Isacle.System.*`): representation-typed *signals*. Concern:
  representation (`Unsigned`/`Signed`/`Bool`/…) + width.

The pattern throughout: a **typed surface** (`IExpr w`, `Sig dom a`) over an
**untyped graph core** (`NetNode`/`WireId`), bridged at lowering. The graph stays
untyped (it needs heterogeneous nodes in lists); the type info that must survive
is a small per-wire tag, read by the emitter.

## Done (commits on the branch, newest last)

1. **Signed type + growing arithmetic** — `HdlType (Signed n)` (`Hdl.Bits`,
   two's-complement `toBits`/`fromBits`); `Hdl.Arith`: exact width-growing
   `add`/`sub` → `MaxN m n + 1`, `mul` → `m + n`; `Unsigned`+`Signed` instances;
   mixed-sign is a *type error*; `Num (Sig dom a)` left as the modular wrap.
2. **Per-wire representation tag → signed emission** — `Hdl.Net`
   `Repr (RUnsigned|RSigned, extensible)` / `NRepr` node / `reprWire` (mirrors
   `NHint`); `HdlType.hdlRepr` (default `RUnsigned`, `Signed → RSigned`);
   `Hdl.Types.withRepr` tags leaves. Emitter (`Hdl.Emit.Vhdl`): `VSigned`,
   `wireVTypeR`, and `reprOf` which reads an explicit tag and otherwise
   **propagates** it through arithmetic ops (like `inferWidth` for widths) — so
   only leaves (ports/registers) need tagging. Wire + port declarations are
   repr-aware. Type drives the VHDL declaration; `numeric_std` overloading does
   the signed arithmetic (no `PMulSigned` zoo).
3. **Public-surface tightening** — `primSig2` dropped from `Hdl.Types` exports
   (the loose escape hatch; `Hdl.Arith` now builds its ops from `sigResize` +
   `Num`); width-typed `sConcat`.
4. **`Hdl.Reduce` convenience layer** (own opt-in module to avoid clashing with
   peripherals' local `sigTrue`/`sigFalse`): `sigLit`/`sigTrue`/`sigFalse`;
   `orAll`/`andAll`/`sumModular`; `SigAny`/`SigAll`/`SigSum` Monoid wrappers.
5. **ISA `Term`/`IExpr` width-checked adapters** (`Isacle.ISA.IR`, additive,
   opt-in): `zeroExtendC`/`signExtendC :: (k <= w) =>`, `truncateC :: (w <= k)
   =>`, `sliceC @hi @lo :: (lo <= hi, hi+1 <= k, w ~ hi-lo+1) =>`. Out-of-range
   slice or wrong-direction resize is a compile error. Loose adapters remain.
6. **Signed register init/reset literals** — `ppLitR` (repr-aware `to_signed`)
   at the register `:=` and reset sites (a signed reg was emitting
   `:= to_unsigned(0,8)`, a VHDL type error).
7. **`sigReinterpret` / `PReinterpret` — the unsigned↔signed seam.** A same-width
   bit reinterpretation that emits a real VHDL `signed(..)`\/`unsigned(..)` cast
   (a *new* wire), unlike `withRepr` which retags a leaf in place. Needed because
   a single bus wire is consumed at *two* representations (unsigned for the read
   mux, signed for the datapath) — one wire can't carry two reprs, so the cast
   must materialise a distinct wire. `reprOf` reports the cast wire's target repr;
   `combExpr` emits `signed(w)`\/`unsigned(w)` (identity at width 1). Added
   `Ord Repr` so `PrimOp` still derives `Ord` for the memo map.
8. **Signed `Ramp` peripheral (#3c) — GHDL-verified.** `Isacle.Periph.Ramp`:
   bus is `Unsigned 8`, datapath is `Signed 8` via `asSigned`\/`asUnsigned`
   (= `sigReinterpret`). `rampDef` (SETPOINT/STEP RW, CURRENT RO) + `rampFSM`
   (signed compare/clamp toward setpoint) + `rampUnit`. A throwaway harness
   emitted `ramp_top` and a self-checking testbench drove the demonstrator
   ramp 0 → +5 (clamped) → −3 (through zero, reading back 255=−1, 253=−3): all
   assertions pass under `ghdl -r`.

### Verified (GHDL, via the scratchpad NetM→emitVhdl→ghdl pattern)
- Signed add: `-3 + -5 = -8`, `120 + 120 = 240` (growth prevents 8-bit overflow).
- Reductions: `orAll`/`andAll`/`foldMap SigAny` across input combinations.
- **Signed ramp datapath**: a `current` register ramps `0 → +5 → −3` (through
  zero into negative) with signed compare + signed `+`/`-`; emits clean signed
  `numeric_std`. This is the core of the #3 demonstrator.

Reproduce a check: build a small `NetM` that emits `NInput`/`NOutput` ports
(tag signed ones with `reprWire w RSigned`), wire the datapath with `Sig` ops,
`runDesign`/`emitVhdl` to a `.vhd`, then `ghdl -a/-e/-r` against a testbench.
(See the throwaway harnesses used during development; none are committed.)

## Remaining work

### #3 — signed peripheral, integrated (the chosen demonstrator)
The signed *datapath* is proven; the standalone peripheral is done and
GHDL-verified (#3c, item 8 above). What's left is framework plumbing (#3b) and
integration over a real CPU bus (#3d).

- **#3c Wrap the ramp as a `PeriphDef` peripheral.** ✅ DONE — `Isacle.Periph.Ramp`
  (see Done item 8). Note: the planned `withRepr . coerce` reinterpret was wrong
  for a non-leaf wire used at two reprs; replaced by the new `sigReinterpret`
  cast (Done item 7).
- **#3b Width-parametric ports.** ✅ DONE for the bus-master seam, **widths only**.
  A bus is *just wires* (`std_logic_vector`); signed-vs-unsigned is a
  *peripheral's* interpretation, never a property of the bus — so the bus type
  carries wire counts, not representation. `BusHandle`
  (`Isacle.System.BusHandle`) is now `BusHandle (addrW :: Nat) (dataW :: Nat)`.
  `createBus` returns `BusHandle 32 (Width dat)`; `createHarvardCPU` consumes
  `BusHandle busAddrW (Width dat)` (data wires must line up) with
  `(KnownNat busAddrW, addrW <= busAddrW)` (CPU address must fit the bus);
  `createL1Cache` consumes `BusHandle busAddrW busDataW`. Effect: binding a CPU
  whose address width exceeds the bus's is a **type error**
  (`Cannot satisfy: 64 <= 32`, caught at `cabal build`); a data-width mismatch
  is likewise unrepresentable. clavr's `avr_soc` builds unchanged (params
  inferred: `busAddrW=32`, `dataW=8`, `addrW=16`). The runtime `Int` width
  fields remain for the `WireId` core but are populated from the type.
  - REJECTED design: an earlier cut put the full data *type* (`BusHandle 32
    (Signed 8)`) on the handle. Wrong — that asserts a representation on raw
    wires. Representation belongs on **peripheral registers** (see #3e), which
    is also where it feeds documentation / C-header generation.
  - NOT done (deliberately): `BusPort`/`CpuMemIface` keep bare `WireId`+`Int`
    widths — typing those means typing the whole `WireId` interconnect core, a
    separate larger effort. `BusHandle` is the user-facing master↔bus bind.

### #3e — typed peripheral registers → documentation & C headers
A bus is untyped wires; the *interpretation* (signedness, width, bit-fields)
lives on each peripheral register. The signed/unsigned info belongs here — the
ramp's SETPOINT/STEP/CURRENT are signed *registers*, not a signed bus.

- **Metadata foundation** ✅ DONE. `FieldSpec` (`Isacle.System.Periph`) gains
  `fieldRepr :: Repr`. New typed declaration `fieldOf @a` derives the register's
  width (`Width a` → RW8/16/32) *and* representation (`hdlRepr a`) from an
  `HdlType`; `field`/`field8`/`register` keep the unsigned default (back-compat).
  The ramp now declares `fieldOf @(Signed 8)` for SETPOINT/STEP/CURRENT, so the
  spec records them as `RSigned`. Verified: `fieldOf @(Signed 8)`→`RW8/RSigned`,
  `@(Signed 16)`→`RW16/RSigned`, `field8`→`RW8/RUnsigned`. Synthesis output is
  unchanged (metadata-only). clavr/cl51 build unchanged.
- **DEFERRED (intentionally): the C-header / doc emitter.** `sysGenCHeader`
  (`Isacle.System.Generate`) currently emits only `#define <PERIPH>_<REG>` byte
  offsets. Teaching it to emit correct C types (`int8_t`/`uint8_t`/… from
  `fieldWidth`+`fieldRepr`) — as typed accessor macros or a packed register
  struct — is the payoff but is left for a follow-up. The motivation for typed
  peripherals *is* this header, but the types are valuable on their own.
- **#3d Integrate into a test SoC.** ✅ STRUCTURAL INTEGRATION DONE. `createRamp`
  added to `Isacle.System.SystemDSL` (mirrors `createTimer`; token
  `PeriphToken Ramp dom (Unsigned 8) ()`, no physical outputs). Attached to the
  AVR SoC at `0x0070` (`clavr/example/Example/SocSpec.hs`). The full generated
  SoC (`cpu`+`databus`+peripherals+`ramp0`) **GHDL-analyzes and elaborates**;
  `ramp0.vhd` emits the signed datapath (`signed(..)` regs, `signed(..)`/
  `unsigned(..)` reinterpret casts, `to_signed` init) and `databus.vhd` decodes
  it at `cs_0x70` into the read mux.
  - **BEHAVIOURAL ✅ DONE & GHDL-PASSED.** `clavr/tests/fixtures/ramp_demo.S`
    (+`.bin`) writes STEP=2 and SETPOINT=−6 to the ramp over the bus, waits for
    convergence, reads CURRENT back, and drives it onto PORT_A.
    `clavr/tests/ghdl/ramp_tb.vhd` observes `gpio_port = 0xFA` (= signed −6) →
    "RAMP END-TO-END CHECK PASSED". Run: synth with
    `avr-soc-synth tests/fixtures/ramp_demo.bin build/ramp_demo`, then ghdl
    -a/-e/-r including `ramp0.vhd`. The ramp's tick is now `sigTrue` in
    `SocSpec.hs` (added `sigTrue` to SystemDSL). VCD-verified the internal
    trajectory 0→−2→−4→−6.
  - **CPU `rjmp`/`rcall` bug found & FIXED** (`clavr/src/AVR/ISA/Branch.hs`):
    backward (negative-offset) relative jumps were broken — the body of a
    `rjmp` loop ran only once (counter froze at 1; ramp poll froze mid-ramp at
    −2). Root cause: `instrRJMP`/`instrRCALL` did `k <- immediate "kkkk…"` with
    the result type inferred at PC width, so the 12-bit offset was **zero**-
    extended (k=−5 → +4091, flinging the PC into nop-filled ROM). The conditional
    branches (BRBS/BRBC) already did it right: extract `IExpr 7` then
    `signExtendBits`. Fixed RJMP/RCALL to extract `IExpr 12` and `signExtendBits`
    to PC width. Verified: counter loop now counts (1→32), ramp poll loop reads
    back −6, alias demo still 0x01.
  - 💡 **This is a poster child for typed ISA defs** (see the ISA-adapters
    known-gap): the bug existed because `immediate`'s width was *inferred* and
    silently zero-extended. If `immediate` were typed to its field width
    (`IExpr 12`) and widening to PC width *required* an explicit `signExtendC` /
    `zeroExtendC`, the silent default would be impossible — the author would be
    forced to choose, and the wrong choice would be a visible, checkable one.

### Known gaps / follow-ons
- **Signed literals in *expressions*** ✅ FIXED. `combExpr` (`Hdl.Emit.Vhdl`)
  now casts a *literal* operand to `signed(..)` when the op's data operands are
  signed (`dataRepr`/`castLit`: the repr comes from the first non-literal data
  operand, since a literal is a shared unsigned bit-pattern constant with no repr
  of its own). Applies to `+ - *`, comparisons (`<`, `=`), and mux branches.
  All-unsigned ops are byte-for-byte identical (output-neutral). GHDL-verified:
  `x + signed(C_3_8)` and `x < signed(C_0_8)` give `-5+3=-2`, `-5<0=true`,
  `10<0=false`; avr_soc regenerates and still passes its self-check. So signed
  constants in the datapath are now safe (the ramp could use a literal `step`).
- **`Repr` is extensible** but only `RUnsigned`/`RSigned` populated. Add
  `RStdLogicVector`/fixed-point/enum/struct + the `HdlType.hdlRepr` + one emitter
  case when needed.
- **Exact growing reduction** over a `Vec n` (tight `w + CLog2 n` width) — the
  `sumTo`/balanced-tree combinator. `Hdl.Reduce.sumModular` is only the modular
  (wrapping) list version; `Hdl.Arith.add` is the exact pairwise op.
- **`Sum`/`Prod` proper classes** + more Monoid instances (currently just the
  `SigAny`/`SigAll`/`SigSum` newtype wrappers).
- **ISA width adapters — migration STARTED (loose → `*C`).** Done so far (the
  cases whose width law holds with *concrete* widths, pure wins, no new
  constraints): clavr `Branch.hs` (`truncateC` retHi:16→8; `zeroExtendC` sss:3→8),
  clavr `BitOps.hs` (all 8 `zeroExtendC`: sss:3→8, ioaddr:5/6→16), cl51
  `Branch.hs` (`zeroExtendC` hi3:3→16). All three repos build; clavr (8 ghdl-sim
  tests) and cl51 (18 tests) green.
  - **Motivating example:** the RJMP/RCALL sign-extend bug (#3d) was a *silent
    zero-extend* of an inferred-width immediate. cl51 avoided it with a shared
    `relTarget` helper that does `signExtendBits` — the pattern clavr lacked.
  - **BLOCKER → fundamentals session:** the *majority* of adapter uses operate on
    **abstract widths** — `pcW` (PC width) and `Word m` (data width) — where
    `k <= w` is unprovable without extra constraints. These bodies silently
    assume **pcW = 16** and **wordW = 8** (e.g. `pushRetAddr`, `retFromStack`,
    IJMP/JMP writing `zeroExtend`/`truncateB` across pcW). Migrating them means
    *pinning the PC/word-width model* as explicit type constraints — a deliberate
    design decision, not a rote swap. Likewise the ALU class's `immediate` /
    `signExtendBits` / `resizeBits` are loose (no width law); making `immediate`
    return the field width and forcing explicit extension is the structural fix.
    Both belong in the system-fundamentals session.

### Hygiene
- This is a stack: `feat/typed-hdl-signals` sits on `feat/ir-bus-cpu-foundation`
  (ISACLE), alongside the clavr/cl51 `feat/ir-deep-embedding` branches. Land them
  to `main` when ready so the stack stops growing.

## Key files
- `src-hdl/Hdl/Net.hs` — `Repr`/`NRepr`/`reprWire`, `PrimOp`, `NetNode`.
- `src-hdl/Hdl/Types.hs` — `Sig`, `HdlType` (+ `hdlRepr`), `withRepr`,
  `sigReinterpret` (unsigned↔signed cast), ops; `Num`.
- `src-hdl/Hdl/Net.hs` — `PReinterpret Repr` primitive (+ `Ord Repr`).
- `src/Isacle/Periph/Ramp.hs` — signed ramp peripheral (#3c demonstrator).
- `src-hdl/Hdl/Bits.hs` / `Hdl/Prim.hs` — `Signed`/`Unsigned` + `HdlType`.
- `src-hdl/Hdl/Arith.hs` — `HdlArith` (exact growing add/sub/mul), `sConcat`.
- `src-hdl/Hdl/Reduce.hs` — constants, reductions, Monoid wrappers.
- `src-hdl/Hdl/Emit/Vhdl.hs` — `reprOf`/`wireVTypeR`/`VSigned`/`ppLitR`.
- `src/Isacle/ISA/IR.hs` — `Term`/`IExpr`, loose adapters, `*C` width-checked ones.
- `src/Isacle/System/Periph.hs` + `src/Isacle/Periph/Timer.hs` — peripheral DSL
  and the model to copy for the signed ramp.
