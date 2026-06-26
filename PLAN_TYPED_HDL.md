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
The signed *datapath* is proven; what's left is framework plumbing.

- **#3c Wrap the ramp as a `PeriphDef` peripheral.** Model on
  `Isacle.Periph.Timer` (`timerDef` register map + `counterFSM` HDL FSM +
  `timerUnit` bus wrapper). Register map: `setpoint` (RW), `step` (RW),
  `current` (RO). The bus `dat` is `Unsigned 8`; reinterpret to/from `Signed 8`
  with `withRepr . coerce` (same bits, retag). FSM = the verified ramp logic.
- **#3b Width-parametric ports.** Peripheral output ports are already `Sig`-typed
  (HdlPorts derives width from `Width a`); the gap is the bus binding —
  `CpuMemIface` (`Isacle.ISA.Backend.SynthCPU`) and `BusPort`/`BusHandle`
  (`Isacle.System.BusArch`/`BusHandle`) thread bare `WireId` + `Int` widths
  (`cmiWordW` etc.). Make these width/representation-parametric so a mismatched
  bind is a type error. NOTE: the CPU/bus *core* is `WireId`-based (not `Sig`),
  so full typing there is a larger rework — peripherals are the natural place.
- **#3d Integrate into a test SoC** (extend `clavr/example/Example/SocSpec.hs`
  or a fresh one) and GHDL-sim the signed behaviour over the bus end-to-end.

### Known gaps / follow-ons
- **Signed literals in *expressions*** still emit `to_unsigned` (only register
  init/reset is fixed). `combExpr` (`Hdl.Emit.Vhdl`) needs to emit a literal
  operand as `to_signed` when it appears in a signed-result op. Until then, keep
  signed constants out of the datapath (use signals — as the ramp's `step` does).
- **`Repr` is extensible** but only `RUnsigned`/`RSigned` populated. Add
  `RStdLogicVector`/fixed-point/enum/struct + the `HdlType.hdlRepr` + one emitter
  case when needed.
- **Exact growing reduction** over a `Vec n` (tight `w + CLog2 n` width) — the
  `sumTo`/balanced-tree combinator. `Hdl.Reduce.sumModular` is only the modular
  (wrapping) list version; `Hdl.Arith.add` is the exact pairwise op.
- **`Sum`/`Prod` proper classes** + more Monoid instances (currently just the
  `SigAny`/`SigAll`/`SigSum` newtype wrappers).
- **ISA width adapters are opt-in** — migrate clavr/cl51 instruction bodies from
  the loose `slice`/`zeroExtend`/`truncateB` to the `*C` variants incrementally
  to actually catch latent width bugs.

### Hygiene
- This is a stack: `feat/typed-hdl-signals` sits on `feat/ir-bus-cpu-foundation`
  (ISACLE), alongside the clavr/cl51 `feat/ir-deep-embedding` branches. Land them
  to `main` when ready so the stack stops growing.

## Key files
- `src-hdl/Hdl/Net.hs` — `Repr`/`NRepr`/`reprWire`, `PrimOp`, `NetNode`.
- `src-hdl/Hdl/Types.hs` — `Sig`, `HdlType` (+ `hdlRepr`), `withRepr`, ops; `Num`.
- `src-hdl/Hdl/Bits.hs` / `Hdl/Prim.hs` — `Signed`/`Unsigned` + `HdlType`.
- `src-hdl/Hdl/Arith.hs` — `HdlArith` (exact growing add/sub/mul), `sConcat`.
- `src-hdl/Hdl/Reduce.hs` — constants, reductions, Monoid wrappers.
- `src-hdl/Hdl/Emit/Vhdl.hs` — `reprOf`/`wireVTypeR`/`VSigned`/`ppLitR`.
- `src/Isacle/ISA/IR.hs` — `Term`/`IExpr`, loose adapters, `*C` width-checked ones.
- `src/Isacle/System/Periph.hs` + `src/Isacle/Periph/Timer.hs` — peripheral DSL
  and the model to copy for the signed ramp.
