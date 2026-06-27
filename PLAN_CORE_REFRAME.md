# Plan — whole-core-as-`HdlType` reframe (C1 full)

> Status: **planning** (2026-06-26). The remaining large step of C1. The SREG
> bit-map (`flagRec`/`Sreg`) and the core-state record (`AvrState`) are already
> landed and GHDL-verified; this plan covers replacing the handle-based *access*
> path with typed projection and eliminating the free `pcW`. It is written so the
> migration can be executed in a supervised session, keeping clavr/cl51 building
> and GHDL-green at every step.

## 1. Target

Instruction bodies reach CPU state by **typed field projection** of an `HdlType`
core record, with **length-by-default**: the result width of a field access *is*
that field's `Width`. The free `pcW` parameter disappears — the PC width is the
`Width` of the core's PC field. Register/signal/doc names come from the **record
field names** (C3), single-sourced.

## 2. Current state (what exists today)

- **`AvrState pcW`** (clavr `AVR.ISA.Types`) — the core state as one `HdlType`
  record: `asGPR :: Vec 32 (Unsigned 8)`, `asSP/asX/asY/asZ :: Unsigned 16`,
  `asPC :: Unsigned pcW`, `asSREG :: Sreg`. *Already an `HdlType`* (Width 344 for
  pcW=16, round-trips). **Not yet used for access** — purely structural today.
- **`AVRALU pcW`** — the *handle* record actually driving synthesis: fields are
  `CPURegister w` / `CPURegFile count w` / `CPUFlag`, i.e. string-named handles
  produced by `CPUDef` (`reg "PC"`, `regFile "GPR" …`, `flagRec @Sreg "SREG"`).
- **Access path:** `MonadALU.register sel field`, `readReg :: CPURegister w ->
  m (IExpr w)`, `writeReg`. `ISABuild` turns a `CPURegister key` into an
  `IReadReg (RegRef key)` / `SWriteReg` IR node; the key is a **`String`**
  (`"GPR:..."`, `"SREG"`, `"PC"`). Width `w` is a phantom on the handle.
- **Synthesis backends** (`SynthCPU`, `SynthVnCPU`): build `scalarRegMap` /
  register-file maps **keyed by the string name** from `CPUSchema`
  (`schRegisters`/`schRegFiles`/`schStatusRegs`); each `IReadReg`/`SWriteReg`
  resolves its `RegRef` name against those maps to a `WireId`.
- **`pcW` thread:** appears in `AVRALU pcW`, the `AVR m pcW` constraint alias
  (`CodeAddr m ~ IExpr pcW`), and width math in branch/jump bodies.

## 3. The gap

The handle is a `(String, phantom Width)`. Typed projection wants the field's
`Width` to come from the *record field type*, not a phantom on a separately
constructed handle — and the name to come from the field selector, not a string
passed to `reg`. Two couplings block a clean swap:

1. **Name = key.** Synthesis resolves `RegRef String` against the schema's
   string-keyed maps. Projection must still yield a stable key per field.
2. **`pcW` is free.** `CodeAddr ~ IExpr pcW` is independent of any field; tying
   it to `Width (PC field)` requires the core record to be the source of widths.

## 4. Migration steps (each ends green + GHDL-verified)

1. **Field selectors → keys (additive).** Add a typed projection layer over
   `AvrState`: for each field, a `CPURegister (Width field)` whose key is the
   record's `selName` (via `GFields`/Generic, the same mechanism `recordFields`
   already gives). Provide `coreReg :: (AvrState pcW -> field) -> CPURegister w`
   built from the selector. *Verify:* the derived keys equal today's strings
   (`"PC"`, `"SREG"`, `"GPR"`); a unit test asserts key+width equality. No
   synthesis change yet.
2. **Single-source `CPUDef` from the record.** Generate the `CPUSchema`
   (registers + widths + names) from `AvrState`'s `GFields` instead of hand
   `reg "PC"` calls — so the schema's names/widths *are* the record's. Keep
   `aliasReg`/`flagRec` as today. *Verify:* `runCPUDef` produces a byte-identical
   schema (same names, widths, order); GHDL suite unchanged.
3. **Access via projection.** Change `avrPC`/`avrSP`/… call sites in clavr ISA
   bodies to obtain handles via the step-1 projection rather than the `AVRALU`
   fields. Mechanical, one body group at a time, `cabal build` + GHDL after each.
4. **Eliminate `pcW` as a free param.** Once the PC handle's width is
   `Width (asPC field)`, replace `AVRALU pcW`/`AVR m pcW`'s free `pcW` with the
   projection of the core's PC field. `CodeAddr ~ IExpr (Width PcField)`. This is
   the type-surgery step — do it behind a type alias first, then inline.
   *Verify:* `avrCPUDef @16` / `@22` still elaborate; GHDL for both widths.
5. **Retire `AVRALU` (optional).** If steps 1–4 leave `AVRALU` as a thin shim
   over `AvrState` projections, collapse them into one type.

## 5. Risks & mitigations

- **Name drift breaks synthesis resolution** (the string keys feed
  `scalarRegMap`). *Mitigation:* step 1 asserts derived keys == current strings
  before any behavioural change; the GHDL suite is the backstop.
- **`pcW` surgery is type-invasive** (touches `CodeAddr`, branch width math).
  *Mitigation:* alias-first (introduce `type PcW = Width PcField`, migrate, then
  inline); both pcW=16 and 22 GHDL-tested.
- **Register file as array vs `CPURegFile`.** `asGPR :: Vec 32 (Unsigned 8)` is
  an `HdlType` array, but synthesis treats the file specially (`schRegFiles`,
  indexed access). *Mitigation:* keep the file on the existing `CPURegFile`
  path; only scalar registers + SREG move to projection first. The array-field
  access is a later sub-step (needs indexed projection), not a blocker for 1–4.

## 6. Verification at every step

- `cabal build clavr` + `cabal build cl51` (downstream green).
- `cabal test ghdl-sim` in clavr — all 8 benches (alu/branch/mem/imm/timer/uart/
  gpio/ramp) must stay PASS; the alu/branch/imm benches exercise the registers
  and SREG flags, so any resolution/width drift shows up there.
- `cabal run test-avr-synth` emits VHDL for both PC widths.

## 7. Why deferred from the autonomous session

Steps 3–4 edit clavr's ISA bodies and the `CodeAddr`/`pcW` types — a coordinated
change across the IR, both synth backends, and every instruction group. A
mid-migration breakage leaves the GHDL stack red, which must not happen
unattended. The additive groundwork (steps 1–2) is low-risk and could be done
first; the type surgery (step 4) wants a human in the loop.
