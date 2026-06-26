# ISA IR migration — de-risked plan

Goal: make the ISA backend a **deep embedding** (like `BusDef`): instruction
bodies build a typed `InstrIR` source of truth; `Sim`/`Synth`/`Doc` are
*renderers* over it. This removes the `WireId`-in-`Unsigned` hack, so `sp - 1`
builds `IBin PSub` (a real subtractor) and can never corrupt a wire id.

Status: foundation **done and committed green** (`Isacle.ISA.IR`,
`Isacle.ISA.Build`, `Isacle.ISA.Backend.Lower` incl. `renderInstr`; proven by
`test-ir-lower`). The migration below is mechanical but large (~multi-day).

## Locked design decisions
- **Value type is concrete `IExpr`**, not an abstract `Val m`. GHC forbids a
  type family in a quantified constraint (`forall w. KnownNat w => Num (Val m w)`),
  so `Val m` cannot be abstracted. `MonadALU` methods return `IExpr w`; backends
  render the resulting `InstrIR`.
- **Class defaults do the value work** via `Term` ops, so the IR-builder
  instance implements only *effectful* methods:
  `resizeBits = pure . tResize`, `signExtendBits = pure . tSignExt`,
  `isZero = pure . tIsZero`, `litC = pure . tLit`,
  `aluOp PNot a _ = pure (tUn PNot a)` / `aluOp op a b = pure (tBin op a b)`.

## IR additions needed (all small; tried and reverted this turn)
- `ISlice hi lo :: IExpr k -> IExpr w` (+ `tSlice`) — bit extraction.
- `IFlagRead :: CPUFlag -> IExpr 1` — `getFlag` lowers via the renderer.
- `IIrqVector :: KnownNat w => IExpr w` — `irqVector`.
- Bit adapters named to match `Hdl.Bits` so body call-sites don't change:
  `zeroExtend = IZeroExt`, `signExtend = ISignExt`, `truncateB = ITrunc`,
  `bitCoerce = IResize`, `slice = ISlice`.
- `InstrIR` gains `iirGate :: Maybe (IExpr 1)` for `irqGate` (renderer ANDs it
  into the match wire).
- `LowerCtx` gains `lcReadFlag :: CPUFlag -> NetM WireId` and
  `lcIrqVector :: NetM WireId`; add `lowerExpr` cases for the 3 new leaves.

## `MonadALU` class (ALU.hs) — exact edits (all verified to compile)
- imports: drop broad `Hdl.Bits`/`Data.Proxy`/`natVal`; `import Hdl.Bits (Bit(..))`,
  `import GHC.TypeLits (KnownNat, Nat)`, `import Isacle.ISA.IR`.
- methods `Unsigned w → IExpr w` everywhere (`immediate` gains `KnownNat n`).
- helpers: `DataAddr m ~ Unsigned X → ~ IExpr X`; `relJump` takes `IExpr w`
  (drop `Signed`); `bitCoerce`/`±` now resolve to the IR versions.

## `ISABuild` becomes the one instance (Build.hs)
Restructure to `ReaderT alu (State BuildSt)` (mirrors `SynthM`’s reader for the
ALU record). Implement: `cpu`/`cpuFlag` (`asks`), `register` (encode
`"rf:field"` into `CPURegister`), `immediate` (`IField`), `mnemonic`/`doc`/
`encoding` (record into `InstrIR`, **no wire**), `readReg` (parse key → `RegRef`,
`IReadReg`), `writeReg`/`writeMem`/`setFlag` (emit `IStmt`), `readMem`/`readCode`
(emit `SReadMem`/`SReadCode`, return `IReadRes tok`), `getFlag` (`IFlagRead`),
`absJumpIf` (`SJumpIf`), `irqVector` (`IIrqVector`), `irqGate` (set `iirGate`).

## Consumers — the real bulk
- **SynthCPU / SynthVnCPU**: per instruction, `runISABuild → InstrIR`; build the
  match wire from `iirEncoding` (AND with `iirGate`); pre-scan the IR for
  `IReadReg (RegFile …)` to allocate register-file **read ports** (one per slot,
  as today); build a `LowerCtx` (`lcReadReg` scalar→reg-out / file→port,
  `lcField` extract-from-instr-word, `lcReadRes` per-cycle data bus,
  `lcReadFlag` status-reg slice, `lcIrqVector`); `renderInstr → Rendered`; feed
  `Rendered` into the **existing** write arbiters + exec sequencer (already
  built). Delete `runSynthM`/`SynthResult`/`SynthM` and the wire-id hack.
- **Sim / Doc**: replace their `MonadALU` instances with `renderSim` /
  `renderDoc` over `InstrIR`.

## Bodies (clavr AVR ×5, ISACLE Tiny/TinyVN)
- 27 `:: Unsigned n` → `:: IExpr n`; import IR bit adapters instead of
  `Hdl.Bits` versions.
- BRBS/BRBC: replace `let Unsigned sssId = sss; sss8 = Unsigned sssId :: Unsigned 8`
  (constructor destructuring) with `zeroExtend`/`slice`.

## Done-when
`clavr test-avr-synth` regenerates `avr_cpu.vhd`; `sp - 1` in the IRQ `push`
path is a `PSub` subtractor (the `resize(stall,16)` garbage is gone); `tiny_sys`
+ `test-ir-lower` still pass.
