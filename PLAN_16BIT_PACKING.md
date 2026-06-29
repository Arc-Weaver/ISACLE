# PLAN: 16-bit pack/split/cast + expression-indexed register files

Status: **queued** (design agreed 2026-06-29). Unblocks the last 4 red harness
checks in clavr (ADIW / SBIW / MOVW high-byte + pair copy).

## Goal & philosophy

Make merging/splitting words and signed↔unsigned casts *clean to write at the
ISA layer*, by leaning on the arithmetic that already exists in the expression
system. Keep the high-level structure all the way down to VHDL and let the
synthesiser clean it up. Concretely, packing two bytes is just arithmetic:

```haskell
word16 = 256 * hi + lo          -- pack
hi     = word16 `shiftR` 8      -- split (or truncate of >>8)
lo     = truncateB word16       -- split low byte
```

A `* 256` is a left-shift-by-8; the synthesiser constant-folds the power-of-two
multiply into a shift + concat, i.e. pure wiring. We deliberately write the
*arithmetic* form, not hand-rolled bit twiddling — same rationale as the rest of
the "rich HDL layer" direction.

## What already exists (verified 2026-06-29)

Nothing new is needed in the core expression system — the building blocks are
all present and already lower to real VHDL:

- `instance Num (IExpr a)` with `fromInteger = ILit`, and `(*)`/`(+)` as
  `IBin PMul`/`IBin PAdd` → so `256 * hi + lo` typechecks at a fixed width.
- Width-growing `add`/`mul` (`AddR`/`MulR` result types) for when you want the
  result strictly wider than the inputs.
- `asSigned` / `asUnsigned` / `reinterpret` (same-width `IReinterpret` casts).
- `zeroExtend` / `signExtend` / `truncateB` (+ `*C` width-checked variants).
- `shiftL` / `shiftR` (`PShiftL` / `PShiftR`).
- VHDL emitter (`src-hdl/Hdl/Emit/Vhdl.hs`) emits real `+`, `*`, `shift_left`
  for `PAdd`/`PMul`/`PShiftL`.

### One sharp edge to document, not fix

The `Num` `(*)`/`(+)` are **same-width** (`a -> a -> a`). So `256 * hi` where
`hi :: IExpr (Unsigned 8)` overflows to 0 (256 mod 256). For packing you must
widen the operands to the result width first:

```haskell
256 * (zeroExtend hi :: IExpr (Unsigned 16)) + zeroExtend lo
```

A thin helper hides the widen so the call site stays the user's clean idiom.

## Piece 1 — thin merge/split/cast helpers (clavr-side or ISACLE convenience)

Convenience over the existing arithmetic — no new IR nodes:

```haskell
-- pack hi:lo into a double-width word
packBytes :: IExpr (Unsigned 8) -> IExpr (Unsigned 8) -> IExpr (Unsigned 16)
packBytes hi lo = 256 * zeroExtend hi + zeroExtend lo

loByte :: IExpr (Unsigned 16) -> IExpr (Unsigned 8)
loByte = truncateB

hiByte :: IExpr (Unsigned 16) -> IExpr (Unsigned 8)
hiByte w = truncateB (w `shiftR` 8)
```

Signed/unsigned stays as `asSigned` / `asUnsigned`. (If a generic n-way concat
is wanted later, generalise `packBytes`, but two 8-bit halves cover AVR.)

## Piece 2 — expression-indexed register-file access (THE enabler)

Today the register-file index is either a `Field` placeholder or a literal
`Int`, spread across **five** redundant variants in `Isacle/ISA/ALU.hs`:
`readRegFile{,Offset,F,At,FOffset}` (× write). Collapse them to ONE pair whose
index is a full `IExpr`:

```haskell
readRegFile  :: (MonadALU m, HdlType t)
             => (AluDef m -> CPURegFile count t) -> IExpr (Unsigned idxW) -> m (IExpr t)
writeRegFile :: (MonadALU m, HdlType t)
             => (AluDef m -> CPURegFile count t) -> IExpr (Unsigned idxW) -> IExpr t -> m ()
```

This subsumes every existing variant:

| old                              | new                                   |
|----------------------------------|---------------------------------------|
| `readRegFileF gpr d`             | `readRegFile gpr (immediateF d)`      |
| `readRegFileFOffset gpr d 16`    | `readRegFile gpr (immediateF d + 16)` |
| `readRegFileAt gpr 0`            | `readRegFile gpr 0`                   |
| ADIW pair base (new)             | `readRegFile gpr (2 * immediateF d + 24)` |
| pair high byte (new)             | `readRegFile gpr (idx + 1)`           |

### IR / backend changes

- `RegRef`'s `RegFile` variant currently carries `(FieldRef, offset :: Int)`.
  Change it to carry the index **`IExpr`** directly. `FieldRef k` becomes the
  special case `IField (FieldRef k)`; offset becomes `+ lit`.
- **Sim** (`Backend/Sim.hs`): evaluate the index expression to a concrete `Int`
  (it already does `evalE` on `IField + offset`; generalise to the whole expr)
  → `"GPR:n"` key. Low effort.
- **Synth** (`Backend/SynthCPU.hs`): the index is a runtime expression →
  hardware index.
  - *Read*: mux tree over the file entries, selector = lowered index wire.
    (`inFileRange` / `fileIndexW` machinery already exists for the data-space
    file alias — reuse/generalise it.)
  - *Write*: the block-of-registers path already emits per-entry
    `(idx, data, en)`; make each entry's enable `idx == i` against the lowered
    index wire (a `PEq` per entry, already used for the alias decoder).
  This is the only non-trivial synthesis work; everything else is plumbing.

Delete the four extra variants once call sites are migrated (clavr is the only
consumer; cl51 will adopt the same API).

## Piece 3 — the four red instructions (clavr)

With Pieces 1–2 in place:

```haskell
-- ADIW Rd+1:Rd, K   d∈{0..3} → pair base reg 24/26/28/30
instrADIW = do
    (d, k) <- defineInstruction ...
    let idx = 2 * immediateF d + 24
    lo <- readRegFile avrGPR idx
    hi <- readRegFile avrGPR (idx + 1)
    let r = packBytes hi lo + zeroExtend (immediateF k)   -- Unsigned 16
    writeRegFile avrGPR idx        (loByte r)
    writeRegFile avrGPR (idx + 1)  (hiByte r)
    -- flags off the 16-bit result (C = bit16 of growing add, Z, N=bit15, V, S)

-- SBIW: same shape, subtract K.
-- MOVW Rd, Rr: idx_d = 2*immediateF d, idx_r = 2*immediateF r; copy both bytes.
```

ADIW/SBIW flag algebra mirrors the 8-bit `addF`/`subF` helpers, widened to 16.

## Verification

- clavr harness: the 4 ADIW/SBIW/MOVW reds → green (658/658).
- clavr GHDL suite stays 13/13 (ADIW/SBIW exercised by a 16-bit testbench;
  add one if `test_*` coverage is thin).
- Add a focused pack/split round-trip check (`packBytes (hiByte w) (loByte w) == w`)
  to the harness.
- ISACLE `cabal test` + `clash --vhdl` on the AVR top entity after the SynthCPU
  index-mux change (per the "verify includes Clash synthesis" rule).
