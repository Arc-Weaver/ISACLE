# PLAN: Code memory is a bus — sequenced multi-word instruction reads

## Problem

The synthesis treats the code bus as a fixed dual-read: `instr_word = code[PC]`
and `code_rd_data = code[PC+1]`, and lowers **every** `readCode(PC+off)` to the
one combinational `code_rd_data` wire (`Synth.hs:172-176`, the read address `_`
is discarded). So a CPU can read at most **one** operand word. This is fine for
AVR's 2-word instructions (need only `code[PC+1]`) but breaks any instruction
needing a third word:

- **8051**: 3-byte instructions (`MOV direct,#imm`, `LJMP`, `LCALL`, `MOV
  DPTR,#data16`, …) need `code[PC+2]` — both operand reads currently collapse to
  `code[PC+1]`. (Found by cl51 `ghdl-sim`: 4 xfails.)
- **AVR**: works today only because it never needs `code[PC+2]`.

## Model (decided)

**The code memory is a bus, like the data bus. Each instruction word is another
read on it, sequenced.** The fetch is a read at PC; operands are reads at PC+1,
PC+2, … driven onto a code operand address and returned on the code data bus,
sequenced by the existing exec-cycle sequencer — exactly how data reads already
work. "More words → more reads." No "max one operand word" assumption.

## Key nuance: reads are CODE WORDS, width `codeWordW` (per-CPU)

The sequenced reads are **code words**, not bytes. `codeWordW` is already a type
parameter:

- **AVR**: `codeWordW = 16` → each read is a 16-bit word; address increments by 1
  code word (word-addressed PC).
- **8051**: `codeWordW = 8` → each read is a byte; address increments by 1.

So a code read's value is `s dom (Unsigned codeWordW)` and its address is
`s dom (Unsigned codeAddrW)`, offset by `+1 codeword` per operand. This is why
the read latch can't be a single fixed width shared with data reads: data reads
latch `wordW`, code reads latch `codeWordW`, and for AVR these differ (8 vs 16).

## Design — mirror the data-read path onto the code bus

### 1. IR / Synth.hs
- `SReadCode tok addr` already carries the address. Add a request type
  `CodeReadReq { crMatch, crTok, crAddr :: s dom (Unsigned codeAddrW) }`
  (mirror of `MemReadReq`) and collect `srCodeReads :: [CodeReadReq]` in
  `SynthResult` — the code-read analogue of `srMemReads`.
- `readResSig` for code tokens must point at the **sequencer** result
  (`rcReadRes ctx tok`), NOT `rcCodeBus`. (`rcCodeBus` / the combinational alias
  goes away.)

### 2. SynthCPU.hs — sequence code reads (the hard part)
- Code reads become sequencer **accesses**: `seqNAcc` counts
  `srMemReads + srMemWrites + srCodeReads`, so a multi-word instruction spans the
  right number of cycles.
- **Two read-latch sets, sharing one exec-cycle counter** (because widths
  differ):
  - data reads → latched at `wordW`, address mux → `dmemRdAddr`, value ← data bus
    (existing path);
  - code reads → latched at `codeWordW`, address mux → **`cmiCodeOperandAddr`**
    (new), value ← code bus (`code_rd_data`).
  Generalise `buildExecSequencer`/`buildReadLatches` to take the code read list +
  `codeWordW`, or run a second latch pass keyed on the same `execCyc`.
- `esReadRes tok`: code token → code latch; data token → data latch.
- Drive `cmiCodeOperandAddr = priorityMux` over gated code reads (mirror
  `dmemRdAddr`), each read's address being `PC + off` (in code words).

### 3. CpuMemIface
- Add `cmiCodeOperandAddr :: s dom (Unsigned codeAddrW)` (the CPU now tells memory
  which code word it wants for an operand read; the fetch address stays
  `cmiCodeRdAddr = PC`).

### 4. SoC (SystemDSL.createHarvardCPU) + standalone tops
- Replace the fixed dual-read `code_rd_data = rom[PC+1]` with an **addressed**
  read `code_rd_data = rom[cmiCodeOperandAddr]`. The fetch read `instr_word =
  rom[cmiCodeRdAddr]` is unchanged. (Two read ports on the code ROM: fetch +
  operand, both `codeWordW`-wide.)
- Update clavr `SocSpec` / `TestAVRSynth`, cl51 `synth.hs` / `vhdl_sim.hs`
  testbench generator (code ROM answers the operand address).

## AVR impact (green-lit)

AVR single-word instructions have **no** `readCode` → no code access → unchanged
timing. Only 2-word instructions (LDS/STS/JMP/CALL/LDD) gain a sequenced operand
read (one extra cycle) instead of the combinational dual-read — more correct, and
matches the model. **Must re-verify clavr `ghdl-sim` 15/15 + `test-library`
658/658** (final-state assertions should survive the extra cycle; give sims
headroom).

## Verification

- cl51 `ghdl-sim`: the 4 xfails (`mov_direct_imm`, `mov_a_direct`, `ljmp`,
  `lcall_ret`) become **passes** → 26/26, drop `knownLimits`.
- clavr `ghdl-sim` 15/15, `test-library` 658/658 unchanged.
- ISACLE `test-library` unchanged.

## STATUS (2026-07-03): DONE — sequenced code-bus reads work.

**Result:** clavr `ghdl-sim` 16/16 + `test-library` 658/658; cl51 `ghdl-sim`
**26/26** (all four 3-byte instructions — `mov_direct_imm`, `mov_a_direct`,
`ljmp`, `lcall_ret` — now pass); ISACLE all green.

### Final design (as built)
- **Operand words are per-cycle registers** (`opWords` in `SynthCPU.hs`): word
  `j` is read from `PC+1+j` on exec cycle `j` and latched. A body reads back the
  *register* (`rcCodeWord j`, added to `RenderCtx`), not a mux with the live bus —
  so a code-read result is a plain wire.
- **Two-pass structure.** Pass 1 is pure static ISA analysis (`stmtsOf`/
  `codeReadsOf`/`dataAccOf` count each body's IR) giving `maxCode` and `maxCyc`
  — no elaboration monad, so no render-result dependency. Pass 2 is the
  elaboration `mdo`, wrapped in a CPS `reflectNat (addrBitsFor maxCyc)` prelude
  that opens the counter width `cyW` as a real type. The exec-cycle counter is
  therefore `Unsigned cyW` at its **exact** width (e.g. `unsigned(1 downto 0)`),
  with its `execNxt` feedback staying inside the one `mdo`. Built from
  `allResults0` *before* the alias override; `commitCycleOf` gives pure code-read
  instructions one settle cycle so the final word is latched before commit.
  (`reflectNat` is a pure `case` that wraps, not enters, the `mfix`, so it never
  gets forced mid-tie — that was the real reason the earlier nested-`mdo`
  attempt black-holed, now moot with deferred `regBankRead`.)
- **`cmiCodeOpAddr = PC + 1 + exec_cycle`** drives the code bus; the SoC / test
  ROMs answer that address (`code_rd_data = ROM[code_op_addr]`).
- `readResSig` (Synth.hs) routes each `readCode` result to `rcCodeWord j` (its
  program-order index); `maxCode` is a STATIC ISA count (render-independent).

### The mfix loop — root cause and fix
The `<<loop>>` was NOT in the sequencer per se. The alias override
(`aliasReadOf` → `regBankRead`) **eagerly materialised** the read address; for a
code-fetched `LDS`/`JMP` address that address is an `mdo`-bound `opWords`
register, and forcing any `mdo`-bound value mid-`mfix` demands the whole
`mfix` result → black hole. (A function *argument* like `cmemRdData` is outside
the recursion, which is why the old combinational path never looped.)
**Fix:** made `regBankRead` (NetM instance, `Hdl/Monad.hs`) **deferred** — it
allocates its output wire eagerly but materialises the index in a `defer`ed
action (exactly as `registerW` does), so the override no longer forces the
address during elaboration. One-line-idea, but it took bisecting the whole
elaboration order to find.

## Notes
- The sim backend (`Isacle.ISA.Backend.Sim`) already reads code at any address
  (`ssCodeMem`), so it needs no change — it's the datasheet-faithful reference.
- This removes `rcCodeBus` from the RenderCtx; `rcInstrWire` (opcode for decode)
  stays as the fetch word.
