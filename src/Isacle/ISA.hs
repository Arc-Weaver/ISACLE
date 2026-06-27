-- | ISA definition framework.
--
-- Provides a monad-based DSL for defining CPU architectures independent
-- of word width, addressing, and pipeline implementation.
--
-- Typical usage:
--
-- 1. Define the CPU's state with 'CPUDef' — registers, flags, endianness,
--    and data-space aliases.
-- 2. Write instruction definitions using 'MonadALU' (or 'MonadHarvardALU'
--    for split code/data architectures).
-- 3. Assemble an 'ISADef' binding architectural roles (PC, interrupt
--    enable, context save list, reset state) to the named state elements.
-- 4. Provide a backend runner (simulation, synthesis, documentation).
module Isacle.ISA
    ( -- * Width helper
      width
    , w8, w16, w32, w64
    , byte
      -- * CPU state element references
    , CPUFlag
    , CPURegister
    , CPURegFile
      -- * CPU definition monad
    , CPUDef
    , CPUSchema
    , runCPUDef
    , endianness
    , regFile
    , reg
    , flagPack
    , flagRec
    , aliasReg
    , aliasFile
      -- * Endianness
    , Endianness(..)
      -- * ALU primitives
    , ALUPrim(..)
      -- * MonadALU
    , MonadALU(..)
      -- * MonadHarvardALU
    , MonadHarvardALU(..)
      -- * MonadIRQ
    , MonadIRQ(..)
      -- * ISA definition
    , ISADef(..)
    , ResetDef
    , SomeCPURegister(..)
    , resetReg
    , resetFlag
    , defineISA
      -- * Instruction helpers
    , relJump
    , absJump
    , push
    , pop
    , indirectRead
    , indirectWrite
    , indirectReadPostInc
    , indirectReadPreDec
    , indirectReadOffset
      -- * Encoding utilities
    , EncodingInfo(..)
    , FieldName
    , parseEncoding
    , fieldKey
    , extractField
    , matchesWord
      -- * IR (source of truth) + builder
    , module Isacle.ISA.IR
    , ISABuild
    , runISABuild
      -- * Documentation renderer
    , InstrSpec(..)
    , OperandSpec(..)
    , docInstr
    , docISA
      -- * Simulation renderer
    , SimState(..)
    , SimCPU(..)
    , emptySim
    , runInstr
    , execInstr
    , runIrq
      -- * Synthesis renderer
    , SynthResult(..)
    , RegWriteReq(..)
    , MemWriteReq(..)
    , FlagWriteReq(..)
    , RenderCtx(..)
    , renderSynth
      -- * CPU synthesis
    , synthHarvardCPU
    ) where

import Isacle.ISA.Types
import Isacle.ISA.CPUDef
import Isacle.ISA.ALU
import Isacle.ISA.Def
import Isacle.ISA.Encoding
import Isacle.ISA.IR
import Isacle.ISA.Build
import Isacle.ISA.Backend.Doc
import Isacle.ISA.Backend.Sim
import Isacle.ISA.Backend.Synth
import Isacle.ISA.Backend.SynthCPU
