# ISACLE

Instruction Set Architecture Clash Environment.

This repository packages the ISA-agnostic CPU-building blocks extracted from
[`JonathanFraser/clavr`](https://github.com/JonathanFraser/clavr) into a small
Clash project. It centralizes reusable building blocks for creating CPU cores,
including:

- generic memory interfaces
- generic ALU composition helpers
- instruction fetch/decode helpers
- Harvard-style pipeline and ISA abstractions
- reusable GPIO peripheral logic

## Building

```bash
stack test
```
