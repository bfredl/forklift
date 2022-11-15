# Certified Forklift Operator

A jit compiler backend for the Zig language. Currently only x86-64 is supported, with a focus of float/vector code (using AVX/AVX2 instructions). AArch64 might be considered in the future.

The codebase consists of two major parts. `src/CFO.zig` implements a JIT assembler where intructions are directly assembled into a memory buffer.

Features:

- [x] 64-bit register and memory operations (mov and add-like opcodes)
- [x] Conditional jumps
- [x] Full support for effective adresses like `[rax + 8*rcx + imm32]`
- [ ] byte/word/dword memory operations
- [x] VEX encoded instructions (scalar and vector f32/f64-math)
- [ ] SSE instructions (when lacking AVX/AVX2 support)
- [ ] integer AVX2 instructions
- [x] Unaligned and aligned load/store of XMM/YMM vectors
- [x] Dump final output using `ndisasm`
- [x] Tracebacks to generated code (using a custom signal handler or patch to stlib)

Secondly, `src/FLIR.zig` implements a simple SSA IR on top of the above. What is implemented is a few basic compiler passes, like

- [x] Conversion from mutable temporaries to proper SSA form
- [ ] liveliness analysis (partially, doesn't handle nested loops yet)
- [x] linear scan Register allocation
- [x] x86-64 code generation.
