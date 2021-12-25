# Certified Forklift Operator

This is a x86 JIT assembler for the Zig language. Later on it might become a simple code generator for array expression graphs.

Currently only x86-64 is supported. AArch64 might be considered in the future.

Features:

- [x] 64-bit register and memory operations (mov and add-like opcodes)
- [x] Conditional jumps
- [x] Full support for effective adresses like `[rax + 8*rcx + imm32]`
- [ ] byte/word/dword memory operations
- [x] VEX encoded instructions (scalar and vector f32/f64-math)
- [ ] SSE instructions (when lacking AVX/AVX2 support)
- [x] Unaligned and aligned load/store of vectors
- [x] Dump final output using `ndisasm`
- [x] Tracebacks to generated code (using a custom signal handler or patch to stlib)
- [x] _Partial_ support for being compiled with zig stage2
