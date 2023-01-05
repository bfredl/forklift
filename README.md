# Certified Forklift Operator

A jit compiler backend written in the Zig language (for now, following the
master version of zig). Currently only x86-64 is supported. Float/vector
operations assume AVX/AVX2 instructions being available. AArch64 might be
considered in the future.

Two layers are currently being implemented. `src/CFO.zig` is a JIT assembler
where intructions are directly assembled into a memory buffer. A IR format for
instructions are also being developed, with some basic analysis steps
available.

Features for the Assembler:

- [x] 64-bit register and memory operations (mov and add-like opcodes)
- [x] Conditional jumps
- [x] Full support for effective adresses like `[rax + 8*rcx + imm32]`
- [ ] byte/word/dword memory operations (partially available)
- [x] VEX encoded instructions (scalar and vector f32/f64-math)
- [ ] SSE instructions (when lacking AVX/AVX2 support)
- [ ] integer AVX2 instructions
- [x] Unaligned and aligned load/store of XMM/YMM vectors
- [x] Dump final output using `ndisasm`
- [x] Tracebacks to generated code (using a custom signal handler or patch to stdlib)

A low-level IR with a textual representation is implemented in `src/FLIR.zig`.
The current aim is to implement an IR with a low memory footprint suitable for
jits under performance and memory constraints. Numerical indicies are used
throughout instead of pointers inside the IR.

FLIR uses SSA-form through the entire pipeline, though multable temporaries are
supported in the input format, similar to QBE. These get converted to
SSA form early in analysis. The values in the IR is considered untyped,
except for a classification of values that are to be stored in a general
purpose register or in a XMM/YMM register.

The scope of this layer is to implement basic optimizations such as register
allocation with efficent interval splitting, copy propagation, and perhaps
simple loop transformations.

- [x] Conversion from mutable temporaries to proper SSA form
- [x] liveliness analysis
- [x] basic (very conservative) linear scan register allocation
- [ ] efficient register allocation with interval spliting
- [ ] function calls with C ABI
- [x] x86-64 code generation.
- [ ] EBPF code generation (currently investigated as a [separate project](https://github.com/bfredl/eiri))
