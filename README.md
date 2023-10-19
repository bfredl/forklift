# Certified Forklift Operator

A jit compiler backend written in the Zig language. Currently only x86-64 and
EBPF are supported. x86 assumes x86-64-3, i e. float/vector operations use
AVX/AVX2 instructions. ARMv7 and AARCH64 might be considered in the future.

The core data structure is implemented in `src/FLIR.zig` (ForkLift IR). To keep
memory footprint low, the same representation is using throughout the analysis
pipeline, all the way to code generation. Numerical indices are used
instead of pointers between nodes/instructions to keep it compact.

FLIR uses SSA-form through the entire pipeline, though mutable references are
supported in the input format, similar to QBE. The values in the IR are typed
only by their register class, general purpose registers (integer/pointer
values) and vector/float registers. The control flow graph is assumed to be
reducible, at least in the input.

- [x] SSA-construction (for mutables in the input)
- [x] liveness analysis
- [ ] target-independent optimizations (Dead code elimination, copy propagation)
- [ ] loop analysis and optimizations.
- [x] precise linear scan register allocation on SSA-form
- [ ] efficient spilling by interval splitting
- [x] function calls with C ABI (sys-V)
- [x] Linux syscalls (and EBPF "helper" calls)
- [x] code generation for x86-64 and EBPF

`src/x86Asm.zig` could also be used standalone as a simple JIT assembler.

- [x] 64-bit register and memory operations (mov and add-like opcodes)
- [x] Conditional jumps
- [x] Full forms of mov/lea with `[rax + 8*rcx + imm32]` adresses
- [ ] byte/word/dword memory operations (partially available)
- [x] VEX encoded instructions (scalar and vector f32/f64-math)
- [ ] SSE instructions (when lacking AVX/AVX2 support)
- [ ] integer AVX2 instructions
- [x] Unaligned and aligned load/store of XMM/YMM vectors
- [x] Dump final output using `ndisasm`
- [x] Tracebacks to generated code (using a custom signal handler or patch to stdlib)

For EBPF, instruction assembly is available in the zig standard library (`std.os.linux.BPF`)
