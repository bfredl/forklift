pub const CodeBuffer = @import("./CodeBuffer.zig");
pub const X86Asm = @import("./X86Asm.zig");
pub const FLIR = @import("./FLIR.zig");

pub const BPFCode = CFOModule.BPFCode;
pub const codegen_bpf = @import("./codegen_bpf.zig").codegen;

pub const codegen_x86_64 = @import("./codegen.zig").codegen;
pub const parse_mod = @import("./CFOScript.zig").parse_mod;
pub const bpf = @import("./bpf.zig");
pub const dump_bpf = bpf.dump_bpf;
pub const CFOModule = @import("./CFOModule.zig");

const defs = @import("./defs.zig");
pub const debug_options = defs.debug_options;
pub const DebugOptions = defs.DebugOptions;
