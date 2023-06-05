pub const CFO = @import("./CFO.zig");
pub const FLIR = @import("./FLIR.zig");

// TODO: crufty!
pub const BPFCode = @import("./codegen_bpf.zig").Code;
pub const codegen_bpf = @import("./codegen_bpf.zig").codegen;

pub const codegen_x86_64 = @import("./codegen.zig").codegen;
pub const IRParse = @import("./IRParse.zig");
pub const dump_bpf = @import("./bpf.zig").dump_bpf;
pub const bpf_rt = @import("./bpf_rt.zig");
