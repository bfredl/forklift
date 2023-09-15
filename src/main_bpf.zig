const bpf_rt = @import("./bpf_rt.zig");
const Parser = @import("./Parser.zig");
const std = @import("std");

pub var options = struct {
    dbg_raw_ir: bool = false,
    dbg_analysed_ir: bool = false,
    dbg_disasm: bool = false,
    dbg_vregs: bool = false,
    dbg_trap: bool = false,
    dbg_disasm_ir: bool = false,
}{};

pub fn main() !void {
    const ir = "";
    var allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const gpa = allocator.allocator();

    var parser = try Parser.init(ir, gpa);
    var mod = bpf_rt.BPFModule.init(gpa);
    defer parser.deinit();
    defer mod.deinit_mem();

    parser.parse_bpf(true) catch |e| {
        std.debug.print("fail at {}\n", .{parser.pos});
        return e;
    };
}
