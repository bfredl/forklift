const CFOModule = @import("./CFOModule.zig");
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

    var mod = try CFOModule.init(gpa);
    var parser = try Parser.init(ir, gpa, &mod);
    defer parser.deinit();
    defer mod.deinit_mem();

    parser.parse(true, false) catch |e| {
        parser.t.fail_pos();
        return e;
    };
}
