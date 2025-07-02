const CFOModule = @import("./CFOModule.zig");
const parse_mod = @import("./CFOScript.zig").parse_mod;
const std = @import("std");
const defs = @import("./defs.zig");

pub var options: defs.DebugOptions = .{};

pub fn main() !void {
    const ir = "";
    var allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const gpa = allocator.allocator();

    var mod = try CFOModule.init(gpa);
    defer mod.deinit_mem();

    try parse_mod(&mod, gpa, ir, true, false);
}
