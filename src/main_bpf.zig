const CFOModule = @import("./CFOModule.zig");
const Parser = @import("./Parser.zig");
const std = @import("std");
const common = @import("./common.zig");

pub var options: common.DebugOptions = .{};

pub fn main() !void {
    const ir = "";
    var allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const gpa = allocator.allocator();

    var mod = try CFOModule.init(gpa);
    defer mod.deinit_mem();

    try Parser.parse(&mod, gpa, ir, true, false);
}
