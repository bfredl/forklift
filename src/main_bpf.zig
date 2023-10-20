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
    var parser = try Parser.init(ir, gpa, &mod);
    defer parser.deinit();
    defer mod.deinit_mem();

    parser.parse(true, false) catch |e| {
        parser.t.fail_pos();
        return e;
    };
}
