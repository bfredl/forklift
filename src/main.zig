const std = @import("std");
const print = std.debug.print;
const CFO = @import("./CFO.zig");

pub fn main() !void {
    const allocator = std.testing.allocator;
    print("Yes, I am your CFO (certified forklift operator)\n", .{});

    var cfo = try CFO.init(allocator);
    _ = try cfo.inst_1byte(0x00);
}
