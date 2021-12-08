const std = @import("std");
const print = std.debug.print;
const CFO = @import("./CFO.zig");

pub fn main() !void {
    const allocator = std.testing.allocator;
    print("Yes, I am your CFO (certified forklift operator)\n", .{});

    var cfo = try CFO.init(allocator);
    try cfo.ret();
    var fptr = try cfo.test_finalize();
    var val = fptr(4, 10);
    print("did: {}\n", .{val});
}
