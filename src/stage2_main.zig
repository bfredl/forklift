const page_allocator = std.heap.page_allocator;
const std = @import("std");
const CFO = @import("./CFO.zig");

pub fn main2() !void {
    // wasteful but (doesn't) works :D
    // const allocator = page_allocator;
    // var cfo = CFO.init(allocator) catch unreachable;
    var cfo = CFO.init_stage2();
    try cfo.ret();
    // cfo.finalize();
    // const ArrayList = std.ArrayList;
    // var code = ArrayList(u8).initCapacity(allocator, 4096);
}

pub fn main() void {
    main2() catch unreachable;
}
