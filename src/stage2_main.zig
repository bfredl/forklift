const page_allocator = std.heap.page_allocator;
const std = @import("std");
const CFO = @import("./CFO.zig");

pub fn main2() !void {
    // wasteful but (doesn't) works :D
    // const allocator = page_allocator;
    // var cfo = CFO.init(allocator) catch unreachable;
    var cfo = CFO.init_stage2();
    var pos = cfo.get_target();
    try cfo.ret();
    try cfo.finalize_stage2();

    var pointer = cfo.get_ptr_stage2(pos, fn () callconv(.C) void);
    pointer();
}

pub fn main() void {
    main2() catch unreachable;
}
