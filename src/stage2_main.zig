const page_allocator = std.heap.page_allocator;
const std = @import("std");
const CFO = @import("./CFO.zig");

pub fn main2() !void {
    // wasteful but (doesn't) works :D
    // const allocator = page_allocator;
    // var cfo = CFO.init(allocator) catch unreachable;
    var cfo = CFO.init_stage2();
    var pos = cfo.get_target();
    try cfo.rex_wrxb(true, false, false, false);
    try cfo.wb(0x8b); // OP reg, \rm
    try cfo.modRm(0b11, 0, 6);
    try cfo.ret();
    try cfo.finalize_stage2();

    var fun = cfo.get_ptr_stage2(pos, fn (usize, usize) callconv(.C) usize);
    std.os.exit(@intCast(u8, fun(10, 5)));
}

pub fn main() void {
    main2() catch unreachable;
}
