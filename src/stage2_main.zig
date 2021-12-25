const page_allocator = std.heap.page_allocator;
const std = @import("std");
const CFO = @import("./CFO.zig");

pub fn main2() !void {
    // wasteful but (doesn't) works :D
    // const allocator = page_allocator;
    // var cfo = CFO.init(allocator) catch unreachable;
    //
    const size = 1024 * 8;
    var arr1 = try std.heap.page_allocator.alloc(f64, size);
    var arr2 = try std.heap.page_allocator.alloc(f64, size);

    const IPReg = CFO.IPReg;
    const idx: IPReg = .rcx;
    const arg1: IPReg = .rdi;
    const arg2: IPReg = .rsi;
    const arg3: IPReg = .rdx;
    const v0: u4 = 0;

    arr1[5] = 7.0;
    arr2[5] = 6.5;

    var cfo = CFO.init_stage2();
    var pos = cfo.get_target();
    try cfo.enter();
    try cfo.arit(.xor, idx, idx);
    const loop = cfo.get_target();
    try cfo.vmovrm(.sd, v0, CFO.qi(arg1, idx));
    try cfo.vmathrm(.add, .sd, v0, v0, CFO.qi(arg2, idx));
    try cfo.vmovmr(.sd, CFO.qi(arg1, idx), v0);
    try cfo.aritri(.add, idx, 1);
    try cfo.arit(.cmp, idx, arg3);
    try cfo.jbck(.l, loop);
    try cfo.mov(.rax, idx);
    try cfo.leave();
    try cfo.ret();
    try cfo.finalize_stage2();

    const runcount: usize = 137;
    var fun = cfo.get_ptr_stage2(pos, fn ([*]f64, [*]f64, usize) callconv(.C) usize);
    var ret = fun(arr1.ptr, arr2.ptr, runcount);
    std.os.exit(@truncate(u8, ret));
    std.os.exit(@floatToInt(u8, 2.0 * arr1[5]));
}

pub fn main() void {
    main2() catch unreachable;
}
