const page_allocator = std.heap.page_allocator;
const test_allocator = std.testing.allocator;
const std = @import("std");
const CFO = @import("./CFO.zig");
const parse = @import("./parse.zig");
const FLIR = @import("./Old_FLIR.zig");

const builtin = @import("builtin");

const s2 = builtin.zig_backend != .stage1;

pub fn main2() !void {
    // wasteful but (doesn't) works :D
    // const allocator = page_allocator;
    // var cfo = CFO.init(allocator) catch unreachable;
    //
    const size = 1024 * 16;
    var arr1 = try page_allocator.alloc(f64, size);
    var arr2 = try page_allocator.alloc(f64, size);

    const IPReg = CFO.IPReg;
    const idx: IPReg = .rcx;
    const arg1: IPReg = .rdi;
    const arg2: IPReg = .rsi;
    const arg3: IPReg = .rdx;
    const v0: u4 = 0;

    arr1[5] = 7.0;
    arr2[5] = 8.5;

    var cfo = try CFO.init(page_allocator);
    var pos = cfo.get_target();

    if (false) {
        try cfo.enter();
        try cfo.arit(.xor, idx, idx);
        const loop = cfo.get_target();
        try cfo.vmovurm(.sd, v0, CFO.qi(arg1, idx));
        try cfo.vmathfrm(.add, .sd, v0, v0, CFO.qi(arg2, idx));
        try cfo.vmovumr(.sd, CFO.qi(arg1, idx), v0);
        try cfo.aritri(.add, idx, 1);
        try cfo.arit(.cmp, idx, arg3);
        try cfo.jbck(.l, loop);
        try cfo.mov(.rax, idx);
        try cfo.leave();
        try cfo.ret();
    } else {
        var flir = try FLIR.init_stage2(0, page_allocator);
        try flir.loop_start();
        //_ = try parse.parse(&flir, "xi = xi + yi;");
        const Inst = FLIR.Inst;
        const l1 = try flir.put(Inst{ .tag = .load, .opspec = 0x10, .op1 = 0 });
        const l2 = try flir.put(Inst{ .tag = .load, .opspec = 0x11, .op1 = 0 });
        const addi: FLIR.Inst = Inst{ .tag = .vmath, .opspec = CFO.VMathOp.add.off(), .op1 = l1, .op2 = l2 };
        const add = try flir.put(addi);
        var inst2: FLIR.Inst = .{ .tag = .store, .opspec = 0x10, .op1 = add, .op2 = 0 };
        _ = try flir.put(inst2);

        try flir.loop_end();
        flir.live(true);
        _ = try flir.scanreg(true);
        // defer flir.deinit();
        flir.debug_print(false);

        try cfo.enter();
        _ = try flir.codegen(&cfo, false);
        try cfo.leave();
        try cfo.ret();
    }

    try cfo.finalize();

    const runcount: usize = 137;
    // THANKS WERK
    const ptrtype = if (s2) *const fn ([*]f64, [*]f64, usize) callconv(.C) usize else fn ([*]f64, [*]f64, usize) callconv(.C) usize;
    var fun = cfo.get_ptr(pos, ptrtype);
    var ret = fun(arr1.ptr, arr2.ptr, runcount);
    std.os.exit(@floatToInt(u8, 2.0 * arr1[5]));
    std.os.exit(@truncate(u8, ret));
}

pub fn main() void {
    main2() catch unreachable;
}
