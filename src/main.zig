const std = @import("std");
const print = std.debug.print;
const CFO = @import("./CFO.zig");
const OSHA = @import("./OSHA.zig");
const parse = @import("./parse.zig");
const FLIR = @import("./Old_FLIR.zig");

const page_allocator = std.heap.page_allocator;

var the_cfo: ?*CFO = null;
pub fn addr_lookup(addr: usize) usize {
    return if (the_cfo) |c| c.lookup(addr) else addr;
}

pub fn main() !void {
    print("Yes, I am your CFO (certified forklift operator)\n", .{});

    const size = 1024 * 32;

    var arr1 = try std.heap.page_allocator.alloc(f64, size);
    var arr2 = try std.heap.page_allocator.alloc(f64, size);
    var i: usize = 0;
    while (i < size) : (i += 1) {
        arr1[i] = @intToFloat(f64, i);
        arr2[i] = 100000.0 * @intToFloat(f64, i);
    }

    const IPReg = CFO.IPReg;
    const idx: IPReg = .rcx;
    const arg1: IPReg = .rdi;
    const arg2: IPReg = .rsi;
    const arg3: IPReg = .rdx;
    const v0: u4 = 0;
    const v1: u4 = 1;

    const allocator = std.testing.allocator;
    var cfo = try CFO.init(allocator);
    defer cfo.deinit();

    const start = cfo.get_target();
    try cfo.enter();
    try cfo.arit(.xor, idx, idx);
    const loop = cfo.get_target();
    // try cfo.vmovurm(.sd, v0, CFO.a(idx));
    try cfo.vmovurm(.sd, v0, CFO.qi(arg1, idx));
    //try cfo.vmathfrm(.add, .sd, v0, v0, CFO.qi(arg2, idx));
    try cfo.vmovurm(.sd, v1, CFO.qi(arg2, idx));
    try cfo.vmathf(.add, .sd, v0, v0, v1);
    try cfo.vmovumr(.sd, CFO.qi(arg1, idx), v0);
    try cfo.aritri(.add, idx, 1);
    try cfo.arit(.cmp, idx, arg3);
    try cfo.jbck(.l, loop);
    // try cfo.trap();
    try cfo.leave();
    try cfo.ret();

    const start_parse = cfo.get_target();
    var flir = try FLIR.init(0, allocator);
    defer flir.deinit();

    try flir.loop_start();
    _ = try parse.parse(&flir, "xi = xi + yi;");
    try flir.loop_end();
    flir.live(true);
    _ = try flir.scanreg(true);
    flir.debug_print(false);

    try cfo.enter();
    _ = try flir.codegen(&cfo, true);
    try cfo.leave();
    try cfo.ret();

    try cfo.dbg_nasm(allocator);

    const start_simd = cfo.get_target();
    try cfo.enter();
    try cfo.arit(.xor, idx, idx);
    const loop2 = cfo.get_target();
    try cfo.vmovarm(.pd4, v0, CFO.qi(arg1, idx));
    try cfo.vmathfrm(.add, .pd4, v0, v0, CFO.qi(arg2, idx));
    try cfo.vmovamr(.pd4, CFO.qi(arg1, idx), v0);
    try cfo.aritri(.add, idx, 4);
    try cfo.arit(.cmp, idx, arg3);
    try cfo.jbck(.l, loop2);
    try cfo.vzeroupper();
    try cfo.leave();
    try cfo.ret();

    const start_simd2 = cfo.get_target();
    try cfo.enter();
    try cfo.arit(.xor, idx, idx);
    const loop3 = cfo.get_target();
    try cfo.vmovarm(.pd4, v0, CFO.qi(arg1, idx));
    try cfo.vmovarm(.pd4, v1, CFO.qi(arg1, idx).o(32));
    try cfo.vmathfrm(.add, .pd4, v0, v0, CFO.qi(arg2, idx));
    try cfo.vmathfrm(.add, .pd4, v1, v1, CFO.qi(arg2, idx).o(32));
    try cfo.vmovamr(.pd4, CFO.qi(arg1, idx), v0);
    try cfo.vmovamr(.pd4, CFO.qi(arg1, idx).o(32), v1);
    try cfo.aritri(.add, idx, 8);
    try cfo.arit(.cmp, idx, arg3);
    try cfo.jbck(.l, loop3);
    try cfo.vzeroupper();
    try cfo.leave();
    // try cfo.retnasm();
    try cfo.ret();

    // try cfo.dbg_test();
    try cfo.finalize();
    the_cfo = &cfo;
    defer the_cfo = null;

    OSHA.install(&cfo);
    defer OSHA.clear();

    const scalar_add = cfo.get_ptr(start, fn (arg1: [*]f64, arg2: [*]f64, arg3: u64) callconv(.C) void);
    const parse_add = cfo.get_ptr(start_parse, fn (arg1: [*]f64, arg2: [*]f64, arg3: ?[*]f64, arg3: u64) callconv(.C) void);
    const simd_add = cfo.get_ptr(start_simd, fn (arg1: [*]f64, arg2: [*]f64, arg3: u64) callconv(.C) void);
    const simd2_add = cfo.get_ptr(start_simd2, fn (arg1: [*]f64, arg2: [*]f64, arg3: u64) callconv(.C) void);

    var timer = try std.time.Timer.start();
    i = 0;
    while (i < 10) : (i += 1) {
        parse_add(arr1.ptr, arr2.ptr, null, size);
        const tid1p = timer.lap();
        scalar_add(arr1.ptr, arr2.ptr, size);
        const tid1 = timer.lap();
        simd_add(arr1.ptr, arr2.ptr, size);
        const tid2 = timer.lap();
        simd2_add(arr1.ptr, arr2.ptr, size);
        const tid3 = timer.lap();
        print("tidning: {}, {}, {}, {}\n", .{ tid1, tid1p, tid2, tid3 });
        _ = timer.lap();
    }

    print("did: {}\n", .{arr1[1]});
    print("did: {}\n", .{arr1[2]});
    print("did: {}\n", .{arr1[3]});
    print("did: {}\n", .{arr1[1023]});
}
