const std = @import("std");
const print = std.debug.print;
const CFO = @import("./CFO.zig");

const page_allocator = std.heap.page_allocator;

pub fn main() !void {
    const allocator = std.testing.allocator;
    print("Yes, I am your CFO (certified forklift operator)\n", .{});

    const size = 1024;

    var arr1 = try std.heap.page_allocator.alloc(f64, size);
    var arr2 = try std.heap.page_allocator.alloc(f64, size);
    var i: usize = 0;
    while (i < size) : (i += 1) {
        arr1[i] = @intToFloat(f64, i);
        arr2[i] = 100000.0 * @intToFloat(f64, i);
    }

    const IPReg = CFO.IPReg;
    const AOp = CFO.AOp;

    const idx = IPReg.rcx;
    const arg1 = IPReg.rdi;
    const arg2 = IPReg.rsi;
    const arg3 = IPReg.rdx;
    const sd = CFO.FMode.sd;
    const v0 = 0;

    var cfo = try CFO.init(allocator);
    const start = cfo.get_target();
    try cfo.arit(AOp.xor, idx, idx);
    const loop = cfo.get_target();
    try cfo.vmovrm(sd, v0, CFO.qi(arg1, idx));
    try cfo.vmathrm(CFO.VMathOp.add, sd, v0, v0, CFO.qi(arg2, idx));
    try cfo.vmovmr(sd, CFO.qi(arg1, idx), v0);
    try cfo.aritri(AOp.add, idx, 1);
    try cfo.arit(AOp.cmp, idx, arg3);
    try cfo.jbck(CFO.Cond.l, loop);
    try cfo.ret();
    try cfo.finalize();
    const scalar_add = cfo.get_ptr(start, fn (arg1: [*]f64, arg2: [*]f64, arg3: u64) callconv(.C) void);
    scalar_add(arr1.ptr, arr2.ptr, size);

    print("did: {}\n", .{arr1[1]});
    print("did: {}\n", .{arr1[2]});
    print("did: {}\n", .{arr1[3]});
    print("did: {}\n", .{arr1[1023]});
}
