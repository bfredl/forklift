const forklift = @import("forklift");
const FLIR = forklift.FLIR;
const CodeBuffer = forklift.CodeBuffer;
const CFOModule = forklift.CFOModule;
const parse_mod = forklift.parse_mod;
const print = std.debug.print;
const codegen = forklift.codegen_x86_64;
const std = @import("std");
const test_allocator = std.testing.allocator;

var res: [8]usize = undefined;

const EightBallFn = *const fn (a: usize, b: usize, c: usize, d: usize, e: usize, f: usize, g: usize, h: usize) callconv(.c) void;
fn target_int8(a: usize, b: usize, c: usize, d: usize, e: usize, f: usize, g: usize, h: usize) callconv(.c) void {
    res = .{ a, b, c, d, e, f, g, h };
}

const ArgSpec = union(enum) { arg: usize, val: usize };
fn p(arg: usize) ArgSpec {
    return .{ .arg = arg };
}
fn v(val: usize) ArgSpec {
    return .{ .val = val };
}

fn fluffer(narg: usize, spec: []const ArgSpec) !void {
    var mod = try CFOModule.init(test_allocator);
    defer mod.deinit_mem();

    var self = try FLIR.init(8, test_allocator);
    defer self.deinit();
    const start = try self.addNode();

    const CANTER = 8;
    if (narg > CANTER or spec.len > CANTER) return error.FixThis;

    var args: [CANTER]u16 = @splat(FLIR.NoRef);
    for (0..narg) |i| {
        args[i] = try self.arg(.{ .intptr = .quadword });
    }

    const call = try self.call(start, .fun_addr, try self.const_uint(@intFromPtr(&target_int8)));
    var arglist = call;
    for (spec) |s| {
        const val = switch (s) {
            .arg => |ia| if (ia < narg) args[ia] else return error.CanTFixThis,
            .val => |va| try self.const_uint(va),
        };

        arglist = try self.callarg(start, arglist, val, .{ .intptr = .quadword });
    }

    try self.ret(start);
    try self.test_analysis(FLIR.X86ABI, true);
    // self.debug_print();

    const target = try codegen(&self, &mod, false, null);
    try mod.code.finalize();

    const fun = mod.code.get_ptr(target, EightBallFn);
    fun(10, 11, 12, 13, 14, 15, 16, 17);

    const n = spec.len;
    var expect: [CANTER]usize = undefined;
    for (expect[0..n], spec) |*e, s| {
        e.* = switch (s) {
            .arg => |ia| 10 + ia,
            .val => |va| va,
        };
    }

    try std.testing.expectEqualSlices(usize, expect[0..n], res[0..n]);
}

test "basic" {
    try fluffer(6, &.{ p(0), p(1), p(2), p(3), p(4), p(5) });
}

test "shift in" {
    try fluffer(6, &.{ v(67), p(0), p(1), p(2), p(3), p(4) });
    try fluffer(6, &.{ v(1001), v(1010), p(0), p(1), p(2), p(3) });
}

test "swap 2" {
    try fluffer(6, &.{ p(1), p(0), p(2), p(3), p(4), p(5) });
}

test "swap 3" {
    try fluffer(6, &.{ p(2), p(0), p(1), p(3), p(4), p(5) });
    try fluffer(6, &.{ p(1), p(2), p(0), p(3), p(4), p(5) });
}

test "from stack" {
    try fluffer(7, &.{p(6)});
    try fluffer(8, &.{ p(7), p(6) });
}
