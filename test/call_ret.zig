const forklift = @import("forklift");
const FLIR = forklift.FLIR;
const CodeBuffer = forklift.CodeBuffer;
const CFOModule = forklift.CFOModule;
const parse_mod = forklift.parse_mod;
const print = std.debug.print;
const codegen = forklift.codegen_x86_64;
const std = @import("std");
const test_allocator = std.testing.allocator;

var res: struct { a: usize, b: usize, c: usize, d: usize, e: usize, f: usize, g: usize, h: usize } = undefined;

const EightBallFn = *const fn (a: usize, b: usize, c: usize, d: usize, e: usize, f: usize, g: usize, h: usize) callconv(.c) void;
fn target_int8(a: usize, b: usize, c: usize, d: usize, e: usize, f: usize, g: usize, h: usize) callconv(.c) void {
    res = .{ .a = a, .b = b, .c = c, .d = d, .e = e, .f = f, .g = g, .h = h };
}

const ArgSpec = union(enum) { arg: usize, val: usize };
fn p(arg: usize) ArgSpec {
    return .{ .arg = arg };
}
fn v(val: usize) ArgSpec {
    return .{ .val = val };
}

fn fluffer(mod: *CFOModule, narg: usize, spec: []const ArgSpec) !EightBallFn {
    var self = try FLIR.init(8, test_allocator);
    defer self.deinit();
    const start = try self.addNode();

    const CANTER = 8;
    if (narg > CANTER) return error.FixThis;

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
    self.debug_print();

    const target = try codegen(&self, mod, false, null);
    try mod.code.finalize();

    const fun = mod.code.get_ptr(target, EightBallFn);
    return fun;
}

test "basic" {
    var mod = try CFOModule.init(test_allocator);
    defer mod.deinit_mem();

    const fun = try fluffer(&mod, 6, &.{ p(0), p(1), p(2), p(3), p(4), p(5) });

    fun(10, 11, 12, 13, 14, 15, 16, 17);
}

test "shift in" {
    var mod = try CFOModule.init(test_allocator);
    defer mod.deinit_mem();

    const fun = try fluffer(&mod, 6, &.{ v(67), p(0), p(1), p(2), p(3), p(4) });

    fun(10, 11, 12, 13, 14, 15, 16, 17);
}
