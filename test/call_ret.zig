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

test "basic" {
    var self = try FLIR.init(8, test_allocator);
    defer self.deinit();

    const start = try self.addNode();

    const CANTER = 6; // TODO: make me eight!!!

    var args: [8]u16 = @splat(FLIR.NoRef);
    for (0..CANTER) |i| {
        args[i] = try self.arg(.{ .intptr = .quadword });
    }

    const call = try self.call(start, .fun_addr, try self.const_uint(@intFromPtr(&target_int8)));
    var arglist = call;
    for (0..CANTER) |i| {
        arglist = try self.callarg(start, arglist, args[i], .{ .intptr = .quadword });
    }

    try self.ret(start);
    self.debug_print();
    try self.test_analysis(FLIR.X86ABI, true);
    self.debug_print();

    var mod = try CFOModule.init(test_allocator);
    defer mod.deinit_mem();
    const target = try codegen(&self, &mod, false, null);
    try mod.code.finalize();

    const fun = mod.code.get_ptr(target, EightBallFn);
    fun(10, 11, 12, 13, 14, 15, 16, 17);
}
