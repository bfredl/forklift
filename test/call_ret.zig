const forklift = @import("forklift");
const FLIR = forklift.FLIR;
const CodeBuffer = forklift.CodeBuffer;
const CFOModule = forklift.CFOModule;
const parse_mod = forklift.parse_mod;
const print = std.debug.print;
const std = @import("std");
const test_allocator = std.testing.allocator;

var res: struct { a: usize, b: usize, c: usize, d: usize, e: usize, f: usize, g: usize, h: usize } = undefined;

fn target_int8(a: usize, b: usize, c: usize, d: usize, e: usize, f: usize, g: usize, h: usize) callconv(.c) void {
    res = .{ .a = a, .b = b, .c = c, .d = d, .e = e, .f = f, .g = g, .h = h };
}

test "basic" {
    var self = try FLIR.init(8, test_allocator);
    defer self.deinit();

    const start = try self.addNode();

    const CANTER = 6;

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
}
