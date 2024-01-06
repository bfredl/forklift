const forklift = @import("forklift");
const FLIR = forklift.FLIR;
const CodeBuffer = forklift.CodeBuffer;
const CFOModule = forklift.CFOModule;
const Parser = forklift.Parser;
const print = std.debug.print;

const std = @import("std");

const test_allocator = std.testing.allocator;

pub fn parse_test(ir: []const u8) !CodeBuffer {
    var res = try parse_multi(ir);
    if (res.objs.count() != 1) {
        return error.ExpectedOneFunction;
    }
    // TODO: this is a bit funky..
    res.objs.deinit();
    return res.code;
}

pub fn parse_multi(ir: []const u8) !CFOModule {
    return parse_multi_impl(ir, false);
}

pub fn parse_multi_dbg(ir: []const u8) !CFOModule {
    return parse_multi_impl(ir, true);
}

pub fn parse_multi_impl(ir: []const u8, dbg: bool) !CFOModule {
    var mod = try CFOModule.init(test_allocator);
    var parser = try Parser.init(ir, test_allocator, &mod);
    defer parser.deinit();
    errdefer mod.deinit_mem();
    parser.parse(dbg, false) catch |e| {
        parser.t.fail_pos();
        return e;
    };
    try mod.code.finalize();
    return mod;
}

pub fn expect(comptime T: type, x: T, y: T) !void {
    return std.testing.expectEqual(x, y);
}

const AFunc = *const fn (arg1: usize) callconv(.C) usize;
const BFunc = *const fn (arg1: usize, arg2: usize) callconv(.C) usize;

test "diamond cfg" {
    var self = try FLIR.init(8, test_allocator);
    defer self.deinit();

    const start = try self.addNode();
    const arg1 = try self.arg();
    const arg2 = try self.arg();
    const v = try self.variable(.{ .intptr = .quadword });

    // const const_0 = try self.const_int(0);
    const const_42 = try self.const_int(42);
    try self.putvar(start, v, const_42);
    _ = try self.icmp(start, .lt, arg1, v);

    const left = try self.addNode();
    self.n.items[start].s[0] = left;
    const addl = try self.ibinop(left, .add, v, arg2);
    try self.putvar(left, v, addl);

    const right = try self.addNode();
    self.n.items[start].s[1] = right;
    const addr = try self.ibinop(right, .add, v, arg1);
    try self.putvar(right, v, addr);

    const end = try self.addNode();
    self.n.items[left].s[0] = end;
    self.n.items[right].s[0] = end;

    const const_77 = try self.const_int(77);
    const adde = try self.ibinop(end, .add, v, const_77);
    try self.putvar(end, v, adde);

    try self.ret(end, v);

    try self.test_analysis(FLIR.X86ABI, true);
}

test "maybe_split" {
    if (true) return error.SkipZigTest;

    var mod = try CFOModule.init(test_allocator);
    defer mod.deinit_mem();
    var parser = try Parser.init(
        \\func returner(arg) {
        \\  return arg+1;
        \\}
    , test_allocator, &mod);
    defer parser.deinit();
    _ = parser.parse(false, true) catch |e| {
        parser.t.fail_pos();
        return e;
    };

    // TODO: should be mod.ir I guess??
    const self = &parser.ir;

    const pos = 1; // TODO: get("%c")
    const new_pos = try self.maybe_split(pos);
    try std.testing.expectEqual(self.iref(new_pos).?.tag, .empty);
    try self.test_analysis(FLIR.X86ABI, true);
    var cfo = try CodeBuffer.init(test_allocator);
    defer cfo.deinit();
    _ = try forklift.codegen_x86_64(self, &cfo, false);
    try cfo.finalize();
    const fun = cfo.get_ptr(0, AFunc);
    try expect(usize, 12, fun(11));
}
