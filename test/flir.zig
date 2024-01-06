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
        \\func returner
        \\  %x = arg
        \\  %c = 1
        \\  %y = add %x %c
        \\  ret %y
        \\end
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

test "vopper" {
    // TODO: lol obviously support this without %z arg as well
    var cfo = try parse_test(
        \\func returner
        \\  %x = arg
        \\  %y = arg
        \\  %z = arg
        \\  %xa = load sd [%x %z]
        \\  %ya = load sd [%y %z]
        \\  %za = vop sd add %xa %ya
        \\  store sd [%x %z] %za
        \\  ret 0
        \\end
    );
    defer cfo.deinit();
    var x: f64 = 28.0;
    var y: f64 = 2.75;

    const FFunc = *const fn (arg1: *f64, arg2: *f64, yark: usize) callconv(.C) usize;
    const fun = cfo.get_ptr(0, FFunc);
    try expect(usize, 0, fun(&x, &y, 0));
    try expect(f64, 30.75, x);
}

test "multi function" {
    var res = try parse_multi(
        \\func adder
        \\  %x = arg
        \\  %y = arg
        \\  %z = shl %x 1
        \\  %sum = add %y %z
        \\  ret %sum
        \\end
        \\
        \\func multiplier
        \\  %x = arg
        \\  %y = arg
        \\  %z = mul %x %y
        \\  ret %z
        \\end
    );
    defer res.deinit_mem();

    const fun1 = try res.get_func_ptr("adder", BFunc);
    const fun2 = try res.get_func_ptr("multiplier", BFunc);
    try expect(usize, 210, fun1(100, 10));
    try expect(usize, 1000, fun2(100, 10));
}

test "call near" {
    var res = try parse_multi(
        \\func kuben
        \\  %x = arg
        \\  %prod = mul %x %x
        \\  %prod2 = mul %prod %x
        \\  ret %prod2
        \\end
        \\
        \\func twokube
        \\  %x = arg
        \\  %y = arg
        \\  %xx = call kuben %x
        \\  %yy = call kuben %y
        \\  %summa = add %xx %yy
        \\  ret %summa
        \\end
    );
    defer res.deinit_mem();

    const fun1 = try res.get_func_ptr("kuben", AFunc);
    try expect(usize, 1000000, fun1(100));

    const fun2 = try res.get_func_ptr("twokube", BFunc);
    try expect(usize, 1008, fun2(2, 10));
}
