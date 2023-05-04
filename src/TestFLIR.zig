const FLIR = @import("./FLIR.zig");
const CFO = @import("./CFO.zig");
const codegen = @import("./codegen.zig");
const print = std.debug.print;

const std = @import("std");
const os = std.os;

const IRParse = @import("./IRParse.zig");
const test_allocator = std.testing.allocator;

pub fn parse(self: *FLIR, ir: []const u8) !void {
    var parser = IRParse.init(ir, test_allocator);
    _ = parser.parse_func(self) catch |e| {
        print("fail at {}\n", .{parser.pos});
        return e;
    };
}

pub fn parse_test(ir: []const u8) !CFO {
    var res = try parse_multi(ir);
    if (res.objs.count() != 1) {
        return error.ExpectedOneFunction;
    }
    res.objs.deinit();
    return res.cfo;
}

const Res = struct {
    cfo: CFO,
    objs: std.StringHashMap(u32),

    pub fn get_ptr(self: *Res, name: []const u8, comptime T: type) !T {
        const addr = self.objs.get(name) orelse return error.FAILURE;
        return self.cfo.get_ptr(addr, T);
    }

    pub fn deinit(self: *Res) void {
        self.cfo.deinit();
        self.objs.deinit();
    }
};

pub fn parse_multi(ir: []const u8) !Res {
    return parse_multi_impl(ir, false);
}

pub fn parse_multi_dbg(ir: []const u8) !Res {
    return parse_multi_impl(ir, true);
}

pub fn parse_multi_impl(ir: []const u8, dbg: bool) !Res {
    var parser = IRParse.init(ir, test_allocator);
    var cfo = try CFO.init(test_allocator);
    parser.parse(&cfo, dbg) catch |e| {
        print("fail at {}\n", .{parser.pos});
        return e;
    };
    try cfo.finalize();
    return Res{ .cfo = cfo, .objs = parser.to_map() };
}

pub fn expect(comptime T: type, x: T, y: T) !void {
    return std.testing.expectEqual(x, y);
}

const UFunc = *const fn () callconv(.C) usize;
const AFunc = *const fn (arg1: usize) callconv(.C) usize;
const AIFunc = *const fn (arg1: isize) callconv(.C) isize;
const BFunc = *const fn (arg1: usize, arg2: usize) callconv(.C) usize;

test "returner" {
    var cfo = try parse_test(
        \\func returner
        \\  ret 7
        \\end
    );
    defer cfo.deinit();

    try expect(usize, 7, cfo.get_ptr(0, UFunc)());
}

test "comment" {
    var cfo = try parse_test(
        \\func returner
        \\  ret 7 ; this is a comment
        \\end
    );
    defer cfo.deinit();

    try expect(usize, 7, cfo.get_ptr(0, UFunc)());
}

test "var returner" {
    var cfo = try parse_test(
        \\func returner
        \\  var %myvar
        \\  %myvar := 57
        \\
        \\
        \\  ret %myvar
        \\end
    );
    defer cfo.deinit();

    try expect(usize, 57, cfo.get_ptr(0, UFunc)());
}

test "diamond returner" {
    var cfo = try parse_test(
        \\func returner
        \\  %y = arg
        \\  var %foo
        \\  jl %y 17 :small
        \\:big
        \\  %foo := 98
        \\  jmp :enda
        \\:small
        \\  %foo := 20
        \\:enda
        \\  ret %foo
        \\end
    );
    defer cfo.deinit();

    const fun = cfo.get_ptr(0, AFunc);
    try expect(usize, 20, fun(10));
    try expect(usize, 98, fun(35));
}

test "loop adder" {
    var cfo = try parse_test(
        \\func returner
        \\  %y = arg
        \\  var %i
        \\  var %acc
        \\  %i := 0
        \\  %acc := 0
        \\:loop
        \\  jge %i %y :enda
        \\:run
        \\  %acc := add %acc %i
        \\  %i := add %i 1
        \\  jmp :loop
        \\:enda
        \\  ret %acc
        \\end
    );
    defer cfo.deinit();

    const fun = cfo.get_ptr(0, AFunc);
    try expect(usize, 0, fun(0));
    try expect(usize, 0, fun(1));
    try expect(usize, 10, fun(5));
    try expect(usize, 45, fun(10));
    try expect(usize, 1, fun(2));
    try expect(usize, 3, fun(3));
}

test "equality" {
    var cfo = try parse_test(
        \\func returner
        \\  %x = arg
        \\  %y = arg
        \\  var %res
        \\  jne %x %y :noteq
        \\:eq
        \\  %res := 1
        \\  jmp :enda
        \\:noteq
        \\  %res := 0
        \\:enda
        \\  ret %res
        \\end
    );
    defer cfo.deinit();

    const fun = cfo.get_ptr(0, BFunc);
    try expect(usize, 0, fun(2, 4));
    try expect(usize, 1, fun(3, 3));
    try expect(usize, 0, fun(4, 2));
    try expect(usize, 1, fun(3, 3));
}

test "diamond cfg" {
    var self = try FLIR.init(8, test_allocator);
    defer self.deinit();

    const start = try self.addNode();
    const arg1 = try self.arg();
    const arg2 = try self.arg();
    const v = try self.variable();

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

    try self.test_analysis(FLIR.X86_64ABI, true);
}

test "maybe_split" {
    var self = try FLIR.init(4, test_allocator);
    try parse(&self,
        \\func returner
        \\  %x = arg
        \\  %c = 1
        \\  %y = add %x %c
        \\  ret %y
        \\end
    );
    defer self.deinit();

    const pos = 1; // TODO: get("%c")
    const new_pos = try self.maybe_split(pos);
    try std.testing.expectEqual(self.iref(new_pos).?.tag, .empty);
    try self.test_analysis(FLIR.X86_64ABI, true);
    var cfo = try CFO.init(test_allocator);
    defer cfo.deinit();
    _ = try codegen.codegen(&self, &cfo, false);
    try cfo.finalize();
    const fun = cfo.get_ptr(0, AFunc);
    try expect(usize, 12, fun(11));
}

test "syscall" {
    var cfo = try parse_test(
        \\func returner
        \\  %x = arg
        \\  %y = syscall exit %x
        \\  ret %y
        \\end
    );

    defer cfo.deinit();
    const fun = cfo.get_ptr(0, AFunc);
    const pid = try os.fork();
    if (pid > 0) {
        const status = os.waitpid(pid, 0);
        try expect(usize, 11 * 256, status.status);
    } else {
        _ = fun(11);
        @panic("exit syscall failed");
    }
}

test "bander" {
    var cfo = try parse_test(
        \\func returner
        \\  %x = arg
        \\  %y = arg
        \\  %z = and %x %y
        \\  ret %z
        \\end
    );
    defer cfo.deinit();

    const fun = cfo.get_ptr(0, BFunc);
    try expect(usize, 4, fun(5, 6));
    try expect(usize, 2, fun(10, 34));
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

test "store byte" {
    // TODO: lol obviously support this without %y arg as well
    var cfo = try parse_test(
        \\func storer
        \\  %x = arg
        \\  %y = arg
        \\  %z = arg
        \\  store byte [%x %y] %z
        \\  ret 0
        \\end
    );
    defer cfo.deinit();

    const FFunc = *const fn (arg1: [*]u8, arg2: usize, val: usize) callconv(.C) usize;
    const fun = cfo.get_ptr(0, FFunc);
    var bytes: [4]u8 = .{ 17, 43, 6, 19 };
    try expect(usize, 0, fun(&bytes, 1, 4));
    try expect([4]u8, .{ 17, 4, 6, 19 }, bytes);
}

test "shift hl" {
    var cfo = try parse_test(
        \\func shifter
        \\  %x = arg
        \\  %z = shl %x 2
        \\  ret %z
        \\end
    );
    defer cfo.deinit();

    const fun = cfo.get_ptr(0, AIFunc);
    try expect(isize, 24, fun(6));
    try expect(isize, -12, fun(-3));
}

test "shift ar" {
    var cfo = try parse_test(
        \\func shifter
        \\  %x = arg
        \\  %z = sar %x 3
        \\  ret %z
        \\end
    );
    defer cfo.deinit();

    const fun = cfo.get_ptr(0, AIFunc);
    try expect(isize, 9, fun(75));
    try expect(isize, -1, fun(-3));
}

test "shift hr" {
    var cfo = try parse_test(
        \\func shifter
        \\  %x = arg
        \\  %z = shr %x 3
        \\  ret %z
        \\end
    );
    defer cfo.deinit();

    const fun = cfo.get_ptr(0, AIFunc);
    try expect(isize, 9, fun(75));
    try expect(isize, 0x1fffffffffffffff, fun(-3));
}

test "shift variable" {
    var cfo = try parse_test(
        \\func shifter
        \\  %x = arg
        \\  %y = arg
        \\  %z = shl %x %y
        \\  ret %z
        \\end
    );
    defer cfo.deinit();

    const fun = cfo.get_ptr(0, BFunc);
    try expect(usize, 6, fun(6, 0));
    try expect(usize, 10, fun(5, 1));
    try expect(usize, 24, fun(6, 2));
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
    defer res.deinit();

    const fun1 = try res.get_ptr("adder", BFunc);
    const fun2 = try res.get_ptr("multiplier", BFunc);
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
    defer res.deinit();

    const fun1 = try res.get_ptr("kuben", AFunc);
    try expect(usize, 1000000, fun1(100));

    const fun2 = try res.get_ptr("twokube", BFunc);
    try expect(usize, 1008, fun2(2, 10));
}

test "swap simple" {
    var res = try parse_multi_dbg(
        \\func diff
        \\  %x = arg
        \\  %y = arg
        \\  %d = sub %x %y
        \\  ret %d
        \\end
        \\
        \\func antidiff
        \\  %x = arg
        \\  %y = arg
        \\  %ad = call diff %y %x
        \\  ret %ad
        \\end
    );
    defer res.deinit();

    const fun1 = try res.get_ptr("antidiff", BFunc);
    try expect(usize, 30, fun1(70, 100));

    const fun2 = try res.get_ptr("diff", BFunc);
    try expect(usize, 40, fun2(50, 10));
}
