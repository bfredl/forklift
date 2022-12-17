const FLIR = @import("./FLIR.zig");
const CFO = @import("./CFO.zig");
const print = std.debug.print;

const std = @import("std");
const os = std.os;

const IRParse = @import("./IRParse.zig");
const test_allocator = std.testing.allocator;

pub fn parse(ir: []const u8) !FLIR {
    var self = try FLIR.init(4, test_allocator);
    var parser = IRParse.init(ir);
    parser.parse_func(&self, test_allocator) catch |e| {
        print("fail at {}\n", .{parser.pos});
        return e;
    };
    return self;
}

pub fn analyze_generate(self: *FLIR) !CFO {
    try self.test_analysis(true);
    try self.scan_alloc();
    try self.check_cfg_valid();

    var cfo = try CFO.init(test_allocator);

    _ = try @import("./codegen.zig").codegen(self, &cfo);
    try cfo.finalize();
    return cfo;
}

pub fn parse_test(ir: []const u8) !CFO {
    var self = try parse(ir);
    defer self.deinit();
    return analyze_generate(&self);
}

pub fn expect(comptime T: type, x: T, y: T) !void {
    return std.testing.expectEqual(x, y);
}

const UFunc = *const fn () callconv(.C) usize;
const AFunc = *const fn (arg1: usize) callconv(.C) usize;
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

test "var returner" {
    var cfo = try parse_test(
        \\func returner
        \\  var %myvar
        \\  %myvar := 57
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

    // const const_0 = try self.const_int(start, 0);
    const const_42 = try self.const_int(start, 42);
    try self.putvar(start, v, const_42);
    _ = try self.icmp(start, .l, arg1, v);

    const left = try self.addNode();
    self.n.items[start].s[0] = left;
    const addl = try self.iop(left, .add, v, arg2);
    try self.putvar(left, v, addl);

    const right = try self.addNode();
    self.n.items[start].s[1] = right;
    const addr = try self.iop(right, .add, v, arg1);
    try self.putvar(right, v, addr);

    const end = try self.addNode();
    self.n.items[left].s[0] = end;
    self.n.items[right].s[0] = end;

    const const_77 = try self.const_int(end, 77);
    const adde = try self.iop(end, .add, v, const_77);
    try self.putvar(end, v, adde);

    try self.ret(end, v);

    try self.test_analysis(true);
    try self.scan_alloc();
    try self.check_cfg_valid();
}

test "maybe_split" {
    var self = try parse(
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
    var cfo = try analyze_generate(&self);
    defer cfo.deinit();
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
