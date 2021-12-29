const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const test_allocator = std.testing.allocator;
const FLIR = @import("./FLIR.zig");
const CFO = @import("./CFO.zig");
const print = std.debug.print;

const Self = @This();

flir: *FLIR,
tmp: [10]?u16 = .{null} ** 10,
str: []const u8,
pos: usize,

pub fn nonws(self: *Self) ?u8 {
    while (self.pos < self.str.len) : (self.pos += 1) {
        if (self.str[self.pos] != ' ') {
            return self.str[self.pos];
        }
    }
    return null;
}

pub fn num(self: *Self) ?u4 {
    const c = self.nonws() orelse return null;
    if ('0' <= c and c < '9') {
        self.pos += 1;
        return @intCast(u4, c - '0');
    }
    return null;
}

pub fn expr_0(self: *Self) !?u16 {
    const char = self.nonws() orelse return null;
    switch (char) {
        'a'...'d' => {
            const arg = char - 'a';
            if (arg >= self.flir.narg) return error.InvalidSyntax;
            self.pos += 1;
            return arg;
        },
        'x'...'z' => {
            const arg = char - 'x';
            self.pos += 1;
            const op1 = self.num() orelse 0;
            var inst: FLIR.Inst = .{ .tag = .load, .opspec = arg, .op1 = op1 };
            return try self.flir.put(inst);
        },
        't' => {
            self.pos += 1;
            const i = self.num() orelse return error.InvalidSyntax;
            return self.tmp[i] orelse return error.UndefTemporary;
        },
        else => return null,
    }
}

pub fn expr_1(self: *Self) !?u16 {
    var val = (try self.expr_0()) orelse return null;
    while (self.nonws()) |char| {
        const theop: CFO.VMathOp = switch (char) {
            '*' => .mul,
            '/' => .div,
            else => return val,
        };
        self.pos += 1;
        const op = (try self.expr_0()) orelse return error.EXPR1;
        val = try self.flir.put(.{ .tag = .vmath, .opspec = theop.off(), .op1 = val, .op2 = op });
    }
    return val;
}

pub fn expr_2(self: *Self) !?u16 {
    var val = (try self.expr_1()) orelse return null;
    while (self.nonws()) |char| {
        const theop: CFO.VMathOp = switch (char) {
            '+' => .add,
            '-' => .sub,
            else => return val,
        };
        self.pos += 1;
        const op = (try self.expr_1()) orelse return error.EXPR2;
        val = try self.flir.put(.{ .tag = .vmath, .opspec = theop.off(), .op1 = val, .op2 = op });
    }
    return val;
}

pub fn stmt(self: *Self) !?bool {
    const char = self.nonws() orelse return null;
    var inst: FLIR.Inst = dest: {
        switch (char) {
            'r' => {
                self.pos += 1;
                break :dest .{ .tag = .ret, .op1 = undefined };
            },
            'x'...'z' => {
                self.pos += 1;
                const arg = char - 'x';
                const c2 = self.nonws() orelse ' ';
                var op2: u16 = 0;
                if ('0' <= c2 and c2 < '9') {
                    op2 = c2 - '0';
                    _ = op2;
                    self.pos += 1;
                }
                break :dest .{ .tag = .store, .opspec = arg, .op1 = undefined, .op2 = op2 };
            },
            else => return error.SyntaxError,
        }
    };

    if (self.nonws() != @as(u8, '=')) return error.SyntaxError;
    self.pos += 1;
    inst.op1 = (try self.expr_2()) orelse return error.EOFError;
    if (self.nonws() != @as(u8, ';')) return error.SyntaxError;
    self.pos += 1;

    _ = try self.flir.put(inst);
    return (inst.tag == .ret);
}

pub fn parse(flir: *FLIR, str: []const u8) !bool {
    var didret = false;
    var self: Self = .{ .flir = flir, .str = str, .pos = 0 };
    while (try self.stmt()) |res| {
        if (res) {
            didret = true;
            break;
        }
    }
    if (self.nonws() != null) return error.SyntaxError;
    return didret;
}

pub fn main() !void {
    const arg1 = std.os.argv[1];
    var flir = try FLIR.init(4, test_allocator);
    defer flir.deinit();

    const ret = try parse(&flir, std.mem.span(arg1));
    if (!ret) print("gör du ens\n", .{});
    flir.live();
    try flir.scanreg();
    flir.debug_print();

    var cfo = try CFO.init(test_allocator);
    defer cfo.deinit();
    _ = try flir.codegen(&cfo);
    try cfo.dbg_nasm(test_allocator);
}
