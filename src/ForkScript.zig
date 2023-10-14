const std = @import("std");
const Allocator = std.mem.Allocator;
const test_allocator = std.testing.allocator;
const FLIR = @import("./FLIR.zig");
const CFO = @import("./CFO.zig");
const print = std.debug.print;

const Self = @This();

const Inst = FLIR.Inst;

ir: *FLIR,
// tmp: [10]?u16 = .{null} ** 10,
tmp: [10]?u16 = ([1]?u16{null}) ** 10,
curnode: u16,

str: []const u8,
pos: usize,

pub fn nonws(self: *Self) ?u8 {
    while (self.pos < self.str.len) : (self.pos += 1) {
        if (self.str[self.pos] != ' ' and self.str[self.pos] != '\n') {
            return self.str[self.pos];
        }
    }
    return null;
}

pub fn num(self: *Self) ?u4 {
    const c = self.nonws() orelse return null;
    if ('0' <= c and c < '9') {
        self.pos += 1;
        return @as(u4, @intCast(c - '0'));
    }
    return null;
}

pub fn idx(self: *Self) u8 {
    const c = self.nonws() orelse return 0;
    if (c == 'i') {
        self.pos += 1;
        return 0x10;
    }
    return 0;
}

pub fn expr_0(self: *Self) !?u16 {
    const char = self.nonws() orelse return null;
    switch (char) {
        'a'...'d' => {
            const arg = char - 'a';
            if (arg >= self.ir.narg) return error.InvalidSyntax;
            self.pos += 1;
            return arg;
        },
        't' => {
            self.pos += 1;
            const i = self.num() orelse return error.InvalidSyntax;
            return self.tmp[i] orelse return error.UndefTemporary;
        },
        'k' => {
            self.pos += 1;
            const i = self.num() orelse return error.InvalidSyntax;
            return try self.ir.const_int(i);
        },
        else => return null,
    }
}

pub fn expr_1(self: *Self) !?u16 {
    var val = (try self.expr_0()) orelse return null;
    while (self.nonws()) |char| {
        const theop: FLIR.IntBinOp = switch (char) {
            '*' => .mul,
            '/' => @panic(".div"),
            else => return val,
        };
        self.pos += 1;
        const op = (try self.expr_0()) orelse return error.EXPR1;
        val = try self.ir.ibinop(self.curnode, theop, val, op);
    }
    return val;
}

pub fn expr_2(self: *Self) !?u16 {
    var val = (try self.expr_1()) orelse return null;
    while (self.nonws()) |char| {
        const theop: FLIR.IntBinOp = switch (char) {
            '+' => .add,
            '-' => .sub,
            else => return val,
        };
        self.pos += 1;
        const op = (try self.expr_1()) orelse return error.EXPR2;
        val = try self.ir.ibinop(self.curnode, theop, val, op);
    }
    return val;
}

pub fn stmt(self: *Self) !?bool {
    const char = self.nonws() orelse return null;
    self.pos += 1;
    const extra: u8 =
        switch (char) {
        'r' => undefined,
        't' => self.num() orelse return error.SyntaxError,
        else => return error.SyntaxError,
    };

    if (self.nonws() != @as(u8, '=')) return error.SyntaxError;
    self.pos += 1;
    const res = (try self.expr_2()) orelse return error.EOFError;
    if (self.nonws() != @as(u8, ';')) return error.SyntaxError;
    self.pos += 1;

    switch (char) {
        'r' => {
            try self.ir.ret(self.curnode, res);
            return true;
        },
        // 'x'...'z' => .{ .tag = .store, .opspec = i + char - 'x', .op1 = res, .op2 = extra },
        't' => {
            self.tmp[extra] = res;
            return false;
        },
        else => return error.SyntaxError,
    }
}

pub fn parse(ir: *FLIR, str: []const u8) !bool {
    var didret = false;
    const curnode = try ir.addNode();
    var self: Self = .{ .ir = ir, .str = str, .pos = 0, .curnode = curnode };
    while (try self.stmt()) |res| {
        if (res) {
            didret = true;
            break;
        }
    }
    if (self.nonws() != null) return error.SyntaxError;
    return didret;
}
