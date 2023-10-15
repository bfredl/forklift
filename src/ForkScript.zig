const std = @import("std");
const Allocator = std.mem.Allocator;
const FLIR = @import("./FLIR.zig");
const print = std.debug.print;
const mem = std.mem;
const Tokenizer = @import("./Tokenizer.zig");

const Self = @This();

ir: *FLIR,
// tmp: [10]?u16 = .{null} ** 10,
tmp: [10]?u16 = ([1]?u16{null}) ** 10,
curnode: u16,

t: Tokenizer,

vars: std.StringHashMap(IdInfo),
const IdInfo = struct {
    ref: u16,
    is_mut: bool, // assignable
    // is_avx: bool, // true if avxval
};

pub fn expr_0(self: *Self) !?u16 {
    const char = self.t.nonws() orelse return null;
    switch (char) {
        'a'...'z', 'A'...'Z' => {
            const name = try self.t.identifier();
            const obj = self.vars.get(name) orelse return error.UndefinedName;
            return obj.ref;
        },
        '0'...'9' => {
            const i = self.t.num() orelse return error.InvalidSyntax;
            return try self.ir.const_int(@intCast(i));
        },
        else => return null,
    }
}

pub fn expr_1(self: *Self) !?u16 {
    var val = (try self.expr_0()) orelse return null;
    while (self.t.nonws()) |char| {
        const theop: FLIR.IntBinOp = switch (char) {
            '*' => .mul,
            '/' => @panic(".div"),
            else => return val,
        };
        self.t.pos += 1;
        const op = (try self.expr_0()) orelse return error.EXPR1;
        val = try self.ir.ibinop(self.curnode, theop, val, op);
    }
    return val;
}

pub fn expr_2(self: *Self) !?u16 {
    var val = (try self.expr_1()) orelse return null;
    while (self.t.nonws()) |char| {
        const theop: FLIR.IntBinOp = switch (char) {
            '+' => .add,
            '-' => .sub,
            else => return val,
        };
        self.t.pos += 1;
        const op = (try self.expr_1()) orelse return error.EXPR2;
        val = try self.ir.ibinop(self.curnode, theop, val, op);
    }
    return val;
}

pub fn expr(self: *Self) !u16 {
    return (try self.expr_2()) orelse return error.EOFError;
}

pub fn stmt(self: *Self) !?bool {
    const kw = self.t.keyword() orelse return null;
    var did_ret = false;
    if (mem.eql(u8, kw, "return")) {
        const res = try self.expr();
        try self.ir.ret(self.curnode, res);
        did_ret = true;
    } else if (mem.eql(u8, kw, "let")) {
        const name = self.t.keyword() orelse return error.ParseError;
        const item = try self.vars.getOrPut(name);
        if (item.found_existing) {
            print("duplicate function {s}!\n", .{name});
            return error.ParseError;
        }
        try self.t.expect_char('=');
        const val = try self.expr();
        item.value_ptr.* = .{ .ref = val, .is_mut = false };
    } else {
        return error.SyntaxError;
    }

    try self.t.expect_char(';');
    try self.t.lbrk();

    return did_ret;
}

fn do_parse(self: *Self) !bool {
    var didret = false;
    while (try self.stmt()) |res| {
        if (res) {
            didret = true;
            break;
        }
    }
    if (self.t.nonws() != null) return error.SyntaxError;
    return didret;
}

pub fn parse(ir: *FLIR, str: []const u8, allocator: Allocator) !bool {
    const curnode = try ir.addNode();
    var self: Self = .{ .ir = ir, .t = .{ .str = str }, .curnode = curnode, .vars = std.StringHashMap(IdInfo).init(allocator) };
    defer self.vars.deinit();
    return self.do_parse() catch |e| {
        self.t.fail_pos();
        return e;
    };
}
