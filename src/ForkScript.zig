const std = @import("std");
const Allocator = std.mem.Allocator;
const FLIR = @import("./FLIR.zig");
const print = std.debug.print;
const mem = std.mem;
const Tokenizer = @import("./Tokenizer.zig");

const nonexisting = @import("./Parser.zig").nonexisting;

const Self = @This();
const ParseError = error{ ParseError, SyntaxError, OutOfMemory, FLIRError, UndefinedName };

ir: *FLIR,
// tmp: [10]?u16 = .{null} ** 10,
tmp: [10]?u16 = ([1]?u16{null}) ** 10,
curnode: u16,

t: Tokenizer,

vars: std.StringArrayHashMap(IdInfo),
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
            const i = self.t.num() orelse return error.SyntaxError;
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
        const op = (try self.expr_0()) orelse return error.SyntaxError;
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
        const op = (try self.expr_1()) orelse return error.SyntaxError;
        val = try self.ir.ibinop(self.curnode, theop, val, op);
    }
    return val;
}

pub fn expr(self: *Self) !u16 {
    return (try self.expr_2()) orelse return error.SyntaxError;
}

pub fn stmt(self: *Self) !?bool {
    const kw = self.t.keyword() orelse {
        if (self.t.nonws()) |ch| {
            if (ch != '{') {
                return null;
            }
            self.t.pos += 1;
            try self.t.lbrk();

            const did_ret = try self.scoped_block();

            try self.t.expect_char('}');
            try self.t.lbrk();
            return did_ret;
        }
        return null;
    };
    var did_ret = false;
    if (mem.eql(u8, kw, "return")) {
        const res = try self.expr();
        try self.ir.ret(self.curnode, res);
        did_ret = true;
    } else if (mem.eql(u8, kw, "let")) {
        const name = self.t.keyword() orelse return error.SyntaxError;
        const item = try nonexisting(&self.vars, name, "variable");
        try self.t.expect_char('=');
        const val = try self.expr();
        item.* = .{ .ref = val, .is_mut = false };
    } else {
        // try var assignment
        const v = self.vars.get(kw) orelse return error.SyntaxError;
        if (!v.is_mut) {
            print("neee", .{});
            return error.SyntaxError;
        }
        // not sure how typed assigment would look, like "foo :2d= ree"
        try self.t.expect_char(':');
        try self.t.expect_char('=');
        const res = try self.expr();
        try self.ir.putvar(self.curnode, v.ref, res);
    }

    try self.t.expect_char(';');
    try self.t.lbrk();

    return did_ret;
}

fn block(self: *Self) !bool {
    while (try self.stmt()) |res| {
        if (res) {
            return true;
        }
    }
    return false;
}

fn scoped_block(self: *Self) ParseError!bool {
    const base_len = self.vars.count();
    const did_ret = self.block();

    var now_size = self.vars.count();
    // restore the environment
    while (now_size > base_len) {
        self.vars.swapRemoveAt(now_size - 1);
        now_size -= 1;
    }
    return did_ret;
}

fn do_parse(self: *Self) !bool {
    if (self.t.peek_keyword()) |kw| {
        if (mem.eql(u8, kw, "args")) {
            _ = self.t.keyword();
            while (self.t.keyword()) |name| {
                const item = try nonexisting(&self.vars, name, "argument");
                const val = try self.ir.arg();
                item.* = .{ .ref = val, .is_mut = false };
            }
            try self.t.expect_char(';');
            try self.t.lbrk();
        }
    }
    if (self.t.peek_keyword()) |kw| {
        if (mem.eql(u8, kw, "vars")) {
            _ = self.t.keyword();
            while (self.t.keyword()) |name| {
                const item = try nonexisting(&self.vars, name, "variable");
                const val = try self.ir.variable();
                item.* = .{ .ref = val, .is_mut = true };
            }
            try self.t.expect_char(';');
            try self.t.lbrk();
        }
    }
    const did_ret = self.block();
    if (self.t.nonws() != null) return error.SyntaxError;
    return did_ret;
}

pub fn parse(ir: *FLIR, str: []const u8, allocator: Allocator) !bool {
    const curnode = try ir.addNode();
    var self: Self = .{ .ir = ir, .t = .{ .str = str }, .curnode = curnode, .vars = std.StringArrayHashMap(IdInfo).init(allocator) };
    defer self.vars.deinit();
    return self.do_parse() catch |e| {
        self.t.fail_pos();
        return e;
    };
}
