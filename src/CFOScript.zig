// CFOScript. Not a "real" language, more like a high-level script for
// constructing IR from a structured form for the purpose of testing FLIR.
// No "semantics" are being done here and type errors will be caught (or
// brilliantly ignored) by FLIR analyis later.

const std = @import("std");
const Allocator = std.mem.Allocator;
const FLIR = @import("./FLIR.zig");
const print = std.debug.print;
const mem = std.mem;
const Tokenizer = @import("./Tokenizer.zig");

const nonexisting = @import("./Parser.zig").nonexisting;

const Self = @This();
const ParseError = error{ ParseError, SyntaxError, OutOfMemory, FLIRError, UndefinedName, TooManyArgs };

ir: *FLIR,
// tmp: [10]?u16 = .{null} ** 10,
tmp: [10]?u16 = ([1]?u16{null}) ** 10,
curnode: u16,
curloop: u16 = 0,

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
        '(' => {
            self.t.pos += 1;
            const val = try self.arg_expr();
            try self.t.expect_char(')');
            return val;
        },
        else => return null,
    }
}

pub fn expr_1(self: *Self) !?u16 {
    var val = (try self.expr_0()) orelse return null;
    while (self.t.nonws()) |char| {
        if (char == '[') {
            self.t.pos += 1;
            // TODO: addr[] as shorthand? or maybe [addr] :)
            const addr = try self.arg_expr();
            try self.t.expect_char(']');
            val = try self.ir.load(self.curnode, .{ .intptr = .byte }, val, addr, 0);
        } else {
            break;
        }
    }
    return val;
}

pub fn expr_2(self: *Self) !?u16 {
    var val = (try self.expr_1()) orelse return null;
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

pub fn expr_3(self: *Self) !?u16 {
    var val = (try self.expr_2()) orelse return null;
    while (self.t.nonws()) |char| {
        if (self.t.peek_operator()) |op| {
            // tricky: don't confuse  with |>
            if (op.len > 1) break;
        }

        const theop: FLIR.IntBinOp = switch (char) {
            '+' => .add,
            '-' => .sub,
            '|' => .@"or",
            else => break,
        };
        self.t.pos += 1;
        const op = (try self.expr_1()) orelse return error.SyntaxError;
        val = try self.ir.ibinop(self.curnode, theop, val, op);
    }
    return val;
}

// BULL: not even used for args..
pub fn arg_expr(self: *Self) ParseError!u16 {
    return (try self.expr_3()) orelse return error.SyntaxError;
}

pub fn call_expr(self: *Self) !u16 {
    if (try self.t.prefixed('$')) |name| {
        // TODO: non-native for
        const syscall = std.meta.stringToEnum(std.os.linux.SYS, name) orelse {
            print("unknown syscall: '{s}'\n", .{name});
            return error.ParseError;
        };
        const sysnum = try self.ir.const_int(@intCast(@intFromEnum(syscall)));
        try self.t.expect_char('(');

        var args = [_]u16{0} ** 6;
        var n_arg: u8 = 0;
        while (true) {
            const arg = (try self.expr_3()) orelse break;
            if (n_arg == args.len) {
                return error.TooManyArgs;
            }
            args[n_arg] = arg;
            n_arg += 1;
            if (self.t.nonws() != ',') break;
            self.t.pos += 1;
        }

        try self.t.expect_char(')');

        for (0.., args[0..n_arg]) |i, arg| {
            try self.ir.callarg(self.curnode, @intCast(i), arg);
        }
        return self.ir.call(self.curnode, .syscall, sysnum);
    }
    return self.arg_expr();
}

pub fn cond_op(op: []const u8) ?FLIR.IntCond {
    if (op.len == 1) {
        return switch (op[0]) {
            '<' => .lt,
            '>' => .gt,
            else => null,
        };
    } else if (mem.eql(u8, op, "==")) {
        return .eq;
    } else if (mem.eql(u8, op, ">=")) {
        return .ge;
    } else if (mem.eql(u8, op, "<=")) {
        return .le;
    } else if (mem.eql(u8, op, "!=")) {
        return .ge;
        // unsigned variants (as a treat™)
    } else if (mem.eql(u8, op, "|>")) {
        return .a;
    } else if (mem.eql(u8, op, "|>=")) {
        return .nb;
    } else if (mem.eql(u8, op, "<|")) {
        return .b;
    } else if (mem.eql(u8, op, "<|=")) {
        return .na;
    } else {
        return null;
    }
}

// currently not integrated with expr ( "let a = b < c" no good)
pub fn cond_expr(self: *Self) !u16 {
    const left = try self.arg_expr();
    const op_str = self.t.operator() orelse return error.SyntaxError;
    const op = cond_op(op_str) orelse return error.SyntaxError;
    const right = try self.arg_expr();

    return self.ir.icmp(self.curnode, op, left, right);
}

pub fn braced_block(self: *Self) !?bool {
    if (self.t.nonws() != '{') {
        return null;
    }
    self.t.pos += 1;
    try self.t.lbrk();

    const did_ret = try self.scoped_block();

    try self.t.expect_char('}');
    return did_ret;
}

pub fn loop(self: *Self) !void {
    const entry = try self.ir.addNodeAfter(self.curnode);
    const exit = try self.ir.addNode();

    const save_curloop = self.curloop;
    self.curnode = entry;
    self.curloop = exit;

    _ = try self.braced_block();
    try self.t.lbrk();

    try self.ir.addLink(self.curnode, 0, entry);
    self.curnode = exit;

    self.curloop = save_curloop;
}

pub fn break_stmt(self: *Self, branch: u1) !void {
    if (self.curloop == 0) return error.SyntaxError;
    try self.t.expect_char(';');
    try self.t.lbrk();
    try self.ir.addLink(self.curnode, branch, self.curloop);
}

pub fn if_stmt(self: *Self) !void {
    try self.t.expect_char('(');
    _ = try self.cond_expr();
    try self.t.expect_char(')');
    const prev_node = self.curnode;
    const other = try self.ir.addNodeAfter(prev_node);
    if (self.t.keyword()) |kw2| {
        if (mem.eql(u8, kw2, "break")) {
            try self.break_stmt(1);
            self.curnode = other;
        } else {
            return error.SyntaxError;
        }
    } else {
        const then = try self.ir.addNode();
        self.curnode = then;
        try self.ir.addLink(prev_node, 1, then);
        _ = (try self.braced_block()) orelse return error.SyntaxError;
        if (self.t.keyword()) |kw| {
            if (mem.eql(u8, kw, "else")) {
                // TODO: support if (foo) { stuff; break;} else {bar;} even tho it is technically redundant
                const after = try self.ir.addNodeAfter(self.curnode);
                self.curnode = other;
                _ = (try self.braced_block()) orelse return error.SyntaxError;
                try self.t.lbrk();
                if (self.curnode != FLIR.NoRef) {
                    try self.ir.addLink(self.curnode, 0, after);
                }
                self.curnode = after;
            } else {
                return error.SyntaxError;
            }
        } else {
            try self.t.lbrk();
            if (self.curnode != FLIR.NoRef) {
                try self.ir.addLink(self.curnode, 0, other);
            }
            self.curnode = other;
        }
    }
    if (self.curnode == FLIR.NoRef) {
        return error.SyntaxError;
    }
}

pub fn stmt(self: *Self) !?bool {
    const kw = self.t.keyword() orelse {
        const ret = try self.braced_block();
        if (ret) |_| try self.t.lbrk();
        return ret;
    };
    var did_ret = false;
    if (mem.eql(u8, kw, "return")) {
        const res = try self.arg_expr();
        try self.ir.ret(self.curnode, res);
        did_ret = true;
    } else if (mem.eql(u8, kw, "let")) {
        const name = self.t.keyword() orelse return error.SyntaxError;
        const item = try nonexisting(&self.vars, name, "variable");
        try self.t.expect_char('=');
        const val = try self.call_expr();
        item.* = .{ .ref = val, .is_mut = false };
    } else if (mem.eql(u8, kw, "if")) {
        try self.if_stmt();
        return false;
    } else if (mem.eql(u8, kw, "break")) {
        try self.break_stmt(0);
        // TODO: crufty, really assumes this is right in an if-statement
        self.curnode = FLIR.NoRef;
        return false;
    } else if (mem.eql(u8, kw, "loop")) {
        try self.loop();
        return false;
    } else {
        // try var assignment
        const v = self.vars.get(kw) orelse return error.UndefinedName;
        if (!v.is_mut) {
            return error.SyntaxError;
        }
        // not sure how typed assigment would look, like "foo :2d= ree"
        try self.t.expect_char(':');
        try self.t.expect_char('=');
        const res = try self.call_expr();
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
    return did_ret;
}

pub fn parse(ir: *FLIR, t: *Tokenizer, allocator: Allocator) !void {
    const curnode = try ir.addNode();
    var self: Self = .{ .ir = ir, .t = t.*, .curnode = curnode, .vars = std.StringArrayHashMap(IdInfo).init(allocator) };
    defer self.vars.deinit();
    defer t.* = self.t;
    // TODO: use did_ret for something?
    _ = self.do_parse() catch |e| {
        self.t.fail_pos();
        return e;
    };
}
