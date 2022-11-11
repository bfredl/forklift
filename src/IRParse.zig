str: []const u8,
pos: usize = 0,

const FLIR = @import("./FLIR.zig");
const Self = @This();
const std = @import("std");
const print = std.debug.print;
const mem = std.mem;
const ParseError = error{ ParseError, OutOfMemory, FLIRError };

const Allocator = mem.Allocator;

pub fn init(str: []const u8) Self {
    return .{ .str = str };
}

fn nonws(self: *Self) ?u8 {
    while (self.pos < self.str.len) : (self.pos += 1) {
        if (self.str[self.pos] != ' ') {
            return self.str[self.pos];
        }
    }
    return null;
}

fn lbrk(self: *Self) ParseError!void {
    const val = self.nonws();
    if (val) |v| {
        if (v != '\n') return error.ParseError;
        self.pos += 1;
    }
}

fn idlike(c: u8) bool {
    return ('a' <= c and c <= 'z') or ('A' <= c and c <= 'Z') or ('0' < c and c < '9') or c == '_';
}

const Chunk = []const u8;
fn keyword(self: *Self) ?Chunk {
    const c = self.nonws() orelse return null;
    if (!('a' <= c and c <= 'z') and !('A' <= c and c <= 'Z')) return null;
    const start = self.pos;
    while (self.pos < self.str.len) : (self.pos += 1) {
        const next = self.str[self.pos];
        if (!idlike(next)) {
            break;
        }
    }
    return self.str[start..self.pos];
}

fn prefixed(self: *Self, sigil: u8) ParseError!?Chunk {
    if (self.nonws() != sigil) return null;
    self.pos += 1;
    return try self.identifier();
}

fn expect_char(self: *Self, char: u8) ParseError!void {
    if (self.nonws() == char) {
        self.pos += 1;
    } else {
        print("expected '{c}'\n", .{char});
        return error.ParseError;
    }
}

fn identifier(self: *Self) ParseError!Chunk {
    const start = self.pos;
    while (self.pos < self.str.len) : (self.pos += 1) {
        const next = self.str[self.pos];
        if (!idlike(next)) {
            break;
        }
    }
    if (self.pos == start) return error.ParseError;
    return self.str[start..self.pos];
}

fn varname(self: *Self) ParseError!?Chunk {
    return self.prefixed('%');
}

fn labelname(self: *Self) ParseError!?Chunk {
    return self.prefixed(':');
}

fn num(self: *Self) ?u32 {
    const first = self.nonws() orelse return null;
    if (!('0' <= first and first <= '9')) return null;
    var val: u32 = 0;
    while (self.pos < self.str.len) : (self.pos += 1) {
        const next = self.str[self.pos];
        if ('0' <= next and next <= '9') {
            val = val * 10 + (next - '0');
        } else {
            break;
        }
    }
    return val;
}

fn require(val: anytype, what: []const u8) ParseError!@TypeOf(val.?) {
    return val orelse {
        print("missing {s}\n", .{what});
        return error.ParseError;
    };
}

pub fn parse_func(self: *Self, flir: *FLIR, allocator: Allocator) !void {
    const kw = self.keyword() orelse return error.ParseError;
    if (!mem.eql(u8, kw, "func")) {
        return error.ParseError;
    }
    const name = try require(self.keyword(), "name");
    try self.lbrk();
    _ = name;

    var func: Func = .{
        .ir = flir,
        .refs = std.StringHashMap(u16).init(allocator),
        .labels = std.StringHashMap(u16).init(allocator),
    };

    func.curnode = try func.ir.addNode();
    while (true) {
        if (!try self.stmt(&func)) break;
        try self.lbrk();
    }
}

const Func = struct {
    ir: *FLIR,
    curnode: u16 = FLIR.NoRef,
    refs: std.StringHashMap(u16),
    labels: std.StringHashMap(u16),
};

fn nonexisting(map: anytype, key: []const u8, what: []const u8) ParseError!@TypeOf(map.getPtr(key).?) {
    const item = try map.getOrPut(key);
    if (item.found_existing) {
        print("duplicate {s}{s}!\n", .{ what, key });
        return error.ParseError;
    }
    return item.value_ptr;
}

fn get_label(f: *Func, name: []const u8, allow_existing: bool) ParseError!u16 {
    const item = try f.labels.getOrPut(name);
    if (item.found_existing) {
        if (!allow_existing and !f.ir.empty(item.value_ptr.*, false)) {
            print("duplicate label :{s}!\n", .{name});
            return error.ParseError;
        }
    } else {
        item.value_ptr.* = try f.ir.addNode();
    }
    return item.value_ptr.*;
}

pub fn stmt(self: *Self, f: *Func) ParseError!bool {
    if (self.keyword()) |kw| {
        if (mem.eql(u8, kw, "end")) {
            return false;
        } else if (mem.eql(u8, kw, "ret")) {
            const retval = try require(try self.call_arg(f), "return value");
            try f.ir.ret(f.curnode, retval);
            return true;
        } else if (mem.eql(u8, kw, "lessthan")) {
            const dest = try require(try self.call_arg(f), "dest");
            const src = try require(try self.call_arg(f), "src");
            const target = try require(try self.labelname(), "src");
            _ = try f.ir.binop(f.curnode, .ilessthan, dest, src);

            // TODO: mark current node as DED, need either a new node or an unconditional jump
            f.ir.n.items[f.curnode].s[1] = try get_label(f, target, true);
            return true;
        } else if (mem.eql(u8, kw, "store")) {
            try self.expect_char('[');
            const dest = try require(try self.call_arg(f), "destination");
            try self.expect_char(']');
            const value = try require(try self.call_arg(f), "value");
            _ = dest;
            _ = value;
            unreachable;
            //try f.ir.store(f.curnode, dest, value);
            //return true;
        }
    } else if (try self.varname()) |dest| {
        try self.expect_char('=');
        const item = try nonexisting(&f.refs, dest, "ref %");
        item.* = try self.expr(f);
        return true;
    } else if (try self.labelname()) |label| {
        const item = try f.labels.getOrPut(label);
        if (item.found_existing) {
            if (!f.ir.empty(item.value_ptr.*, false)) {
                print("duplicate label :{s}!\n", .{label});
                return error.ParseError;
            }
        } else {
            item.value_ptr.* = try f.ir.addNode();
        }

        if (f.ir.n.items[f.curnode].s[0] == 0) {
            f.ir.n.items[f.curnode].s[0] = item.value_ptr.*;
        }
        f.curnode = item.value_ptr.*;
        return true;
    }
    return error.ParseError;
}

pub fn call_arg(self: *Self, f: *Func) ParseError!?u16 {
    if (self.num()) |numval| {
        return try f.ir.const_int(f.curnode, @intCast(u16, numval));
    } else if (try self.varname()) |src| {
        return f.refs.get(src) orelse {
            print("undefined ref %{s}!\n", .{src});
            return error.ParseError;
        };
    }
    return null;
}

pub fn expr(self: *Self, f: *Func) ParseError!u16 {
    if (self.num()) |numval| {
        return f.ir.const_int(f.curnode, @intCast(u16, numval));
    } else if (self.keyword()) |kw| {
        if (mem.eql(u8, kw, "arg")) {
            return f.ir.arg();
        } else if (mem.eql(u8, kw, "alloc")) {
            unreachable;
            // return f.ir.alloc(f.curnode);
        }
    }
    return error.ParseError;
}
