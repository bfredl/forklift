str: []const u8,
pos: usize = 0,
lnum: u32 = 0,
lpos: usize = 0,

objs: std.StringHashMap(u32),
allocator: Allocator,

const FLIR = @import("./FLIR.zig");
const codegen = @import("./codegen.zig");
const CFO = @import("./CFO.zig");
const common = @import("./common.zig");
const Self = @This();
const std = @import("std");
const print = std.debug.print;
const mem = std.mem;
const ParseError = error{ ParseError, OutOfMemory, FLIRError };
const meta = std.meta;

const Allocator = mem.Allocator;

pub fn init(str: []const u8, allocator: Allocator) Self {
    return .{
        .str = str,
        .allocator = allocator,
        .objs = @TypeOf(init(str, allocator).objs).init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.objs.deinit();
}

// consumes self
pub fn to_map(self: Self) std.StringHashMap(u32) {
    return self.objs;
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
    var val = self.nonws() orelse return;
    while (true) {
        if (val == ';') {
            while (self.str[self.pos] != '\n') : (self.pos += 1) {
                if (self.pos >= self.str.len - 1) return;
            }
        } else {
            if (val != '\n') return error.ParseError;
        }
        self.pos += 1;
        self.lnum += 1;
        self.lpos = self.pos;

        val = self.nonws() orelse return;
        if (val != '\n' and val != ';') return;
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

pub fn parse(self: *Self, cfo: *CFO, dbg: bool) !void {
    // small init size on purpose: must allow reallocations in place
    var flir = try FLIR.init(4, self.allocator);
    defer flir.deinit();
    while (self.nonws()) |_| {
        const name = try self.parse_func(&flir);
        const item = try self.objs.getOrPut(name);
        if (item.found_existing) {
            print("duplicate function {s}!\n", .{name});
            return error.ParseError;
        }

        try flir.test_analysis(FLIR.X86_64ABI, true);
        if (dbg) flir.debug_print();
        item.value_ptr.* = try codegen.codegen(&flir, cfo, false);
        flir.reinit();
    }
}

pub fn parse_func(self: *Self, flir: *FLIR) ![]const u8 {
    const kw = self.keyword() orelse return error.ParseError;
    if (!mem.eql(u8, kw, "func")) {
        return error.ParseError;
    }
    const name = try require(self.keyword(), "name");
    try self.lbrk();

    var func: Func = .{
        .ir = flir,
        .refs = std.StringHashMap(u16).init(self.allocator),
        .labels = std.StringHashMap(u16).init(self.allocator),
    };
    defer func.refs.deinit();
    defer func.labels.deinit();

    func.curnode = try func.ir.addNode();
    while (true) {
        if (!try self.stmt(&func)) break;
        try self.lbrk();
    }
    try self.lbrk();
    return name;
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

const jmpmap = std.ComptimeStringMap(FLIR.IntCond, .{
    .{ "je", .eq },
    .{ "jne", .neq },
    .{ "jl", .lt },
    .{ "jge", .ge },
    .{ "ja", .a },
    .{ "jna", .na },
});

pub fn stmt(self: *Self, f: *Func) ParseError!bool {
    if (self.keyword()) |kw| {
        if (mem.eql(u8, kw, "end")) {
            return false;
        } else if (mem.eql(u8, kw, "ret")) {
            const retval = try require(try self.call_arg(f), "return value");
            try f.ir.ret(f.curnode, retval);
            return true;
        } else if (mem.eql(u8, kw, "var")) {
            const name = try self.varname() orelse return error.ParseError;
            const item = try nonexisting(&f.refs, name, "ref %");
            item.* = try f.ir.variable();
            return true;
        } else if (mem.eql(u8, kw, "jmp")) {
            const target = try require(try self.labelname(), "target");
            // TODO: mark current node as DED, need a new node
            // TODO: HUR STÅR DET TILL I PALLET? explicit syntax for evaluating lhs before rhs.* !
            const node = try get_label(f, target, true);
            f.ir.n.items[f.curnode].s[0] = node;
            return true;
        } else if (jmpmap.get(kw)) |cond| {
            const dest = try require(try self.call_arg(f), "dest");
            const src = try require(try self.call_arg(f), "src");
            const target = try require(try self.labelname(), "target");
            _ = try f.ir.icmp(f.curnode, cond, dest, src);

            // TODO: mark current node as DED, need either a new node or an unconditional jump
            const node = try get_label(f, target, true);
            f.ir.n.items[f.curnode].s[1] = node;
            return true;
        } else if (mem.eql(u8, kw, "store")) {
            const kind = try require(try self.typename(), "type");
            try self.expect_char('[');
            const dest = try require(try self.call_arg(f), "destination");
            const idx = try require(try self.call_arg(f), "idx");
            try self.expect_char(']');
            const value = try require(try self.call_arg(f), "value");
            const scale: u2 = if (kind == .avxval) 2 else 0;
            _ = try f.ir.store(f.curnode, kind, dest, idx, scale, value);
            return true;
        }
    } else if (try self.varname()) |dest| {
        const next = self.nonws();
        const is_var = (next == @as(u8, ':'));
        if (is_var) self.pos += 1;
        try self.expect_char('=');
        if (is_var) {
            const thevar = f.refs.get(dest) orelse {
                print("undefined var %{s}!\n", .{dest});
                return error.ParseError;
            };
            const val = try self.expr(f);
            try f.ir.putvar(f.curnode, thevar, val);
        } else {
            const item = try nonexisting(&f.refs, dest, "ref %");
            item.* = try self.expr(f);
        }
        return true;
    } else if (self.nonws() == @as(u8, ':')) {
        self.pos += 1;
        var nextnode: u16 = next: {
            if (idlike(self.nonws() orelse return error.ParseError)) {
                const label = try self.identifier();
                const item = try f.labels.getOrPut(label);
                if (item.found_existing) {
                    if (!f.ir.empty(item.value_ptr.*, false)) {
                        print("duplicate label :{s}!\n", .{label});
                        return error.ParseError;
                    }
                } else {
                    item.value_ptr.* = try f.ir.addNode();
                }
                break :next item.value_ptr.*;
            } else {
                break :next try f.ir.addNode();
            }
        };

        if (f.ir.n.items[f.curnode].s[0] == 0) {
            f.ir.n.items[f.curnode].s[0] = nextnode;
        }
        f.curnode = nextnode;
        return true;
    }
    return error.ParseError;
}

pub fn call_arg(self: *Self, f: *Func) ParseError!?u16 {
    if (self.num()) |numval| {
        return try f.ir.const_int(@intCast(numval));
    } else if (try self.varname()) |src| {
        return f.refs.get(src) orelse {
            print("undefined ref %{s}!\n", .{src});
            return error.ParseError;
        };
    }
    return null;
}

pub fn typename(self: *Self) ParseError!?FLIR.SpecType {
    const kw = self.keyword() orelse return null;
    if (meta.stringToEnum(common.ISize, kw)) |size| {
        return .{ .intptr = size };
    } else if (meta.stringToEnum(CFO.FMode, kw)) |mode| {
        return .{ .avxval = mode };
    } else {
        return error.ParseError;
    }
}

pub fn expr(self: *Self, f: *Func) ParseError!u16 {
    if (try self.call_arg(f)) |arg| {
        return arg;
    } else if (self.keyword()) |kw| {
        if (mem.eql(u8, kw, "arg")) {
            return f.ir.arg();
        } else if (mem.eql(u8, kw, "load")) {
            const kind = try require(try self.typename(), "type");
            try self.expect_char('[');
            const base = try require(try self.call_arg(f), "base");
            const idx = try require(try self.call_arg(f), "idx");
            try self.expect_char(']');
            const scale: u2 = if (kind == .avxval) 2 else 0; // TODO UUUGH
            return f.ir.load(f.curnode, kind, base, idx, scale);
        } else if (meta.stringToEnum(FLIR.IntBinOp, kw)) |op| {
            const left = try require(try self.call_arg(f), "left");
            const right = try require(try self.call_arg(f), "right");
            return f.ir.ibinop(f.curnode, op, left, right);
        } else if (mem.eql(u8, kw, "vop")) {
            // TODO: make this optional, if both op1/op2 share a fmode
            const modename = try require(self.keyword(), "fmode");
            const fmode = meta.stringToEnum(CFO.FMode, modename) orelse {
                print("eioouuu: '{s}'\n", .{modename});
                return error.ParseError;
            };
            const opname = try require(self.keyword(), "vop");
            const mathop = meta.stringToEnum(CFO.VMathOp, opname);
            const cmpop = if (mathop == null) meta.stringToEnum(CFO.VCmpOp, opname) else null;
            if (mathop == null and cmpop == null) {
                print("aeue-r: '{s}'\n", .{opname});
                return error.ParseError;
            }

            const left = try require(try self.call_arg(f), "left");
            const right = try require(try self.call_arg(f), "right");
            if (mathop) |op| {
                return f.ir.vmath(f.curnode, op, fmode, left, right);
            } else if (cmpop) |op| {
                return f.ir.vcmpf(f.curnode, op, fmode, left, right);
            } else {
                unreachable;
            }
        } else if (mem.eql(u8, kw, "syscall")) {
            const name = try require(self.keyword(), "name");
            // TODO: non-native for
            const syscall = meta.stringToEnum(std.os.linux.SYS, name) orelse {
                print("unknown syscall: '{s}'\n", .{name});
                return error.ParseError;
            };
            const sysnum = try f.ir.const_int(@intCast(@intFromEnum(syscall)));

            try self.parse_args(f);

            return try f.ir.call(f.curnode, .syscall, sysnum);
        } else if (mem.eql(u8, kw, "call")) {
            const name = try require(self.keyword(), "name");
            const off = self.objs.get(name) orelse return error.ParseError;

            const constoff = try f.ir.const_uint(off);
            try self.parse_args(f);
            return try f.ir.call(f.curnode, .near, constoff);
        } else if (mem.eql(u8, kw, "alloc")) {
            const size = self.num() orelse 1;
            return f.ir.alloc(f.curnode, @intCast(size));
        }
        print("NIN: {s}\n", .{kw});
    }
    return error.ParseError;
}

pub fn parse_args(self: *Self, f: *Func) ParseError!void {
    var argno: u8 = 0;
    // TODO: call_arg could be something more complex later,
    // to simplify analyis we want all .callarg insn
    // in a row before the .call
    while (try self.call_arg(f)) |arg| {
        try f.ir.callarg(f.curnode, argno, arg);
        argno += 1;
    }
}
