str: []const u8,
pos: usize = 0,

const FLIR = @import("./FLIR.zig");
const CFO = @import("./CFO.zig");
const Self = @This();
const std = @import("std");
const print = std.debug.print;
const mem = std.mem;
const ParseError = error{ ParseError, OutOfMemory, FLIRError };
const Cond = CFO.Cond;
const meta = std.meta;

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
    defer func.refs.deinit();
    defer func.labels.deinit();

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

const jmpmap = std.ComptimeStringMap(Cond, .{
    .{ "je", .e },
    .{ "jne", .ne },
    .{ "jl", .l },
    .{ "jge", .nl },
    .{ "ja", .a },
    .{ "jna", .na },
});

const alumap = std.ComptimeStringMap(CFO.AOp, .{
    .{ "add", .add },
    .{ "sub", .sub },
    .{ "and", .band },
    .{ "or", .bor },
    .{ "xor", .xor },
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
            f.ir.n.items[f.curnode].s[0] = try get_label(f, target, true);
            return true;
        } else if (jmpmap.get(kw)) |cond| {
            const dest = try require(try self.call_arg(f), "dest");
            const src = try require(try self.call_arg(f), "src");
            const target = try require(try self.labelname(), "target");
            _ = try f.ir.icmp(f.curnode, cond, dest, src);

            // TODO: mark current node as DED, need either a new node or an unconditional jump
            f.ir.n.items[f.curnode].s[1] = try get_label(f, target, true);
            return true;
        } else if (mem.eql(u8, kw, "vstore")) { // TODO: NIIN
            try self.expect_char('[');
            const dest = try require(try self.call_arg(f), "destination");
            const idx = try require(try self.call_arg(f), "idx");
            try self.expect_char(']');
            const value = try require(try self.call_arg(f), "value");
            _ = try f.ir.store(f.curnode, dest, idx, value);
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
        } else if (mem.eql(u8, kw, "load")) {
            const sizename = try require(self.keyword(), "size");
            const size = meta.stringToEnum(CFO.ISize, sizename) orelse {
                print("NÄ NU: '{s}'\n", .{sizename});
                return error.ParseError;
            };
            try self.expect_char('[');
            const base = try require(try self.call_arg(f), "base");
            const idx = try require(try self.call_arg(f), "idx");
            try self.expect_char(']');
            return f.ir.load(f.curnode, size, base, idx);
        } else if (mem.eql(u8, kw, "vload")) {
            const name = try require(self.keyword(), "fmode");
            const fmode = meta.stringToEnum(CFO.FMode, name) orelse {
                print("eioouuu: '{s}'\n", .{name});
                return error.ParseError;
            };
            // TODO: absract away EAddr properly
            const base = try require(try self.call_arg(f), "base");
            const idx = try require(try self.call_arg(f), "idx");
            return f.ir.vload(f.curnode, fmode, base, idx);
        } else if (alumap.get(kw)) |op| {
            const left = try require(try self.call_arg(f), "left");
            const right = try require(try self.call_arg(f), "right");
            return f.ir.iop(f.curnode, op, left, right);
        } else if (mem.eql(u8, kw, "mul")) {
            const left = try require(try self.call_arg(f), "left");
            const right = try require(try self.call_arg(f), "right");
            return f.ir.imul(f.curnode, left, right);
        } else if (mem.eql(u8, kw, "vop")) {
            // TODO: make this optional, if both op1/op2 share a fmode
            const modename = try require(self.keyword(), "fmode");
            const fmode = meta.stringToEnum(CFO.FMode, modename) orelse {
                print("eioouuu: '{s}'\n", .{modename});
                return error.ParseError;
            };
            const opname = try require(self.keyword(), "vop");
            const op = meta.stringToEnum(CFO.VMathOp, opname) orelse {
                print("aeue-r: '{s}'\n", .{opname});
                return error.ParseError;
            };

            const left = try require(try self.call_arg(f), "left");
            const right = try require(try self.call_arg(f), "right");
            return f.ir.vmath(f.curnode, op, fmode, left, right);
        } else if (mem.eql(u8, kw, "syscall")) {
            const name = try require(self.keyword(), "name");
            // TODO: non-native for
            const syscall = meta.stringToEnum(std.os.linux.SYS, name) orelse {
                print("unknown syscall: '{s}'\n", .{name});
                return error.ParseError;
            };
            var argno: u8 = 0;
            // TODO: call_arg could be something more complex later,
            // to simplify analyis we want all .callarg insn
            // in a row before the .call
            while (try self.call_arg(f)) |arg| {
                try f.ir.callarg(f.curnode, argno, arg);
                argno += 1;
            }
            return try f.ir.call(f.curnode, @intCast(u16, @enumToInt(syscall)));
        } else if (mem.eql(u8, kw, "alloc")) {
            unreachable;
            // return f.ir.alloc(f.curnode);
        }
    }
    return error.ParseError;
}
