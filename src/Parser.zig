t: Tokenizer,

objs: std.StringHashMap(u32),
allocator: Allocator,

// reset for each function; reused just to keep allocated buffers
ir: FLIR,

// tricky: ideally we want an unified module format, and an unified entry point
// BPF is an outlier as code is not a sequence of bytes, but maybe it
// doesn't matter. Because maybe a flirmodule should be able to
// consist of both native and bpf code???
bpf_module: ?*BPFModule,

const FLIR = @import("./FLIR.zig");
const codegen = @import("./codegen.zig");
const codegen_bpf = @import("./codegen_bpf.zig").codegen;
const CFO = @import("./CFO.zig");
const BPF = std.os.linux.BPF;
const common = @import("./common.zig");
const Self = @This();
const std = @import("std");
const print = std.debug.print;
const mem = std.mem;
const ParseError = error{ ParseError, OutOfMemory, FLIRError };
const meta = std.meta;

const Allocator = mem.Allocator;
const BPFModule = bpf_rt.BPFModule;
const bpf_rt = @import("./bpf_rt.zig");

const Tokenizer = @import("./Tokenizer.zig");

pub fn init(str: []const u8, allocator: Allocator) !Self {
    return .{
        .t = .{ .str = str },
        .allocator = allocator,
        .objs = std.StringHashMap(u32).init(allocator),
        .bpf_module = null,
        .ir = try FLIR.init(4, allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.objs.deinit();
    self.ir.deinit();
}

// consumes self
pub fn to_map(self: *Self) std.StringHashMap(u32) {
    self.ir.deinit(); // HaCKERY
    return self.objs;
}

const Chunk = Tokenizer.Chunk;

fn varname(self: *Self) ParseError!?Chunk {
    return self.t.prefixed('%');
}

fn labelname(self: *Self) ParseError!?Chunk {
    return self.t.prefixed(':');
}

fn objname(self: *Self) ParseError!?Chunk {
    return self.t.prefixed('$');
}

fn enumname(self: *Self) ParseError!?Chunk {
    return self.t.prefixed('.');
}

fn require(val: anytype, what: []const u8) ParseError!@TypeOf(val.?) {
    return val orelse {
        print("missing {s}\n", .{what});
        return error.ParseError;
    };
}

pub fn parse_one_func(self: *Self) ![]const u8 {
    const kw = self.t.keyword() orelse return error.ParseError;
    if (!mem.eql(u8, kw, "func")) {
        return error.ParseError;
    }

    const name = try require(self.t.keyword(), "name");
    try self.t.lbrk();
    try self.parse_func_body();
    return name;
}

pub fn parse(self: *Self, cfo: *CFO, dbg: bool) !void {
    while (self.t.nonws()) |_| {
        const kw = self.t.keyword() orelse return error.ParseError;
        if (!mem.eql(u8, kw, "func")) {
            return error.ParseError;
        }
        const name = try require(self.t.keyword(), "name");

        const item = try self.objs.getOrPut(name);
        if (item.found_existing) {
            print("duplicate function {s}!\n", .{name});
            return error.ParseError;
        }

        try self.t.lbrk();
        try self.parse_func_body();

        try self.ir.test_analysis(FLIR.X86_64ABI, true);
        if (dbg) self.ir.debug_print();
        item.value_ptr.* = try codegen.codegen(&self.ir, cfo, false);
        self.ir.reinit();
    }
}

pub fn parse_bpf(self: *Self, dbg: bool) !void {
    const mod = self.bpf_module orelse return error.WHAAAA;
    // small init size on purpose: must allow reallocations in place
    var flir = try FLIR.init(4, self.allocator);
    defer flir.deinit();
    while (self.t.nonws()) |_| {
        const kw = self.t.keyword() orelse return;
        if (mem.eql(u8, kw, "func")) {
            const name = try require(self.t.keyword(), "name");
            const obj_slot = try nonexisting(&mod.objs, name, "object");
            try self.t.lbrk();
            try self.parse_func_body();

            try self.ir.test_analysis(FLIR.BPF_ABI, true);
            if (dbg) flir.debug_print();
            const offset = try codegen_bpf(&self.ir, mod);
            const len = mod.bpf_code.items.len - offset;

            obj_slot.* = .{ .prog = .{ .fd = -1, .code_start = @intCast(offset), .code_len = @intCast(len) } };
            self.ir.reinit();
        } else if (mem.eql(u8, kw, "map")) {
            const name = try require(self.t.keyword(), "name");
            const obj_slot = try nonexisting(&mod.objs, name, "object");
            const kind = try require(try self.enumname(), "kind");
            const key_size = try require(self.t.num(), "key_size");
            const val_size = try require(self.t.num(), "val_size");
            const n_entries = try require(self.t.num(), "n_entries");
            // print("map '{s}' of kind {s}, key={}, val={}\n", .{ name, kind, key_size, val_size });
            const map_kind = meta.stringToEnum(BPF.MapType, kind) orelse {
                print("unknown map kind: '{s}'\n", .{kind});
                return error.ParseError;
            };
            obj_slot.* = .{ .map = .{ .fd = -1, .kind = map_kind, .key_size = key_size, .val_size = val_size, .n_entries = n_entries } };
            try self.t.lbrk();
        }
    }
}

pub fn parse_func_body(self: *Self) !void {
    var func: Func = .{
        .refs = std.StringHashMap(u16).init(self.allocator),
        .labels = std.StringHashMap(u16).init(self.allocator),
    };
    defer func.refs.deinit();
    defer func.labels.deinit();

    func.curnode = try self.ir.addNode();
    while (true) {
        if (!try self.stmt(&func)) break;
        try self.t.lbrk();
    }
    try self.t.lbrk();
}

const Func = struct {
    curnode: u16 = FLIR.NoRef,
    refs: std.StringHashMap(u16),
    labels: std.StringHashMap(u16),
};

pub fn nonexisting(map: anytype, key: []const u8, what: []const u8) ParseError!@TypeOf(map.getPtr(key).?) {
    const item = try map.getOrPut(key);
    if (item.found_existing) {
        print("duplicate {s}{s}!\n", .{ what, key });
        return error.ParseError;
    }
    // NON-EXIST-ENT!
    return item.value_ptr;
}

fn get_label(self: *Self, f: *Func, name: []const u8, allow_existing: bool) ParseError!u16 {
    const item = try f.labels.getOrPut(name);
    if (item.found_existing) {
        if (!allow_existing and !self.ir.empty(item.value_ptr.*, false)) {
            print("duplicate label :{s}!\n", .{name});
            return error.ParseError;
        }
    } else {
        item.value_ptr.* = try self.ir.addNode();
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
    const ir = &self.ir;
    if (self.t.keyword()) |kw| {
        if (mem.eql(u8, kw, "end")) {
            return false;
        } else if (mem.eql(u8, kw, "ret")) {
            const retval = try require(try self.call_arg(f), "return value");
            try ir.ret(f.curnode, retval);
            return true;
        } else if (mem.eql(u8, kw, "var")) {
            const name = try self.varname() orelse return error.ParseError;
            const item = try nonexisting(&f.refs, name, "ref %");
            item.* = try ir.variable();
            return true;
        } else if (mem.eql(u8, kw, "jmp")) {
            const target = try require(try self.labelname(), "target");
            // TODO: mark current node as DED, need a new node
            // TODO: HUR STÅR DET TILL I PALLET? explicit syntax for evaluating lhs before rhs.* !
            const node = try self.get_label(f, target, true);
            ir.n.items[f.curnode].s[0] = node;
            return true;
        } else if (jmpmap.get(kw)) |cond| {
            const dest = try require(try self.call_arg(f), "dest");
            const src = try require(try self.call_arg(f), "src");
            const target = try require(try self.labelname(), "target");
            _ = try ir.icmp(f.curnode, cond, dest, src);

            // TODO: mark current node as DED, need either a new node or an unconditional jump
            const node = try self.get_label(f, target, true);
            ir.n.items[f.curnode].s[1] = node;
            return true;
        } else if (mem.eql(u8, kw, "store")) {
            const kind = try require(try self.typename(), "type");
            try self.t.expect_char('[');
            const dest = try require(try self.call_arg(f), "destination");
            const idx = try require(try self.call_arg(f), "idx");
            try self.t.expect_char(']');
            const value = try require(try self.call_arg(f), "value");
            const scale: u2 = if (kind == .avxval) 2 else 0;
            _ = try ir.store(f.curnode, kind, dest, idx, scale, value);
            return true;
        }
    } else if (try self.varname()) |dest| {
        const next = self.t.nonws();
        const is_var = (next == @as(u8, ':'));
        if (is_var) self.t.pos += 1;
        try self.t.expect_char('=');
        if (is_var) {
            const thevar = f.refs.get(dest) orelse {
                print("undefined var %{s}!\n", .{dest});
                return error.ParseError;
            };
            const val = try self.expr(f);
            try ir.putvar(f.curnode, thevar, val);
        } else {
            const item = try nonexisting(&f.refs, dest, "ref %");
            item.* = try self.expr(f);
        }
        return true;
    } else if (self.t.nonws() == @as(u8, ':')) {
        self.t.pos += 1;
        var nextnode: u16 = next: {
            if (Tokenizer.idlike(self.t.nonws() orelse return error.ParseError)) {
                const label = try self.t.identifier();
                const item = try f.labels.getOrPut(label);
                if (item.found_existing) {
                    if (!ir.empty(item.value_ptr.*, false)) {
                        print("duplicate label :{s}!\n", .{label});
                        return error.ParseError;
                    }
                } else {
                    item.value_ptr.* = try ir.addNode();
                }
                break :next item.value_ptr.*;
            } else {
                break :next try ir.addNode();
            }
        };

        if (ir.n.items[f.curnode].s[0] == 0) {
            ir.n.items[f.curnode].s[0] = nextnode;
        }
        f.curnode = nextnode;
        return true;
    }
    return error.ParseError;
}

pub fn call_arg(self: *Self, f: *Func) ParseError!?u16 {
    if (self.t.num()) |numval| {
        return try self.ir.const_int(@intCast(numval));
    } else if (try self.varname()) |src| {
        return f.refs.get(src) orelse {
            print("undefined ref %{s}!\n", .{src});
            return error.ParseError;
        };
    }
    return null;
}

pub fn typename(self: *Self) ParseError!?FLIR.SpecType {
    const kw = self.t.keyword() orelse return null;
    if (meta.stringToEnum(common.ISize, kw)) |size| {
        return .{ .intptr = size };
    } else if (meta.stringToEnum(CFO.FMode, kw)) |mode| {
        return .{ .avxval = mode };
    } else {
        return error.ParseError;
    }
}

pub fn expr(self: *Self, f: *Func) ParseError!u16 {
    const ir = &self.ir;
    if (try self.call_arg(f)) |arg| {
        return arg;
    } else if (self.t.keyword()) |kw| {
        if (mem.eql(u8, kw, "arg")) {
            return ir.arg();
        } else if (mem.eql(u8, kw, "load")) {
            const kind = try require(try self.typename(), "type");
            try self.t.expect_char('[');
            const base = try require(try self.call_arg(f), "base");
            const idx = try require(try self.call_arg(f), "idx");
            try self.t.expect_char(']');
            const scale: u2 = if (kind == .avxval) 2 else 0; // TODO UUUGH
            return ir.load(f.curnode, kind, base, idx, scale);
        } else if (meta.stringToEnum(FLIR.IntBinOp, kw)) |op| {
            const left = try require(try self.call_arg(f), "left");
            const right = try require(try self.call_arg(f), "right");
            return ir.ibinop(f.curnode, op, left, right);
        } else if (mem.eql(u8, kw, "vop")) {
            // TODO: make this optional, if both op1/op2 share a fmode
            const modename = try require(self.t.keyword(), "fmode");
            const fmode = meta.stringToEnum(CFO.FMode, modename) orelse {
                print("eioouuu: '{s}'\n", .{modename});
                return error.ParseError;
            };
            const opname = try require(self.t.keyword(), "vop");
            const mathop = meta.stringToEnum(CFO.VMathOp, opname);
            const cmpop = if (mathop == null) meta.stringToEnum(CFO.VCmpOp, opname) else null;
            if (mathop == null and cmpop == null) {
                print("aeue-r: '{s}'\n", .{opname});
                return error.ParseError;
            }

            const left = try require(try self.call_arg(f), "left");
            const right = try require(try self.call_arg(f), "right");
            if (mathop) |op| {
                return ir.vmath(f.curnode, op, fmode, left, right);
            } else if (cmpop) |op| {
                return ir.vcmpf(f.curnode, op, fmode, left, right);
            } else {
                unreachable;
            }
        } else if (mem.eql(u8, kw, "syscall")) {
            const name = try require(self.t.keyword(), "name");
            // TODO: non-native for
            const syscall = meta.stringToEnum(std.os.linux.SYS, name) orelse {
                print("unknown syscall: '{s}'\n", .{name});
                return error.ParseError;
            };
            const sysnum = try ir.const_int(@intCast(@intFromEnum(syscall)));

            try self.parse_args(f);

            return try ir.call(f.curnode, .syscall, sysnum);
        } else if (mem.eql(u8, kw, "call")) {
            const name = try require(self.t.keyword(), "name");
            const off = self.objs.get(name) orelse return error.ParseError;

            const constoff = try ir.const_uint(off);
            try self.parse_args(f);
            return try ir.call(f.curnode, .near, constoff);
        } else if (mem.eql(u8, kw, "call_bpf")) {
            const name = try require(self.t.keyword(), "name");
            const helper = meta.stringToEnum(BPF.Helper, name) orelse {
                print("unknown BPF helper: '{s}'\n", .{name});
                return error.ParseError;
            };
            try self.parse_args(f);
            return try ir.call(f.curnode, .bpf_helper, @intCast(@intFromEnum(helper)));
        } else if (mem.eql(u8, kw, "alloc")) {
            const size = self.t.num() orelse 1;
            return ir.alloc(f.curnode, @intCast(size));
        } else if (mem.eql(u8, kw, "map")) {
            return self.get_bpf_map(false, f);
        } else if (mem.eql(u8, kw, "map_value")) {
            return self.get_bpf_map(true, f);
        }
        print("NIN: {s}\n", .{kw});
    }
    return error.ParseError;
}

fn get_bpf_map(self: *Self, is_value: bool, f: *Func) ParseError!u16 {
    const name = try require(self.t.keyword(), "map name");
    const mod = self.bpf_module orelse return error.ParseError;
    const id = mod.objs.getIndex(name) orelse return error.ParseError;
    return self.ir.bpf_load_map(f.curnode, @intCast(id), is_value);
}

pub fn parse_args(self: *Self, f: *Func) ParseError!void {
    var argno: u8 = 0;
    // TODO: call_arg could be something more complex later,
    // to simplify analyis we want all .callarg insn
    // in a row before the .call
    while (try self.call_arg(f)) |arg| {
        try self.ir.callarg(f.curnode, argno, arg);
        argno += 1;
    }
}
