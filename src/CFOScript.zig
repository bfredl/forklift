// CFOScript. Not a "real" language, more like a high-level script for
// constructing IR from a structured form for the purpose of testing FLIR.
// No "semantics" are being done here and type errors will be caught (or
// brilliantly ignored) by FLIR analyis later.

const std = @import("std");
const Allocator = std.mem.Allocator;
const FLIR = @import("./FLIR.zig");
const print = std.debug.print;
const mem = std.mem;
const CFOModule = @import("./CFOModule.zig");
const Tokenizer = @import("./Tokenizer.zig");
const X86Asm = @import("./X86Asm.zig");
const FMode = X86Asm.FMode;
const BPF = std.os.linux.BPF;
const meta = std.meta;
const options = defs.debug_options;
const defs = @import("./defs.zig");
const codegen = @import("./codegen.zig");
const codegen_bpf = @import("./codegen_bpf.zig").codegen;

const Self = @This();
const ParseError = error{ ParseError, SyntaxError, OutOfMemory, FLIRError, UndefinedName, TooManyArgs, TypeError, WIPError };
const SpecType = defs.SpecType;

// NOTE: currently "int_ctx" doesn't affect reads beyond upcast into quadword for further aritmethic
const int_ctx: SpecType = .{ .intptr = .quadword };

fn f_ctx(type_ctx: SpecType) ?FMode {
    return switch (type_ctx) {
        .intptr => null,
        .avxval => |fmode| fmode,
    };
}

mod: *CFOModule,
ir: *FLIR,
// tmp: [10]?u16 = .{null} ** 10,
tmp: [10]?u16 = ([1]?u16{null}) ** 10,
curnode: u16,
cur_loop: ?*const Loop = null,

t: Tokenizer,

vars: std.StringArrayHashMap(IdInfo),
const IdInfo = struct {
    ref: u16,
    is_mut: bool, // assignable
    // is_avx: bool, // true if avxval
};

const Loop = struct {
    entry: u16,
    exit: u16,
    // name: ?[]const u8,
    next: ?*const Loop,
};

fn require(val: anytype, what: []const u8) ParseError!@TypeOf(val.?) {
    return val orelse {
        print("missing {s}\n", .{what});
        return error.ParseError;
    };
}

pub fn nonexisting(map: anytype, key: []const u8, what: []const u8) ParseError!@TypeOf(map.getPtr(key).?) {
    const item = try map.getOrPut(key);
    if (item.found_existing) {
        print("duplicate {s} {s}!\n", .{ what, key });
        return error.ParseError;
    }
    // NON-EXIST-ENT!
    return item.value_ptr;
}

fn nonexisting_obj(mod: *CFOModule, key: []const u8) ParseError!*CFOModule.RTObject {
    const item = try mod.put_nonexisting(key);
    return item orelse {
        print("duplicate {s} {s}!\n", .{ "object", key });
        return error.ParseError;
    };
}

pub fn expr_0(self: *Self, type_ctx: SpecType) !?u16 {
    const char = self.t.nonws() orelse return null;
    switch (char) {
        'a'...'z', 'A'...'Z' => {
            const name = try self.t.identifier();
            const obj = self.vars.get(name) orelse {
                print("unknown var: {s}\n", .{name});
                return error.UndefinedName;
            };
            return obj.ref;
        },
        '0'...'9' => {
            const i = self.t.num() orelse return error.SyntaxError;
            return switch (type_ctx) {
                .avxval => |x| switch (x) {
                    .sd => try self.ir.const_f64(self.curnode, @floatFromInt(i)),
                    else => @panic("HAIII"),
                },
                .intptr => |_| try self.ir.const_uint(@intCast(i)),
            };
        },
        '\'' => {
            const val = try self.t.ascii_quote();
            return switch (type_ctx) {
                .avxval => |_| error.FLIRError, // or maybe, if it becomes simpler
                .intptr => |_| try self.ir.const_int(val),
            };
        },
        '(' => {
            self.t.pos += 1;
            const val = try self.arg_expr(type_ctx);
            try self.t.expect_char(')');
            return val;
        },
        '@' => {
            self.t.pos += 1;
            const memtype: SpecType = try self.maybe_type() orelse switch (type_ctx) {
                .intptr => .{ .intptr = .byte },
                .avxval => type_ctx,
            };

            const addr = try self.arg_expr(int_ctx);
            try self.t.expect_char('[');
            const idx = try self.arg_expr(int_ctx);
            const scale = try self.maybe_scale() orelse 0;
            try self.t.expect_char(']');

            return try self.ir.load(self.curnode, true, false, memtype, addr, idx, scale);
        },
        '~' => { // int to float (vector bcast??)
            self.t.pos += 1;
            const fmode = f_ctx(type_ctx) orelse return error.TypeError;
            const op = try self.expr_0(int_ctx) orelse return error.ParseError;
            return try self.ir.int2float(self.curnode, .convert, fmode, op);
        },
        '#' => { // float to int
            self.t.pos += 1;
            // TODO: bullshit, check if referenced var is sd or ss
            const fmode: FMode = .sd;
            const op = try self.expr_0(.{ .avxval = fmode }) orelse return error.ParseError;
            return try self.ir.float2int(self.curnode, .convert, fmode, op);
        },
        else => return null,
    }
}

pub fn expr_2(self: *Self, type_ctx: SpecType) !?u16 {
    var val = (try self.expr_0(type_ctx)) orelse return null;
    while (self.t.nonws()) |char| {
        if (f_ctx(type_ctx)) |fmode| {
            const theop: X86Asm.VMathOp = switch (char) {
                '*' => .mul,
                '/' => .div,
                else => break,
            };
            self.t.pos += 1;
            const op = (try self.expr_2(type_ctx)) orelse return error.SyntaxError;
            val = try self.ir.vmath(self.curnode, theop, fmode, val, op);
        } else {
            const opstr = self.t.peek_operator() orelse return val;

            const theop: defs.IntBinOp = if (opstr.len == 1)
                switch (opstr[0]) {
                    '*' => .mul,
                    '/' => @panic(".div"),
                    else => return val,
                }
            else if (mem.eql(u8, opstr, "<<"))
                .shl
            else if (mem.eql(u8, opstr, ">>"))
                .sar
            else if (mem.eql(u8, opstr, "|>>"))
                .shr
            else
                return val;

            self.t.pos += opstr.len;
            const op = (try self.expr_0(type_ctx)) orelse return error.SyntaxError;
            // TODO: int types other than qword is not really supported
            val = try self.ir.ibinop(self.curnode, type_ctx.intptr, theop, val, op);
        }
    }
    return val;
}

pub fn expr_3(self: *Self, type_ctx: SpecType) !?u16 {
    var val = (try self.expr_2(type_ctx)) orelse return null;
    while (self.t.nonws()) |char| {
        if (self.t.peek_operator()) |op| {
            // tricky: don't confuse  with |>
            if (op.len > 1) break;
        }

        if (f_ctx(type_ctx)) |fmode| {
            const theop: X86Asm.VMathOp = switch (char) {
                '+' => .add,
                '-' => .sub,
                else => break,
            };
            self.t.pos += 1;
            const op = (try self.expr_2(type_ctx)) orelse return error.SyntaxError;
            val = try self.ir.vmath(self.curnode, theop, fmode, val, op);
        } else {
            const theop: defs.IntBinOp = switch (char) {
                '+' => .add,
                '-' => .sub,
                '|' => .@"or",
                '&' => .@"and",
                else => break,
            };
            self.t.pos += 1;
            const op = (try self.expr_2(type_ctx)) orelse return error.SyntaxError;
            val = try self.ir.ibinop(self.curnode, type_ctx.intptr, theop, val, op);
        }
    }
    return val;
}

// BULL: not even used for args..
pub fn arg_expr(self: *Self, type_ctx: SpecType) ParseError!u16 {
    return (try self.expr_3(type_ctx)) orelse return error.SyntaxError;
}

pub fn expr_bpf_map(self: *Self, is_value: bool) !u16 {
    const name = self.t.keyword() orelse return error.ParseError;
    const id = self.mod.objs_map.get(name) orelse return error.UndefinedName;
    return self.ir.bpf_load_map(self.curnode, @intCast(id), is_value);
}

pub fn expr_toplevel(self: *Self, type_ctx: SpecType) !u16 {
    if (try self.t.prefixed('$')) |kind| {
        return self.call_expr(type_ctx, kind);
    } else if (try self.t.prefixed('%')) |kind| {
        if (mem.eql(u8, kind, "map")) {
            return self.expr_bpf_map(false);
        } else if (mem.eql(u8, kind, "map_value")) {
            return self.expr_bpf_map(true);
        } else if (mem.eql(u8, kind, "alloc")) {
            // not strictly, but make it clear it is not scoped
            if (self.curnode != 0) return error.ParseError;
            const size = self.t.num() orelse return error.ParseError;
            return self.ir.alloc(self.curnode, @intCast(size));
        } else {
            return error.ParseError;
        }
    } else {
        return self.arg_expr(type_ctx);
    }
}

fn requireEnumKey(comptime T: type, key: []const u8, klagel: []const u8) !T {
    return meta.stringToEnum(T, key) orelse {
        print("{s}: '{s}'\n", .{ klagel, key });
        return error.ParseError;
    };
}

pub fn call_expr(self: *Self, type_ctx: SpecType, kind: []const u8) !u16 {
    const calltype: defs.CallKind, const callwhat = target: {
        if (type_ctx != .intptr) return error.TypeError;

        if (mem.eql(u8, kind, "near")) {
            const name = self.t.keyword() orelse return error.ParseError;
            const idx = self.mod.lookup_obj(name) orelse return error.UndefinedName;
            const off = self.mod.get_func_off(idx) orelse return error.TypeError;
            break :target .{ .near, try self.ir.const_uint(off) };
        } else if (mem.eql(u8, kind, "sys")) {
            const name = self.t.keyword() orelse return error.ParseError;
            // TODO: non-native for
            const syscall = try requireEnumKey(std.os.linux.SYS, name, "unknown syscall");
            const sysnum = try self.ir.const_int(@intCast(@intFromEnum(syscall)));
            break :target .{ .syscall, sysnum };
        } else if (mem.eql(u8, kind, "bpf")) {
            const name = self.t.keyword() orelse return error.ParseError;
            const helper = try requireEnumKey(BPF.Helper, name, "unknown BPF helper");
            break :target .{ .bpf_helper, try self.ir.const_int(@intCast(@intFromEnum(helper))) };
        } else if (mem.eql(u8, kind, "memset")) {
            const idx = @intFromEnum(defs.MemoryIntrinsic.memset);
            break :target .{ .memory_intrinsic, try self.ir.const_int(@intCast(idx)) };
        } else {
            return error.ParseError;
        }
    };

    try self.t.expect_char('(');

    var args = [_]u16{0} ** 6;
    var n_arg: u8 = 0;
    while (true) {
        const arg = (try self.expr_3(int_ctx)) orelse break;
        if (n_arg == args.len) {
            return error.TooManyArgs;
        }
        args[n_arg] = arg;
        n_arg += 1;
        if (self.t.nonws() != ',') break;
        self.t.pos += 1;
    }

    try self.t.expect_char(')');

    const call = try self.ir.call(self.curnode, calltype, callwhat);
    var arglist = call;

    for (0.., args[0..n_arg]) |i, arg| {
        _ = i;
        arglist = try self.ir.callarg(self.curnode, arglist, arg, .{ .intptr = .quadword });
    }

    return self.ir.callret(call, .{ .intptr = .quadword });
}

pub fn cond_op(op: []const u8) ?defs.IntCond {
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
        return .neq;
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
pub fn cond_expr(self: *Self, typ: SpecType) !u16 {
    const left = try self.arg_expr(typ);
    const op_str = self.t.operator() orelse return error.SyntaxError;
    const op = cond_op(op_str) orelse return error.SyntaxError;
    const right = try self.arg_expr(typ);

    switch (typ) {
        .avxval => |fmode| return self.ir.fcmp(self.curnode, op, fmode, left, right),
        .intptr => return self.ir.icmp(self.curnode, .quadword, op, left, right),
    }
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
    const entry = try self.ir.addNodeAfter(self.curnode, false);
    const exit = try self.ir.addNode();

    self.curnode = entry;
    const this_loop: Loop = .{
        .entry = entry,
        .exit = exit,
        .next = self.cur_loop,
    };
    self.cur_loop = &this_loop;
    defer self.cur_loop = this_loop.next;

    _ = try self.braced_block();
    try self.t.lbrk();

    try self.ir.addLink(self.curnode, 0, entry, false);
    self.curnode = exit;
}

pub fn break_stmt(self: *Self, branch: u1) !void {
    var step = self.t.num() orelse 1;
    try self.t.expect_char(';');
    try self.t.lbrk();
    if (step <= 0) return error.SyntaxError;
    var loopen = self.cur_loop;
    while (loopen) |l| {
        step -= 1;
        if (step == 0) {
            try self.ir.addLink(self.curnode, branch, l.exit, false);
            return;
        }
        loopen = l.next;
    }
    return error.SyntaxError;
}

pub fn if_stmt(self: *Self) !void {
    const typ = try self.maybe_type() orelse int_ctx;
    try self.t.expect_char('(');
    _ = try self.cond_expr(typ);
    try self.t.expect_char(')');
    const prev_node = self.curnode;
    const other = try self.ir.addNodeAfter(prev_node, false); // maybe sometimes?
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
        try self.ir.addLink(prev_node, 1, then, true);
        _ = (try self.braced_block()) orelse return error.SyntaxError;
        if (self.t.keyword()) |kw| {
            if (mem.eql(u8, kw, "else")) {
                // TODO: support if (foo) { stuff; break;} else {bar;} even tho it is technically redundant
                const after = try self.ir.addNodeAfter(self.curnode, false);
                self.curnode = other;
                _ = (try self.braced_block()) orelse return error.SyntaxError;
                try self.t.lbrk();
                if (self.curnode != FLIR.NoRef) {
                    try self.ir.addLink(self.curnode, 0, after, false);
                }
                self.curnode = after;
            } else {
                return error.SyntaxError;
            }
        } else {
            try self.t.lbrk();
            if (self.curnode != FLIR.NoRef) {
                try self.ir.addLink(self.curnode, 0, other, false);
            }
            self.curnode = other;
        }
    }
    if (self.curnode == FLIR.NoRef) {
        return error.SyntaxError;
    }
}

fn maybe_colon_type(self: *Self) !?SpecType {
    if (self.t.nonws() == ':') {
        self.t.pos += 1;
        // once there is a :, the type is mandatory
        const typ = try self.maybe_type() orelse return error.ParseError;
        return typ;
    }
    return null;
}

fn maybe_type(self: *Self) !?SpecType {
    const first = self.t.nonws() orelse return null;
    if (!(first >= '0' and first <= '9') or self.t.pos + 2 >= self.t.str.len) return null;
    const second = self.t.str[self.t.pos + 1];
    if (!(second >= 'a' and second <= 'z' and !Tokenizer.idlike(self.t.str[self.t.pos + 2]))) return null;
    self.t.pos += 2;

    if (first == '1' and second == 'd') return .{ .avxval = .sd };
    if (first == '4' and second == 'u') return .{ .intptr = .dword };
    return error.ParseError;
}

pub fn maybe_scale(self: *Self) !?u2 {
    const sep = self.t.nonws() orelse return null;
    if (sep != ',') return null;
    self.t.pos += 1;
    const val = self.t.nonws() orelse return error.ParseError;
    self.t.pos += 1;
    return switch (val) {
        '8' => 3,
        '4' => 2,
        '2' => 1,
        '1' => 0,
        else => error.ParseError,
    };
}

pub fn stmt(self: *Self) !?bool {
    const kw = self.t.keyword() orelse {
        const ret = try self.braced_block();
        if (ret) |_| try self.t.lbrk();
        return ret;
    };
    var did_ret = false;
    if (mem.eql(u8, kw, "return")) {
        const type_ctx = try self.maybe_colon_type() orelse int_ctx;
        const res = try self.arg_expr(type_ctx); // BUULLLLLLL
        // TODO: a common wrapper for "return exactly one value" :p
        try self.ir.ret(self.curnode);
        try self.ir.retval(self.curnode, type_ctx, res);
        did_ret = true;
    } else if (mem.eql(u8, kw, "let")) {
        const name = self.t.keyword() orelse return error.SyntaxError;
        const item = try nonexisting(&self.vars, name, "variable");
        const type_ctx = try self.maybe_colon_type() orelse int_ctx;
        try self.t.expect_char('=');
        const val = try self.expr_toplevel(type_ctx);
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
        // try var or array assignment
        const v = self.vars.get(kw) orelse {
            print("unknown name or keyword: {s}\n", .{kw});
            return error.UndefinedName;
        };

        // TODO: rather @arrtype data[...] exprtypeA= val ??
        if (self.t.nonws() == '[') {
            self.t.pos += 1;

            const idx = try self.arg_expr(int_ctx);
            const scale = try self.maybe_scale() orelse 0;
            try self.t.expect_char(']');

            // this is bit of a mess. once we support both 64 and 32 bit internal operations,
            // re-consider how we specify both memory size and integer expression context (see TODO above)
            const memtype = try self.maybe_type() orelse SpecType{ .intptr = .byte };
            const type_ctx: SpecType = switch (memtype) {
                .intptr => int_ctx,
                .avxval => memtype,
            };

            try self.t.expect_char('=');
            // ambigous if idx is evaluated before or after a call, avoid expr_toplevel with calls!
            const val = try self.arg_expr(type_ctx);

            _ = try self.ir.store(self.curnode, memtype, v.ref, idx, scale, val);
        } else {
            if (!v.is_mut) {
                return error.SyntaxError;
            }
            const typ = try self.maybe_colon_type();
            try self.t.expect_char('=');
            const res = try self.expr_toplevel(typ orelse int_ctx);
            try self.ir.putvar(self.curnode, v.ref, res);
        }
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
        self.vars.swapRemoveAt(now_size - 1); // NB: not a swap, as last element
        now_size -= 1;
    }
    return did_ret;
}

fn do_parse(self: *Self) !bool {
    try self.t.expect_char('(');
    while (self.t.keyword()) |name| {
        const item = try nonexisting(&self.vars, name, "argument");
        const val = try self.ir.arg(.{ .intptr = .quadword });
        item.* = .{ .ref = val, .is_mut = false };
        if (self.t.nonws() == ',') {
            self.t.pos += 1;
        } else {
            break;
        }
    }
    try self.t.expect_char(')');
    try self.t.expect_char('{');
    try self.t.lbrk();

    while (self.t.peek_keyword()) |kw| {
        if (mem.eql(u8, kw, "vars")) {
            _ = self.t.keyword();
            while (self.t.keyword()) |name| {
                const item = try nonexisting(&self.vars, name, "variable");
                const typ = try self.maybe_colon_type();

                const val = try self.ir.variable(typ orelse int_ctx, null);
                item.* = .{ .ref = val, .is_mut = true };

                if (self.t.nonws() == ',') {
                    self.t.pos += 1;
                } else {
                    break;
                }
            }
            try self.t.expect_char(';');
            try self.t.lbrk();
        } else {
            break;
        }
    }
    const did_ret = self.block();
    try self.t.expect_char('}');
    try self.t.lbrk();
    return did_ret;
}

pub fn parse_func(mod: *CFOModule, ir: *FLIR, t: *Tokenizer, allocator: Allocator) !void {
    const curnode = try ir.addNode();
    var self: Self = .{ .mod = mod, .ir = ir, .t = t.*, .curnode = curnode, .vars = .init(allocator) };
    defer self.vars.deinit();
    defer t.* = self.t;
    // TODO: use did_ret for something?
    _ = try self.do_parse();

    // TODO: pattern for "is debug mode". Although all of CFOScript is "debug mode" in some sense
    if (!FLIR.minimal) {
        ir.var_names.clearRetainingCapacity();
        try ir.var_names.appendNTimes(self.ir.gpa, null, ir.nvar);
        var iter = self.vars.iterator();
        while (iter.next()) |it| {
            const vref = it.value_ptr.ref;
            const vidx = ir.iref(vref).?.op1;
            if (vidx < ir.nvar) {
                ir.var_names.items[vidx] = it.key_ptr.*;
            }
        }
    }
}

pub fn parse_mod(mod: *CFOModule, allocator: Allocator, str: []const u8, dbg: bool, one: bool) !void {

    // small init size on purpose: must allow reallocations in place
    var ir = try FLIR.init(4, allocator);
    defer ir.deinit();

    var t = Tokenizer{ .str = str };
    errdefer t.fail_pos();

    while (t.nonws()) |_| {
        const kw = t.keyword() orelse return;
        if (mem.eql(u8, kw, "func")) {
            const name = try require(t.keyword(), "name");

            const obj_slot = try nonexisting_obj(mod, name);

            try parse_func(mod, &ir, &t, allocator);
            if (one) return;

            if (options.dbg_raw_ir or dbg) ir.debug_print();
            try ir.test_analysis(FLIR.X86ABI, true);
            if (options.dbg_analysed_ir) ir.debug_print();
            if (options.dbg_vregs) ir.print_intervals();

            const target = try codegen.codegen(&ir, mod, false, null);
            obj_slot.* = .{ .func = .{ .code_start = target } };
            ir.reinit();
        } else if (mem.eql(u8, kw, "bpf_func")) {
            const name = try require(t.keyword(), "name");
            const obj_slot = try nonexisting_obj(mod, name);
            try parse_func(mod, &ir, &t, allocator);

            try ir.test_analysis(FLIR.BPF_ABI, true);
            if (dbg) ir.debug_print();
            const offset = try codegen_bpf(&ir, mod);
            const len = mod.bpf_code.items.len - offset;

            obj_slot.* = .{ .bpf_prog = .{ .fd = -1, .code_start = @intCast(offset), .code_len = @intCast(len) } };
            ir.reinit();
        } else if (mem.eql(u8, kw, "bpf_map")) {
            const name = try require(t.keyword(), "name");
            const obj_slot = try nonexisting_obj(mod, name);
            const kind = try require(try t.prefixed('.'), "kind");
            const key_size = try require(t.num(), "key_size");
            const val_size = try require(t.num(), "val_size");
            const n_entries = try require(t.num(), "n_entries");
            // print("map '{s}' of kind {s}, key={}, val={}\n", .{ name, kind, key_size, val_size });
            const map_kind = meta.stringToEnum(BPF.MapType, kind) orelse {
                print("unknown map kind: '{s}'\n", .{kind});
                return error.ParseError;
            };
            obj_slot.* = .{ .bpf_map = .{ .fd = -1, .kind = map_kind, .key_size = @intCast(key_size), .val_size = @intCast(val_size), .n_entries = @intCast(n_entries) } };
            try t.lbrk();
        } else {
            return error.ParseError;
        }
    }
    if (one) {
        return error.ParseError;
    }
}
