// AOT-style compiler (still in memory). This doesn't interact with Interpreter.zig at all
// but maps WASM stack frames directly to C-ABI stack frames. This will be used as a reference
// to map WASM to FLIR semantics and also to battle-test FLIR (as the IR is going to be quite
// larger than IR extracted from dynamic traces)

const forklift = @import("forklift");
const Instance = @import("./Instance.zig");
const Function = @import("./Function.zig");
const std = @import("std");
const X86Asm = forklift.X86Asm;
const IPReg = X86Asm.IPReg;
const FLIR = forklift.FLIR;
const dbg = std.debug.print;
const defs = @import("./defs.zig");

const HeavyMachineTool = @This();
flir: FLIR,
mod: forklift.CFOModule,
longjmp_func: u32 = undefined,

pub fn init(allocator: std.mem.Allocator) !HeavyMachineTool {
    return .{
        .mod = try .init(allocator),
        .flir = try .init(4, allocator),
    };
}

fn simple_symbol(allocator: std.mem.Allocator, address: usize) ![]u8 {
    const debug_info = try std.debug.getSelfDebugInfo();
    const module = try debug_info.getModuleForAddress(address);
    const s = try module.getSymbolAtAddress(debug_info.allocator, address);
    if (false) return std.fmt.allocPrint(allocator, "{}", .{s});

    if (s.source_location) |l| {
        const name = if (std.mem.lastIndexOfScalar(u8, l.file_name, '/')) |i| l.file_name[i + 1 ..] else l.file_name;
        return std.fmt.allocPrint(allocator, "{s}:{s}:{}", .{ name, s.name, l.line });
    } else {
        return std.fmt.allocPrint(allocator, "?? {s} {s}", .{ s.compile_unit_name, s.name });
    }
}

// why an Instance instead of a module? why not? why ask?
pub fn compileInstance(self: *HeavyMachineTool, in: *Instance) !void {
    const mod = in.mod;
    try mod.mark_exports(); // or already??
    try self.build_longjmp();
    for (0.., mod.funcs_internal) |i, *f| {
        self.compileFunc(in, i, f) catch |e| switch (e) {
            error.NotImplemented => {
                if (f.hmt_error == null) {
                    f.hmt_error = "??UNKNOWN";
                    if (@errorReturnTrace()) |trace| {
                        const n_frames = @min(trace.index, trace.instruction_addresses.len);
                        if (n_frames > 0) {
                            const address = trace.instruction_addresses[0];
                            f.hmt_error = simple_symbol(mod.allocator, address) catch "fuuuuuuuu";
                        }
                    }
                }
                continue; // ok, note the error
            },
            else => return e,
        };
    }
    try X86Asm.dbg_nasm(&.{ .code = &self.mod.code }, in.mod.allocator);
    try self.mod.code.finalize();
    longjmp_f = try self.mod.get_func_ptr_id(self.longjmp_func, @TypeOf(longjmp_f));

    globalExceptionHandler();
}

pub var jmp_buf: JmpBuf = undefined;
pub var jmp_active: bool = false;
pub const JmpBuf = struct { [8]u64 };
pub var longjmp_f: *const fn (status: usize, buf: *const JmpBuf) callconv(.c) void = undefined;
const StackValue = defs.StackValue;
const TrampolineFn = *const fn (mem: [*]u8, mem_size: usize, params: [*]const StackValue, ret: [*]StackValue, jmp_buf: *JmpBuf) callconv(.c) u32;
pub fn execute(self: *HeavyMachineTool, in: *Instance, idx: u32, params: []const StackValue, ret: []StackValue, logga: bool, err_ret: ?*?[]const u8) !u32 {
    if (idx < in.mod.n_funcs_import or idx >= in.mod.n_imports + in.mod.funcs_internal.len) return error.OutOfRange;
    const func = &in.mod.funcs_internal[idx - in.mod.n_funcs_import];
    _ = logga;
    if (func.hmt_error) |err| {
        if (err_ret) |ptr| {
            ptr.* = err;
        } else {
            dbg("ERROR: {s}\n", .{err});
        }
    }

    const trampoline_obj = func.hmt_trampoline orelse return error.NotImplemented;

    const f = try self.mod.get_func_ptr_id(trampoline_obj, TrampolineFn);
    jmp_active = true;
    // std.debug.print("info jmp buf: {x}={}\ntrampolin: {x}={}\n", .{ @intFromPtr(&jmp_buf), @intFromPtr(&jmp_buf), @intFromPtr(f), @intFromPtr(f) });
    // asm volatile ("int3");
    const status = f(in.mem.items.ptr, in.mem.items.len, params.ptr, ret.ptr, &jmp_buf);
    jmp_active = false;
    if (status != 0) return error.WASMTrap;
    return func.n_res;
}

fn is_wide(typ: defs.ValType) !bool {
    return switch (typ) {
        .i32 => false,
        .i64 => true,
        else => error.NotImplemented,
    };
}

inline fn unwide(inst: defs.OpCode, wide: bool, narrow_start: defs.OpCode, wide_start: defs.OpCode) u8 {
    return @intFromEnum(inst) - @intFromEnum(@as(defs.OpCode, if (wide) wide_start else narrow_start));
}

fn iSize(wide: bool) forklift.defs.ISize {
    // a little weak. just like wasm, QBE does, forklift cares about wide vs non-wide for ops
    // 1,2,4 vs 8 bytes enter the picture when memory:p
    return if (wide) .quadword else .dword;
}

pub fn globalExceptionHandler() void {
    const posix = std.posix;
    var act = posix.Sigaction{
        .handler = .{ .sigaction = sigHandler },
        .mask = posix.sigemptyset(),
        // don't mask out signal, as we are going to longjmp out of the handler
        .flags = (posix.SA.SIGINFO | posix.SA.RESTART | posix.SA.NODEFER),
    };

    posix.sigaction(posix.SIG.FPE, &act, null);
}

fn build_longjmp(self: *HeavyMachineTool) !void {
    self.longjmp_func = @intCast(self.mod.objs.items.len);
    const longjmp_target = self.mod.code.get_target();
    try self.mod.objs.append(self.mod.gpa, .{ .obj = .{ .func = .{ .code_start = longjmp_target } }, .name = null });

    var cfo = X86Asm{ .code = &self.mod.code, .long_jump_mode = true };
    try cfo.mov(true, .rax, .rdi); // status = arg1
    const b = X86Asm.a(.rsi); // jmp_buf = arg2
    try cfo.movrm(true, .rbx, b);
    try cfo.movrm(true, .rbp, b.o(8));
    try cfo.movrm(true, .r12, b.o(16));
    try cfo.movrm(true, .r13, b.o(24));
    try cfo.movrm(true, .r14, b.o(32));
    try cfo.movrm(true, .r15, b.o(40));
    try cfo.movrm(true, .rsp, b.o(48)); // NOTE: as inline we don't need to adjust
    try cfo.jmpi_m(b.o(56));
}

fn sigHandler(sig: i32, info: *const std.posix.siginfo_t, ctx_ptr: ?*const anyopaque) callconv(.c) void {
    _ = sig;
    _ = info;
    _ = ctx_ptr;
    if (jmp_active) {
        longjmp_f(7, &jmp_buf);
    } else {
        std.debug.print("very stupid!\n", .{});
        std.posix.exit(11);
    }
}

pub fn compileFunc(self: *HeavyMachineTool, in: *Instance, id: usize, f: *Function) !void {
    _ = id;
    const ir = &self.flir;
    const gpa = in.mod.allocator;
    const c = try f.ensure_parsed(in.mod);
    ir.reinit();
    var locals = try gpa.alloc(u16, f.local_types.len);
    var node = try ir.addNode();
    // I think FLIR can require all args to be first..
    for (0..f.n_params) |i| {
        locals[i] = try ir.arg();
    }
    if (f.args_mut != 0) {
        for (0..f.n_params) |i| {
            const mut = (f.args_mut & (@as(u64, 1) << @as(u6, @intCast((i & 63))))) != 0;
            if (mut) {
                const src = locals[i];
                const typ: forklift.defs.ISize = switch (f.local_types[i]) {
                    .i32 => .dword,
                    .i64 => .quadword,
                    else => return error.NotImplemented,
                };
                locals[i] = try ir.variable(.{ .intptr = typ });
                try ir.putvar(node, locals[i], src);
            }
        }
    }

    var value_stack: std.ArrayList(u16) = .empty;
    defer value_stack.deinit(gpa);

    var label_stack: std.ArrayList(struct { c_ip: u32, ir_target: u16, else_target: u16 = FLIR.NoRef, loop: bool, res_var: u16, value_stack_level: usize }) = .empty;
    defer label_stack.deinit(gpa);

    // only a single node doing "ir.ret"
    const exit_node = try ir.addNode();
    if (f.n_res > 1) return error.NotImplemented;
    const exit_var = try ir.variable(.{ .intptr = .dword });
    // TODO: ir.ret(VOID)
    try ir.ret(exit_node, .{ .intptr = .dword }, if (f.n_res > 0) exit_var else try ir.const_uint(0));
    try label_stack.append(gpa, .{ .c_ip = 0, .ir_target = exit_node, .loop = false, .res_var = if (f.n_res > 0) exit_var else FLIR.NoRef, .value_stack_level = 0 });

    var c_ip: u32 = 0;
    var r = in.mod.reader_at(f.codeoff);

    {
        var i = f.n_params;
        const n_local_defs = try r.readu();
        for (0..n_local_defs) |_| {
            const n_decl = try r.readu();
            const typ: defs.ValType = @enumFromInt(try r.readByte());
            const init_val = StackValue.default(typ) orelse return error.InvalidFormat;
            const ftyp: forklift.defs.ISize = switch (typ) {
                .i32 => .dword,
                .i64 => .quadword,
                else => return error.NotImplemented,
            };
            for (0..n_decl) |_| {
                locals[i] = try ir.variable(.{ .intptr = ftyp });
                try ir.putvar(node, locals[i], try ir.const_uint(init_val.u32()));
                i += 1;
            }
        }
    }

    errdefer ir.debug_print(); // show what we got when it ends

    if (true) {
        if (f.name) |nam| {
            std.debug.print("FOR \"{s}\":\n", .{nam});
        } else if (f.exported) |nam| {
            std.debug.print("FOR export \"{s}\":\n", .{nam});
        }
    }

    // if true, br and br_if should use a icmp/fcmp/etc already emitted
    var cond_pending = false;
    var dead_end = false;

    while (true) {
        const pos = r.pos;
        const inst = try r.readOpCode();
        switch (inst) {
            .drop => {
                _ = value_stack.pop().?;
            },
            .i32_const => {
                const val = try r.readLeb(i32);
                try value_stack.append(gpa, try ir.const_uint(@bitCast(@as(i64, val))));
            },
            .local_set => {
                const idx = try r.readu();
                const src = value_stack.pop().?;
                try ir.putvar(node, locals[idx], src);
            },
            .local_tee => {
                const idx = try r.readu();
                const src = value_stack.items[value_stack.items.len - 1];
                try ir.putvar(node, locals[idx], src);
            },
            .local_get => {
                const idx = try r.readu();
                const val = try ir.read_ref(node, locals[idx]); // idempodent if locals[idx] is argument
                try value_stack.append(gpa, val);
            },
            .loop => {
                const typ = try r.blocktype();
                const n_args, const n_results = try typ.arity(in.mod);
                if (n_args != 0 or n_results != 0) return error.NotImplemented;
                c_ip += 1;
                const entry = try ir.addNodeAfter(node);
                node = entry;
                try label_stack.append(gpa, .{ .c_ip = c_ip, .ir_target = entry, .loop = true, .res_var = FLIR.NoRef, .value_stack_level = value_stack.items.len });
            },
            .block => {
                const typ = try r.blocktype();
                const n_args, const n_results = try typ.arity(in.mod);
                if (n_args > 0 or n_results > 1) return error.NotImplemented;
                c_ip += 1;
                if (c[c_ip].off != pos) @panic("MANIC FEAR");
                const exit = try ir.addNode();
                // technically just a single phi. but FLIR.variable() is meant to be cheap enough to not nead
                // a separate API for "gimmie one phi".
                const res_var = if (n_results > 0) try ir.variable(.{ .intptr = .dword }) else FLIR.NoRef;
                // TODO: with n_args, value_stack_level is without the args
                try label_stack.append(gpa, .{ .c_ip = c_ip, .ir_target = exit, .loop = false, .res_var = res_var, .value_stack_level = value_stack.items.len });
            },
            .if_ => {
                const typ = try r.blocktype();
                const n_args, const n_results = try typ.arity(in.mod);
                if (n_args > 0 or n_results > 1) return error.NotImplemented;

                // note: semi-copy in .br_if
                if (!cond_pending) {
                    const val = value_stack.pop().?;
                    _ = try ir.icmp(node, .dword, .neq, val, try ir.const_uint(0));
                } else cond_pending = false;

                c_ip += 1;
                if (c[c_ip].off != pos) @panic("TREMBLING FEAR");

                var r_target = in.mod.reader_at(c[c[c_ip].jmp_t].off);
                const c_inst = try r_target.readOpCode();

                const then = try ir.addNode();
                const exit = try ir.addNode();
                std.debug.print("INSTRUCTIVE FEAR: {}\n", .{c_inst});
                const else_ = if (c_inst == .else_) try ir.addNode() else exit;

                const res_var = if (n_results > 0) try ir.variable(.{ .intptr = .dword }) else FLIR.NoRef;
                try label_stack.append(gpa, .{ .c_ip = c_ip, .ir_target = exit, .else_target = else_, .loop = false, .res_var = res_var, .value_stack_level = value_stack.items.len });

                try ir.addLink(node, 0, else_);
                try ir.addLink(node, 1, then); // branch taken
                node = then;
            },
            .else_ => {
                c_ip += 1;
                if (c[c_ip].off != pos) @panic("FEAR OF DESICIONS");
                if (label_stack.items.len == 0) @panic("WAVES OF FEAR");
                // reuse same label in the else body
                const label = label_stack.items[label_stack.items.len - 1];

                const has_res = label.res_var != FLIR.NoRef;
                if (has_res) {
                    try ir.putvar(node, label.res_var, value_stack.pop().?);
                }
                try ir.addLink(node, 0, label.ir_target);

                node = label.else_target;
            },
            .end => {
                c_ip += 1;
                if (c[c_ip].off != pos) @panic("UNSEEN FEAR");
                const label = label_stack.pop() orelse @panic("FEAR OF LIMBO");
                const has_res = label.res_var != FLIR.NoRef;
                if (!label.loop) {
                    if (!dead_end) {
                        if (has_res) {
                            try ir.putvar(node, label.res_var, value_stack.pop().?);
                        }
                        try ir.addLink(node, 0, label.ir_target);
                    } else {
                        if (value_stack.items.len < label.value_stack_level) return error.InternalCompilerError;
                        value_stack.items.len = label.value_stack_level;
                    }
                    node = label.ir_target;
                    dead_end = false; // back to lyf
                } else {
                    if (dead_end) {
                        const peekinst: defs.OpCode = @enumFromInt(r.peekByte());
                        // dead code just right after a loop, bleh!
                        if (peekinst != .end) return error.NotImplemented;
                    }
                }

                if (label_stack.items.len == 0) {
                    if (value_stack.items.len != 0) return error.InternalCompilerError;
                    break;
                } else {
                    // if this was the only exit it will be simplified back to the old tip value
                    if (has_res) try value_stack.append(gpa, try ir.read_ref(node, label.res_var));
                }
            },
            .br_if => {
                c_ip += 1;
                if (c[c_ip].off != pos) @panic("FEAR ALL AROUND");
                if (!cond_pending) {
                    const val = value_stack.pop().?;
                    _ = try ir.icmp(node, .dword, .neq, val, try ir.const_uint(0));
                } else cond_pending = false;
                const label = try r.readu();
                if (label > label_stack.items.len - 1) return error.InternalCompilerError;
                const target = label_stack.items[label_stack.items.len - label - 1];
                if (target.res_var != FLIR.NoRef) {
                    if (value_stack.items.len == 0) return error.InternalCompilerError;
                    // don't pop in case branch NOT taken
                    try ir.putvar(node, target.res_var, value_stack.items[value_stack.items.len - 1]);
                }
                try ir.addLink(node, 1, target.ir_target); // branch taken
                node = try ir.addNodeAfter(node);
            },
            .ret => {
                try ir.addLink(node, 0, exit_node);
                if (f.n_res > 0) {
                    if (f.n_res > 1) return error.Notimplemented;
                    try ir.putvar(node, exit_var, value_stack.pop().?);
                }
                const peekinst: defs.OpCode = @enumFromInt(r.peekByte());
                // TODO: dead code is weird in WASM, need to skip over as the stack might not exist
                // TODO: or else I guess
                if (peekinst != .end) return error.NotImplemented;
                dead_end = true;
            },
            .i32_eqz, .i64_eqz => {
                const val = value_stack.pop().?;
                const zero = try ir.const_uint(0);
                const wide = (inst == .i64_eqz);

                // NB: semi-copy in i32_relop
                const peekinst: defs.OpCode = @enumFromInt(r.peekByte());
                if (peekinst == .br_if or peekinst == .if_) {
                    _ = try ir.icmp(node, iSize(wide), .eq, val, zero);
                    cond_pending = true;
                } else {
                    const res = try ir.icmpset(node, iSize(wide), .eq, val, zero);
                    try value_stack.append(gpa, res);
                }
            },
            .call => {
                const idx = try r.readu();
                if (idx < in.mod.n_funcs_import) return error.NotImplemented;
                if (idx >= in.mod.n_funcs_import + in.mod.funcs_internal.len) return error.InvalidFormat;
                const func = &in.mod.funcs_internal[idx - in.mod.n_funcs_import];
                if (func.n_params > 0 or func.n_res > 0) return error.NotImplemented;
                const obj = func.hmt_object orelse return error.NotImplemented;

                const off = self.mod.get_func_off(obj) orelse return error.TypeError;
                const callwhat = try ir.const_uint(off);
                _ = try ir.call(node, .near, callwhat, 0);
            },
            // TODO: this leads to some bloat - some things like binops could be done as a bulk
            inline else => |tag| {
                const category = comptime defs.category(tag);
                switch (category) {
                    .i32_unop, .i64_unop => {
                        const src = value_stack.pop().?;
                        const flir_op: FLIR.IntUnOp = switch (tag) {
                            .i32_popcnt, .i64_popcnt => .popcount,
                            .i32_ctz, .i64_ctz => .ctz,
                            .i32_clz, .i64_clz => .clz,
                            .i32_extend8_s => .sign_extend,
                            .i32_extend16_s => .sign_extend,
                            .i64_extend8_s => .sign_extend,
                            .i64_extend16_s => .sign_extend,
                            .i64_extend32_s => .sign_extend,
                            else => {
                                f.hmt_error = try std.fmt.allocPrint(in.mod.allocator, "inst {s} in the {s} impl TBD, aborting!", .{ @tagName(tag), @tagName(category) });
                                return error.NotImplemented;
                            },
                        };
                        // TODO: wrongggg for sign_extend
                        const size: forklift.defs.ISize = switch (tag) {
                            .i32_extend8_s => .byte,
                            .i32_extend16_s => .word,
                            .i64_extend8_s, .i64_extend16_s, .i64_extend32_s => return error.NotImplemented,
                            else => iSize(category == .i64_unop),
                        };
                        const res = try ir.iunop(node, size, flir_op, src);
                        try value_stack.append(gpa, res);
                    },
                    .i32_binop, .i64_binop => {
                        const rhs = value_stack.pop().?;
                        const lhs = value_stack.pop().?;
                        const wide = category == .i64_binop;
                        // still bloated but preparing for category-wise debloat
                        const tag_reduced: defs.BinOp = @enumFromInt(unwide(inst, wide, .i32_add, .i64_add));
                        const flir_op: FLIR.IntBinOp = switch (tag_reduced) {
                            .add => .add,
                            .sub => .sub,
                            .mul => .mul,
                            .div_s => .sdiv,
                            .div_u => .udiv,
                            .rem_s => .srem,
                            .rem_u => .urem,
                            .@"and" => .@"and",
                            .@"or" => .@"or",
                            .xor => .xor,
                            .shl => .shl,
                            .shr_s => .sar,
                            .shr_u => .shr,
                            .rotl => .rotl,
                            .rotr => .rotr,
                        };
                        const res = try ir.ibinop(node, iSize(wide), flir_op, lhs, rhs);
                        try value_stack.append(gpa, res);
                    },
                    .i32_relop, .i64_relop => {
                        const rhs = value_stack.pop().?;
                        const lhs = value_stack.pop().?;
                        const wide = category == .i64_relop;
                        const tag_op: defs.RelOp = @enumFromInt(unwide(inst, wide, .i32_eq, .i64_eq));
                        const cmpop: FLIR.IntCond = switch (tag_op) {
                            .eq => .eq,
                            .ne => .neq,
                            .lt_s => .lt,
                            .le_s => .le,
                            .gt_s => .gt,
                            .ge_s => .ge,
                            .lt_u => .b,
                            .le_u => .na,
                            .gt_u => .a,
                            .ge_u => .nb,
                        };

                        const peekinst: defs.OpCode = @enumFromInt(r.peekByte());
                        // NB: semi-copy in i32_eqz
                        if (peekinst == .br_if or peekinst == .if_) {
                            _ = try ir.icmp(node, iSize(wide), cmpop, lhs, rhs);
                            cond_pending = true;
                        } else {
                            const res = try ir.icmpset(node, iSize(wide), cmpop, lhs, rhs);
                            try value_stack.append(gpa, res);
                        }
                    },
                    else => |cat| {
                        dbg("inst {s} as {s} TBD, aborting!\n", .{ @tagName(tag), @tagName(cat) });
                        f.hmt_error = @tagName(tag);
                        return error.NotImplemented;
                    },
                }
            },
        }
    }
    ir.debug_print();

    try ir.test_analysis(FLIR.X86ABI, true);
    ir.print_intervals();
    ir.debug_print();

    // TODO: abstraction
    const target = try forklift.codegen_x86_64(ir, &self.mod.code, false);
    f.hmt_object = @intCast(self.mod.objs.items.len);
    try self.mod.objs.append(self.mod.gpa, .{ .obj = .{ .func = .{ .code_start = target } }, .name = null });

    if (f.exported == null) return;

    const max_args = 2;
    if (f.n_params > max_args) return error.NotImplemented;
    var local_types_buf: [max_args]defs.ValType = undefined;
    const ret_type = try in.mod.type_params(f.typeidx, &local_types_buf);
    const local_types = local_types_buf[0..f.n_params];

    var cfo = X86Asm{ .code = &self.mod.code, .long_jump_mode = true };
    const frame = true;

    // TODO: this could be after/shared/whatever?
    const error_exit_target = self.mod.code.get_target();
    try cfo.movri(false, .rax, 1); // TODO: do not. rax is the longjmp return code anyway
    if (frame) try cfo.leave();
    try cfo.ret();

    const trampolin_target = self.mod.code.get_target();
    // trampolin,  *const fn (mem: [*]u8, mem_size: usize, params: [*]const StackValue, ret: [*]StackValue, jmp_buf: *JmpBpub f) u32;
    // arg 1: RDI = mem
    // arg2 : RSI = mem_size
    // arg3 : RDX = params
    // arg4: RCX = ret
    // arg5: R8 = jmp_buf
    // try cfo.trap();
    if (frame) try cfo.enter();
    if (ret_type) |_| try cfo.push(.rcx);

    // inline setjmp
    const b = X86Asm.a(.r8);
    try cfo.movmr(true, b, .rbx);
    try cfo.movmr(true, b.o(8), .rbp);
    try cfo.movmr(true, b.o(16), .r12);
    try cfo.movmr(true, b.o(24), .r13);
    try cfo.movmr(true, b.o(32), .r14);
    try cfo.movmr(true, b.o(40), .r15);
    try cfo.movmr(true, b.o(48), .rsp); // NOTE: as inline we don't need to adjust
    try cfo.lea(.rax, X86Asm.rel(error_exit_target));
    try cfo.movmr(true, b.o(56), .rax);

    // TODO: offset with mem when we start using it
    const ireg: [max_args]IPReg = .{ .rdi, .rsi };

    for (local_types, 0..) |t, i| {
        const w = switch (t) {
            .i32 => false,
            .i64 => true,
            else => return error.NotImplemented,
        };
        try cfo.movrm(w, ireg[i], X86Asm.a(.rdx).o(@intCast(@sizeOf(defs.StackValue) * i)));
    }
    try cfo.call_rel(target);
    if (ret_type) |typ| {
        try cfo.pop(.rcx);
        const w = switch (typ) {
            .i32 => false,
            .i64 => true,
            else => return error.NotImplemented,
        };
        try cfo.movmr(w, X86Asm.a(.rcx), .rax); // only one
    }
    try cfo.zero(.rax); // non-error exit
    // as a silly trick, setjmp target here? nice for debugging
    if (frame) try cfo.leave();
    try cfo.ret();

    f.hmt_trampoline = @intCast(self.mod.objs.items.len);
    try self.mod.objs.append(self.mod.gpa, .{ .obj = .{ .func = .{ .code_start = trampolin_target } }, .name = null });
}
