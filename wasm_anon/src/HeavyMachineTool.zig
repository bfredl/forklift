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
const severe = std.debug.print;
const dbg = severe;
const defs = @import("./defs.zig");
const NoRef = FLIR.NoRef;
const IZero = FLIR.IZero;

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

pub fn reinit(self: *HeavyMachineTool, allocator: std.mem.Allocator) !void {
    // TODO: besser
    self.mod.deinit_mem();
    self.mod = try .init(allocator);
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
pub fn compileInstance(self: *HeavyMachineTool, in: *Instance, filter: ?[]const u8) !void {
    const mod = in.mod;
    try mod.mark_exports(); // or already??
    var any_compiled = false;
    try self.build_longjmp();
    const very_verbose = filter != null; // SILLY GOOSE
    for (0.., mod.funcs_internal) |i, *f| {
        var selekted = false;
        if (filter) |fname| {
            // luring: if not exported it is likely a helper we might need
            // ta den med ändån.
            if (f.exported) |ename| {
                if (!std.mem.eql(u8, fname, ename)) {
                    continue;
                }
                selekted = true;
            }
        }
        self.compileFunc(in, i, f, false) catch |e| switch (e) {
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
                try self.errorStub(f);
                if (selekted) return e else continue; // ok, note the error, unless selekted (NOT OK)
            },
            else => return e,
        };
        any_compiled = true;
    }
    if (any_compiled and very_verbose) try X86Asm.dbg_nasm(&.{ .code = &self.mod.code }, in.mod.allocator);
    try self.mod.load(false);
    try self.mod.code.finalize();
    longjmp_f = try self.mod.get_func_ptr_id(self.longjmp_func, @TypeOf(longjmp_f));

    globalExceptionHandler();
}

pub var jmp_buf: JmpBuf = undefined;
pub var jmp_active: bool = false;
pub var debug_as_self: ?*HeavyMachineTool = null;
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
            const name = if (func.name) |nam| nam else func.exported;
            ptr.* = try std.fmt.allocPrint(in.mod.allocator, "cannot execute {s} due to {s}", .{ name orelse "???", err });
        } else {
            dbg("ERROR: {s}\n", .{err});
        }
    }

    const trampoline_obj = func.hmt_trampoline orelse return error.NotImplemented;

    const f = try self.mod.get_func_ptr_id(trampoline_obj, TrampolineFn);
    jmp_active = true;
    debug_as_self = self;
    // std.debug.print("info jmp buf: {x}={}\ntrampolin: {x}={}\n", .{ @intFromPtr(&jmp_buf), @intFromPtr(&jmp_buf), @intFromPtr(f), @intFromPtr(f) });
    const status = f(in.mem.items.ptr, in.mem.items.len, params.ptr, ret.ptr, &jmp_buf);
    debug_as_self = null;
    jmp_active = false;
    if (status != 0) return error.WASMTrap;
    return func.n_res;
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
    posix.sigaction(posix.SIG.TRAP, &act, null);
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

fn try_inspect(self: *HeavyMachineTool, addr: usize) void {
    const code = self.mod.code;
    const startaddr: usize = @intFromPtr(code.buf.items.ptr);
    const endaddr: usize = startaddr + code.buf.items.len;
    if (startaddr <= addr and addr < endaddr) {
        const off = addr - startaddr;
        severe("OFFSÄTT {} !\n", .{off});
    }
}

fn sigHandler(sig: i32, info: *const std.posix.siginfo_t, ctx_ptr: ?*const anyopaque) callconv(.c) void {
    _ = sig;
    _ = info;

    const ctx: *align(1) const std.posix.ucontext_t = @ptrCast(ctx_ptr);
    var new_ctx: std.posix.ucontext_t = ctx.*;
    std.debug.relocateContext(&new_ctx);
    severe("hej!\n", .{});

    // TODO: some of this logic in OSHA.zig. update it and make it generic!
    // NB: we want to be able to debug JIT code even if there is no SelfInfo
    severe("first hello: {x}\n", .{new_ctx.mcontext.gregs[std.posix.REG.RIP]});
    if (debug_as_self) |self| self.try_inspect(new_ctx.mcontext.gregs[std.posix.REG.RIP]);
    var it = std.debug.StackIterator.init(null, new_ctx.mcontext.gregs[std.posix.REG.RBP]);
    defer it.deinit();
    while (it.next()) |return_address| {
        severe("hallå eller: {x}\n", .{return_address});
        if (debug_as_self) |self| self.try_inspect(return_address);
    }

    {
        const stderr = std.debug.lockStderrWriter(&.{});
        defer std.debug.unlockStderrWriter();
        std.debug.dumpStackTraceFromBase(&new_ctx, stderr);
        stderr.print("hejdå!\n", .{}) catch unreachable;
    }
    if (jmp_active) {
        longjmp_f(7, &jmp_buf);
    } else {
        std.debug.print("very stupid!\n", .{});
        std.posix.exit(11);
    }
}

pub fn specType(typ: defs.ValType) ?forklift.defs.SpecType {
    return switch (typ) {
        .i32 => .{ .intptr = .dword },
        .i64 => .{ .intptr = .quadword },
        .f32 => .{ .avxval = .ss },
        .f64 => .{ .avxval = .sd },
        .funcref, .externref => .{ .intptr = .dword },
        else => null,
    };
}

// TODO: missing clear CFO vs wasm_shelf abstraction, just winging it
pub fn declareFunc(self: *HeavyMachineTool, f: *Function, pos: ?u32) !u32 {
    if (f.hmt_object) |idx| {
        if (pos) |p| {
            const obj = &self.mod.objs.items[idx].obj;
            if (obj.func.code_start != forklift.defs.INVALID_OFFSET) {
                return error.InternalCompilerError;
            }
            obj.func.code_start = p;
        }
        return idx;
    } else {
        const idx: u32 = @intCast(self.mod.objs.items.len);
        f.hmt_object = idx;
        const target = if (pos) |p| p else forklift.defs.INVALID_OFFSET;

        // this doesn't have to be accurate/unique, just nice to have a name for debugging
        const name = if (f.name) |nam| nam else f.exported;
        try self.mod.objs.append(self.mod.gpa, .{ .obj = .{ .func = .{ .code_start = target } }, .name = name });
        return idx;
    }
}

pub fn compileFunc(self: *HeavyMachineTool, in: *Instance, id: usize, f: *Function, verbose: bool) !void {
    _ = id;
    const ir = &self.flir;
    const gpa = in.mod.allocator;
    const c = try f.ensure_parsed(in.mod);
    ir.reinit();
    var locals = try gpa.alloc(u16, f.local_types.len);
    var node = try ir.addNode();
    // I think FLIR can require all args to be first..
    const mem_base = try ir.arg(.{ .intptr = .quadword });
    const mem_size = try ir.arg(.{ .intptr = .quadword });
    for (0..f.n_params) |i| {
        locals[i] = try ir.arg(specType(f.local_types[i]) orelse return error.NotImplemented);
    }
    if (f.args_mut != 0) {
        for (0..f.n_params) |i| {
            const mut = (f.args_mut & (@as(u64, 1) << @as(u6, @intCast((i & 63))))) != 0;
            if (mut) {
                const src = locals[i];
                // dbg("TYP: {}\n", .{f.local_types[i]});
                const typ: forklift.defs.SpecType = specType(f.local_types[i]) orelse return error.NotImplemented;
                locals[i] = try ir.variable(typ);
                try ir.putvar(node, locals[i], src);
            }
        }
    }

    var value_stack: std.ArrayList(u16) = .empty;
    defer value_stack.deinit(gpa);

    var label_stack: std.ArrayList(struct { c_ip: u32, ir_target: u16, else_target: u16 = NoRef, loop: bool, res_var: u16, value_stack_level: usize }) = .empty;
    defer label_stack.deinit(gpa);

    const max_args = 5;
    const max_res = 2;
    if (f.n_params > max_args or f.n_res > max_res) {
        f.hmt_error = try std.fmt.allocPrint(gpa, "VERKLIGEN VILL DU: {} => {}", .{ f.n_params, f.n_res });
        return error.NotImplemented;
    }

    const arg_types = f.local_types[0..f.n_params];

    // only a single node doing "ir.ret"
    const exit_node = try ir.addNode();
    var exit_vars: [max_res]u16 = @splat(NoRef);

    try ir.ret(exit_node);
    for (0..f.n_res) |i| {
        const tret = specType(f.res_types[i]) orelse return error.NotImplemented;
        exit_vars[i] = try ir.variable(tret);
        try ir.retval(exit_node, tret, exit_vars[i]);
    }
    // FAIL: integrate with multiple res_vars because of the wasm multivalue blocks extension
    try label_stack.append(gpa, .{ .c_ip = 0, .ir_target = exit_node, .loop = false, .res_var = NoRef, .value_stack_level = 0 });

    var c_ip: u32 = 0;
    var r = in.mod.reader_at(f.codeoff);

    {
        var i = f.n_params;
        const n_local_defs = try r.readu();
        for (0..n_local_defs) |_| {
            const n_decl = try r.readu();
            const typ: defs.ValType = @enumFromInt(try r.readByte());
            const init_val = StackValue.default(typ) orelse return error.InvalidFormat;
            for (0..n_decl) |_| {
                const t = specType(typ) orelse return error.NotImplemented;
                locals[i] = try ir.variable(t);
                // JÄMMER: maybe just ir.zero(spectype) ??
                const initv = try switch (t) {
                    .avxval => |k| if (k == .sd) ir.const_f64(node, 0) else ir.const_f32(node, 0),
                    .intptr => ir.const_uint(init_val.u32()),
                };
                try ir.putvar(node, locals[i], initv);
                i += 1;
            }
        }
    }

    errdefer if (verbose) ir.debug_print(); // show what we got when it ends

    if (verbose) {
        if (f.name) |nam| {
            std.debug.print("\nFOR \"{s}\":\n", .{nam});
        } else if (f.exported) |nam| {
            std.debug.print("\nFOR export \"{s}\":\n", .{nam});
        } else {
            std.debug.print("\nFOR ?????\n", .{});
        }
    }

    // if true, br and br_if should use a icmp/fcmp/etc already emitted
    var cond_pending = false;
    var dead_end = false;

    while (true) {
        const pos = r.pos;
        const inst = try r.readInst();
        switch (inst) {
            .drop => {
                _ = value_stack.pop().?;
            },
            .unreachable_ => {
                _ = try ir.trap(node, false);

                // TODO: likely FLIR just allowing unlikely orphaned exits should be fine
                // but check this. for now emit a fake "ret" instruction
                try ir.addLink(node, 0, exit_node); // branch taken
                dead_end = true;
                if (f.n_res > 0) {
                    if (f.n_res > 1) return error.NotImplemented;
                    if (f.n_res == 1) try ir.putvar(node, exit_vars[0], IZero);
                }
                if (r.peekOpCode() != .end) return error.NotImplemented;
            },
            .local_set => |idx| {
                const src = value_stack.pop().?;
                try ir.putvar(node, locals[idx], src);
            },
            .local_tee => |idx| {
                const src = value_stack.items[value_stack.items.len - 1];
                try ir.putvar(node, locals[idx], src);
            },
            .local_get => |idx| {
                const val = try ir.read_ref(node, locals[idx]); // idempodent if locals[idx] is (non-mutable) argument
                try value_stack.append(gpa, val);
            },
            .global_get, .global_set => |idx| {
                const globel = in.get_global(idx);
                const typ = in.mod.global_types[idx];
                const addr = try ir.const_uint(@intFromPtr(globel));

                if (typ.t != .i32 and typ.t != .i64) return error.NotImplemented;
                const wide = typ.t == .i64;

                if (inst == .global_get) {
                    const load = try ir.load(node, wide, false, specType(typ.t).?, addr, IZero, 0);
                    try value_stack.append(gpa, load);
                } else {
                    const val = value_stack.pop().?;
                    _ = try ir.store(node, specType(typ.t).?, addr, IZero, 0, val);
                }
            },
            .loop => |typ| {
                const n_args, const n_results = try typ.arity(in.mod);
                if (n_args != 0 or n_results > 1) return error.NotImplemented;
                c_ip += 1;
                const entry = try ir.addNodeAfter(node);
                node = entry;
                try label_stack.append(gpa, .{ .c_ip = c_ip, .ir_target = entry, .loop = true, .res_var = NoRef, .value_stack_level = value_stack.items.len });
            },
            .block => |typ| {
                const n_args, const n_results = try typ.arity(in.mod);
                if (n_args > 0 or n_results > 1) return error.NotImplemented;
                c_ip += 1;
                if (c[c_ip].off != pos) @panic("MANIC FEAR");
                const exit = try ir.addNode();
                // technically just a single phi. but FLIR.variable() is meant to be cheap enough to not nead
                // a separate API for "gimmie one phi".
                const res_var = if (n_results > 0) try ir.variable(.{ .intptr = .dword }) else NoRef;
                // TODO: with n_args, value_stack_level is without the args
                try label_stack.append(gpa, .{ .c_ip = c_ip, .ir_target = exit, .loop = false, .res_var = res_var, .value_stack_level = value_stack.items.len });
            },
            .if_ => |typ| {
                const n_args, const n_results = try typ.arity(in.mod);
                if (n_args > 0 or n_results > 1) return error.NotImplemented;

                // note: semi-copy in .br_if
                if (!cond_pending) {
                    const val = value_stack.pop().?;
                    _ = try ir.icmp(node, .dword, .neq, val, IZero);
                } else cond_pending = false;

                c_ip += 1;
                if (c[c_ip].off != pos) @panic("TREMBLING FEAR");

                var r_target = in.mod.reader_at(c[c[c_ip].jmp_t].off);
                const c_inst = try r_target.readOpCode();

                const then = try ir.addNode();
                const exit = try ir.addNode();
                const else_ = if (c_inst == .else_) try ir.addNode() else exit;

                const res_var = if (n_results > 0) try ir.variable(.{ .intptr = .dword }) else NoRef;
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

                if (!dead_end) {
                    const has_res = label.res_var != NoRef;
                    if (has_res) {
                        try ir.putvar(node, label.res_var, value_stack.pop().?);
                    }
                    try ir.addLink(node, 0, label.ir_target);
                } else {
                    dead_end = false;
                    if (value_stack.items.len < label.value_stack_level) return error.InternalCompilerError;
                    value_stack.items.len = label.value_stack_level;
                }

                node = label.else_target;
            },
            .end => {
                c_ip += 1;
                if (c[c_ip].off != pos) @panic("UNSEEN FEAR");
                const label = label_stack.pop() orelse @panic("FEAR OF LIMBO");
                const has_res = label.res_var != NoRef;
                if (!label.loop) {
                    if (!dead_end) {
                        // FAIL: reintegrate with multivalue
                        if (label_stack.items.len == 0) {
                            // it's return time
                            if (value_stack.items.len != f.n_res) return error.InternalCompilerError;
                            for (0..f.n_res) |i| {
                                try ir.putvar(node, exit_vars[i], value_stack.items[i]);
                            }
                            value_stack.items.len = 0;
                        } else if (has_res) {
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
            .ret => {
                try ir.addLink(node, 0, exit_node);
                if (f.n_res > 0) {
                    if (f.n_res > 1) return error.NotImplemented;
                    if (f.n_res == 1) {
                        try ir.putvar(node, exit_vars[0], value_stack.pop().?);
                    }
                }
                // TODO: dead code is weird in WASM, need to skip over as the stack might not exist
                // TODO: or else I guess
                if (r.peekOpCode() != .end) return error.NotImplemented;
                dead_end = true;
            },
            .br, .br_if => |label| {
                if (inst == .br_if) {
                    c_ip += 1;
                    if (c[c_ip].off != pos) @panic("FEAR ALL AROUND");
                    if (!cond_pending) {
                        const val = value_stack.pop().?;
                        _ = try ir.icmp(node, .dword, .neq, val, IZero);
                    } else cond_pending = false;
                }
                if (label > label_stack.items.len - 1) return error.InternalCompilerError;
                const target = label_stack.items[label_stack.items.len - label - 1];
                if (target.res_var != NoRef) {
                    if (value_stack.items.len == 0) return error.InternalCompilerError;
                    // don't pop in case branch NOT taken
                    try ir.putvar(node, target.res_var, value_stack.items[value_stack.items.len - 1]);
                }
                try ir.addLink(node, if (inst == .br_if) 1 else 0, target.ir_target); // branch taken
                if (inst == .br_if) {
                    node = try ir.addNodeAfter(node);
                } else {
                    if (r.peekOpCode() != .end and r.peekOpCode() != .else_) {
                        const exit_from = label_stack.items[label_stack.items.len - 1].c_ip;
                        const jump_to = c[exit_from].jmp_t;
                        const old_pos = r.pos;
                        r.pos = c[jump_to].off; // don't skip it, execute end or else_ as needed
                        const target_op = r.peekOpCode();
                        if (target_op != .end and target_op != .else_) return error.InternalCompilerError; // foooka amnitabl
                        if (old_pos > r.pos) {
                            if (target_op != .else_) return error.InternalCompilerError;
                            // PERNICIOUS: we were actually in the else branch already, jump again
                            // TODO: or fix how this is prepresented, change `label_stack.items[label_stack.items.len - 1].c_ip` when processing else??
                            const jump_again = c[jump_to].jmp_t;
                            r.pos = c[jump_again].off; // don't skip it, execute end
                            if (r.peekOpCode() != .end) return error.InternalCompilerError;
                        }
                    }
                    dead_end = true; // TODO: as below
                }
            },

            .i32_const => |val| try value_stack.append(gpa, try ir.const_uint(@bitCast(@as(i64, val)))),
            .i64_const => |val| try value_stack.append(gpa, try ir.const_uint(@bitCast(val))),
            .f32_const => |val| try value_stack.append(gpa, try ir.const_f32(node, val)),
            .f64_const => |val| try value_stack.append(gpa, try ir.const_f64(node, val)),
            .i32_binop, .i64_binop => |tag| {
                const rhs = value_stack.pop().?;
                const lhs = value_stack.pop().?;
                const wide = inst == .i64_binop;
                const res = try ir.ibinop(node, iSize(wide), tag.into(), lhs, rhs);
                try value_stack.append(gpa, res);
            },
            .i32_relop, .i64_relop => |tag| {
                const rhs = value_stack.pop().?;
                // careful now:
                const lhs = if (tag == .eqz) IZero else value_stack.pop().?;
                const wide = inst == .i64_relop;
                const cmpop: FLIR.IntCond = tag.into() orelse .eq; // AHAHHAHAHA

                const peekinst: defs.OpCode = @enumFromInt(r.peekByte());
                if (peekinst == .br_if or peekinst == .if_) {
                    _ = try ir.icmp(node, iSize(wide), cmpop, lhs, rhs);
                    cond_pending = true;
                } else {
                    const res = try ir.icmpset(node, iSize(wide), cmpop, lhs, rhs);
                    try value_stack.append(gpa, res);
                }
            },

            .i32_unop, .i64_unop => |tag| {
                const src = value_stack.pop().?;
                const flir_op: FLIR.IntUnOp = switch (tag) {
                    .clz => .clz,
                    .ctz => .ctz,
                    .popcount => .popcount,
                };
                const res = try ir.iunop(node, iSize(inst == .i64_unop), flir_op, src);
                try value_stack.append(gpa, res);
            },
            .i32_sext, .i64_sext => |size| {
                const src = value_stack.pop().?;
                const flir_op: FLIR.IntUnOp = switch (size) {
                    .byte => .sign_extend8,
                    .word => .sign_extend16,
                    .dword => .sign_extend32,
                    .quadword => unreachable,
                };
                const res = try ir.iunop(node, iSize(inst == .i64_sext), flir_op, src);
                try value_stack.append(gpa, res);
            },

            .i_load => |i| {
                var addr = value_stack.pop().?;
                if (i.offset > 0) {
                    // WIDE because u33
                    addr = try ir.ibinop(node, .quadword, .add, addr, try ir.const_uint(i.offset));
                }
                const load = try ir.load(node, i.wide, i.sext, .{ .intptr = i.memsize }, mem_base, addr, 0);
                try value_stack.append(gpa, load);
            },
            .i_store => |i| {
                const val = value_stack.pop().?;
                var addr = value_stack.pop().?;
                if (i.offset > 0) {
                    // WIDE because u33
                    addr = try ir.ibinop(node, .quadword, .add, addr, try ir.const_uint(i.offset));
                }
                _ = try ir.store(node, .{ .intptr = i.memsize }, mem_base, addr, 0, val);
            },

            .f32_binop, .f64_binop => |tag| {
                const rhs = value_stack.pop().?;
                const lhs = value_stack.pop().?;
                const theop: forklift.X86Asm.VMathOp = switch (tag) {
                    .add => .add,
                    .sub => .sub,
                    .mul => .mul,
                    .div => .div,
                    .min => .min,
                    .max => .max,
                    .copysign => return error.NotImplemented,
                };
                const fmode: forklift.X86Asm.FMode = if (inst == .f64_binop) .sd else .ss;
                const val = try ir.vmath(node, theop, fmode, lhs, rhs);
                if (theop == .min or theop == .max) {
                    const lhs_isnan = try ir.vcmpf(node, .unord, fmode, lhs, lhs);
                    const nanblend = try ir.vblendf(node, fmode, val, lhs, lhs_isnan);
                    try value_stack.append(gpa, nanblend);
                } else {
                    try value_stack.append(gpa, val);
                }
            },
            .select => |_| {
                const pred = value_stack.pop().?;
                const val1 = value_stack.pop().?;
                const val2 = value_stack.pop().?;
                // TODO: NOT ALWAYS WIIIDE
                const val = try ir.select(node, true, pred, val1, val2);
                try value_stack.append(gpa, val);
            },
            .f32_unop, .f64_unop => |tag| {
                const ival = value_stack.pop().?;
                const theop: forklift.defs.VUnOp = switch (tag) {
                    .floor => .floor,
                    .ceil => .ceil,
                    .trunc => .trunc,
                    .nearest => .nearest,
                    .sqrt => .sqrt,
                    else => return error.NotImplemented,
                };
                const fmode: forklift.X86Asm.FMode = if (inst == .f64_unop) .sd else .ss;
                const val = try ir.vunop(node, theop, fmode, ival);
                try value_stack.append(gpa, val);
            },
            .int_reinterpret_float => |wide| {
                const fval = value_stack.pop().?;
                const val = try ir.float2int(node, .bitcast, if (wide) .sd else .ss, fval);
                try value_stack.append(gpa, val);
            },
            .float_reinterpret_int => |wide| {
                _ = wide;
                return error.NotImplemented;
            },
            .call => |idx| {
                // TODO: reunite functypes
                const call, const typidx, const n_params, const n_res = call: {
                    if (idx < in.mod.n_funcs_import) {
                        const imp = &in.funcs_imported[idx];
                        // todo: unified ABI for callbacks
                        const addr = imp.cb_direct orelse return error.NotImplemented;
                        const typ = in.mod.funcs_imported_types[idx];
                        break :call .{ try ir.call(node, .fun_addr, try ir.const_uint(@intFromPtr(addr))), typ, imp.n_args, imp.n_res };
                    }
                    if (idx >= in.mod.n_funcs_import + in.mod.funcs_internal.len) return error.InvalidFormat;
                    const func = &in.mod.funcs_internal[idx - in.mod.n_funcs_import];
                    _ = try func.ensure_parsed(in.mod); // TODO: aschually only need the type, maybe we should preparse mod.funcs[*].local_types|res_types early??
                    const obj = try self.declareFunc(func, null);
                    func.hmt_call_emitted = true;

                    break :call .{ try ir.call(node, .cfo_obj, try ir.const_uint(obj)), func.typeidx, func.n_params, func.n_res };
                };

                const max_params = 4;

                if (n_params > max_params or n_res > 1) {
                    f.hmt_error = try std.fmt.allocPrint(gpa, "THERE WERE NO CALLS TODAY: {} => {}", .{ n_params, n_res });
                    return error.NotImplemented;
                }

                // TODO: just has all types in pre-parsed form anyway??
                // would just be a cute little arena at module-parse time
                var call_arg_types: [max_params]defs.ValType = undefined;
                var call_res_types: [1]defs.ValType = undefined;
                try in.mod.type_params(typidx, &call_arg_types, &call_res_types);

                var arglist = call;
                arglist = try ir.callarg(node, arglist, mem_base, .{ .intptr = .quadword });
                arglist = try ir.callarg(node, arglist, mem_size, .{ .intptr = .quadword });
                if (n_params > value_stack.items.len) return error.InternalCompilerError;
                const argbase = value_stack.items.len - n_params;
                for (0..n_params) |i| {
                    const argtyp = specType(call_arg_types[i]) orelse return error.NotImplemented;
                    arglist = try ir.callarg(node, arglist, value_stack.items[argbase + i], argtyp);
                }
                value_stack.items.len = argbase;
                if (n_res > 0) {
                    const typ = specType(call_res_types[0]) orelse return error.NotImplemented;
                    const res = try ir.callret(call, typ);
                    try value_stack.append(gpa, res);
                }
            },
            .memory_size => {
                // const size: i32 = @intCast(in.mem.items.len / 0x10000);
                // TODO: if we use guard pages, mem_size=ir.arg() could just be the page count
                const calc = try ir.ibinop(node, .dword, .shr, mem_size, try ir.const_uint(16));
                try value_stack.append(gpa, calc);
            },
            .other__fixme => |tag| {
                f.hmt_error = try std.fmt.allocPrint(gpa, "inst {s} TBD", .{@tagName(tag)});
                return error.NotImplemented;
            },
        }
    }
    if (verbose) ir.debug_print();

    try ir.test_analysis(FLIR.X86ABI, true);
    if (verbose) ir.print_intervals();
    if (verbose) ir.debug_print();

    // TODO: abstraction

    // this is a very silly song
    const obj = try self.declareFunc(f, null);
    const target = try forklift.codegen_x86_64(ir, &self.mod, false, obj);
    _ = try self.declareFunc(f, target);

    if (f.exported == null) return;

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
    if (f.n_res > 0) try cfo.push(.rcx);

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

    // rdi, rsi kept for memory
    const ireg: [max_args]IPReg = .{ .rdx, .rcx, .r8, .r9, undefined };
    try cfo.mov(true, .r10, .rdx);

    var i_count: u32 = 0;
    var f_count: u32 = 0;

    for (arg_types, 0..) |t, i| {
        const wher: X86Asm.EAddr = X86Asm.a(.r10).o(@intCast(@sizeOf(defs.StackValue) * i));
        switch (t) {
            .i32, .i64 => {
                if (i_count >= 4) return error.NotImplemented;
                try cfo.movrm(t == .i64, ireg[i_count], wher);
                i_count += 1;
            },
            .f32, .f64 => {
                try cfo.vmovurm(if (t == .f64) .sd else .ss, @intCast(f_count), wher);
                f_count += 1;
            },
            else => return error.NotImplemented,
        }
    }
    try cfo.call_rel(target);
    if (f.n_res > 0) try cfo.pop(.rcx);
    for (0..f.n_res) |i| {
        const typ = f.res_types[i];
        if (i > 0) return error.NotImplemented;
        switch (typ) {
            .i32, .i64 => try cfo.movmr(typ == .i64, X86Asm.a(.rcx), .rax), // only one,
            .f32, .f64 => try cfo.vmovumr(if (typ == .f64) .sd else .ss, X86Asm.a(.rcx), 0),
            else => return error.NotImplemented,
        }
    }

    try cfo.zero(.rax); // non-error exit
    // as a silly trick, setjmp target here? nice for debugging
    if (frame) try cfo.leave();
    try cfo.ret();

    f.hmt_trampoline = @intCast(self.mod.objs.items.len);
    try self.mod.objs.append(self.mod.gpa, .{ .obj = .{ .func = .{ .code_start = trampolin_target } }, .name = null });
}

pub fn errorStub(self: *HeavyMachineTool, f: *Function) !void {
    const stub_target = self.mod.code.get_target();
    var cfo = X86Asm{ .code = &self.mod.code, .long_jump_mode = true };
    // TODO: set some state. or maybe just the location is enough if the handler were to lookup $RIP
    try cfo.trap();
    _ = try self.declareFunc(f, stub_target);
}
