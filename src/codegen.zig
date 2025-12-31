const std = @import("std");
const mem = std.mem;
const FLIR = @import("./FLIR.zig");
const print = std.debug.print;
const X86Asm = @import("./X86Asm.zig");
const IPReg = defs.IPReg;
const Inst = FLIR.Inst;
const IPMCVal = defs.IPMCVal;
const uv = FLIR.uv;
const ValType = defs.ValType;
const FMode = X86Asm.FMode;
const VMathOp = X86Asm.VMathOp;
const EAddr = X86Asm.EAddr;
const AOp = X86Asm.AOp;
const CFOModule = @import("./CFOModule.zig");

const defs = @import("./defs.zig");
const options = defs.debug_options;
fn r(reg: IPReg) X86Asm.IPReg {
    return @enumFromInt(reg.id());
}

fn slotea(slotid: anytype) EAddr {
    const off: i32 = -8 * (1 + @as(i32, @intCast(slotid)));
    return X86Asm.a(.rbp).o(off);
}

fn memargea(argid: u32) EAddr {
    return X86Asm.a(.rbp).o(8 * @as(i32, @intCast(argid)));
}

fn movri_zero(cfo: *X86Asm, w: bool, dst: IPReg, src: i32) !void {
    if (src != 0) { // TODO: proper constval
        try cfo.movri((w and src < 0), r(dst), src);
    } else {
        // THANKS INTEL
        try cfo.zero(r(dst));
    }
}

fn regmovmc(cfo: *X86Asm, w: bool, dst: IPReg, src: IPMCVal) !void {
    switch (src) {
        .frameslot => |f| try cfo.movrm(w, r(dst), slotea(f)),
        .memarg => |f| try cfo.movrm(w, r(dst), memargea(f)),
        .ipreg => |reg| if (dst != reg) try cfo.mov(w, r(dst), r(reg)),
        .constval => |c| try movri_zero(cfo, w, dst, @intCast(c)),
        .constref, .constptr => |idx| {
            if (src == .constptr) {
                return error.WIPError;
            } else {
                try cfo.movrm(w, r(dst), X86Asm.rel_placeholder());
            }
            try cfo.code.func_constants.append(cfo.code.gpa, .{ .pos = cfo.code.get_target() - 4, .idx = idx });
        },
    }
}

fn avxmovconst(cfo: *X86Asm, fmode: FMode, dst: u4, const_idx: u16) !void {
    try cfo.vmovurm(fmode, dst, X86Asm.rel_placeholder());
    try cfo.code.func_constants.append(cfo.code.gpa, .{ .pos = cfo.code.get_target() - 4, .idx = const_idx });
}

fn regaritmc(cfo: *X86Asm, w: bool, op: AOp, dst: IPReg, src: IPMCVal) !void {
    switch (src) {
        // TODO: merge frameslot and memarg at the time we have a IPMCVal?
        .frameslot => |f| try cfo.aritrm(op, w, r(dst), slotea(f)),
        .memarg => |f| try cfo.aritrm(op, w, r(dst), memargea(f)),
        .ipreg => |reg| try cfo.arit(op, w, r(dst), r(reg)),
        .constval => |c| try cfo.aritri(op, w, r(dst), @intCast(c)),
        .constref, .constptr => return error.NotImplemented,
    }
}

fn mcmovreg(cfo: *X86Asm, w: bool, dst: IPMCVal, src: IPReg) !void {
    switch (dst) {
        .frameslot => |f| try cfo.movmr(w, slotea(f), r(src)),
        .memarg => |f| try cfo.movmr(w, memargea(f), r(src)),
        .ipreg => |reg| if (reg != src) try cfo.mov(w, r(reg), r(src)),
        .constval, .constref, .constptr => return error.FLIRError, // AURORA BOREALIS?
    }
}

fn movmcs(cfo: *X86Asm, w: bool, dst: IPMCVal, src: IPMCVal) !void {
    // sadge
    // if (dst.mckind == src.mckind and dst.mcidx == src.mcidx) {
    //     return;
    // }
    // TODO: encode size of src so we can use 32-bit move sometimes
    switch (dst) {
        .ipreg => |reg| try regmovmc(cfo, w, reg, src),
        else => switch (src) {
            .ipreg => |reg| try mcmovreg(cfo, w, dst, reg),
            else => return error.SpillError, // TODO: mov [slot], imm32 (s.e. 64) OK
        },
    }
}

fn regswapmc(cfo: *X86Asm, w: bool, lhs: IPReg, rhs: IPMCVal) !void {
    switch (rhs) {
        .ipreg => |reg| try cfo.xchg(w, r(lhs), r(reg)),
        .frameslot => |f| try cfo.xchg_rm(w, r(lhs), slotea(f)),
        else => return error.FLIRError,
    }
}

fn swapmcs(cfo: *X86Asm, w: bool, dst: IPMCVal, src: IPMCVal) !void {
    switch (dst) {
        .ipreg => |reg| try regswapmc(cfo, w, reg, src),
        else => switch (src) {
            .ipreg => |reg| try regswapmc(cfo, w, reg, dst),
            else => return error.SpillError,
        },
    }
}
pub fn makejmp(self: *FLIR, cfo: *X86Asm, cond: ?X86Asm.Cond, ni: u16, si: u1, labels: []u32, targets: [][2]u32) !void {
    const succ = self.find_nonempty(self.n.items[ni].s[si]);
    // NOTE: we assume blk 0 always has the prologue (push rbp; mov rbp, rsp)
    // at least, so that even if blk 0 is empty, blk 1 has target larger than 0x00
    if (labels[succ] != 0) {
        try cfo.jbck(cond, labels[succ]);
    } else {
        targets[ni][si] = try cfo.jfwd(cond);
    }
}

// lol no co-recursive inferred error sets
const EERROR = error{ SpillError, @"TODO: unfused lea", FLIRError };
fn get_eaddr(self: *FLIR, ref: u16) EERROR!EAddr {
    const i = self.iref(ref).?;
    if (i.tag == .alloc) {
        return slotea(i.op1);
    } else if (i.tag == .lea) {
        if (i.ipreg()) |addr| return X86Asm.a(r(addr));
        return get_eaddr_load_or_lea(self, i.*);
    } else {
        const base = i.ipreg() orelse return error.SpillError;
        return X86Asm.a(r(base));
    }
}

// i must either be a load or lea instruction.
fn get_eaddr_load_or_lea(self: *FLIR, i: Inst) !EAddr {

    // TODO: if base is an un-fused .lea we should use its target
    // ipreg instead.
    var eaddr = try get_eaddr(self, i.op1);

    if (i.op2 == FLIR.NoRef) return eaddr;
    const idx = self.ipval(i.op2) orelse return error.FLIRError;
    switch (idx) {
        .ipreg => |reg| {
            if (eaddr.index != null) {
                return error.@"TODO: unfused lea";
            }
            eaddr.index = r(reg);
            eaddr.scale = i.scale();
        },
        .constval => |c| {
            eaddr = eaddr.o(@intCast(c)); // TODO: scale just disappears??
        },
        else => return error.SpillError,
    }
    return eaddr;
}

pub fn set_pred(self: *FLIR, cfo: *X86Asm, targets: [][2]u32, ni: u16) !void {
    var iter = self.predIter(@intCast(ni));
    while (iter.next()) |pred| {
        const pr = &self.n.items[pred];
        if (pr.is_empty) {
            try set_pred(self, cfo, targets, pred);
        } else {
            const si: u1 = if (pr.s[0] == ni) 0 else 1;
            if (targets[pred][si] != 0) {
                try cfo.set_target_jmp(targets[pred][si]);
                targets[pred][si] = 0;
            }
        }
    }
}

// TODO: a lot of codegen.zig should be shared between plattforms
const ABI = FLIR.X86ABI;

pub fn codegen(self: *FLIR, mod: *CFOModule, dbg: bool, owner_obj_idx: ?u32) !u32 {
    const labels = try self.gpa.alloc(u32, self.n.items.len);
    const targets = try self.gpa.alloc([2]u32, self.n.items.len);
    defer self.gpa.free(labels);
    defer self.gpa.free(targets);

    const code = &mod.code;
    var cfo = X86Asm{ .code = code };
    @memset(labels, 0);
    @memset(targets, .{ 0, 0 });

    cfo.long_jump_mode = true; // TODO: as an option

    const entry = code.get_target();

    // TODO: bull, this should be an instruction
    if (options.dbg_trap) {
        try cfo.trap();
    }

    const stacksize = 8 * @as(i32, self.nslots);
    const do_prelude = self.n.items.len > 2 or stacksize > 0 or self.codegen_has_call; // PÅ ETT UNGEFÄR

    if (do_prelude) try cfo.enter();
    var odd_stack = !do_prelude; // it is even with do_prelude because RET + FRAME
    for (ABI.callee_saved[0..self.nsave]) |reg| {
        try cfo.push(r(reg));
        odd_stack = !odd_stack;
    }
    if (do_prelude) {
        const padding = (-stacksize - (if (odd_stack) @as(i32, 8) else 0)) & 0xF;
        try cfo.aritri(.sub, true, .rsp, stacksize + padding);
    }

    for (self.n.items, 0..) |*n, ni| {
        if ((n.dfnum == 0 or n.is_empty) and ni > 0) {
            // non-entry block not reached by df search is dead.
            // TODO: these should already been cleaned up at this point
            continue;
        }
        const n_target = code.get_target();
        labels[ni] = n_target;
        if (dbg) print("block {}: {x}\n", .{ ni, n_target });

        // set jump targets of past blocks which jump forward here
        try set_pred(self, &cfo, targets, uv(ni));

        if (n.npred > 1) {
            if (options.dbg_regmap) {
                self.print_debug_map(uv(ni), n_target);
            }
            if (options.dbg_trap_join_nodes) {
                try cfo.trap();
            }
        }

        var cond: ?X86Asm.Cond = null;
        var it = self.ins_iterator(n.firstblk);
        var swap_source: ?defs.IPMCVal = null;
        while (it.next()) |item| {
            const i = item.i;

            switch (i.tag) {
                .freelist => @panic("KATASTROFEN"),
                .phi => {
                    // work is done by putphi, this is just optional debug info
                    if (options.dbg_trap_join_nodes) {
                        if (i.ipreg()) |reg| {
                            if (self.dbg_get_varname(item.ref)) |nam| {
                                try code.value_map.append(self.gpa, .{ .pos = n_target, .reg = reg, .name = nam });
                            }
                        }
                    }
                },
                .arg => {
                    // TODO: if "arg" is like a move instruction it should be a movins!
                    // ALSO TRICKY: should i.op1 actually be the specific register?
                    switch (i.mem_type()) {
                        .intptr => |size| {
                            if (i.op1 < ABI.argregs.len) {
                                const src = ABI.argregs[i.op1];
                                const dst = i.ipval() orelse return error.FLIRError;
                                try mcmovreg(&cfo, size == .quadword, dst, src);
                            } else {
                                const argidx = i.op1 - 6;
                                // TODO: when handning spilling, we need to handle the special case
                                // of values beginning as spill slots (as we treat parent frame as
                                // immutable, we can load it multiple times)
                                const dst = i.ipreg() orelse return error.FLIRError;
                                // 0 and 1 are caller's RBP and the return address, respectively
                                const ipval: IPMCVal = .{ .memarg = @intCast(2 + argidx) };
                                try regmovmc(&cfo, true, dst, ipval);
                            }
                        },
                        .avxval => |fmode| {
                            const reg = i.avxreg() orelse return error.FLIRError;
                            if (i.op1 != reg) {
                                try cfo.vmovf(fmode, 0, reg);
                            }
                        },
                    }
                },
                // lea relative RBP when used
                .alloc => {},
                .iunop => {
                    const src = self.ipval(i.op1) orelse return error.FLIRError;
                    const dst = i.ipreg() orelse return error.SpillError;
                    const op = i.iunop();
                    const w = i.f.wide;

                    if (op.is_bitop()) {
                        switch (src) {
                            .frameslot, .memarg => |_| return error.WIPError,
                            .ipreg => |reg| try cfo.bitunop(op, w, r(dst), r(reg)),
                            .constval, .constref, .constptr => return error.FLIRError,
                        }
                    } else if (op.is_sign_extend()) |size| {
                        switch (src) {
                            .frameslot, .memarg => |_| return error.WIPError,
                            .ipreg => |reg| try cfo.movsx(w, r(dst), r(reg), size),
                            .constval, .constref, .constptr => return error.FLIRError,
                        }
                    } else return error.NotImplemented;
                },
                .ibinop => {
                    var lhs = self.ipval(i.op1) orelse return error.FLIRError;
                    var rhs = self.ipval(i.op2) orelse return error.FLIRError;
                    const dst = i.ipreg() orelse return error.SpillError;
                    const op = i.ibinop();
                    const w = i.f.wide;

                    var dst_conflict = (dst == rhs.as_ipreg() and dst != lhs.as_ipreg());
                    // TODO: later on all these cases might be handled earlier in isel
                    if (dst_conflict and op.symmetric()) {
                        mem.swap(IPMCVal, &lhs, &rhs);
                        dst_conflict = false;
                    }

                    if (op.asAOP()) |aop| {
                        // TODO: fugly: remove once we have constraint handling in isel/regalloc
                        if (dst_conflict) @panic("conflict!");
                        try regmovmc(&cfo, w, dst, lhs); // often elided if lhs has the same reg
                        try regaritmc(&cfo, w, aop, dst, rhs);
                    } else if (op == .mul) {
                        switch (rhs) {
                            .constval => |c| {
                                const src = lhs.as_ipreg() orelse dstsrc: {
                                    try regmovmc(&cfo, w, dst, lhs);
                                    break :dstsrc dst;
                                };
                                try cfo.imulrri(w, r(dst), r(src), @intCast(c));
                            },
                            .ipreg => if (rhs.as_ipreg()) |rhsreg| {
                                try regmovmc(&cfo, w, dst, lhs);
                                try cfo.imulrr(w, r(dst), r(rhsreg));
                            },
                            else => return error.SpillError, // TODO: can be mem if lhs reg
                        }
                    } else if (op.asShift()) |sop| {
                        switch (rhs) {
                            .constval => |c| {
                                try regmovmc(&cfo, w, dst, lhs);
                                try cfo.sh_ri(w, r(dst), sop, @intCast(c));
                            },
                            .ipreg => |src2| {
                                const src1 = lhs.as_ipreg() orelse src1: {
                                    if (dst == src2) return error.FLIRError; // conflict
                                    try regmovmc(&cfo, w, dst, lhs);
                                    break :src1 dst;
                                };
                                // TODO: hint so we can use ECX
                                try cfo.sx(w, sop, r(dst), r(src1), r(src2));
                            },
                            else => return error.SpillError,
                        }
                    } else if (op == .rotr or op == .rotl) {
                        const is_right = op == .rotr;
                        try regmovmc(&cfo, w, dst, lhs);
                        switch (rhs) {
                            .constval => |c| {
                                try cfo.rot_ri(w, r(dst), is_right, @intCast(c));
                            },
                            .ipreg => |count| {
                                if (r(count) != .rcx) {
                                    if (r(dst) == .rcx) return error.TODO;
                                    try cfo.mov(false, .rcx, r(count));
                                }
                                try cfo.rot_rc(w, r(dst), is_right);
                            },
                            else => return error.NotImplemented,
                        }
                    } else if (op == .sdiv or op == .udiv or op == .srem or op == .urem) {
                        switch (lhs) {
                            .constval => |c| {
                                try cfo.movri(w, .rax, c);
                            },
                            .ipreg => |reg| {
                                if (r(reg) != .rax) {
                                    try cfo.mov(w, .rax, r(reg));
                                }
                            },
                            else => {
                                return error.NotImplemented;
                            },
                        }
                        const rhs_reg = switch (rhs) {
                            .constval => return error.NotImplemented,
                            .ipreg => |reg| r(reg),
                            else => return error.NotImplemented, // @panic("implement 'div [mem]' form")
                        };
                        if (rhs_reg == .rax) return error.CRINGE; // just checking...
                        try cfo.zero(.rdx); // baaaa
                        const signed = op == .sdiv or op == .srem;
                        if (signed) try cfo.cdq_cqo(w);
                        try cfo.div(w, rhs_reg, signed);
                        const fixed_res: X86Asm.IPReg = if (op == .srem or op == .urem) .rdx else .rax;
                        if (r(dst) != fixed_res) try cfo.mov(w, r(dst), fixed_res);
                    } else {
                        return error.NotImplemented;
                    }
                },
                .icmp, .icmpset => {
                    // TODO: we can actually cmp [slot], reg as well as cmp reg, [slot].
                    // just `cmp [slot1], [slot2]` is a violation
                    // although "cmp imm, reg" needs an operand swap.
                    // best if an earlier ABI step takes care of all of this
                    const lhs = self.ipreg(i.op1) orelse return error.SpillError;
                    const rhs = self.ipval(i.op2) orelse return error.FLIRError;
                    try regaritmc(&cfo, i.f.wide, .cmp, lhs, rhs);
                    const xcond = i.intcond().asX86Cond();
                    if (i.tag == .icmp) {
                        cond = xcond;
                    } else {
                        const dst = i.ipreg() orelse return error.SpillError;
                        try cfo.set(r(dst), xcond orelse return error.FLIRError);
                        try cfo.movzx_byte(r(dst), r(dst)); // SILLY!
                    }
                },
                // parallel move family
                // callret wants to be be friends :pleading_face:
                .putphi, .callarg, .retval => {
                    // fubbigt: the idea is than movins_read will return null
                    // exactly when reading an UNDEF, incase this insn becomes a no-op
                    const w = true; // TODO: aaaaaaaaaaaa
                    if (try self.movins_read_val(i)) |readval| switch (readval) {
                        .ipval => |src| {
                            const dest = (try self.movins_dest(i)).ipval() orelse return error.FLIRError;
                            if (i.f.do_swap) {
                                if (swap_source) |swap_src| {
                                    try swapmcs(&cfo, w, dest, swap_src); // PLOCKA INTE UPP DEN
                                } else {
                                    try swapmcs(&cfo, w, dest, src);
                                    swap_source = src;
                                }
                            } else if (i.f.swap_done) {
                                // do nothing, for N cyclic values we do N-1 swaps
                                // but clear the state for another group
                                swap_source = null;
                            } else {
                                // TODO: phi of avxval
                                try movmcs(&cfo, w, dest, src);
                            }
                        },
                        .avxreg => |src| {
                            if (i.f.do_swap) {
                                // nooooo you cannot; haha avx vals go swappy
                                return error.NotImplemented;
                            } else if (i.f.swap_done) {
                                // swap_source_avx = null;
                            } else {
                                const dest = try self.movins_dest(i);
                                const destreg = dest.avxreg() orelse return error.FLIRError;
                                // TODO: always mem_type of dest??
                                try cfo.vmovf(dest.mem_type().avxval, destreg, src);
                            }
                        },
                    };
                },
                .load, .load_signext => {
                    const eaddr = addr: {
                        // TODO: doubplicated with lea!
                        if (self.constval(i.op1)) |val| {
                            // this probably should have been iseled earlier to lea + load:
                            // but we don't have a isel step :sob:
                            if (val > 0xFFFFFFFF) {
                                // TODO: assert, due to optimization/isel, that [const+reg] is mubbled into [reg+const]
                                if (i.op2 != FLIR.IZero) return error.FLIRError;
                                const addrtmp = i.ipreg() orelse return error.SpillError;
                                try cfo.movabs(r(addrtmp), val);
                                break :addr X86Asm.a(r(addrtmp));
                            }
                        }

                        break :addr try get_eaddr_load_or_lea(self, i.*);
                    };
                    const mem_type = i.mem_type();
                    switch (mem_type) {
                        .intptr => |size| {
                            // tbh, loading from memory into a spill slot is bit stupid
                            const dst = i.ipreg() orelse return error.SpillError;
                            if (i.tag == .load_signext) {
                                try cfo.movrm_sx(i.f.wide, r(dst), eaddr, size);
                            } else {
                                // always zeroextends to 64-bit, with no extra cost, i.f.wide ignored + cringe + didn't ask
                                try cfo.movrm_zx(r(dst), eaddr, size);
                            }
                        },
                        .avxval => |fmode| {
                            const dst = i.avxreg() orelse return error.FLIRError;
                            try cfo.vmovurm(fmode, dst, eaddr);
                        },
                    }
                },
                .lea => {
                    // TODO: spill spall supllit?
                    // const eaddr = X86Asm.qi(base, idx);
                    if (i.mckind == .fused) {
                        // oki
                    } else if (self.constval(i.op1)) |val| {
                        // TODO: assert, due to optimization/isel, that [const+reg] is mubbled into [reg+const]
                        if (i.op2 != FLIR.IZero) return error.FLIRError;
                        const dst = i.ipreg() orelse return error.SpillError;
                        try cfo.movabs(r(dst), val);
                    } else {
                        if (true) return error.NotImplemented;
                        const base = self.ipreg(i.op1) orelse return error.SpillError;
                        const idx = self.ipreg(i.op2) orelse return error.SpillError;
                        const dst = i.ipreg() orelse return error.SpillError;
                        try cfo.lea(dst, X86Asm.qi(base, idx));
                    }
                },
                .store => {
                    const eaddr = try get_eaddr(self, i.op1);
                    switch (i.mem_type()) {
                        .intptr => |size| {
                            const val = self.ipval(i.op2) orelse return error.FLIRError;
                            switch (val) {
                                .ipreg => |reg| try cfo.movmr_size(eaddr, r(reg), size),
                                .constval => |c| {
                                    switch (size) {
                                        .byte => {
                                            try cfo.movmi_byte(eaddr, @truncate(@as(u32, @bitCast(c))));
                                        },
                                        .quadword => try cfo.movmi(true, eaddr, @intCast(c)),
                                        .dword => try cfo.movmi(false, eaddr, @intCast(c)),
                                        else => return error.NotImplemented,
                                    }
                                },
                                else => return error.SpillError,
                            }
                        },
                        .avxval => |fmode| {
                            const src = self.avxreg(i.op2) orelse return error.FLIRError;
                            try cfo.vmovumr(fmode, eaddr, src);
                        },
                    }
                },
                .select => {
                    return error.NotImplemented;
                },
                .fconst => {
                    const dst = i.avxreg() orelse return error.FLIRError;
                    const uconst = self.constval(i.op1) orelse return error.FLIRError;
                    if (uconst == 0) { // also "good enough" for f64 -0.0 and f32 -0.0 ????
                        // scalar xor doesn't exist, this seems to be the standard messaround
                        const fmode_adj: FMode = switch (i.fmode_op()) {
                            .sd => .pd2,
                            .ss => .ps4,
                            else => |m| m,
                        };
                        try cfo.vbitopf(.xor, fmode_adj, dst, dst, dst);
                    } else {
                        // TODO: pd/ps should mean "broadcast"
                        if (!i.fmode_op().scalar()) return error.WIPError;
                        try avxmovconst(&cfo, i.fmode_op(), dst, FLIR.constidx(i.op1).?);
                    }
                },
                .vmath => {
                    const x = self.avxreg(i.op1) orelse return error.FLIRError;
                    const y = self.avxreg(i.op2) orelse return error.FLIRError;
                    const dst = i.avxreg() orelse return error.FLIRError;
                    try cfo.vmathf(i.vmathop(), i.fmode_op(), dst, x, y);
                },
                .vcmpf => {
                    const x = self.avxreg(i.op1) orelse return error.FLIRError;
                    const y = self.avxreg(i.op2) orelse return error.FLIRError;
                    const dst = i.avxreg() orelse return error.FLIRError;
                    try cfo.vcmpf(i.vcmpop(), i.fmode_op(), dst, x, y);
                },
                .fcmp => {
                    const x = self.avxreg(i.op1) orelse return error.FLIRError;
                    const y = self.avxreg(i.op2) orelse return error.FLIRError;
                    try cfo.fcmp(i.fmode_op(), x, y);
                    cond = i.fcmpop().asX86Cond();
                },
                .vblendf => {
                    const x = self.avxreg(i.op1) orelse return error.FLIRError;
                    const y = self.avxreg(i.op2) orelse return error.FLIRError;
                    const z = self.avxreg(i.next) orelse return error.FLIRError;
                    const dst = i.avxreg() orelse return error.FLIRError;
                    const mode = switch (i.fmode_op()) {
                        .ss => .ps4, // cannot blend scalars, so expand a little
                        .sd => .pd2,
                        else => |m| m,
                    };
                    try cfo.vblendv(mode, dst, x, y, z);
                },
                .vunop => {
                    const x = self.avxreg(i.op1) orelse return error.FLIRError;
                    const dst = i.avxreg() orelse return error.FLIRError;

                    switch (i.vunop()) {
                        .floor => try cfo.vround(9, i.fmode_op(), dst, x, x),
                        .ceil => try cfo.vround(10, i.fmode_op(), dst, x, x),
                        .trunc => try cfo.vround(11, i.fmode_op(), dst, x, x),
                        .nearest => try cfo.vround(11, i.fmode_op(), dst, x, x), // HAHAHAHA, yes!
                        .sqrt => try cfo.vsqrt(i.fmode_op(), dst, x, null),
                    }
                },
                .int2vf => {
                    const val = self.ipval(i.op1) orelse return error.FLIRError;
                    const dst = i.avxreg() orelse return error.FLIRError;
                    const ispec: defs.Int2VOp = @enumFromInt(i.low_spec());
                    switch (ispec) {
                        .convert => switch (val) {
                            // TODO:
                            // .frameslot => |f| try cfo.movrm(w, r(dst), slotea(f)),
                            .frameslot, .memarg => return error.NotImplemented,
                            .ipreg => |reg| try cfo.vcvtsi2s_rr(i.fmode_op(), dst, true, r(reg)),
                            .constval, .constref, .constptr => return error.FLIRError, // mandatory cfold for things like this?
                        },
                        .bitcast => {
                            return error.NotImplemented;
                        },
                    }
                },
                .vf2int => {
                    const src = self.avxreg(i.op1) orelse return error.FLIRError;
                    const dst = i.ipreg() orelse return error.FLIRError;
                    const ispec: defs.V2IntOp = @enumFromInt(i.low_spec());
                    switch (ispec) {
                        .convert => try cfo.vcvtsx2si_rr(i.fmode_op(), true, r(dst), src),
                        .bitcast => try cfo.vmovdq_iv(i.fmode_op().double(), r(dst), src),
                    }
                },
                .call => {
                    const kind: defs.CallKind = @enumFromInt(i.spec);
                    switch (kind) {
                        .syscall => {
                            try regmovmc(&cfo, false, X86Asm.IPReg.rax.into(), self.ipval(i.op1).?);
                            try cfo.syscall();
                        },
                        .near => {
                            const val = self.constval(i.op1) orelse return error.FLIRError;
                            try cfo.call_rel(@intCast(val));
                        },
                        .fun_addr => {
                            if (self.constval(i.op1)) |val| {
                                // TODO: this really should be allocated. custom callconv
                                // might actually use RAX..
                                try cfo.movabs(.rax, val);
                                try cfo.call_ptr(.rax);
                            } else {
                                return error.NotImplemented;
                            }
                        },
                        .cfo_obj => {
                            const idx = self.constval(i.op1) orelse return error.FLIRError;
                            const off = switch (mod.objs.items[idx].obj) {
                                .func => |f| f.code_start,
                                else => return error.FLIRError,
                            };
                            try cfo.call_rel(@intCast(off));
                            // only relocate later if unknown
                            if (off == defs.INVALID_OFFSET) {
                                try mod.relocations.append(code.gpa, .{ .pos = mod.code.get_target() - 4, .obj_idx = @intCast(idx), .user_obj_idx_for_debugging = owner_obj_idx });
                            }
                        },
                        .memory_intrinsic => {
                            const idx = self.constval(i.op1) orelse return error.FLIRError;
                            const size: defs.ISize = @enumFromInt(i.op2);
                            const intrinsic: defs.MemoryIntrinsic = @enumFromInt(idx);
                            if (intrinsic == .memset) {
                                try cfo.stos(true, size);
                            }
                        },
                        else => return error.FLIRError,
                    }
                },
                // TODO: into parallel move group
                .callret => {
                    switch (i.mem_type()) {
                        .intptr => |size| {
                            const src: IPReg = @enumFromInt(i.op2);
                            const dest = i.ipval() orelse return error.FLIRError;
                            try mcmovreg(&cfo, size == .quadword, dest, src); // TODO: wide
                        },
                        .avxval => |fmode| {
                            const src: u4 = @intCast(i.op2);
                            const dest = i.avxreg() orelse return error.FLIRError;
                            if (dest != src) {
                                try cfo.vmovf(fmode, dest, src);
                            }
                        },
                    }
                },
                .copy => {
                    // TODO: of course also avxvals can be copied!
                    const src = self.ipval(i.op1) orelse return error.FLIRError;
                    const dest = i.ipval() orelse return error.FLIRError;
                    try movmcs(&cfo, true, dest, src); // TODO: wide
                },
                .trap => {
                    try cfo.trap(); // PALLAS CAT EARLY
                },
                .bpf_load_map => {
                    print("platform unsupported: {}\n", .{i.tag});
                    return error.FLIRError;
                },
                .variable, .putvar, .xadd => {
                    print("unhandled tag: {}\n", .{i.tag});
                    return error.FLIRError;
                },
            }
        }

        if (n.s[1] != 0) {
            try makejmp(self, &cfo, cond.?, uv(ni), 1, labels, targets);
        }
        if (n.s[0] != 0) {
            const succ = self.find_nonempty(self.n.items[ni].s[0]);
            var fallthru = uv(ni + 1);
            // TODO: This is quite ad-hoc. need a proper CFG clean-up pass
            // post-resolution (including reversing conditions if s[1] is fallthrough)
            while (fallthru < succ) {
                if (fallthru >= self.n.items.len) @panic("le banik");
                const fnode = &self.n.items[fallthru];
                if (fnode.is_empty and fnode.s[0] == fallthru + 1) {
                    fallthru = fallthru + 1;
                } else {
                    break;
                }
            }
            if (succ != fallthru) {
                try makejmp(self, &cfo, null, uv(ni), 0, labels, targets);
            }
        }
    }

    var isave = self.nsave;
    while (isave > 0) {
        isave -= 1;
        try cfo.pop(r(ABI.callee_saved[isave]));
    }
    if (do_prelude) try cfo.leave();
    try cfo.ret();

    if (cfo.code.func_constants.items.len > 0) {
        // TODO: have this as scratch space already in FLIR.constvals
        var const_targets = try self.gpa.alloc(?u32, self.constvals.items.len);
        defer self.gpa.free(const_targets);
        @memset(const_targets, null);

        while (cfo.code.buf.items.len & 7 != 0) {
            try cfo.wb(0);
        }

        for (cfo.code.func_constants.items) |reloc| {
            const target = found_target: {
                if (const_targets[reloc.idx]) |t| {
                    break :found_target t;
                } else {
                    const t = cfo.code.get_target();
                    const_targets[reloc.idx] = t;
                    try cfo.wq(self.constvals.items[reloc.idx]);
                    break :found_target t;
                }
            };
            cfo.set_target_32(reloc.pos, target);
        }
        code.func_constants.items.len = 0; // private to function
    }
    if (options.dbg_disasm) try cfo.dbg_nasm(self.gpa);
    return entry;
}
