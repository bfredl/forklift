const std = @import("std");
const mem = std.mem;
const FLIR = @import("./FLIR.zig");
const print = std.debug.print;
const X86Asm = @import("./X86Asm.zig");
const IPReg = common.IPReg;
const Inst = FLIR.Inst;
const IPMCVal = FLIR.IPMCVal;
const uv = FLIR.uv;
const ValType = FLIR.ValType;
const FMode = X86Asm.FMode;
const VMathOp = X86Asm.VMathOp;
const EAddr = X86Asm.EAddr;
const AOp = X86Asm.AOp;
const CodeBuffer = @import("./CodeBuffer.zig");

const w = true; // TODO: not so fast

const common = @import("./common.zig");
const options = common.debug_options;
fn r(reg: IPReg) X86Asm.IPReg {
    return @enumFromInt(reg.id());
}

fn slotoff(slotid: anytype) i32 {
    return -8 * (1 + @as(i32, @intCast(slotid)));
}

fn slotea(slotid: anytype) EAddr {
    return X86Asm.a(.rbp).o(slotoff(slotid));
}

fn movri_zero(cfo: *X86Asm, dst: IPReg, src: i32) !void {
    if (src != 0) { // TODO: proper constval
        try cfo.movri(true, r(dst), src);
    } else {
        // THANKS INTEL
        try cfo.zero(r(dst));
    }
}

fn regmovmc(cfo: *X86Asm, dst: IPReg, src: IPMCVal) !void {
    switch (src) {
        .frameslot => |f| try cfo.movrm(w, r(dst), slotea(f)),
        .ipreg => |reg| if (dst != reg) try cfo.mov(w, r(dst), r(reg)),
        .constval => |c| try movri_zero(cfo, dst, @intCast(c)),
        .constref, .constptr => |idx| {
            if (src == .constptr) {
                return error.WIPError;
            } else {
                try cfo.movrm(w, r(dst), X86Asm.rel_placeholder());
            }
            try cfo.code.relocations.append(.{ .pos = cfo.code.get_target() - 4, .idx = idx });
        },
    }
}

fn avxmovconst(cfo: *X86Asm, fmode: FMode, dst: u4, const_idx: u16) !void {
    try cfo.vmovurm(fmode, dst, X86Asm.rel_placeholder());
    try cfo.code.relocations.append(.{ .pos = cfo.code.get_target() - 4, .idx = const_idx });
}

fn regaritmc(cfo: *X86Asm, op: AOp, dst: IPReg, src: IPMCVal) !void {
    switch (src) {
        .frameslot => |f| try cfo.aritrm(op, w, r(dst), X86Asm.a(.rbp).o(slotoff(f))),
        .ipreg => |reg| try cfo.arit(op, w, r(dst), r(reg)),
        .constval => |c| try cfo.aritri(op, w, r(dst), @intCast(c)),
        .constref, .constptr => {
            @panic("le wip");
        },
    }
}

fn mcmovreg(cfo: *X86Asm, dst: IPMCVal, src: IPReg) !void {
    switch (dst) {
        .frameslot => |f| try cfo.movmr(w, slotea(f), r(src)),
        .ipreg => |reg| if (reg != src) try cfo.mov(w, r(reg), r(src)),
        .constval, .constref, .constptr => return error.AURORA_BOREALIS,
    }
}

fn movmcs(cfo: *X86Asm, dst: IPMCVal, src: IPMCVal) !void {
    // sadge
    // if (dst.mckind == src.mckind and dst.mcidx == src.mcidx) {
    //     return;
    // }
    // TODO: encode size of src so we can use 32-bit move sometimes
    switch (dst) {
        .ipreg => |reg| try regmovmc(cfo, reg, src),
        else => switch (src) {
            .ipreg => |reg| try mcmovreg(cfo, dst, reg),
            else => return error.SpillError, // TODO: mov [slot], imm32 (s.e. 64) OK
        },
    }
}

fn regswapmc(cfo: *X86Asm, lhs: IPReg, rhs: IPMCVal) !void {
    switch (rhs) {
        .ipreg => |reg| try cfo.xchg(w, r(lhs), r(reg)),
        .frameslot => |f| try cfo.xchg_rm(w, r(lhs), slotea(f)),
        else => return error.FLIRError,
    }
}

fn swapmcs(cfo: *X86Asm, dst: IPMCVal, src: IPMCVal) !void {
    switch (dst) {
        .ipreg => |reg| try regswapmc(cfo, reg, src),
        else => switch (src) {
            .ipreg => |reg| try regswapmc(cfo, reg, dst),
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
        return X86Asm.a(.rbp).o(slotoff(i.op1));
    } else if (i.tag == .lea) {
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
    for (self.preds(ni)) |pred| {
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

pub fn codegen(self: *FLIR, code: *CodeBuffer, dbg: bool) !u32 {
    const labels = try self.a.alloc(u32, self.n.items.len);
    const targets = try self.a.alloc([2]u32, self.n.items.len);
    defer self.a.free(labels);
    defer self.a.free(targets);

    var cfo = X86Asm{ .code = code };
    @memset(labels, 0);
    @memset(targets, .{ 0, 0 });

    cfo.long_jump_mode = true; // TODO: as an option

    const entry = code.get_target();

    // TODO: bull, this should be an instruction
    if (options.dbg_trap) {
        try cfo.trap();
    }

    try cfo.enter();
    for (ABI.callee_saved[0..self.nsave]) |reg| {
        try cfo.push(r(reg));
    }
    const stacksize = 8 * @as(i32, self.nslots);
    if (stacksize > 0) {
        const padding = (-stacksize) & 0xF;
        // print("size: {}, extrasize: {}\n", .{ stacksize, padding });
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
        var swap_source: ?FLIR.IPMCVal = null;
        while (it.next()) |item| {
            const i = item.i;

            switch (i.tag) {
                .freelist => @panic("KATASTROFEN"),
                .phi => {
                    // work is done by putphi, this is just optional debug info
                    if (options.dbg_trap_join_nodes) {
                        if (i.ipreg()) |reg| {
                            if (self.get_varname(i.op1)) |nam| {
                                try code.value_map.append(.{ .pos = n_target, .reg = reg, .name = nam });
                            }
                        }
                    }
                },
                .arg => {
                    // TRICKY: should i.op1 actually be the specific register?
                    const src = ABI.argregs[i.op1];
                    const dst = i.ipval() orelse return error.FLIRError;
                    try mcmovreg(&cfo, dst, src);
                },
                // lea relative RBP when used
                .alloc => {},
                .ret => {
                    // TODO: extend i.res_type() to a useful SpecType from self.val(REF) ??
                    // then we won't need to encode the type on .ret separately...
                    switch (i.mem_type()) {
                        .intptr => |_| {
                            const ipval = self.ipval(i.op1) orelse return error.FLIRError;
                            try regmovmc(&cfo, X86Asm.IPReg.rax.into(), ipval);
                        },
                        .avxval => |fmode| {
                            const avxval = self.iref(i.op1) orelse return error.FLIRError;
                            const reg = avxval.avxreg() orelse return error.FLIRError;
                            if (reg != 0) {
                                try cfo.vmovf(fmode, 0, reg);
                            }
                        },
                    }
                },
                .ibinop => {
                    var lhs = self.ipval(i.op1) orelse return error.FLIRError;
                    var rhs = self.ipval(i.op2) orelse return error.FLIRError;
                    const dst = i.ipreg() orelse return error.SpillError;
                    const op: FLIR.IntBinOp = @enumFromInt(i.spec);

                    var dst_conflict = (dst == rhs.as_ipreg() and dst != lhs.as_ipreg());
                    // TODO: later on all these cases might be handled earlier in isel
                    if (dst_conflict and op.symmetric()) {
                        mem.swap(IPMCVal, &lhs, &rhs);
                        dst_conflict = false;
                    }

                    if (op.asAOP()) |aop| {
                        // TODO: fugly: remove once we have constraint handling in isel/regalloc
                        if (dst_conflict) @panic("conflict!");
                        try regmovmc(&cfo, dst, lhs); // often elided if lhs has the same reg
                        try regaritmc(&cfo, aop, dst, rhs);
                    } else if (op == .mul) {
                        switch (rhs) {
                            .constval => |c| {
                                const src = lhs.as_ipreg() orelse dstsrc: {
                                    try regmovmc(&cfo, dst, lhs);
                                    break :dstsrc dst;
                                };
                                try cfo.imulrri(true, r(dst), r(src), @intCast(c));
                            },
                            .ipreg => if (rhs.as_ipreg()) |rhsreg| {
                                try regmovmc(&cfo, dst, lhs);
                                try cfo.imulrr(true, r(dst), r(rhsreg));
                            },
                            else => return error.SpillError, // TODO: can be mem if lhs reg
                        }
                    } else if (op.asShift()) |sop| {
                        switch (rhs) {
                            .constval => |c| {
                                try regmovmc(&cfo, dst, lhs);
                                try cfo.sh_ri(r(dst), sop, @intCast(c));
                            },
                            .ipreg => |src2| {
                                const src1 = lhs.as_ipreg() orelse src1: {
                                    if (dst == src2) return error.FLIRError; // conflict
                                    try regmovmc(&cfo, dst, lhs);
                                    break :src1 dst;
                                };
                                try cfo.sx(sop, r(dst), r(src1), r(src2));
                            },
                            else => return error.SpillError,
                        }
                    }
                },
                .icmp => {
                    // TODO: we can actually cmp [slot], reg as well as cmp reg, [slot].
                    // just `cmp [slot1], [slot2]` is a violation
                    // although "cmp imm, reg" needs an operand swap.
                    // best if an earlier ABI step takes care of all of this
                    const lhs = self.ipreg(i.op1) orelse return error.SpillError;
                    const rhs = self.ipval(i.op2) orelse return error.FLIRError;
                    try regaritmc(&cfo, .cmp, lhs, rhs);
                    cond = @as(FLIR.IntCond, @enumFromInt(i.spec)).asX86Cond();
                },
                // parallel move family
                .putphi, .callarg => {
                    // fubbigt: the idea is than movins_read will return null
                    // exactly when reading an UNDEF, incase this insn becomes a no-op
                    if (try self.movins_read2(i)) |src| {
                        const dest = (try self.movins_dest(i)).ipval() orelse return error.FLIRError;
                        if (i.f.do_swap) {
                            if (swap_source) |swap_src| {
                                try swapmcs(&cfo, dest, swap_src); // PLOCKA INTE UPP DEN
                            } else {
                                try swapmcs(&cfo, dest, src);
                                swap_source = src;
                            }
                        } else if (i.f.swap_done) {
                            // do nothing, for N cyclic values we do N-1 swaps
                            // but clear the state for another group
                            swap_source = null;
                        } else {
                            // TODO: phi of avxval
                            try movmcs(&cfo, dest, src);
                        }
                    }
                },
                .load => {
                    const eaddr = try get_eaddr_load_or_lea(self, i.*);
                    const mem_type = i.mem_type();
                    switch (mem_type) {
                        .intptr => |size| {
                            // tbh, loading from memory into a spill slot is bit stupid
                            const dst = i.ipreg() orelse return error.SpillError;
                            switch (size) {
                                .byte => {
                                    try cfo.movrm_byte(w, r(dst), eaddr);
                                },
                                .quadword => try cfo.movrm(w, r(dst), eaddr),
                                else => return error.NotImplemented,
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
                    if (i.mckind == .fused) {} else {
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
                                .ipreg => |reg| {
                                    switch (size) {
                                        .byte => {
                                            try cfo.movmr_byte(eaddr, r(reg));
                                        },
                                        .quadword => try cfo.movmr(w, eaddr, r(reg)),
                                        else => return error.NotImplemented,
                                    }
                                },
                                .constval => |c| {
                                    switch (size) {
                                        .byte => {
                                            try cfo.movmi_byte(eaddr, @truncate(@as(u32, @bitCast(c))));
                                        },
                                        .quadword => try cfo.movmi(w, eaddr, @intCast(c)),
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
                .fconst => {
                    const dst = i.avxreg() orelse return error.FLIRError;
                    const fval: f64 = @bitCast(self.constval(i.op1) orelse return error.FLIRError);
                    if (fval == 0.0) {
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
                    cond = @as(FLIR.IntCond, @enumFromInt(i.spec)).asX86Cond();
                },
                .fcmp => {
                    const x = self.avxreg(i.op1) orelse return error.FLIRError;
                    const y = self.avxreg(i.op2) orelse return error.FLIRError;
                    try cfo.fcmp(i.fmode_op(), x, y);
                    cond = i.fcmpop().asX86Cond();
                },
                .int2vf => {
                    const val = self.ipval(i.op1) orelse return error.FLIRError;
                    const dst = i.avxreg() orelse return error.FLIRError;
                    // TODO: more ops than sitosd/sitoss
                    switch (val) {
                        // TODO:
                        // .frameslot => |f| try cfo.movrm(w, r(dst), X86Asm.a(.rbp).o(slotoff(f))),
                        .frameslot => return error.NotImplemented,
                        .ipreg => |reg| try cfo.vcvtsi2s_rr(i.fmode_op(), dst, true, r(reg)),
                        .constval, .constref, .constptr => return error.FLIRError, // mandatory cfold for things like this?
                    }
                },
                .vf2int => {
                    const src = self.avxreg(i.op1) orelse return error.FLIRError;
                    const dst = i.ipreg() orelse return error.FLIRError;
                    // TODO: more ops than sitosd/sitoss
                    try cfo.vcvtsx2si_rr(i.fmode_op(), true, r(dst), src);
                },
                .call => {
                    const kind: FLIR.CallKind = @enumFromInt(i.spec);
                    switch (kind) {
                        .syscall => {
                            try regmovmc(&cfo, X86Asm.IPReg.rax.into(), self.ipval(i.op1).?);
                            try cfo.syscall();
                        },
                        .near => {
                            const val = self.constval(i.op1) orelse return error.FLIRError;
                            try cfo.call_rel(@intCast(val));
                        },
                        .memory_intrinsic => {
                            const idx = self.constval(i.op1) orelse return error.FLIRError;
                            const size: common.ISize = @enumFromInt(i.op2);
                            const intrinsic: FLIR.MemoryIntrinsic = @enumFromInt(idx);
                            if (intrinsic == .memset) {
                                try cfo.stos(true, size);
                            }
                        },
                        else => return error.FLIRError,
                    }
                },
                .copy => {
                    // TODO: of course also avxvals can be copied!
                    const src = self.ipval(i.op1) orelse return error.FLIRError;
                    const dest = i.ipval() orelse return error.FLIRError;
                    try movmcs(&cfo, dest, src);
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
    try cfo.leave();
    try cfo.ret();

    if (cfo.code.relocations.items.len > 0) {
        // TODO: have this as scratch space already in FLIR.constvals
        var const_targets = try self.a.alloc(?u32, self.constvals.items.len);
        defer self.a.free(const_targets);
        @memset(const_targets, null);

        while (cfo.code.buf.items.len & 7 != 0) {
            try cfo.wb(0);
        }

        for (cfo.code.relocations.items) |reloc| {
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
    }
    if (options.dbg_disasm) try cfo.dbg_nasm(self.a);
    return entry;
}
