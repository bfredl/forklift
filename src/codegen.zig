const std = @import("std");
const mem = std.mem;
const FLIR = @import("./FLIR.zig");
const print = std.debug.print;
const CFO = @import("./CFO.zig");
const IPReg = CFO.IPReg;
const VMathOp = CFO.VMathOp;
const Inst = FLIR.Inst;
const uv = FLIR.uv;
const ValType = FLIR.ValType;
const EAddr = CFO.EAddr;

fn slotoff(slotid: anytype) i32 {
    return -8 * (1 + @intCast(i32, slotid));
}

fn regmovmc(cfo: *CFO, dst: IPReg, src: Inst) !void {
    switch (src.mckind) {
        .frameslot => try cfo.movrm(dst, CFO.a(.rbp).o(slotoff(src.mcidx))),
        .ipreg => {
            const reg = @intToEnum(IPReg, src.mcidx);
            if (dst != reg) try cfo.mov(dst, reg);
        },
        .fused => {
            if (src.tag != .constant) return error.TheDinnerConversationIsLively;
            if (src.op1 != 0) { // TODO: proper constval
                try cfo.movri(dst, src.op1);
            } else {
                // THANKS INTEL
                try cfo.zero(dst);
            }
        },
        else => return error.AAA_AA_A,
    }
}

fn regaritmc(cfo: *CFO, op: CFO.AOp, dst: IPReg, i: Inst) !void {
    switch (i.mckind) {
        .frameslot => try cfo.aritrm(op, dst, CFO.a(.rbp).o(-8 * @as(i32, i.mcidx))),
        .ipreg => {
            const reg = @intToEnum(IPReg, i.mcidx);
            try cfo.arit(op, dst, reg);
        },
        .fused => {
            if (i.tag != .constant) return error.GetLostHeIsNeverComingBack;
            try cfo.aritri(op, dst, i.op1); // TODO: proper constval

        },
        else => return error.AAA_AA_A,
    }
}

fn mcmovreg(cfo: *CFO, dst: Inst, src: IPReg) !void {
    switch (dst.mckind) {
        .frameslot => try cfo.movmr(CFO.a(.rbp).o(-8 * @as(i32, dst.mcidx)), src),
        .ipreg => {
            const reg = @intToEnum(IPReg, dst.mcidx);
            if (reg != src) try cfo.mov(reg, src);
        },
        else => return error.AAA_AA_A,
    }
}

fn mcmovi(cfo: *CFO, i: Inst) !void {
    switch (i.mckind) {
        .frameslot => try cfo.movmi(CFO.a(.rbp).o(-8 * @as(i32, i.mcidx)), i.op1),
        .ipreg => {
            const reg = @intToEnum(IPReg, i.mcidx);
            // TODO: cfo.movriz to check this condition ?
            if (i.op1 != 0) {
                try cfo.movri(reg, i.op1);
            } else {
                // THANKS INTEL
                try cfo.zero(reg);
            }
        },
        .fused => {}, // let user lookup value
        else => return error.AAA_AA_A,
    }
}

fn movmcs(cfo: *CFO, dst: Inst, src: Inst) !void {
    if (dst.mckind == src.mckind and dst.mcidx == src.mcidx) {
        return;
    }
    if (dst.mckind == .ipreg) {
        try regmovmc(cfo, @intToEnum(IPReg, dst.mcidx), src);
    } else if (src.mckind == .ipreg) {
        try mcmovreg(cfo, dst, @intToEnum(IPReg, src.mcidx));
    } else {
        @panic("unspill/respill");
    }
}

pub fn makejmp(self: *FLIR, cfo: *CFO, cond: ?CFO.Cond, ni: u16, si: u1, labels: []u32, targets: [][2]u32) !void {
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
const EERROR = error{ WHAAAAA, @"TODO: unfused lea", VOLKTANZ };
fn get_eaddr(self: *FLIR, i: Inst) EERROR!EAddr {
    if (i.tag == .alloc) {
        return CFO.a(.rbp).o(slotoff(i.op1));
    } else if (i.tag == .lea) {
        return get_eaddr_load_or_lea(self, i);
    } else {
        // TODO: spill spall supllit?
        const base = i.ipreg() orelse return error.WHAAAAA;
        return CFO.a(base);
    }
}

// i must either be a load or lea instruction.
fn get_eaddr_load_or_lea(self: *FLIR, i: Inst) !EAddr {
    const base = self.iref(i.op1).?.*;

    // TODO: if base is an un-fused .lea we should use its target
    // ipreg instead.
    var eaddr = try get_eaddr(self, base);

    const idx = self.iref(i.op2) orelse return eaddr;
    if (idx.ipreg()) |reg| {
        if (eaddr.index != null) {
            return error.@"TODO: unfused lea";
        }
        eaddr.index = reg;
        eaddr.scale = @intCast(u2, i.high_spec());
    } else if (idx.tag == .constant) {
        eaddr = eaddr.o(idx.op1);
    } else {
        return error.VOLKTANZ;
    }
    return eaddr;
}

pub fn set_pred(self: *FLIR, cfo: *CFO, targets: [][2]u32, ni: u16) !void {
    for (self.preds(ni)) |pred| {
        const pr = &self.n.items[pred];
        if (pr.is_empty) {
            try set_pred(self, cfo, targets, pred);
        } else {
            const si: u1 = if (pr.s[0] == ni) 0 else 1;
            if (targets[pred][si] != 0) {
                try cfo.set_target(targets[pred][si]);
                targets[pred][si] = 0;
            }
        }
    }
}

pub fn codegen(self: *FLIR, cfo: *CFO, dbg: bool) !u32 {
    var labels = try self.a.alloc(u32, self.n.items.len);
    var targets = try self.a.alloc([2]u32, self.n.items.len);
    defer self.a.free(labels);
    defer self.a.free(targets);
    mem.set(u32, labels, 0);
    mem.set([2]u32, targets, .{ 0, 0 });

    cfo.long_jump_mode = true; // TODO: as an option

    const target = cfo.get_target();
    try cfo.enter();
    for (FLIR.callee_saved[0..self.nsave]) |reg| {
        try cfo.push(reg);
    }
    const stacksize = 8 * @as(i32, self.nslots);
    if (stacksize > 0) {
        const padding = (-stacksize) & 0xF;
        // print("size: {}, extrasize: {}\n", .{ stacksize, padding });
        try cfo.aritri(.sub, .rsp, stacksize + padding);
    }

    for (self.n.items) |*n, ni| {
        if ((n.dfnum == 0 or n.is_empty) and ni > 0) {
            // non-entry block not reached by df search is dead.
            // TODO: these should already been cleaned up at this point
            continue;
        }
        labels[ni] = cfo.get_target();
        if (dbg) print("block {}: {x}\n", .{ ni, labels[ni] });

        // set jump targets of past blocks which jump forward here
        try set_pred(self, cfo, targets, uv(ni));

        var fused_inst: ?*Inst = null;
        var cond: ?CFO.Cond = null;
        var it = self.ins_iterator(n.firstblk);
        while (it.next()) |item| {
            const i = item.i;

            var was_fused: bool = false;
            switch (i.tag) {
                // empty doesn't flush fused value
                .empty => continue,
                // these expect their values to be in place when executed
                .arg, .phi => {},
                // lea relative RBP when used
                .alloc => {},
                .ret => try regmovmc(cfo, .rax, self.iref(i.op1).?.*),
                .iop => {
                    const lhs = self.iref(i.op1).?;
                    const rhs = self.iref(i.op2).?;
                    const dst = i.ipreg() orelse @panic("missing spill");
                    const op = @intToEnum(FLIR.IntBinOp, i.spec);

                    if (op.asAOP()) |aop| {
                        // TODO: fugly: remove once we have constraint handling in regalloc
                        if (dst == rhs.ipreg() and dst != lhs.ipreg()) @panic("conflict!");
                        try regmovmc(cfo, dst, lhs.*); // often elided if lhs has the same reg
                        try regaritmc(cfo, aop, dst, rhs.*);
                    } else if (op == .mul) {
                        if (rhs.tag == .constant) {
                            const src = lhs.ipreg() orelse unreachable;
                            try cfo.imulrri(dst, src, @intCast(i8, rhs.op1));
                        } else {
                            if (rhs.ipreg()) |rhsreg| {
                                try regmovmc(cfo, dst, lhs.*);
                                try cfo.imulrr(dst, rhsreg);
                            } else {
                                unreachable;
                            }
                        }
                    } else if (op.asShift()) |sop| {
                        if (rhs.tag == .constant) {
                            try regmovmc(cfo, dst, lhs.*);
                            try cfo.sh_ri(dst, sop, @intCast(u8, rhs.op1));
                        } else {
                            unreachable;
                        }
                    }
                },
                .constant => try mcmovi(cfo, i.*),
                .icmp => {
                    // TODO: we can actually cmp [slot], reg as well as cmp reg, [slot].
                    // just `cmp [slot1], [slot2]` is a violation
                    const firstop = self.iref(i.op1).?.ipreg() orelse @panic("missing unspill");
                    try regaritmc(cfo, .cmp, firstop, self.iref(i.op2).?.*);
                    cond = @intToEnum(CFO.Cond, i.spec);
                },
                .putphi => {
                    // TODO: actually check for parallell-move conflicts
                    // either here or as an extra deconstruction step
                    try movmcs(cfo, self.iref(i.op2).?.*, self.iref(i.op1).?.*);
                },
                .load => {
                    var eaddr = try get_eaddr_load_or_lea(self, i.*);
                    const spec_type = i.mem_type();
                    switch (spec_type) {
                        .intptr => |size| {
                            // tbh, loading from memory into a spill slot is bit stupid
                            const dst = i.ipreg() orelse @panic("missing spill");
                            switch (size) {
                                .byte => {
                                    try cfo.movrm_byte(dst, eaddr);
                                },
                                .quadword => try cfo.movrm(dst, eaddr),
                                else => unreachable,
                            }
                        },
                        .avxval => |fmode| {
                            const dst = i.avxreg() orelse unreachable;
                            try cfo.vmovurm(fmode, dst, eaddr);
                        },
                    }
                },
                .lea => {
                    // TODO: spill spall supllit?
                    // const eaddr = CFO.qi(base, idx);
                    if (i.mckind == .fused) {} else {
                        if (true) unreachable;
                        const base = self.iref(i.op1).?.ipreg() orelse unreachable;
                        const idx = self.iref(i.op2).?.ipreg() orelse unreachable;
                        const dst = i.ipreg() orelse @panic("missing spill");
                        try cfo.lea(dst, CFO.qi(base, idx));
                    }
                },
                .store => {
                    var eaddr = try get_eaddr(self, self.iref(i.op1).?.*);
                    const val = self.iref(i.op2).?;
                    const spec_type = i.mem_type();
                    if (@as(ValType, spec_type) != val.res_type()) return error.NoYouCantPutThatThere;
                    switch (spec_type) {
                        .intptr => |size| {
                            if (val.ipreg()) |ipreg| {
                                switch (size) {
                                    .byte => {
                                        try cfo.movmr_byte(eaddr, ipreg);
                                    },
                                    .quadword => try cfo.movmr(eaddr, ipreg),
                                    else => unreachable,
                                }
                            } else {
                                if (val.tag != .constant) unreachable;
                                const constval = val.op1;
                                switch (size) {
                                    .byte => {
                                        try cfo.movmi_byte(eaddr, @truncate(u8, constval));
                                    },
                                    .quadword => try cfo.movmi(eaddr, constval),
                                    else => unreachable,
                                }
                            }
                        },
                        .avxval => |fmode| {
                            const src = val.avxreg() orelse unreachable;
                            try cfo.vmovumr(fmode, eaddr, src);
                        },
                    }
                },
                .vmath => {
                    const x = self.iref(i.op1).?.avxreg() orelse unreachable;
                    const y = self.iref(i.op2).?.avxreg() orelse unreachable;
                    const dst = i.avxreg() orelse unreachable;
                    try cfo.vmathf(i.vop(), i.fmode(), dst, x, y);
                },
                .callarg => {
                    try regmovmc(cfo, i.ipreg().?, self.iref(i.op1).?.*);
                },
                .call => {
                    if (i.op1 != 0) { // TODO: le wat
                        try cfo.movri(.rax, i.op1);
                    } else {
                        // THANKS INTEL
                        try cfo.zero(.rax);
                    }
                    try cfo.syscall();
                },

                else => {
                    print("unhandled tag: {}\n", .{i.tag});
                    return error.Panik;
                },
            }
            fused_inst = if (was_fused) i else null;
        }

        // TODO: handle trivial critical-edge block.
        const fallthru = ni + 1;
        if (n.s[1] != 0) {
            try makejmp(self, cfo, cond.?, uv(ni), 1, labels, targets);
        }
        if (n.s[0] != fallthru and n.s[0] != 0) {
            try makejmp(self, cfo, null, uv(ni), 0, labels, targets);
        }
    }

    var isave = self.nsave;
    while (isave > 0) {
        isave -= 1;
        try cfo.pop(FLIR.callee_saved[isave]);
    }
    try cfo.leave();
    try cfo.ret();
    return target;
}
