const std = @import("std");
const mem = std.mem;
const FLIR = @import("./FLIR.zig");
const print = std.debug.print;
const CFO = @import("./CFO.zig");
const IPReg = CFO.IPReg;
const VMathOp = CFO.VMathOp;
const Inst = FLIR.Inst;
const IPMCVal = FLIR.IPMCVal;
const uv = FLIR.uv;
const ValType = FLIR.ValType;
const EAddr = CFO.EAddr;

fn slotoff(slotid: anytype) i32 {
    return -8 * (1 + @intCast(i32, slotid));
}

fn movri_zero(cfo: *CFO, dst: IPReg, src: i32) !void {
    if (src != 0) { // TODO: proper constval
        try cfo.movri(dst, src);
    } else {
        // THANKS INTEL
        try cfo.zero(dst);
    }
}

fn regmovmc(cfo: *CFO, dst: IPReg, src: IPMCVal) !void {
    switch (src) {
        .frameslot => |f| try cfo.movrm(dst, CFO.a(.rbp).o(slotoff(f))),
        .ipreg => |reg| if (dst != reg) try cfo.mov(dst, reg),
        .constval => |c| try movri_zero(cfo, dst, @intCast(i32, c)),
    }
}

fn regaritmc(cfo: *CFO, op: CFO.AOp, dst: IPReg, src: IPMCVal) !void {
    switch (src) {
        .frameslot => |f| try cfo.aritrm(op, dst, CFO.a(.rbp).o(slotoff(f))),
        .ipreg => |reg| try cfo.arit(op, dst, reg),
        .constval => |c| try cfo.aritri(op, dst, @intCast(i32, c)),
    }
}

fn mcmovreg(cfo: *CFO, dst: IPMCVal, src: IPReg) !void {
    switch (dst) {
        .frameslot => |f| try cfo.movmr(CFO.a(.rbp).o(slotoff(f)), src),
        .ipreg => |reg| if (reg != src) try cfo.mov(reg, src),
        .constval => return error.AURORA_BOREALIS,
    }
}

fn movmcs(cfo: *CFO, dst: IPMCVal, src: IPMCVal) !void {
    // sadge
    // if (dst.mckind == src.mckind and dst.mcidx == src.mcidx) {
    //     return;
    // }
    // TODO: encode size of src so we can use 32-bit move sometimes
    switch (dst) {
        .ipreg => |reg| try regmovmc(cfo, reg, src),
        else => switch (src) {
            .ipreg => |reg| try mcmovreg(cfo, dst, reg),
            else => @panic("unspill/respill"),
        },
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
fn get_eaddr(self: *FLIR, ref: u16) EERROR!EAddr {
    const i = self.iref(ref).?;
    if (i.tag == .alloc) {
        return CFO.a(.rbp).o(slotoff(i.op1));
    } else if (i.tag == .lea) {
        return get_eaddr_load_or_lea(self, i.*);
    } else {
        // TODO: spill spall supllit?
        const base = i.ipreg() orelse return error.WHAAAAA;
        return CFO.a(base);
    }
}

// i must either be a load or lea instruction.
fn get_eaddr_load_or_lea(self: *FLIR, i: Inst) !EAddr {

    // TODO: if base is an un-fused .lea we should use its target
    // ipreg instead.
    var eaddr = try get_eaddr(self, i.op1);

    if (i.op2 == FLIR.NoRef) return eaddr;
    const idx = self.ipval(i.op2) orelse return error.WHAAAAA;
    switch (idx) {
        .ipreg => |reg| {
            if (eaddr.index != null) {
                return error.@"TODO: unfused lea";
            }
            eaddr.index = reg;
            eaddr.scale = @intCast(u2, i.high_spec());
        },
        .constval => |c| {
            eaddr = eaddr.o(@intCast(i32, c)); // TODO: scale just disappears??
        },
        else => return error.VOLKTANZ,
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

        var cond: ?CFO.Cond = null;
        var it = self.ins_iterator(n.firstblk);
        while (it.next()) |item| {
            const i = item.i;

            switch (i.tag) {
                // empty doesn't flush fused value
                .empty => continue,
                // these expect their values to be in place when executed
                .arg, .phi => {},
                // lea relative RBP when used
                .alloc => {},
                .ret => try regmovmc(cfo, .rax, self.ipval(i.op1).?),
                .ibinop => {
                    const lhs = self.ipval(i.op1) orelse return error.FLIRError;
                    const rhs = self.ipval(i.op2) orelse return error.FLIRError;
                    const dst = i.ipreg() orelse @panic("missing spill");
                    const op = @intToEnum(FLIR.IntBinOp, i.spec);

                    if (op.asAOP()) |aop| {
                        // TODO: fugly: remove once we have constraint handling in regalloc
                        if (dst == rhs.as_ipreg() and dst != lhs.as_ipreg()) @panic("conflict!");
                        try regmovmc(cfo, dst, lhs); // often elided if lhs has the same reg
                        try regaritmc(cfo, aop, dst, rhs);
                    } else if (op == .mul) {
                        switch (rhs) {
                            .constval => |c| {
                                const src = lhs.as_ipreg() orelse @panic("nheee");
                                try cfo.imulrri(dst, src, @intCast(i8, c));
                            },
                            .ipreg => if (rhs.as_ipreg()) |rhsreg| {
                                try regmovmc(cfo, dst, lhs);
                                try cfo.imulrr(dst, rhsreg);
                            },
                            else => unreachable,
                        }
                    } else if (op.asShift()) |sop| {
                        switch (rhs) {
                            .constval => |c| {
                                try regmovmc(cfo, dst, lhs);
                                try cfo.sh_ri(dst, sop, @intCast(u8, c));
                            },
                            .ipreg => |src2| {
                                const src1 = lhs.as_ipreg() orelse {
                                    try regmovmc(cfo, dst, lhs);
                                    break dst;
                                };
                                try cfo.sx(sop, dst, src1, src2);
                            },
                            else => @panic("fast... nej."),
                        }
                    }
                },
                .icmp => {
                    // TODO: we can actually cmp [slot], reg as well as cmp reg, [slot].
                    // just `cmp [slot1], [slot2]` is a violation
                    // although "cmp imm, reg" needs an operand swap.
                    // best if an earlier ABI step takes care of all of this
                    const lhs = self.ipreg(i.op1) orelse @panic("missing unspill");
                    const rhs = self.ipval(i.op2) orelse return error.FLIRError;
                    try regaritmc(cfo, .cmp, lhs, rhs);
                    cond = @intToEnum(CFO.Cond, i.spec);
                },
                .putphi => {
                    // TODO: actually check for parallell-move conflicts
                    // either here or as an extra deconstruction step
                    // TODO: phi of avxval
                    const dest = self.ipval(i.op2) orelse return error.FLIRError;
                    const src = self.ipval(i.op1) orelse return error.FLIRError;
                    try movmcs(cfo, dest, src);
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
                        const base = self.ipreg(i.op1) orelse unreachable;
                        const idx = self.ipreg(i.op2) orelse unreachable;
                        const dst = i.ipreg() orelse @panic("missing spill");
                        try cfo.lea(dst, CFO.qi(base, idx));
                    }
                },
                .store => {
                    var eaddr = try get_eaddr(self, i.op1);
                    switch (i.mem_type()) {
                        .intptr => |size| {
                            const val = self.ipval(i.op2) orelse return error.NoYouCantPutThatThere;
                            switch (val) {
                                .ipreg => |reg| {
                                    switch (size) {
                                        .byte => {
                                            try cfo.movmr_byte(eaddr, reg);
                                        },
                                        .quadword => try cfo.movmr(eaddr, reg),
                                        else => unreachable,
                                    }
                                },
                                .constval => |c| {
                                    switch (size) {
                                        .byte => {
                                            try cfo.movmi_byte(eaddr, @truncate(u8, c));
                                        },
                                        .quadword => try cfo.movmi(eaddr, @intCast(i32, c)),
                                        else => unreachable,
                                    }
                                },
                                else => @panic("respill"),
                            }
                        },
                        .avxval => |fmode| {
                            const src = self.avxreg(i.op2) orelse return error.NoYouCantPutThatThere;
                            try cfo.vmovumr(fmode, eaddr, src);
                        },
                    }
                },
                .vmath => {
                    const x = self.avxreg(i.op1) orelse return error.FLIRError;
                    const y = self.avxreg(i.op2) orelse return error.FLIRError;
                    const dst = i.avxreg() orelse return error.FLIRError;
                    try cfo.vmathf(i.vop(), i.fmode(), dst, x, y);
                },
                .callarg => {
                    try regmovmc(cfo, i.ipreg().?, self.ipval(i.op1).?);
                },
                .call => {
                    const kind = @intToEnum(FLIR.CallKind, i.spec);
                    switch (kind) {
                        .syscall => {
                            try regmovmc(cfo, .rax, self.ipval(i.op1).?);
                            try cfo.syscall();
                        },
                        else => unreachable,
                    }
                },
                else => {
                    print("unhandled tag: {}\n", .{i.tag});
                    return error.Panik;
                },
            }
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
