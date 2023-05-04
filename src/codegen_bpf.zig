const std = @import("std");
const mem = std.mem;
const FLIR = @import("./FLIR.zig");
const bpf = @import("./bpf.zig");
const print = std.debug.print;
const IPReg = common.IPReg;
const Inst = FLIR.Inst;
const uv = FLIR.uv;
const linux = std.os.linux;
const BPF = linux.BPF;
const Reg = BPF.Insn.Reg;
const Allocator = mem.Allocator;
const fd_t = linux.fd_t;
const IPMCVal = FLIR.IPMCVal;

const common = @import("./common.zig");
fn r(reg: IPReg) Reg {
    return @intToEnum(Reg, reg.id());
}

const ArrayList = std.ArrayList;

const Insn = BPF.Insn;
const I = Insn;

pub const Code = ArrayList(Insn);

const builtin = @import("builtin");
const options = if (!builtin.is_test) &@import("root").options else null;

const EAddr = struct {
    reg: u4,
    off: i16,
    fn with_off(self: EAddr, off: i16) EAddr {
        return .{ .reg = self.reg, .off = self.off + off };
    }
};

fn get_eaddr(self: *FLIR, i: FLIR.Inst, comptime may_lea: bool) !EAddr {
    if (may_lea and i.tag == .lea) {
        const base = self.iref(i.op1).?.*;
        const baseval = try get_eaddr(self, base, false);
        const off = @bitCast(i16, i.op2);
        return baseval.with_off(off);
    } else if (i.mckind == .ipreg) {
        return .{ .reg = @intCast(u4, i.mcidx), .off = 0 };
    } else if (i.tag == .alloc) {
        return .{ .reg = 10, .off = slotoff(i.op1) };
    } else {
        return error.InvalidAddress;
    }
}

pub fn get_target(code: *Code) u32 {
    return @intCast(u32, code.items.len);
}

pub fn set_target(code: *Code, pos: u32) void {
    var off = get_target(code) - (pos + 1);
    code.items[pos].off = @intCast(i16, off);
}

pub fn put(code: *Code, insn: Insn) !void {
    if (@TypeOf(options) != @TypeOf(null) and options.dbg_disasm_ir) {
        print("    ", .{});
        bpf.dump_ins(insn, code.items.len);
    }
    try code.append(insn);
}

pub fn slotoff(slotid: anytype) i16 {
    return -8 * (1 + @intCast(i16, slotid));
}

pub fn ld_map_fd(code: *Code, reg: IPReg, map_fd: fd_t, spec: u8) !void {
    var insn = I.ld_map_fd1(r(reg), map_fd);
    if (spec == 1) { // BPF_PSEUDO_MAP_VALUE
        insn.src = BPF.PSEUDO_MAP_VALUE;
    } else {
        std.debug.assert(spec == 0);
    }
    try put(code, insn);
    // TODO: PSEUDO_MAP_VALUE allows us to code an offset into the second instruction
    try put(code, I.ld_map_fd2(map_fd));
}

pub fn jeq(code: *Code, src: IPReg, dst: anytype) !u32 {
    var pos = get_target(code);
    try put(code, I.jeq(src, dst, -0x7FFF));
    return pos;
}

fn mov(code: *Code, dst: IPReg, src: anytype) !void {
    try put(code, I.mov(r(dst), src));
}

const r0 = @intToEnum(common.IPReg, 0);

fn regmovmc(code: *Code, dst: IPReg, src: IPMCVal) !void {
    switch (src) {
        .frameslot => |f| try put(code, I.ldx(.double_word, r(dst), .r10, slotoff(f))),
        .ipreg => |reg| if (dst != reg) try mov(code, dst, r(reg)),
        .constval => |c| try mov(code, dst, @intCast(i32, c)),
    }
    // .fused => {
    //     if (src.tag != .alloc) return error.BBB_BBB;
    //     try mov(code, dst, .r10);
    //     try put(code, I.add(r(dst), slotoff(src.op1)));
    // },
}

fn regjmpmc(code: *Code, op: Insn.JmpOp, dst: IPReg, src: IPMCVal) !u32 {
    _ = dst;
    _ = op;
    const pos = get_target(code);
    switch (src) {
        .frameslot => return error.FLIRError,
        .ipreg => |_| unreachable,
        // TODO: FLIR.ABI needs to encode constraints like "imm which fits in a i32"
        .constval => |_| unreachable,
        //     var inst = I.jmp(op, dst, src.op1, 0x7FFF);
    }
    return pos;
}

fn regaritmc(code: *Code, op: Insn.AluOp, dst: IPReg, src: IPMCVal) !void {
    switch (src) {
        .frameslot => return error.FLIRError,
        .ipreg => |reg| try put(code, I.alu(64, op, r(dst), r(reg))),
        // TODO: FLIR.ABI needs to encode constraints like "imm which fits in a i32"
        .constval => |c| try put(code, I.alu(64, op, r(dst), @intCast(i32, c))),
    }
}

fn mcmovreg(code: *Code, dst: IPMCVal, src: IPReg) !void {
    switch (dst) {
        .frameslot => |f| try put(code, I.stx(.double_word, .r10, slotoff(f), r(src))),
        .ipreg => |reg| if (reg != src) try mov(code, reg, r(src)),
        else => return error.AAA_AA_A,
    }
}

fn mcmovi(code: *Code, i: Inst) !void {
    switch (i.mckind) {
        .frameslot => {
            // TODO: just store??
            try put(code, I.mov(.r0, i.op1));
            try mcmovreg(code, i, .r0);
        },
        .ipreg => {
            const reg = @intToEnum(IPReg, i.mcidx);
            try mov(code, reg, i.op1);
        },
        .fused => {}, // let user lookup value
        .constant => {}, // let user lookup value
        else => return error.AAA_AA_A,
    }
}

fn stx(code: *Code, dst: EAddr, src: IPReg) !void {
    try put(code, I.stx(.double_word, @intToEnum(Reg, dst.reg), dst.off, r(src)));
}

fn st(code: *Code, dst: EAddr, src: anytype) !void {
    // TODO: AAAA wrong size
    try put(code, I.st(.double_word, @intToEnum(Reg, dst.reg), dst.off, src));
}

fn addrmovmc(code: *Code, dst: EAddr, src: Inst) !void {
    switch (src.mckind) {
        // .constant => {
        //     if (src.tag != .constant) unreachable;
        //     try st(code, dst, src.op1);
        // },
        .ipreg => {
            try stx(code, dst, @intToEnum(IPReg, src.mcidx));
        },
        else => unreachable,
    }
}

fn regmovaddr(code: *Code, dst: IPReg, src: EAddr) !void {
    try put(code, I.ldx(.double_word, r(dst), @intToEnum(Reg, src.reg), src.off));
}

fn movmcs(code: *Code, dst: IPMCVal, src: IPMCVal) !void {
    // sadge
    // if (dst.mckind == src.mckind and dst.mcidx == src.mcidx) {
    //     return;
    // }
    switch (dst) {
        .ipreg => |reg| try regmovmc(code, reg, src),
        else => switch (src) {
            .ipreg => |reg| try mcmovreg(code, dst, reg),
            else => return error.SpillError, // TODO: mov [slot], imm32 (s.e. 64) OK?
        },
    }
}

// pub fn makejmp(self: *FLIR, code: *Code, op: ?Insn.JmpOp, ni: u16, si: u1, labels: []u32, targets: [][2]u32) !void {
//     const succ = self.n.items[ni].s[si];
//     // NOTE: we assume blk 0 always has the prologue (push rbp; mov rbp, rsp)
//     // at least, so that even if blk 0 is empty, blk 1 has target larger than 0x00
//     if (labels[succ] != 0) {
//         // try jbck(code, cond, labels[succ]);
//         unreachable;
//     } else {
//         targets[ni][si] = try jfwd(code, op);
//     }
// }

pub fn codegen(self: *FLIR, code: *Code) !u32 {
    var labels = try self.a.alloc(u32, self.n.items.len);
    var targets = try self.a.alloc([2]u32, self.n.items.len);

    // const color_map = self.a.alloc(u8, self.n_ins()) catch @panic("OOM in debug_print");
    // defer self.a.free(color_map);
    // mem.set(u8, color_map, 0);
    // var last_color: u8 = 0;

    defer self.a.free(labels);
    defer self.a.free(targets);
    @memset(labels, 0);
    @memset(targets, .{ 0, 0 });

    const target = get_target(code);

    for (self.n.items, 0..) |*n, ni| {
        if ((n.dfnum == 0 or n.is_empty) and ni > 0) {
            // non-entry block not reached by df search is dead.
            // TODO: these should already been cleaned up at this point
            continue;
        }
        labels[ni] = get_target(code);
        // print("LABEL: {x} {}\n", .{ labels[ni], ni });
        for (self.preds(uv(ni))) |pred| {
            const pr = &self.n.items[pred];
            const si: u1 = if (pr.s[0] == ni) 0 else 1;
            if (targets[pred][si] != 0) {
                set_target(code, targets[pred][si]);
                targets[pred][si] = 0;
            }
        }

        var fallthru = ni + 1;
        while (fallthru < self.n.items.len and self.n.items[fallthru].npred == 0) {
            fallthru += 1;
        }
        var default_branch: u8 = 0;

        var it = self.ins_iterator(n.firstblk);
        while (it.next()) |item| {
            const i = item.i;

            if (@TypeOf(options) != @TypeOf(null) and options.dbg_disasm_ir) {
                // FLIR.print_insn(FLIR.toref(blk, uv(ii)), i.*, color_map, &last_color);
                print("guuuh\n", .{});
            }

            switch (i.tag) {
                // empty doesn't flush fused value
                .empty => continue,
                // work is done by putphi
                .phi => {},
                .ret => try regmovmc(code, r0, self.ipval(i.op1).?),
                .ibinop => {
                    const dst = i.ipreg() orelse return error.FLIRError;
                    const op = @intToEnum(FLIR.IntBinOp, i.spec).asBpfAluOp() orelse return error.FLIRError;
                    const op1 = self.ipval(i.op1).?;
                    const op2 = self.ipval(i.op2).?;
                    try regmovmc(code, dst, op1);
                    try regaritmc(code, op, dst, op2);
                    unreachable;
                },
                .icmp => {
                    const op1 = i.ipreg() orelse return error.FLIRError;
                    const op2 = self.ipval(i.op2).?;
                    var spec = @intToEnum(FLIR.IntCond, i.spec);
                    var taken: u1 = 1;

                    // TODO: this should have been optimized earlier!
                    if (n.s[1] == fallthru and n.s[0] != 0) {
                        spec = spec.invert();
                        default_branch = 1;
                        taken = 0;
                    }

                    const op = spec.asBpfJmpOp() orelse return error.FLIRError;

                    const pos = try regjmpmc(code, op, op1, op2);
                    targets[ni][taken] = pos;
                },
                .putphi => {
                    // TODO: actually check for parallell-move conflicts
                    try movmcs(code, self.ipval(i.op2).?, self.ipval(i.op1).?);
                },
                .load => {
                    // TODO: spill spall supllit?
                    const addr = self.iref(i.op1).?.*;
                    const off = @intCast(i16, i.op2);
                    const eaddr: EAddr = (try get_eaddr(self, addr, false)).with_off(off);
                    const dst = i.ipreg() orelse r0;
                    try regmovaddr(code, dst, eaddr);
                    try mcmovreg(code, i.ipval().?, dst); // elided if dst is register
                },
                .lea => {
                    // TODO: keep track of lifetime extensions of fused values somewhere
                    if (i.mckind == .fused) {
                        // pass
                    } else {
                        unreachable;
                    }
                },
                .store => {
                    const addr = self.iref(i.op1).?.*;
                    const eaddr: EAddr = try get_eaddr(self, addr, true);
                    const val = self.iref(i.op2).?;
                    try addrmovmc(code, eaddr, val.*);
                },
                .bpf_load_map => {
                    const reg = if (i.mckind == .ipreg) @intToEnum(IPReg, i.mcidx) else r0;
                    try ld_map_fd(code, reg, i.op1, i.spec);
                    try mcmovreg(code, i.ipval().?, reg);
                },
                .alloc => {},
                .callarg => {
                    unreachable;
                },
                .call => {
                    //try regmovmc(code, .r1, self.iref(i.op1).?.*);
                    //try regmovmc(code, .r2, self.iref(i.op2).?.*);
                    // if (nexti) |iarg| {
                    //     if (iarg.tag == .callarg) {
                    //         try regmovmc(code, .r3, self.iref(iarg.op1).?.*);
                    //         if (iarg.op2 != FLIR.NoRef) {
                    //             try regmovmc(code, .r4, self.iref(iarg.op2).?.*);
                    //         }
                    //     }
                    // }
                    try put(code, I.call(@intToEnum(BPF.Helper, i.spec)));
                },
                .copy => {
                    try movmcs(code, i.ipval().?, self.ipval(i.op1).?);
                },
                .xadd => {
                    const dest = self.iref(i.op1).?.*;
                    const dest_addr = try get_eaddr(self, dest, true);
                    const src = self.ipval(i.op2).?;
                    // TODO: JORD OCH SMUTS
                    const sreg = switch (src) {
                        .ipreg => |reg| reg,
                        else => r0,
                    };
                    try regmovmc(code, sreg, src);
                    var insn = I.xadd(@intToEnum(Reg, dest_addr.reg), r(sreg));
                    // TODO: if this works, upstream!
                    insn.off = dest_addr.off;
                    try put(code, insn);
                },
                .arg => {
                    if (i.op1 != 0) unreachable;
                    try mcmovreg(code, i.ipval().?, @intToEnum(IPReg, 1));
                },
                .vmath, .vcmpf => {
                    print("platform unsupported: {}\n", .{i.tag});
                    return error.FLIRError;
                },
                .variable, .putvar => {
                    print("unhandled tag: {}\n", .{i.tag});
                    return error.FLIRError;
                },
            }
        }

        if (n.s[default_branch] != fallthru and n.s[default_branch] != 0) {
            const pos = get_target(code);
            try put(code, I.ja(0x7FFF)); // unconditional
            targets[ni][default_branch] = pos;
        }
    }

    try put(code, I.exit());
    return target;
}
