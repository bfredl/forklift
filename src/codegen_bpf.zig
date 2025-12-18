const std = @import("std");
const mem = std.mem;
const FLIR = @import("./FLIR.zig");
const bpf = @import("./bpf.zig");
const print = std.debug.print;
const IPReg = defs.IPReg;
const Inst = FLIR.Inst;
const uv = FLIR.uv;
const linux = std.os.linux;
const BPF = linux.BPF;
const Reg = BPF.Insn.Reg;
const Allocator = mem.Allocator;
const fd_t = linux.fd_t;
const IPMCVal = defs.IPMCVal;

const CFOModule = @import("./CFOModule.zig");
const BPFCode = CFOModule.BPFCode;

const options = defs.debug_options;

const defs = @import("./defs.zig");
fn r(reg: IPReg) Reg {
    return @enumFromInt(reg.id());
}

const ArrayList = std.ArrayList;

const Insn = BPF.Insn;
const I = Insn;

const EAddr = struct {
    reg: u4,
    off: i16,
    fn o(self: EAddr, off: i16) EAddr {
        return .{ .reg = self.reg, .off = self.off + off };
    }
};

fn get_eaddr(self: *FLIR, i: FLIR.Inst, comptime may_lea: bool) !EAddr {
    if (may_lea and i.tag == .lea) {
        const base = self.iref(i.op1).?.*;
        var eval = try get_eaddr(self, base, false);
        const idx = self.ipval(i.op2) orelse return error.FLIRError;
        switch (idx) {
            .constval => |c| {
                eval = eval.o(@intCast(c)); // TODO: scale just disappears??
            },
            else => unreachable,
        }
        return eval;
    } else if (i.mckind == .ipreg) {
        return .{ .reg = @intCast(i.mcidx), .off = 0 };
    } else if (i.tag == .alloc) {
        return .{ .reg = 10, .off = slotoff(i.op1) };
    } else {
        return error.InvalidAddress;
    }
}

pub fn get_target(code: *BPFCode) u32 {
    return @intCast(code.items.len);
}

pub fn set_target(code: *BPFCode, pos: u32) void {
    const off = get_target(code) - (pos + 1);
    code.items[pos].off = @intCast(off);
}

pub fn put(code: *CFOModule, insn: Insn) !void {
    if (options.dbg_disasm_ir) {
        print("    ", .{});
        // bpf.dump_ins(writer, insn, code.items.len);
    }
    try code.bpf_code.append(code.gpa, insn);
}

pub fn slotoff(slotid: anytype) i16 {
    return -8 * (1 + @as(i16, @intCast(slotid)));
}

pub fn ld_map_fd(code: *CFOModule, reg: IPReg, map_fd: fd_t, spec: u8) !void {
    var insn = I.ld_map_fd1(r(reg), map_fd);
    if (spec == 0) {
        // ok
    } else if (spec == 1) { // BPF_PSEUDO_MAP_VALUE
        insn.src = BPF.PSEUDO_MAP_VALUE;
    } else {
        return error.FLIRError;
    }
    try put(code, insn);
    // TODO: PSEUDO_MAP_VALUE allows us to code an offset into the second instruction
    try put(code, I.ld_map_fd2(map_fd));
}

pub fn jeq(code: *CFOModule, src: IPReg, dst: anytype) !u32 {
    const pos = get_target(code);
    try put(code, I.jeq(src, dst, -0x7FFF));
    return pos;
}

fn mov(code: *CFOModule, dst: IPReg, src: anytype) !void {
    try put(code, I.mov(r(dst), src));
}

const r0: defs.IPReg = @enumFromInt(0);

fn regmovmc(code: *CFOModule, dst: IPReg, src: IPMCVal) !void {
    switch (src) {
        .frameslot => |f| try put(code, I.ldx(.double_word, r(dst), .r10, slotoff(f))),
        .ipreg => |reg| if (dst != reg) try mov(code, dst, r(reg)),
        .constval => |c| try mov(code, dst, @as(i32, @intCast(c))),
        // need .data segment as a bpftable..
        .constref, .constptr => return error.FLIRError,
    }
    // .fused => {
    //     if (src.tag != .alloc) return error.BBB_BBB;
    //     try mov(code, dst, .r10);
    //     try put(code, I.add(r(dst), slotoff(src.op1)));
    // },
}

fn regjmpmc(code: *CFOModule, op: Insn.JmpOp, dst: IPReg, src: IPMCVal) !u32 {
    _ = dst;
    _ = op;
    const pos = get_target(&code.bpf_code);
    switch (src) {
        .frameslot => return error.FLIRError,
        .ipreg => |_| unreachable,
        // TODO: FLIR.ABI needs to encode constraints like "imm which fits in a i32"
        .constval => |_| unreachable,
        .constref, .constptr => return error.FLIRError,
        //     var inst = I.jmp(op, dst, src.op1, 0x7FFF);
    }
    return pos;
}

fn regaritmc(code: *CFOModule, op: Insn.AluOp, dst: IPReg, src: IPMCVal) !void {
    switch (src) {
        .frameslot => return error.FLIRError,
        .ipreg => |reg| try put(code, I.alu(64, op, r(dst), r(reg))),
        // TODO: FLIR.ABI needs to encode constraints like "imm which fits in a i32"
        .constval => |c| try put(code, I.alu(64, op, r(dst), @as(i32, @intCast(c)))),
        .constref, .constptr => return error.FLIRError,
    }
}

fn mcmovreg(code: *CFOModule, dst: IPMCVal, src: IPReg) !void {
    switch (dst) {
        .frameslot => |f| try put(code, I.stx(.double_word, .r10, slotoff(f), r(src))),
        .ipreg => |reg| if (reg != src) try mov(code, reg, r(src)),
        else => return error.AAA_AA_A,
    }
}

fn mcmovi(code: *CFOModule, i: Inst) !void {
    switch (i.mckind) {
        .frameslot => {
            // TODO: just store??
            try put(code, I.mov(.r0, i.op1));
            try mcmovreg(code, i, .r0);
        },
        .ipreg => {
            const reg: IPReg = @enumFromInt(i.mcidx);
            try mov(code, reg, i.op1);
        },
        .fused => {}, // let user lookup value
        .constant => {}, // let user lookup value
        else => return error.AAA_AA_A,
    }
}

fn bpf_size(size: defs.ISize) Insn.Size {
    return switch (size) {
        .byte => .byte,
        .word => .half_word,
        .dword => .word,
        .quadword => .double_word,
    };
}

fn addrmovmc(code: *CFOModule, dst: EAddr, src: IPMCVal, size: defs.ISize) !void {
    const bsize = bpf_size(size);
    switch (src) {
        .ipreg => |reg| try put(code, I.stx(bsize, @enumFromInt(dst.reg), dst.off, r(reg))),
        .constval => |c| try put(code, I.st(bsize, @enumFromInt(dst.reg), dst.off, @intCast(c))),
        else => unreachable,
    }
}

fn regmovaddr(code: *CFOModule, dst: IPReg, src: EAddr, size: defs.ISize) !void {
    try put(code, I.ldx(bpf_size(size), r(dst), @enumFromInt(src.reg), src.off));
}

fn movmcs(code: *CFOModule, dst: IPMCVal, src: IPMCVal) !void {
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

// pub fn makejmp(self: *FLIR, code: *CFOModule, op: ?Insn.JmpOp, ni: u16, si: u1, labels: []u32, targets: [][2]u32) !void {
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

pub fn codegen(self: *FLIR, mod: *CFOModule) !u32 {
    const code = &mod.bpf_code;
    var labels = try self.gpa.alloc(u32, self.n.items.len);
    var targets = try self.gpa.alloc([2]u32, self.n.items.len);

    // const color_map = self.a.alloc(u8, self.n_ins()) catch @panic("OOM in debug_print");
    // defer self.a.free(color_map);
    // mem.set(u8, color_map, 0);
    // var last_color: u8 = 0;

    defer self.gpa.free(labels);
    defer self.gpa.free(targets);
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

            if (options.dbg_disasm_ir) {
                // FLIR.print_insn(FLIR.toref(blk, uv(ii)), i.*, color_map, &last_color);
                print("guuuh\n", .{});
            }

            switch (i.tag) {
                // empty doesn't flush fused value
                .freelist => @panic("KATASTROFEN"),
                // work is done by putphi
                .phi => {},
                .ibinop => {
                    const dst = i.ipreg() orelse return error.FLIRError;
                    const op = i.ibinop().asBpfAluOp() orelse return error.FLIRError;
                    const op1 = self.ipval(i.op1).?;
                    const op2 = self.ipval(i.op2).?;
                    try regmovmc(mod, dst, op1);
                    try regaritmc(mod, op, dst, op2);
                    unreachable;
                },
                .icmp => {
                    const op1 = i.ipreg() orelse return error.FLIRError;
                    const op2 = self.ipval(i.op2).?;
                    var spec = i.intcond();
                    var taken: u1 = 1;

                    // TODO: this should have been optimized earlier!
                    if (n.s[1] == fallthru and n.s[0] != 0) {
                        spec = spec.invert();
                        default_branch = 1;
                        taken = 0;
                    }

                    const op = spec.asBpfJmpOp() orelse return error.FLIRError;

                    const pos = try regjmpmc(mod, op, op1, op2);
                    targets[ni][taken] = pos;
                },
                .icmpset => return error.NotImplemented,
                .iunop => return error.NotImplemented,
                .select, .trap => {
                    return error.NotImplemented;
                },
                .putphi, .callarg, .retval => {
                    if (i.f.do_swap) return error.NotImplemented;
                    const dest = (try self.movins_dest(i)).ipval() orelse return error.FLIRError;
                    if (try self.movins_read_val(i)) |val| switch (val) {
                        .ipval => |src| try movmcs(mod, dest, src),
                        .avxreg => return error.NotImplemented,
                    };
                },
                .load, .load_signext => {
                    // TODO: spill spall supllit?
                    const addr = self.iref(i.op1).?.*;
                    const off = off_val: {
                        switch (self.ipval(i.op2).?) {
                            .constval => |c| break :off_val c,
                            else => return error.FLIRError,
                        }
                    };

                    const eaddr: EAddr = (try get_eaddr(self, addr, false)).o(@intCast(off));
                    const dst = i.ipreg() orelse r0;
                    try regmovaddr(mod, dst, eaddr, i.mem_type().intptr);
                    try mcmovreg(mod, i.ipval().?, dst); // elided if dst is register
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
                    const size = i.mem_type().intptr;
                    const addr = self.iref(i.op1).?.*;
                    const eaddr: EAddr = try get_eaddr(self, addr, true);
                    const val = self.ipval(i.op2).?;
                    try addrmovmc(mod, eaddr, val, size);
                },
                .bpf_load_map => {
                    const reg: IPReg = if (i.mckind == .ipreg) @enumFromInt(i.mcidx) else r0;
                    try mod.relocations.append(mod.gpa, .{ .pos = get_target(code), .obj_idx = i.op1 });
                    try ld_map_fd(mod, reg, 0x7FFFFFFF, i.spec);
                    try mcmovreg(mod, i.ipval().?, reg);
                },
                .alloc => {},
                .call => {
                    const kind: defs.CallKind = @enumFromInt(i.spec);
                    switch (kind) {
                        .bpf_helper => {
                            try put(mod, I.call(@as(BPF.Helper, @enumFromInt(i.spec))));
                        },
                        else => return error.FLIRError,
                    }
                },
                .copy => {
                    try movmcs(mod, i.ipval().?, self.ipval(i.op1).?);
                },
                .callret => {
                    // HAII
                    try movmcs(mod, i.ipval().?, self.ipval(i.op1).?);
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
                    try regmovmc(mod, sreg, src);
                    var insn = I.xadd(@as(Reg, @enumFromInt(dest_addr.reg)), r(sreg));
                    // TODO: if this works, upstream!
                    insn.off = dest_addr.off;
                    try put(mod, insn);
                },
                .arg => {
                    if (i.op1 != 0) unreachable;
                    try mcmovreg(mod, i.ipval().?, @enumFromInt(1));
                },
                .fconst => {},
                .vmath, .vcmpf, .int2vf, .vf2int, .fcmp, .vblendf, .vunop => {
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
            try put(mod, I.ja(0x7FFF)); // unconditional
            targets[ni][default_branch] = pos;
        }
    }

    try put(mod, I.exit());
    return target;
}
