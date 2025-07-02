pub const EMPTY: Inst = .{ .tag = .empty, .op1 = 0, .op2 = 0 };

pub const Tag = enum(u8) {
    freelist = 0, // item in freelist. must not be refered to!
    alloc,
    arg,
    variable,
    putvar, // unresolved phi assignment: VAR op2 := op1
    phi,
    /// put op1 into phi op2 of (only) successor
    putphi,
    copy,
    load,
    lea,
    store,
    ibinop,
    icmp,
    // in theory same as "intval to float bitcast", but separate fconst
    // make life easier. add a int2vf mode for bitcasts
    fconst, // op1 is a constref, opspec for type
    vmath, // binops specifically
    vcmpf,
    fcmp,
    int2vf,
    vf2int,
    ret,
    call,
    callarg,
    bpf_load_map,
    xadd,
};

const Inst = @This();

tag: Tag,
spec: u8 = 0,
op1: u16,
op2: u16,
mckind: defs.MCKind = .unallocated_raw,
mcidx: u8 = undefined,
vreg_scratch: u16 = FLIR.NoRef,
// can share space with vreg_scratch later?
node_delete_this: u16 = FLIR.NoRef,
// only var, phi and putphi currently use this. other types can assign other meaning:
next: u16 = FLIR.NoRef,
f: packed struct {
    // note: if the reference is a vreg, it might
    // be alive in an other branch processed later
    // in phi node: "kill_op1" is overloaded as resolved (it doesn't use any op:s in the normal sense)
    // NOTE: putphis doesn't set kill_op1 because they don't need to (what is dead may never die)
    kill_op1: bool = false,
    kill_op2: bool = false,
    killed: bool = false, // if false after calc_use, a non-vreg is never used
    is_vreg: bool = false, // if true: vreg_scratch is a vreg number. otherwise it is free real (scratch) estate

    // if true: vregs_scratch encodes a bitset of conflicting regs.
    // for a vreg, this is always false, but check vregs[v].conflicts
    conflicts: bool = false,

    // for the parallel move class of instructions:
    // perform a swap between target and dest
    do_swap: bool = false,
    // gen as a no-op as previous instruction was a swap
    swap_done: bool = false,

    // note this can be more than two instructions, like
    // do_swap a->b
    // do_swap c->a
    // swap_done b->c
    // is a legal lowering of a->b, b->c, c->a or any permutation thereof
} = .{},

pub fn vreg(self: Inst) ?u16 {
    return if (self.f.is_vreg) self.vreg_scratch else return null;
}

fn spec_type(self: Inst) defs.ValType {
    return self.mem_type();
}

// For load/store insts that can handle both intptr and avxvalues
// TODO: reconsider it all so we can say "load 8/16 bits into 32/64 bit register"
pub fn mem_type(self: Inst) defs.SpecType {
    return .from(self.low_spec());
}

pub fn scale(self: Inst) u2 {
    return @intCast(self.high_spec());
}

const LOW_MASK: u8 = (1 << 5) - 1;
const HIGH_MASK: u8 = ~LOW_MASK;

fn high_spec(self: Inst) u3 {
    return @intCast((self.spec & HIGH_MASK) >> 5);
}

fn low_spec(self: Inst) u5 {
    return @intCast(self.spec & LOW_MASK);
}

pub fn vmathop(self: Inst) X86Asm.VMathOp {
    return @enumFromInt(self.low_spec());
}

pub fn vcmpop(self: Inst) X86Asm.VCmpOp {
    return @enumFromInt(self.low_spec());
}

pub fn fcmpop(self: Inst) defs.IntCond {
    return @enumFromInt(self.low_spec());
}

// only valid for op instructions. otherwise use mem_type
pub fn fmode_op(self: Inst) X86Asm.FMode {
    return @enumFromInt(self.high_spec());
}

pub fn intcond(self: *Inst) defs.IntCond {
    return @enumFromInt(self.low_spec());
}

pub fn ibinop(self: *Inst) defs.IntBinOp {
    return @enumFromInt(self.low_spec());
}

// valid for .ibinop and .icmp
pub fn iop_size(self: *Inst) defs.ISize {
    return @enumFromInt(self.high_spec());
}

pub fn res_type(inst: Inst) ?defs.ValType {
    return switch (inst.tag) {
        .freelist => null,
        .arg => inst.mem_type(), // TODO: haIIIII
        .variable => inst.mem_type(), // gets preserved to the phis
        .putvar => null,
        .phi => inst.mem_type(),
        .putphi => null, // stated in the phi instruction
        .alloc => .intptr, // the type of .alloc is a pointer to it
        .copy => inst.mem_type(),
        .load => inst.mem_type(),
        .lea => .intptr, // Lea? Who's Lea??
        .store => null,
        .ibinop => .intptr,
        .icmp => null, // technically the FLAG register but anyway
        .vmath => .avxval,
        .vcmpf => .avxval,
        .fcmp => null, // matching icmp
        .int2vf => .avxval, // convert int to float, or move int from gp to vector reg
        .fconst => .avxval,
        .vf2int => .intptr,
        .ret => null,
        .call => .intptr,
        .callarg => null,
        .bpf_load_map => .intptr,
        .xadd => null,
    };
}

pub fn has_res(inst: Inst) bool {
    return inst.res_type() != null;
}

// number of op:s which are inst references.
// otherwise they can store whatever data
pub fn n_op(inst: Inst, rw: bool) u2 {
    return switch (inst.tag) {
        .freelist => 0,
        .arg => 0,
        .variable => 0,
        // really only one, but we will get rid of this lie
        // before getting into any serious analysis.
        .putvar => 2, // TODO: if (rw) 2 else 1, FAST ÅT ANDRA HÅLLET
        .phi => 0,
        .putphi => if (rw) 2 else 1,
        .copy => 1,
        .load => 2, // base, idx
        .lea => 2, // base, idx. elided when only used for a store!
        .store => 2, // addr, val
        .ibinop => 2,
        .icmp => 2,
        .vmath => 2,
        .vcmpf => 2,
        .fcmp => 2,
        .int2vf => 1,
        .fconst => 1, // but note: always a constval
        .vf2int => 1,
        .ret => 1,
        .callarg => 1,
        .call => 1, // could be for funptr/dynamic syscall?
        .alloc => 0,
        .bpf_load_map => 0,
        .xadd => 2, // TODO: atomic instruction group
    };
}

pub fn ops(i: *Inst, rw: bool) []u16 {
    std.debug.assert(@intFromPtr(&i.op2) - @intFromPtr(&i.op1) == @sizeOf(u16));
    return @as([*]u16, @ptrCast(&i.op1))[0..i.n_op(rw)];
}

pub fn ipreg(i: Inst) ?defs.IPReg {
    return if (i.mckind == .ipreg) @as(defs.IPReg, @enumFromInt(i.mcidx)) else null;
}

pub fn avxreg(i: Inst) ?u4 {
    return if (i.mckind == .vfreg) @as(u4, @intCast(i.mcidx)) else null;
}

pub fn ipval(i: Inst) ?defs.IPMCVal {
    if (i.ipreg()) |reg| return .{ .ipreg = reg };
    if (i.mckind == .frameslot) return .{ .frameslot = i.mcidx };
    return null;
}

pub fn sphigh(high: u3, low: u5) u8 {
    return @as(u8, high) << 5 | low;
}

pub fn vmathspec(vop: X86Asm.VMathOp, fmode: X86Asm.FMode) u8 {
    return sphigh(@intFromEnum(fmode), @intCast(vop.off()));
}

pub fn vcmpfspec(vcmp: X86Asm.VCmpOp, fmode: X86Asm.FMode) u8 {
    return sphigh(@intFromEnum(fmode), vcmp.val());
}

pub fn fcmpspec(cond: defs.IntCond, fmode: X86Asm.FMode) u8 {
    return sphigh(@intFromEnum(fmode), cond.off());
}

// TODO: will generalize
pub fn vcvtspec(fmode: X86Asm.FMode) u8 {
    return sphigh(@intFromEnum(fmode), 0);
}

const defs = @import("./defs.zig");
const X86Asm = @import("./X86Asm.zig");
const FLIR = @import("./FLIR.zig");
const std = @import("std");
