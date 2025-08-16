const std = @import("std");
const math = std.math;
const mem = std.mem;
const Allocator = mem.Allocator;
const Self = @This();
const print = std.debug.print;
const defs = @import("./defs.zig");
const X86Asm = @import("./X86Asm.zig");
const builtin = @import("builtin");
const BPF = std.os.linux.BPF;

const options = if (!builtin.is_test) &@import("root").options else null;
// remove verifying printing code regardless
pub const minimal = false;

const verify_ir = @import("./verify_ir.zig");
pub const check_ir_valid = verify_ir.check_ir_valid;
pub const check_vregs = verify_ir.check_vregs;
pub const debug_print = verify_ir.debug_print;
pub const print_debug_map = verify_ir.print_debug_map;
pub const print_intervals = verify_ir.print_intervals;

const SSA_GVN = @import("./SSA_GVN.zig");
pub const resolve_ssa = SSA_GVN.resolve_ssa;
pub const read_ref = SSA_GVN.read_ref;

const ArrayList = std.ArrayList;
const assert = std.debug.assert;

pub const Inst = @import("./Inst.zig");

pub const IntUnOp = defs.IntUnOp;
pub const IntBinOp = defs.IntBinOp;
pub const IntCond = defs.IntCond;

a: Allocator,
// TODO: unmanage all these:
n: ArrayList(Node),
b: ArrayList(Block),
i: ArrayList(Inst),
preorder: ArrayList(u16),
blkorder: ArrayList(u16),
// This only handles int and float64 constants,
// we need something else for "address into string table"
constvals: ArrayList(u64), // raw data for constants, can contain embedded data
// This is only used for preds currently, but use it for more stuff??
refs: ArrayList(u16),
narg: u16 = 0,

// free list for blocks. single linked, only use b.items[b_free].pred !
b_free: u16 = NoRef,

// free list for blocks. single linked, only use i.items[i_free].op1 !
i_free: u16 = NoRef,

// variables are not really real, they are not referenced as such.
// They act as a shorthand for adding phi nodes during IR construction
// in a later ssa_gvn pass these get properly connected
nvar: u16 = 0,
var_list: u16 = NoRef, // first variable, then i.next

// this is a bit crude, we assume all nodes are unsealed during construction
// (i e any node could have not yet known predecessors)
// and then seal all blocks at once in the ssa cleanup step.
unsealed: bool = true,

// variables 2.0: virtual registero
// vregs is any result which is live outside of its defining node, that's it.
nvreg: u16 = 0,
vregs: ArrayList(struct { ref: u16, def_node: u16, live_in: u64 = 0, conflicts: u16 = 0 }),

// 8-byte slots in stack frame
nslots: u8 = 0,
nsave: u8 = 0,
ndf: u16 = 0,
call_clobber_mask: u16 = undefined,
abi_tag: ABITag = undefined,

var_names: ArrayList(?[]const u8),

// currently mandatory for correctness
construction_peep: bool = true,

// filler value for unintialized refs. not a sentinel for
// actually invalid refs!
pub const DEAD: u16 = 0xFEFF;
// For blocks: we cannot have more than 2^14 blocks anyway
// for vars: don't allocate last block!
pub const NoRef: u16 = 0xFFFF;
// 255 constants should be enough for everyone
// could be smarter, like dedicated Block type for constants!
pub const ConstOff: u16 = 0xFF00;

pub fn uv(s: usize) u16 {
    return @intCast(s);
}

pub const Node = struct {
    s: [2]u16 = .{ 0, 0 }, // sucessors
    dfnum: u16 = 0,

    predref: u16 = 0,
    npred: u16 = 0,
    // NB: might be NoRef if the node was deleted,
    // a reachable node must have at least one block even if empty!
    // TODO: now what blocks are not "referenced", first block should be inline (and sm0ler, mayhaps)
    firstblk: u16,
    lastblk: u16,
    live_in: u64 = 0, // TODO: globally allocate a [n_nodes*nvreg] multibitset

    loop: u16 = 0,
    rpolink: u8 = 0,
    is_header: bool = false, // if true, loop refers to parent loop
    loop_end: u16 = NoRef, // if is_header, first node after the loop

    // if true, block will not contain any instructions and will not be emitted
    // codegen should chase through n.s[0] until an non-empty block is found
    is_empty: bool = false,

    phi_list: u16 = NoRef, // currently also scheduled (CRINGE)
    putphi_list: u16 = NoRef, // also used for "putvar" entries in the construction phase
};

// amd64 ABI

// really 14 usable, but let's keep simple by not renumbering ipregs
pub const n_ipreg = 16;

// erase(xregs: [N]X86Asm.IPReg) [N]defs.IPReg
pub fn erase(xregs: anytype) [xregs.len]defs.IPReg {
    var r: [xregs.len]defs.IPReg = undefined;
    for (xregs, 0..) |x, i| {
        r[i] = @enumFromInt(@intFromEnum(x));
    }
    return r;
}

// TODO: messy. first ABI was just a comptime struct, now it is also a tag. reorganize!
pub const ABITag = enum {
    X86,
    BPF,
};

pub const X86ABI = struct {
    const tag: ABITag = .X86;
    const Reg = X86Asm.IPReg;
    // Args: used used both for incoming args and nested calls (including syscalls)
    pub const argregs = erase([6]Reg{ .rdi, .rsi, .rdx, .rcx, .r8, .r9 });
    pub const ret_reg = Reg.rax.into();

    // canonical order of callee saved registers. nsave>0 means
    // the first nsave items needs to be saved and restored
    pub const callee_saved = erase([5]Reg{ .rbx, .r12, .r13, .r14, .r15 });
    pub const call_unsaved = erase([9]Reg{ .rax, .rcx, .rdx, .rsi, .rdi, .r8, .r9, .r10, .r11 });

    // excludes: stack reg, frame reg (just say NO to -fomiting the framepointer!)
    const reg_order: [14]defs.IPReg = call_unsaved ++ callee_saved;
};

pub const BPF_ABI = struct {
    const tag: ABITag = .BPF;
    const Reg = BPF.Insn.Reg;
    // Args: used used both for incoming args and nested calls (including syscalls)
    pub const argregs = erase([5]Reg{ .r1, .r2, .r3, .r4, .r5 });
    pub const ret_reg: defs.IPReg = @enumFromInt(0);

    // canonical order of callee saved registers. nsave>0 means
    // the first nsave items needs to be saved and restored
    pub const callee_saved = erase([4]Reg{ .r6, .r7, .r8, .r9 });
    pub const call_unsaved = erase([6]Reg{ .r0, .r1, .r2, .r3, .r4, .r5 });

    // excludes: r10 (immutable frame pointer)
    const reg_order: [10]defs.IPReg = call_unsaved ++ callee_saved;
};

pub const BLK_SIZE = 16;
pub const Block = struct {
    node: u16,
    succ: u16 = NoRef,
    pred: u16 = NoRef,
    fakenum_: u16 = undefined, // only used for printing. could be eliminated if we really need it
    i: [BLK_SIZE]u16 = .{NoRef} ** BLK_SIZE,

    pub fn next(self: @This()) ?u16 {
        return if (self.succ != NoRef) self.succ else null;
    }

    pub fn prev(self: @This()) ?u16 {
        return if (self.pred != NoRef) self.pred else null;
    }
};

// looks cute, might delete later
pub fn intspec(s: defs.ISize) defs.SpecType {
    return .{ .intptr = s };
}

test "sizey" {
    // @compileLog(@sizeOf(Inst));
    // @compileLog(@sizeOf(Block));
    assert(@sizeOf(Block) <= 64);
}

fn ipreg_flag(reg: u4) u16 {
    return @as(u16, 1) << reg;
}

fn vreg_flag(reg: u6) u64 {
    return @as(u64, 1) << reg;
}

fn node_flag(node: u6) u64 {
    return @as(u64, 1) << node;
}

pub fn constidx(ref: u16) ?u16 {
    if (ref == NoRef) return null;
    if (ref >= ConstOff) {
        return ref - ConstOff;
    }
    return null;
}

pub fn constval(self: *Self, ref: u16) ?u64 {
    const idx = constidx(ref) orelse return null;
    return self.constvals.items[idx];
}

// if null: still could be a value, just not an ipval
pub fn ipval(self: *Self, ref: u16) ?defs.IPMCVal {
    if (constidx(ref)) |idx| {
        const val: u64 = self.constvals.items[idx];
        const vali32: i32 = @bitCast(@as(u32, @truncate(val)));
        // this is a silly way of saing "is there a i32,
        // which sign-extended to i64 is bit-equal to this value?"
        if (@as(i64, @bitCast(val)) == vali32) {
            return .{ .constval = vali32 };
        } else {
            return .{ .constref = idx };
        }
    } else if (self.iref(ref)) |i| {
        return i.ipval();
    } else {
        return null;
    }
}

pub fn ipreg(self: *Self, ref: u16) ?defs.IPReg {
    return (self.iref(ref) orelse return null).ipreg();
}

pub fn avxreg(self: *Self, ref: u16) ?u4 {
    return (self.iref(ref) orelse return null).avxreg();
}

fn initConsts(self: *Self) void {
    self.constvals.appendAssumeCapacity(0);
    self.constvals.appendAssumeCapacity(1);
}

pub fn init(n: u16, allocator: Allocator) !Self {
    var self = Self{
        .a = allocator,
        .n = try .initCapacity(allocator, n),
        .b = try .initCapacity(allocator, n),
        .i = try .initCapacity(allocator, 4 * n),
        .vregs = .init(allocator),
        .blkorder = .init(allocator),
        .preorder = .init(allocator),
        .var_names = .init(allocator),
        .refs = try .initCapacity(allocator, 4 * n),
        .constvals = try .initCapacity(allocator, 8),
    };
    self.initConsts();
    return self;
}

pub fn reinit(self: *Self) void {
    const info = @typeInfo(Self).@"struct";

    // 1. reset fields with initializers to their initial value (most int members)
    // 2. set ArrayList members to empty (but keep existing allocations)
    inline for (info.fields) |field| {
        if (field.defaultValue()) |default_value| {
            @field(self, field.name) = default_value;
        } else if (@typeInfo(field.type) == .@"struct") {
            if (@hasField(field.type, "items")) {
                @field(self, field.name).items.len = 0;
            }
        }
    }

    self.initConsts();
}

pub fn deinit(self: *Self) void {
    self.n.deinit();
    self.b.deinit();
    self.i.deinit();
    self.blkorder.deinit();
    self.preorder.deinit();
    self.var_names.deinit(); // actual strings are owned by producer
    self.refs.deinit();
    self.constvals.deinit();
    self.vregs.deinit();
}

pub fn toref() u16 {
    @compileError("REF USED");
}

pub fn iref(self: *Self, ref: u16) ?*Inst {
    if (ref >= ConstOff) {
        return null;
    }
    if (ref >= self.i.items.len) @panic("INVALID REF :(((");
    return &self.i.items[ref];
}

pub fn addNode(self: *Self) !u16 {
    const n = try self.n.addOne();
    const b = try self.b.addOne();
    const nodeid = uv(self.n.items.len - 1);
    const blkid = uv(self.b.items.len - 1);
    n.* = .{ .firstblk = blkid, .lastblk = blkid };
    b.* = .{ .node = nodeid };
    return nodeid;
}

// not-taken/unconditional branch of `n` set to the new node
pub fn addNodeAfter(self: *Self, node: u16) !u16 {
    const new_node = try self.addNode();
    try self.addLink(node, 0, new_node);
    return new_node;
}

pub fn addLink(self: *Self, node_from: u16, branch: u1, node_to: u16) !void {
    const succ = &self.n.items[node_from].s[branch];
    if (succ.* != 0) return error.FLIRError;
    succ.* = node_to;
}

pub fn addRawInst(self: *Self, inst: Inst) !u16 {
    if (self.i_free != NoRef) {
        const ref = self.i_free;
        self.i_free = self.i.items[ref].op1;
        self.i.items[ref] = inst;
        return ref;
    } else {
        const ref = uv(self.i.items.len);
        try self.i.append(inst);
        return ref;
    }
}

pub fn addInst(self: *Self, node: u16, inst: Inst) !u16 {
    var i = inst;
    if (self.unsealed) {
        const nop = i.n_op(false);
        if (nop > 0) {
            i.op1 = try self.read_ref(node, i.op1);
            if (nop > 1) {
                i.op2 = try self.read_ref(node, i.op2);
            }
        }
    }
    const ref = try self.addRawInst(i);
    try self.addInstRef(node, ref);
    return ref;
}

// add inst to the end of block
pub fn addInstRef(self: *Self, node: u16, inst: u16) !void {
    if (inst >= ConstOff) @panic("NOT LIKE THIS");
    const n = &self.n.items[node];
    // must exist:
    var blkid = n.lastblk;

    self.iref(inst).?.node_delete_this = node; // This Says itself, DELET THIS eventually

    // TODO: later we can add more constraints for where "NoRef" ins can be
    var lastfree: u8 = BLK_SIZE;
    var i: u8 = BLK_SIZE - 1;
    while (true) : (i -= 1) {
        if (self.b.items[blkid].i[@as(u8, @intCast(i))] == NoRef) {
            lastfree = i;
        } else {
            break;
        }
        if (i == 0) {
            break;
        }
    }

    if (lastfree == BLK_SIZE) {
        const prevblk = blkid;
        blkid = try self.new_blk();
        self.b.items[prevblk].succ = blkid;
        self.b.items[blkid] = .{ .node = node, .pred = prevblk };
        n.lastblk = blkid;
        lastfree = 0;
    }

    self.b.items[blkid].i[lastfree] = inst;
}

pub fn new_blk(self: *Self) !u16 {
    if (self.b_free != NoRef) {
        const blkid = self.b_free;
        self.b_free = self.b.items[blkid].pred;
        return blkid;
    }

    const blkid = uv(self.b.items.len);
    _ = try self.b.addOne();
    return blkid;
}

pub fn const_uint(self: *Self, val: u64) !u16 {
    for (self.constvals.items, 0..) |v, i| {
        if (v == val) return uv(ConstOff + i);
    }
    if (self.constvals.items.len == 511) return error.FLIRError;
    const ref = ConstOff + self.constvals.items.len;
    try self.constvals.append(val);
    return uv(ref);
}

pub fn const_int(self: *Self, val: i32) !u16 {
    return const_uint(self, @as(u32, @bitCast(val)));
}

const sphigh = Inst.sphigh;
// TODO: f64 vs f32
// TODO: special case zero, one, mayhaps?
// TODO: tricky with vectors, we might both simple broadcast and vector constants.
pub fn const_float(self: *Self, node: u16, val: f64) !u16 {
    const constref = try self.const_uint(@bitCast(val));
    return self.addInst(node, .{ .tag = .fconst, .op1 = constref, .op2 = NoRef, .spec = sphigh(@intFromEnum(FMode.sd), 0) });
}

pub fn binop(self: *Self, node: u16, tag: Inst.Tag, op1: u16, op2: u16) !u16 {
    return self.addInst(node, .{ .tag = tag, .op1 = op1, .op2 = op2 });
}

pub fn load(self: *Self, node: u16, kind: defs.SpecType, base: u16, idx: u16, scale: u2) !u16 {
    return self.addInst(node, .{ .tag = .load, .op1 = base, .op2 = idx, .spec = sphigh(scale, kind.into()) });
}
pub fn store(self: *Self, node: u16, kind: defs.SpecType, base: u16, idx: u16, scale: u2, val: u16) !u16 {
    // FUBBIT: all possible instances of fusing should be detected in analysis anyway
    const addr = if (idx != NoRef) try self.addInst(node, .{ .tag = .lea, .op1 = base, .op2 = idx, .mckind = .fused, .spec = sphigh(scale, 0) }) else base;
    return self.addInst(node, .{ .tag = .store, .op1 = addr, .op2 = val, .spec = sphigh(0, kind.into()) });
}

pub fn bpf_load_map(self: *Self, node: u16, map_idx: u32, is_value: bool) !u16 {
    assert(map_idx < 0x10000);
    const low_idx: u16 = @truncate(map_idx);
    return self.addInst(node, .{ .tag = .bpf_load_map, .op1 = low_idx, .op2 = 0, .spec = if (is_value) 1 else 0 });
}

const FMode = X86Asm.FMode;
pub fn vmath(self: *Self, node: u16, vop: X86Asm.VMathOp, fmode: FMode, op1: u16, op2: u16) !u16 {
    // TODO: somewhere, typecheck that FMode matches fmode of args..
    return self.addInst(node, .{ .tag = .vmath, .spec = Inst.vmathspec(vop, fmode), .op1 = op1, .op2 = op2 });
}

pub fn vcmpf(self: *Self, node: u16, vop: X86Asm.VCmpOp, fmode: FMode, op1: u16, op2: u16) !u16 {
    return self.addInst(node, .{ .tag = .vcmpf, .spec = Inst.vcmpfspec(vop, fmode), .op1 = op1, .op2 = op2 });
}

// TODO: a bit contradictory naming with IntCond
pub fn fcmp(self: *Self, node: u16, cond: IntCond, fmode: FMode, op1: u16, op2: u16) !u16 {
    if (!fmode.scalar()) return error.FLIRError;
    return self.addInst(node, .{ .tag = .fcmp, .spec = Inst.fcmpspec(cond, fmode), .op1 = op1, .op2 = op2 });
}

pub fn int2float(self: *Self, node: u16, fmode: FMode, op1: u16) !u16 {
    // maybe a packed should implicitly convert and then broadcast?
    if (!fmode.scalar()) return error.FLIRError;
    return self.addInst(node, .{ .tag = .int2vf, .spec = Inst.vcvtspec(fmode), .op1 = op1, .op2 = NoRef });
}

pub fn float2int(self: *Self, node: u16, fmode: FMode, op1: u16) !u16 {
    if (!fmode.scalar()) return error.FLIRError;
    return self.addInst(node, .{ .tag = .vf2int, .spec = Inst.vcvtspec(fmode), .op1 = op1, .op2 = NoRef });
}

// TODO: 32bit vs 64bit (also for int in i2f and f2i, and so on)
pub fn iunop(self: *Self, node: u16, size: defs.ISize, op: IntUnOp, op1: u16) !u16 {
    if (self.construction_peep) {
        if (try self.peep_iunop(op, size, op1)) |res| {
            return res;
        }
    }
    return self.addInst(node, .{ .tag = .iunop, .spec = sphigh(@intFromEnum(size), @intFromEnum(op)), .op1 = op1, .op2 = NoRef });
}

// TODO: 32bit vs 64bit (also for int in i2f and f2i, and so on)
pub fn ibinop(self: *Self, node: u16, size: defs.ISize, op: IntBinOp, op1: u16, op2: u16) !u16 {
    return self.addInst(node, .{ .tag = .ibinop, .spec = sphigh(@intFromEnum(size), @intFromEnum(op)), .op1 = op1, .op2 = op2 });
}

pub fn icmp(self: *Self, node: u16, size: defs.ISize, cond: IntCond, op1: u16, op2: u16) !u16 {
    return self.addInst(node, .{ .tag = .icmp, .spec = sphigh(@intFromEnum(size), cond.off()), .op1 = op1, .op2 = op2 });
}

pub fn icmpset(self: *Self, node: u16, size: defs.ISize, cond: IntCond, op1: u16, op2: u16) !u16 {
    if (self.construction_peep) {
        if (self.peep_icmp(cond, size, op1, op2)) |res| {
            return self.const_uint(if (res) 1 else 0);
        }
    }
    return self.addInst(node, .{ .tag = .icmpset, .spec = sphigh(@intFromEnum(size), cond.off()), .op1 = op1, .op2 = op2 });
}

pub fn putvar(self: *Self, node: u16, vref: u16, value: u16) !void {
    // value could be another varref
    const refval = try self.read_ref(node, value);
    const v = self.iref(vref) orelse return error.FLIRError;
    if (v.tag != .variable) return error.FLIRError;

    const n = &self.n.items[node];
    var put_iter = n.putphi_list;
    while (put_iter != NoRef) {
        const p = &self.i.items[put_iter];
        if (p.tag == .putvar and p.op2 == v.op1) {
            p.op1 = refval;
            return;
        }
        put_iter = p.next;
    }
    n.putphi_list = try self.addRawInst(.{ .tag = .putvar, .op1 = refval, .op2 = v.op1, .next = n.putphi_list, .node_delete_this = node });
}

pub fn ret(self: *Self, node: u16, kind: defs.SpecType, val: u16) !void {
    _ = try self.addInst(node, .{ .tag = .ret, .op1 = val, .op2 = 0, .spec = sphigh(0, kind.into()) });
}

pub fn callarg(self: *Self, node: u16, num: u8, ref: u16) !void {
    _ = try self.addInst(node, .{
        .tag = .callarg,
        .spec = num,
        .op1 = ref,
        .op2 = 0,
    });
}

pub fn call(self: *Self, node: u16, kind: defs.CallKind, num: u16, extra: u16) !u16 {
    const c = try self.addInst(node, .{
        .tag = .call,
        .spec = @intFromEnum(kind),
        .op1 = num,
        .op2 = extra,
    });

    // TODO: this should just be a Empty padding. ABI copies should be generated by regalloc!
    return self.addInst(node, .{ .tag = .copy, .spec = intspec(.dword).into(), .op1 = c, .op2 = NoRef });
}

pub fn addPhi(self: *Self, node: u16, vidx: u16, vspec: u8) !u16 {
    const n = &self.n.items[node];
    const ref = try self.addRawInst(.{ .tag = .phi, .op1 = vidx, .op2 = NoRef, .spec = vspec, .f = .{ .kill_op1 = true }, .next = n.phi_list, .node_delete_this = node });
    n.phi_list = ref;
    return ref;
}

// TODO: maintain wf of block 0: first all args, then all vars.

pub fn arg(self: *Self) !u16 {
    if (self.n.items.len == 0) return error.FLIRError;
    const inst = try self.addInst(0, .{ .tag = .arg, .op1 = self.narg, .op2 = 0, .spec = intspec(.dword).into() });
    self.narg += 1;
    return inst;
}

pub fn variable(self: *Self, typ: defs.SpecType) !u16 {
    if (self.n.items.len == 0) return error.FLIRError;
    const inst = try self.addRawInst(.{ .tag = .variable, .op1 = self.nvar, .op2 = 0, .spec = typ.into(), .next = self.var_list });
    self.var_list = inst;
    self.nvar += 1;
    return inst;
}

pub fn preds(self: *Self, i: u16) []u16 {
    const v = self.n.items[i];
    return self.refs.items[v.predref..][0..v.npred];
}

pub fn cleanup_preds(self: *Self, i: u16) void {
    const v = &self.n.items[i];
    const p = self.refs.items[v.predref..];
    var pos: u16 = 0;
    while (pos < v.npred) {
        if (p[pos] != NoRef) {
            pos += 1;
        } else {
            // special case pos=(v.npred - 1) works due to immediate deletion:p
            p[pos] = p[v.npred - 1];
            v.npred -= 1;
        }
    }
}

fn predlink(self: *Self, i: u16, si: u1, split: bool) !void {
    var n = self.n.items;
    const s = n[i].s[si];
    if (s == 0) return;

    if (split and n[s].npred > 1) {
        const inter = try self.addNode();
        n = self.n.items; // haii
        n[inter].npred = 1;
        n[i].s[si] = inter;
        n[inter].s[0] = s;
        try addpred(self, s, inter);
        try addpred(self, inter, i);
    } else {
        try addpred(self, s, i);
    }
}

fn addpred(self: *Self, s: u16, i: u16) !void {
    const n = self.n.items;
    // tricky: build the reflist per node backwards,
    // so the end result is the start index
    if (n[s].predref == 0) {
        try self.refs.appendNTimes(DEAD, n[s].npred);
        n[s].predref = uv(self.refs.items.len);
    }
    n[s].predref -= 1;
    self.refs.items[n[s].predref] = i;
}

pub fn calc_preds(self: *Self) !void {
    // TODO: policy for rebuilding refs from scratch?
    if (self.refs.items.len > 0) unreachable;
    for (self.n.items) |v| {
        if (v.s[0] > 0) {
            self.n.items[v.s[0]].npred += 1;
        }
        if (v.s[1] > 0 and v.s[1] != v.s[0]) {
            self.n.items[v.s[1]].npred += 1;
        }
    }

    for (0..self.n.items.len) |i| {
        // by value. predlink might reallocate self.n in place!
        const v = self.n.items[i];
        const shared = v.s[1] > 0 and v.s[1] == v.s[0];
        if (shared) return error.NotSureAboutThis;
        const split = v.s[1] > 0;
        try self.predlink(@intCast(i), 0, split);
        try self.predlink(@intCast(i), 1, split);
    }
}

pub fn calc_loop(self: *Self) !void {
    _ = try self.rpo_visit(0);
    const big_n = self.preorder.items.len;
    try self.blkorder.append(0); // ROOT
    if (big_n > 1) {
        try self.loop_order(uv(big_n - 1));
    }
}

// h=preorder[ph] is either 0 (the entire graph) or a loop header
pub fn loop_order(self: *Self, ph: u16) !void {
    var i = ph;
    const h = self.preorder.items[ph];
    var last_item = self.blkorder.items.len;
    while (true) {
        i -= 1;
        const node = self.preorder.items[i];
        const n = &self.n.items[node];
        if (n.loop == h) {
            try self.blkorder.append(node);
            if (n.is_header) {
                // try self.loop_order(i);
                try loop_order(self, i);
            }
            last_item = self.blkorder.items.len;
        }
        if (i == 0) break;
    }
    self.n.items[h].loop_end = uv(last_item);
}

pub fn rpo_visit(self: *Self, node: u16) !?u16 {
    const n = &self.n.items[node];
    if (n.rpolink != 0) {
        if (n.rpolink == 1) {
            return node;
        } else {
            return if (n.loop != 0) n.loop else null;
        }
    }
    n.dfnum = self.ndf;
    self.ndf += 1;
    n.rpolink = 1;
    var loop: ?u16 = null;
    if (n.s[0] != 0) {
        // loop = try self.rpo_visit(n.s[0]);
        loop = try rpo_visit(self, n.s[0]);
    }
    if (n.s[1] != 0) {
        // const loop2 = try self.rpo_visit(n.s[1]);
        const loop2 = try rpo_visit(self, n.s[1]);
        if (loop) |l| {
            if (loop2) |l2| {
                // TODO: there could be a loop lX so that l < lX < l2
                if (self.n.items[l2].dfnum > self.n.items[l].dfnum) {
                    self.n.items[l2].loop = l;
                    loop = l2;
                } else if (l != l2) {
                    self.n.items[l].loop = l2;
                }
            }
        } else {
            loop = loop2;
        }
    }
    try self.preorder.append(node);
    n.rpolink = 2;

    if (node == loop) {
        n.is_header = true;
        const parent = if (n.loop != 0) n.loop else null;
        return parent;
    } else {
        if (loop) |l| n.loop = l;
        return loop;
    }
}

pub fn reorder_nodes(self: *Self) !void {
    const newlink = try self.a.alloc(u16, self.n.items.len);
    defer self.a.free(newlink);
    @memset(newlink, NoRef);
    const oldlink = try self.a.alloc(u16, self.n.items.len);
    defer self.a.free(oldlink);
    @memset(oldlink, NoRef);
    var newpos: u16 = 0;

    for (self.blkorder.items) |old_ni| {
        const ni = if (oldlink[old_ni] != NoRef) oldlink[old_ni] else old_ni;
        const n = &self.n.items[ni];

        newlink[old_ni] = newpos;

        if (ni != newpos) {
            const oldval = if (oldlink[newpos] != NoRef) oldlink[newpos] else newpos;
            oldlink[ni] = oldval;

            oldlink[oldval] = ni;
        }

        if (ni != newpos) {
            mem.swap(Node, n, &self.n.items[newpos]);
        }
        newpos += 1;
    }

    assert(newpos <= self.n.items.len);
    // oopsie woopsie, we killed some dead nodes!
    self.n.items.len = newpos;

    // fixup references:
    for (self.n.items, 0..) |*n, ni| {
        for (&n.s) |*s| {
            if (s.* != NoRef) {
                s.* = newlink[s.*];
            }
        }

        for (self.preds(uv(ni))) |*pi| {
            pi.* = newlink[pi.*];
        }
        self.cleanup_preds(uv(ni));

        n.loop = newlink[n.loop];

        var cur_blk: ?u16 = n.firstblk;
        while (cur_blk) |blk| {
            const b = &self.b.items[blk];
            b.node = uv(ni);

            cur_blk = b.next();
        }

        var it = self.ins_iterator(n.firstblk);
        while (it.next()) |item| {
            item.i.node_delete_this = uv(ni); // This Says itself, DELET THIS eventually
        }

        var phi = n.phi_list;
        while (phi != NoRef) {
            const i = self.iref(phi) orelse return error.FLIRError;
            i.node_delete_this = uv(ni);
            phi = i.next;
        }
    }
}

// not a complete optimization pass yet, just the thing needed for legalization
pub fn const_fold_legalize(self: *Self) !void {
    for (self.n.items, 0..) |*n, ni| {
        var it = self.ins_iterator(n.firstblk);
        while (it.next()) |item| {
            const i = item.i;
            if (i.tag == .icmp) {
                if (self.peep_icmp(i.intcond(), i.iop_size(), i.op1, i.op2)) |res| {
                    const deleted = if (res) n.s[0] else n.s[1];
                    if (res) n.s[0] = n.s[1];
                    n.s[1] = 0;
                    self.delete_itersafe(item);

                    const v = &self.n.items[deleted];
                    // TODO: rethink this, deleting preds should be easier..
                    if (v.npred > 0) {
                        const p = self.refs.items[v.predref..];
                        for (0..v.npred) |ipred| {
                            if (p[ipred] == ni) {
                                p[ipred] = p[v.npred - 1];
                                v.npred -= 1;
                                break;
                            }
                        }
                    }
                }
            }
        }
    }
}

pub fn peep_icmp(self: *Self, cond: IntCond, size: defs.ISize, op1: u16, op2: u16) ?bool {
    if (self.constval(op1)) |lhs| {
        if (self.constval(op2)) |rhs| {
            // TODO: haha, 64-bit signed comp??
            _ = size;
            const i_lhs: i32 = @intCast(@as(u32, @truncate(lhs)));
            const i_rhs: i32 = @intCast(@as(u32, @truncate(rhs)));
            return switch (cond) {
                .eq => lhs == rhs,
                .neq => lhs != rhs,
                .gt => i_lhs > i_rhs,
                .ge => i_lhs >= i_rhs,
                .lt => i_lhs < i_rhs,
                .le => i_lhs <= i_rhs,
                .a => lhs > rhs,
                .na => lhs <= rhs,
                .b => lhs < rhs,
                .nb => lhs >= rhs,
            };
        }
    }
    return null;
}

// on success, return constval
pub fn peep_iunop(self: *Self, op: IntUnOp, size: defs.ISize, input: u16) !?u16 {
    if (self.constval(input)) |lhs| {
        _ = size; // TODi: truncate lhs to "size" bitwidth
        switch (op) {
            .ctz => return try self.const_uint(@ctz(lhs)),
            else => return null,
        }
    }
    return null;
}

// ni = node id of user
pub fn adduse(self: *Self, ni: u16, user: u16, used: u16) void {
    _ = user;
    const i = self.iref(used) orelse return;
    //ref.i.n_use += 1;
    // it leaks to another block: give it a virtual register number
    if (i.node_delete_this != ni) {
        if (!i.f.is_vreg) {
            i.f.is_vreg = true;
            i.vreg_scratch = self.nvreg;
            self.nvreg += 1;
            self.vregs.appendAssumeCapacity(.{ .ref = used, .def_node = i.node_delete_this });
        }
    }
}

// TODO: this is really LIVENESS for the sake of regalloc.
// want simpler use-def/def-use for optimizations, especially if we
// go sea-of-nodeish with unscheduled opts
//
// not idempotent! does not reset n_use=0 first.
pub fn calc_live(self: *Self) !void {
    // TODO: NOT LIKE THIS
    try self.vregs.ensureTotalCapacity(64);

    // step1: allocate vregs for results that leak from the block
    for (self.n.items, 0..) |*n, ni| {
        var it = self.ins_iterator(n.firstblk);
        while (it.next()) |item| {
            const i = item.i;
            for (i.ops(false)) |*op| {
                self.adduse(uv(ni), item.ref, op.*);
            }
        }

        var item = n.putphi_list;
        while (item != NoRef) {
            const i = self.iref(item) orelse return error.FLIRError;
            self.adduse(uv(ni), item, i.op1);
            item = i.next;
        }
    }

    // step 2: process the dominator tree backwards (flattened works fine) to
    // progagate uses to predecessors
    {
        var ni: u16 = uv(self.n.items.len - 1);
        // TODO: at this point the number of vregs is known. so a bitset for
        // node X vreg can be allocated here.

        if (self.nvreg > 64) return error.DoTheWork;

        while (true) : (ni -= 1) {
            const n = &self.n.items[ni];
            var live_vregs: u64 = 0;
            for (n.s) |s| {
                if (s != 0) {
                    live_vregs |= self.n.items[s].live_in;
                }
            }
            // print("LIVEUT {}: {x} (dvs {})\n", .{ ni, live_vregs, @popCount(live_vregs) });

            var neg_counter: u16 = 0;
            var last_clobber: [n_ipreg]u16 = .{0} ** n_ipreg;

            var node_has_clobber: bool = false;

            var listit = n.putphi_list;
            while (listit != NoRef) {
                // putphi:s arent ordered, so they all have neg_counter == 0
                const i = self.iref(listit) orelse return error.FLIRError;
                self.mark_ops_as_used(i, &live_vregs, neg_counter);
                listit = i.next;
            }

            var it = self.ins_iterator_rev(n.lastblk);
            while (it.next_rev()) |item| {
                neg_counter += 1;
                const i = item.i;

                self.check_live_or_conflicts(i, &live_vregs, node_has_clobber, &last_clobber);

                // TODO: currently this forbids clobbers as inputs, which is wrong when killing the op.
                // just need to check for mismatches
                // consider x86 IDIV: as op1 is fixed to be EAX, op2 conflicts with EAX even when killed!
                self.mark_ops_as_used(i, &live_vregs, neg_counter);

                // registers clobbered by this instruction
                // TOOO: or having it as a fixed input, which is not ALWAYS a clobber
                // (like two shr intructions in a row sharing the same ecx input)
                const clobber_mask: u16 = try self.get_clobbers(i);

                // TODO: very ad-hoc,
                // currently there is no praxis for back-propagating reghints
                // like this one should obviosly work across a (%1 = phi; ret %1) and so on and so on
                if (i.tag == .ret) {
                    if (self.abi_tag == .X86) {
                        if (self.iref(i.op1)) |ref| {
                            if (ref.mckind == .unallocated_raw and i.res_type() == .intptr) {
                                ref.mckind = .unallocated_ipreghint;
                                ref.mcidx = X86Asm.IPReg.rax.id();
                            }
                        }
                    }
                }

                if (clobber_mask != 0) {
                    node_has_clobber = true; // quick skipahead
                    for (self.vregs.items, 0..) |*v, vi| {
                        if (live_vregs & vreg_flag(@as(u6, @intCast(vi))) != 0) {
                            v.conflicts |= clobber_mask;
                        }
                    }
                    for (0..n_ipreg) |r| {
                        if ((clobber_mask & ipreg_flag(@intCast(r))) != 0) {
                            last_clobber[r] = neg_counter;
                        }
                    }
                }
            }

            var phi = n.phi_list;
            while (phi != NoRef) {
                const i = self.iref(phi) orelse return error.FLIRError;
                self.check_live_or_conflicts(i, &live_vregs, node_has_clobber, &last_clobber);
                phi = i.next;
            }

            n.live_in = live_vregs;
            // print("LIVEIN {}: {x} (dvs {})\n", .{ ni, n.live_in, @popCount(n.live_in) });

            if (n.is_header) {
                // TODO: make me a loop membership bitset globally
                // TODO: if there was a call anywhere in the loop
                // propagate call clobbers onto the loop invariant vregs
                if (self.n.items.len > 64) unreachable;
                var loop_set: u64 = node_flag(@intCast(ni));
                for (ni + 1..self.n.items.len) |ch_i| {
                    const ch_n = &self.n.items[ch_i];
                    const ch_loop: u64 = node_flag(@intCast(ch_i));
                    if ((node_flag(@intCast(ch_n.loop)) & loop_set) != 0) {
                        if (ch_n.is_header) {
                            loop_set |= ch_loop;
                        }
                        ch_n.live_in |= n.live_in;
                    } else {
                        break; // assuming the contigous property by now
                    }
                }
            }
            if (ni == 0) break;
        }
    }

    // step 3: mark the instructions which kill a live range, which will be used by regalloc
    for (self.n.items, 0..) |*n, ni| {
        // transpose the node.live_in[vreg] bitfield into vreg.live_in[node] bitfield
        for (self.vregs.items, 0..) |*v, vi| {
            if ((n.live_in & vreg_flag(@intCast(vi))) != 0) {
                v.live_in |= node_flag(@intCast(ni));
            }
        }

        var live_out: u64 = 0;
        for (n.s) |s| {
            live_out |= self.n.items[s].live_in;
        }
        var killed = n.live_in & ~live_out;

        var listit = n.putphi_list;
        while (listit != NoRef) {
            const i = self.iref(listit) orelse return error.FLIRError;
            self.mark_ops_who_kill(i, &killed);
            listit = i.next;
        }

        var it = self.ins_iterator_rev(n.lastblk);
        while (it.next_rev()) |item| {
            self.mark_ops_who_kill(item.i, &killed);
        }
    }
}

fn check_live_or_conflicts(self: *Self, i: *Inst, live_vregs: *u64, node_has_clobber: bool, last_clobber: *const [n_ipreg]u16) void {
    _ = self;

    // TODO: need to consider phi:s even when they are unscheduled
    if (i.vreg()) |vreg| {
        live_vregs.* &= ~vreg_flag(@intCast(vreg));
    } else {
        if (i.vreg_scratch != NoRef) {
            if (node_has_clobber) { // quick skipahead when no clobbers
                var conflicts: u16 = 0;
                for (0..n_ipreg) |r| {
                    // negative counter: is the last_clobber _before_ the kill position
                    if (i.vreg_scratch < last_clobber[r]) {
                        conflicts |= ipreg_flag(@intCast(r));
                    }
                }
                if (conflicts != 0) {
                    i.f.conflicts = true;
                }
                i.vreg_scratch = conflicts;
            } else {
                i.vreg_scratch = 0;
            }
        }
    }
}

fn mark_ops_as_used(self: *Self, i: *Inst, live: *u64, neg_counter: u16) void {
    for (i.ops(false)) |op| {
        if (self.iref(op)) |ref| {
            if (ref.vreg()) |vreg| {
                live.* |= vreg_flag(@intCast(vreg));
            } else {
                if (ref.vreg_scratch == NoRef) {
                    ref.vreg_scratch = neg_counter;
                }
            }
        }
    }
}

fn mark_ops_who_kill(self: *Self, i: *Inst, killed: *u64) void {
    for (i.ops(false), 0..) |op, i_op| {
        var kill: bool = false;
        if (self.iref(op)) |ref| {
            if (ref.vreg()) |vreg| {
                const bit = vreg_flag(@intCast(vreg));
                if ((killed.* & bit) != 0) {
                    killed.* &= ~bit;
                    kill = true;
                }
            } else {
                if (!ref.f.killed) {
                    ref.f.killed = true;
                    kill = true;
                }
            }
        }
        if (kill) {
            if (i_op == 1) {
                i.f.kill_op2 = true;
            } else {
                i.f.kill_op1 = true;
            }
        }
    }
}

pub fn get_clobbers(self: *Self, i: *Inst) !u16 {
    // other platforms: why can't you just be normal. x86:
    const is_x86 = self.abi_tag == .X86;

    if (is_x86 and i.tag == .ibinop) {
        const Reg = X86Asm.IPReg;
        const op = i.ibinop();
        if (op == .sdiv or op == .udiv or op == .srem or op == .urem) {
            if (i.mckind == .unallocated_raw) {
                // FULING, but sure,  why now now?
                i.mckind = .unallocated_ipreghint;
                i.mcidx = Reg.rax.id();
            }
            return ipreg_flag(Reg.rax.id()) | ipreg_flag(Reg.rdx.id());
        } else if (op == .rotl or op == .rotr) {
            // TODO: rather input constraint only
            // TODO: but only if non-const!!
            return ipreg_flag(Reg.rcx.id());
        }
    }

    if (i.tag != .call) {
        return 0;
    }

    // call: result not considered live.
    // arguments are handled in callarg and are also not conflicting
    const kind: defs.CallKind = @enumFromInt(i.spec);
    if (kind == .memory_intrinsic and is_x86) {
        const Reg = X86Asm.IPReg;
        const idx = self.constval(i.op1) orelse return error.FLIRError;
        const intrinsic: defs.MemoryIntrinsic = @enumFromInt(idx);
        const base: u16 = ipreg_flag(Reg.rdi.id()) | ipreg_flag(Reg.rcx.id());
        if (intrinsic == .memset) {
            return base + ipreg_flag(Reg.rax.id());
        } else {
            return error.FLIRError;
        }
    }
    return self.call_clobber_mask;
}

pub fn alloc(self: *Self, node: u16, size: u8) !u16 {
    if (self.nslots == 255) {
        return error.OutOfMemory; // TODO: yes, but actually no
    }
    const slot = self.nslots + size - 1;
    self.nslots += size;
    // TODO: store actual size in .spec (alloc is always typed as a pointer)
    return self.addInst(node, .{ .tag = .alloc, .op1 = slot, .op2 = 0, .spec = intspec(.dword).into(), .mckind = .fused });
}

// makes space for a slot after instruction "after"
// invalidates all refs after "after" (in self.n order), but leaves earlier untouched.
// returns the slot (currenty having an Empty filler)
// no need to split if there is an Empty in the same block (or just after if "after" is last)
//
// assumes no direct references between blocks! that should via self.vregs by now!
//
// TODO: take an arg for how many slots are needed?
pub fn maybe_split(self: *Self, after: u16) !u16 {
    const r = undefined; // fromref(after);
    const blk = &self.b.items[r.block];
    const node = &self.n.items[blk.node];
    if (r.idx < BLK_SIZE - 1) {
        if (blk.i[r.idx + 1].tag == .empty) {
            return after + 1;
        }
    } else {
        if (blk.succ != NoRef) {
            const blk2 = &self.b.items[blk.succ];
            if (blk2.i[0].tag == .empty) {
                return toref(blk.succ, 0);
            }
        }
    }

    // TODO: if block looks like {xx, after, yy, empty}, we could instead move yy to empty
    // and return its place

    const old_succ = blk.succ;
    const blkid = uv(self.b.items.len);
    blk.succ = blkid;
    var newblk = try self.b.addOne();
    newblk.* = .{ .node = blk.node, .succ = old_succ, .pred = r.block };
    if (old_succ != NoRef) {
        self.b.items[old_succ].pred = blkid;
    } else {
        node.lastblk = blkid;
    }

    if (r.idx < BLK_SIZE - 1) {
        @memcpy(newblk.i[r.idx + 1 ..], blk.i[r.idx + 1 ..]);
        @memset(blk.i[r.idx + 1 ..], .EMPTY);
        for (r.idx + 1..BLK_SIZE) |i| {
            self.renumber(blkid, toref(r.block, uv(i)), toref(blkid, uv(i)));
            if (newblk.i[i].vreg()) |vreg| {
                self.vregs.items[vreg].ref = toref(blkid, uv(i));
            }
        }
        return toref(r.block, r.idx + 1);
    } else {
        return toref(blkid, 0);
    }
}

fn renumber(self: *Self, firstblk: u16, from: u16, to: u16) void {
    var cur_blk = firstblk;
    while (true) {
        const blk = &self.b.items[cur_blk];
        for (&blk.i) |*i| {
            const nop = i.n_op(true);
            if (nop > 0) {
                if (i.op1 == from) i.op1 = to;
                if (nop > 1) {
                    if (i.op2 == from) i.op2 = to;
                }
            }
        }
        cur_blk = blk.next() orelse break;
    }
}

// fills up some registers, and then goes to the stack.
// reuses op1 if it is from the same block and we are the last user
pub fn trivial_alloc(self: *Self) !void {
    // TRRICKY: start with the ABI arg registers, and just skip as many args as we have
    const regs: [8]defs.IPReg = undefined;
    var used: usize = self.narg;
    var avxused: u8 = 0;
    for (self.n.items) |*n| {
        var cur_blk: ?u16 = n.firstblk;
        while (cur_blk) |blk| {
            var b = &self.b.items[blk];
            for (b.i, 0..) |*i, idx| {
                const ref = toref(blk, uv(idx));

                if (i.has_res() and i.mckind.unallocated()) {
                    const regkind: defs.MCKind = if (i.res_type() == .avxval) .vfreg else .ipreg;
                    const op1 = if (i.n_op(false) > 0) self.iref(i.op1) else null;
                    if (op1) |o| {
                        if (o.mckind == regkind and o.vreg() == null and o.last_use == ref) {
                            i.mckind = regkind;
                            i.mcidx = o.mcidx;
                            continue;
                        }
                    }
                    if (i.res_type() == .avxval) {
                        if (avxused == 16) {
                            return error.GOOOF;
                        }
                        i.mckind = .vfreg;
                        i.mcidx = avxused;
                        avxused += 1;
                    } else if (used < regs.len) {
                        i.mckind = .ipreg;
                        i.mcidx = regs[used].id();
                        used += 1;
                    } else {
                        i.mckind = .frameslot;
                        if (self.nslots == 255) {
                            return error.UDunGoofed;
                        }
                        i.mcidx = self.nslots;
                        self.nslots += 1;
                    }
                }
            }
            cur_blk = b.next();
        }
    }
}

// we consider there to be 3 kinds of intervals:
// vreg intervalls:
//   1. born at definition point
//   2a. at the beginning of each node: activated or deactivated as per live_in
//   2b. deactivated at a kill flag
// temporary intervalls:
//   1. born at definition (a flag indicates that the result is ever used
//   2. deactivated at a kill flag in the same block (must exist if-and-only-if born flag was set)
// fixed intervalls: ABI and instruction constraints mandating specific register
//   -- TODO: not implemented in first iteration
pub fn scan_alloc(self: *Self, comptime ABI: type) !void {
    // as allocation is greedy (currently) we will only use the latter when the 8 first are all filled
    const reg_first_save = 9;
    var highest_used: u8 = 0;

    for (self.n.items) |*n| {
        // registers currently free.
        var free_regs_ip: [n_ipreg]bool = .{true} ** n_ipreg;
        var free_regs_avx: [16]bool = .{true} ** 16;

        // NOTE: registers not in ABI.reg_order will never be used, make this more explicit?

        // any vreg which is "live in" should already be allocated. mark these as non-free
        for (self.vregs.items, 0..) |vref, vi| {
            const vr = self.iref(vref.ref).?;
            const flag = vreg_flag(@intCast(vi));
            if ((flag & n.live_in) != 0) {
                if (vr.mckind == .ipreg) free_regs_ip[vr.mcidx] = false;
                if (vr.mckind == .vfreg) free_regs_avx[vr.mcidx] = false;
            }
        }

        var phi = n.phi_list;
        while (phi != NoRef) {
            const i = &self.i.items[phi];
            try self.alloc_inst(ABI, i, &free_regs_ip, &free_regs_avx, &highest_used);
            phi = i.next;
        }
        var it = self.ins_iterator(n.firstblk);
        while (it.next()) |item| {
            const i = item.i;
            // const ref = item.ref;

            const is_avx = (i.res_type() == .avxval);

            if (i.f.kill_op1) {
                if (self.iref(i.op1)) |op| {
                    if (op.mckind == .ipreg) free_regs_ip[op.mcidx] = true;
                    if (op.mckind == .vfreg) free_regs_avx[op.mcidx] = true;
                    if (i.mckind == .unallocated_raw and op.mckind == .ipreg and !is_avx) {
                        i.mckind = .unallocated_ipreghint;
                        i.mcidx = op.mcidx;
                    }
                }
            }

            // TODO: reghint for killed op2? (if symmetric, like add usw)
            if (i.f.kill_op2) {
                if (self.iref(i.op2)) |op| {
                    if (op.mckind == .ipreg) free_regs_ip[op.mcidx] = true;
                    if (op.mckind == .vfreg) free_regs_avx[op.mcidx] = true;
                }
            }

            try self.alloc_inst(ABI, i, &free_regs_ip, &free_regs_avx, &highest_used);
        }

        var put_iter = n.putphi_list;
        while (put_iter != NoRef) {
            const i = &self.i.items[put_iter];
            if (i.tag == .putphi) {
                // NB: we don't need to worry about kill i.op1 as we are at the end of the node
                const from = if (self.iref(i.op1)) |f| f.ipreg() else null;
                if (from) |reg| {
                    const to = self.iref(i.op2).?;
                    if (to.mckind == .unallocated_raw) {
                        to.mckind = .unallocated_ipreghint;
                        to.mcidx = reg.id();
                    }
                }
            }
            put_iter = i.next;
        }
    }

    if (@TypeOf(options) != @TypeOf(null) and options.dbg_vregs) {
        print("used {} general purpose regs\n", .{highest_used + 1});
    }

    if (highest_used >= reg_first_save) {
        self.nsave = highest_used - reg_first_save + 1;
    }
}

pub fn alloc_inst(self: *Self, comptime ABI: type, i: *Inst, free_regs_ip: *[n_ipreg]bool, free_regs_avx: *[16]bool, highest_used: *u8) !void {
    if (!(i.has_res() and i.mckind.unallocated())) {
        // TODO: handle vregs with a pre-allocated register
        return;
    }

    const is_avx = (i.res_type() == .avxval);
    var usable_regs: [16]bool = undefined;
    var free_regs = if (is_avx) free_regs_avx else free_regs_ip;
    const reg_kind: defs.MCKind = if (is_avx) .vfreg else .ipreg;

    @memcpy(&usable_regs, free_regs);
    var conflicts: u16 = 0;
    if (i.f.conflicts) {
        conflicts |= i.vreg_scratch;
    }

    if (i.vreg()) |vreg| {
        const v = self.vregs.items[vreg];
        const imask = v.live_in;
        conflicts |= v.conflicts; // fixed reg conflicts
        for (self.vregs.items) |vref| {
            const vr = self.iref(vref.ref).?;
            if (vr.mckind != reg_kind) continue;
            if ((imask & vref.live_in) != 0) {
                // mckind checked above
                usable_regs[vr.mcidx] = false;
                break;
            }
        }
    }

    if (conflicts != 0) {
        for (0..n_ipreg) |r| {
            if ((conflicts & ipreg_flag(@intCast(r))) != 0) {
                usable_regs[r] = false;
            }
        }
    }

    // TODO: this would be handled by materializing the constant 1 in "VAL2 = 1 << VAL1"
    // as a register, in a proper target dependent isel-pass, IF WE HAD ONE
    if (i.tag == .ibinop and i.ibinop().asShift() != null and constidx(i.op1) != null) {
        if (self.iref(i.op2)) |amt| {
            if (amt.mckind == .ipreg) {
                usable_regs[amt.mcidx] = false;
            }
        }
    }

    var chosen_reg: ?u8 = null;
    if (i.mckind == .unallocated_ipreghint) {
        if (i.res_type() != .intptr) unreachable;
        if (usable_regs[i.mcidx]) {
            chosen_reg = i.mcidx;
        }
    }

    if (chosen_reg == null) {
        // TODO: in the og wimmer paper they do sorting of the "longest" free interval. how do we
        // do this if without reorder_inst?
        if (is_avx) {
            for (usable_regs, 0..) |usable, reg| {
                if (usable) {
                    chosen_reg = @as(u8, @intCast(reg)); // TODO remove @as
                    break;
                }
            }
        } else {
            for (ABI.reg_order, 0..) |reg_try, reg_i| {
                if (usable_regs[reg_try.id()]) {
                    chosen_reg = reg_try.id();
                    if (reg_i > highest_used.*) {
                        highest_used.* = @intCast(reg_i);
                    }
                    break;
                }
            }
        }
    }

    const chosen = chosen_reg orelse @panic("implement interval splitting");

    free_regs[chosen] = false;
    i.mckind = reg_kind;
    i.mcidx = chosen;
}

// TODO: clobbers should be in here :P
const ABICallInfo = struct {
    args: []const defs.IPReg,
    ret: defs.IPReg,
};

pub fn abi_call_info(self: *Self, comptime ABI: type, i: *Inst) !ABICallInfo {
    const kind: defs.CallKind = @enumFromInt(i.spec);
    if (kind == .memory_intrinsic and ABI.tag == .X86) {
        print("PILUTTA\n", .{});
        const Reg = X86Asm.IPReg;
        const idx = self.constval(i.op1) orelse return error.FLIRError;
        const intrinsic: defs.MemoryIntrinsic = @enumFromInt(idx);
        if (intrinsic == .memset) { // REP STOS
            const args = comptime erase([_]Reg{ .rdi, .rax, .rcx }); // (dest, val, len)
            // choice of ret is a bit arbitrary, but rdi is at least dest+len :P
            return .{ .args = &args, .ret = Reg.rdi.into() };
        } else {
            return error.FLIRError;
        }
    }
    return .{ .args = &ABI.argregs, .ret = ABI.ret_reg };
}

pub fn set_abi(self: *Self, comptime ABI: type) !void {
    for (self.n.items) |*n| {
        // TODO: if args existed nested in i_call.next we wouldn't need this :relieved:
        var it = self.ins_iterator_rev(n.lastblk);
        var last_call: ?ABICallInfo = null;
        while (it.next_rev()) |item| {
            const i: *Inst = item.i;
            // non-standard callconvs can be annotated by pre-setting mckind/mcindx
            if (!i.mckind.unallocated()) continue;
            switch (i.tag) {
                .callarg => {
                    const num = i.spec;
                    // TODO: floatarg
                    i.mckind = .ipreg;
                    const argregs = (last_call orelse return error.FLIRError).args;
                    if (num >= argregs.len) return error.FLIRError;
                    i.mcidx = @intFromEnum(argregs[num]);
                },
                .call => {
                    const call_info = try self.abi_call_info(ABI, i);
                    i.mckind = .ipreg;
                    i.mcidx = @intFromEnum(call_info.ret);
                    last_call = call_info;
                },
                .arg => {
                    if (i.op1 >= ABI.argregs.len) return error.ARA;
                    // tricky: do we use this to encode that the arg came from there?
                    // or should spec be changed to the reg number
                    i.mckind = .unallocated_ipreghint;
                    i.mcidx = ABI.argregs[i.op1].id();
                },
                else => {
                    last_call = null; // gentlemen please
                },
            }
        }
    }

    var mask: u16 = 0;
    for (ABI.call_unsaved) |r| {
        mask |= ipreg_flag(r.id());
    }
    self.call_clobber_mask = mask;

    // this is a bit of a placeholder, either we support the X86 memory ops or nothing :P
    self.abi_tag = ABI.tag;
}

fn mcshow(inst: *Inst) struct { defs.MCKind, u16 } {
    return .{ inst.mckind, inst.mcidx };
}

// Resolve a block of parallel moves. Currently supported:
// callarg instruction: source in i.op1, destination i itself
// putphi instruction: source in i.op1, destination in i.op2 (the phi instruction)
// pos is advanced to first ins (or null) after the move group
pub fn resolve_callargs_temp(self: *Self, pos: *InsIterator, tag: Inst.Tag) !void {
    while (true) {
        var phi_1 = pos.*;
        var any_ready = false;
        while (phi_1.next()) |p1| {
            if (p1.i.tag != tag) break;
            const ready = is_ready: {
                if (try self.trivial(p1.i)) {
                    break :is_ready true;
                }
                var phi_2 = pos.*; // unordered, we cannot start after phi_1!
                while (phi_2.next()) |p2| {
                    // pointer comparison: skip diagonal element
                    if (p1.i == p2.i) continue;
                    const after = p2.i;
                    if (after.tag != tag) break;
                    if (try self.conflict(p1.i, after)) {
                        break :is_ready false;
                    }
                }
                break :is_ready true;
            };
            if (ready) {
                // we cannot run out of pos as it is a slower (or always equal) iterator
                mem.swap(Inst, p1.i, pos.next().?.i);
                any_ready = true; // we advanced
            }
        }
        if (!any_ready) {
            break;
        }
    }

    while (pos.peek()) |px| {
        if (px.i.tag != tag) {
            return;
        }
        _ = pos.next();
        // we found the end of a swap group when we find a write to this location
        const group_last_write = try self.movins_read(px.i) orelse @panic("trivial move in cycle group?");
        var next_read = try self.movins_dest(px.i);
        var phi_1 = pos.*;
        px.i.f.do_swap = true;
        while (phi_1.next()) |p1| {
            if (p1.i.tag != tag) @panic("naieeee");
            const p1_read = (try self.movins_read(p1.i)) orelse @panic("trivial mode in cycle group?");
            if (mc_equal(p1_read, next_read)) {
                // we cannot run out of pos as it is a slower (or always equal) iterator
                const p1_dest = try self.movins_dest(p1.i);
                const p1iloc = pos.next().?.i;
                mem.swap(Inst, p1.i, p1iloc);
                if (mc_equal(p1_dest, group_last_write)) {
                    p1iloc.f.swap_done = true;
                    break;
                } else {
                    p1iloc.f.do_swap = true;
                    next_read = p1_dest;
                    phi_1 = pos.*;
                }
            }
        }
    }
}

// currently only putphi, args should also become a list before scheduled..
pub fn resolve_movelist(self: *Self, node: u16, list: u16) !void {
    // phase one, kill trivial puts, emit non-conflicting puts
    while (true) {
        var phi_1 = list;
        var any_ready = false;
        while (phi_1 != NoRef) {
            var p1 = self.iref(phi_1) orelse return error.FLIRError;
            if (!p1.f.killed) {
                // fast path: a lot of putphis are going to be trivial "RAX := RAX" stuff
                if (try self.trivial(p1)) {
                    p1.f.killed = true;
                }
            }
            if (p1.f.killed) {
                phi_1 = p1.next;
                continue;
            }
            const ready = is_ready: {
                var phi_2 = list; // unordered, we cannot start after phi_1!
                while (phi_2 != NoRef) {
                    const p2 = self.iref(phi_2) orelse return error.FLIRError;
                    // pointer comparison: skip diagonal element
                    if (!(p2.f.killed or p1 == p2)) {
                        if (try self.conflict(p1, p2)) {
                            break :is_ready false;
                        }
                    }
                    phi_2 = p2.next;
                }
                break :is_ready true;
            };
            if (ready) {
                p1.f.killed = true;
                try self.addInstRef(node, phi_1);

                any_ready = true; // we advanced
            }
            phi_1 = p1.next;
        }
        if (!any_ready) {
            break;
        }
    }

    var phi = list;
    while (phi != NoRef) {
        const p = self.iref(phi) orelse return error.FLIRError;
        if (p.f.killed) {
            phi = p.next;
            continue;
        }

        const group_last_write = try self.movins_read(p) orelse @panic("trivial move in cycle group?");
        var next_read = try self.movins_dest(p);
        p.f.killed = true;
        p.f.do_swap = true;
        try self.addInstRef(node, phi);
        var phi_1 = list;
        while (phi_1 != NoRef) {
            const p1 = self.iref(phi_1) orelse return error.FLIRError;
            if (p1.f.killed) {
                phi_1 = p1.next;
                continue;
            }
            const p1_read = (try self.movins_read(p1)) orelse @panic("trivial mode in cycle group?");
            if (mc_equal(p1_read, next_read)) {
                // we cannot run out of pos as it is a slower (or always equal) iterator
                const p1_dest = try self.movins_dest(p1);
                try self.addInstRef(node, phi_1);
                p1.f.killed = true;
                if (mc_equal(p1_dest, group_last_write)) {
                    p1.f.swap_done = true;
                    break;
                } else {
                    p1.f.do_swap = true;
                    next_read = p1_dest;
                    phi_1 = list;
                }
            } else {
                phi_1 = p1.next;
            }
        }

        phi = p.next;
    }
}

pub fn resolve_moves(self: *Self) !void {
    for (self.n.items, 0..) |*n, ni| {
        try self.resolve_movelist(uv(ni), n.putphi_list);

        var iter = self.ins_iterator(n.firstblk);
        while (iter.peek()) |it| {
            if (it.i.tag == .callarg) {
                try self.resolve_callargs_temp(&iter, it.i.tag);
            } else {
                _ = iter.next();
            }
        }
    }
}

pub fn movins_dest(self: *Self, movins: *Inst) !*Inst {
    return switch (movins.tag) {
        .putphi => self.iref(movins.op2) orelse return error.FLIRError,
        .callarg => movins,
        else => error.FLIRError,
    };
}

// if reading a constant or undef, returns null
fn movins_read(self: *Self, movins: *Inst) !?*Inst {
    return switch (movins.tag) {
        .putphi => self.iref(movins.op1),
        .callarg => self.iref(movins.op1),
        else => error.FLIRError,
    };
}

// fubbigt: use IPMCVal even in the analysis stage
pub fn movins_read2(self: *Self, movins: *Inst) !?defs.IPMCVal {
    return switch (movins.tag) {
        .putphi => self.ipval(movins.op1),
        .callarg => self.ipval(movins.op1),
        else => error.FLIRError,
    };
}

fn mc_equal(rhs: anytype, lhs: anytype) bool {
    return (rhs.mckind == lhs.mckind and rhs.mcidx == lhs.mcidx);
}

pub fn trivial(self: *Self, movins: *Inst) !bool {
    const written = try self.movins_dest(movins);
    const read = try self.movins_read(movins) orelse return false; // in principle moving UNDEF anywhere is trivial, but it doesn't matter here
    return mc_equal(written, read);
}

pub fn conflict(self: *Self, before: *Inst, after: *Inst) !bool {
    const written = try self.movins_dest(before);
    const read = try self.movins_read(after) orelse return false;
    return mc_equal(written, read);
}

const InsIterator = struct {
    self: *Self,
    cur_blk: u16,
    idx: u16,

    pub const IYtem = struct { i: *Inst, ref: u16, blk: u16, idx_in_blk: u16 };

    pub fn next(it: *InsIterator) ?IYtem {
        return it.get(true);
    }

    pub fn peek(it: *InsIterator) ?IYtem {
        return it.get(false);
    }

    fn get(it: *InsIterator, advance: bool) ?IYtem {
        while (true) {
            if (it.cur_blk == NoRef) return null;

            const ref = it.self.b.items[it.cur_blk].i[it.idx];
            const retval: IYtem = .{
                .i = if (ref != NoRef) &it.self.i.items[ref] else undefined, // GESUNDHEIT
                .ref = ref,
                .blk = it.cur_blk,
                .idx_in_blk = it.idx,
            };
            if (!advance and ref != NoRef) {
                return retval;
            }

            it.idx += 1;
            if (it.idx == BLK_SIZE) {
                it.idx = 0;
                it.cur_blk = it.self.b.items[it.cur_blk].succ;
            }
            if (ref != NoRef) {
                return retval;
            }
        }
    }

    /// NB: semantics of using _both_ forward and rev on the same iterator has not been checked
    pub fn next_rev(it: *InsIterator) ?IYtem {
        return it.get_rev(true);
    }

    fn check_empty(self: *Self, blk: u16, pred: u16) void {
        const first = (pred == NoRef);
        const b = &self.b.items[blk];
        const succ = b.succ;
        const last = (succ == NoRef);
        // cannot remove the only block
        if (first and last) return;

        for (b.i) |i| {
            if (i != NoRef) return; // not empty
        }

        const n = &self.n.items[b.node];
        if (first) {
            n.firstblk = succ;
        } else {
            self.b.items[pred].succ = succ;
        }

        if (last) {
            n.lastblk = pred;
        } else {
            self.b.items[succ].pred = pred;
        }

        b.pred = self.b_free;
        self.b_free = blk;
    }

    fn get_rev(it: *InsIterator, advance: bool) ?IYtem {
        while (true) {
            if (it.cur_blk == NoRef) return null;
            if (it.idx == 0) {
                it.idx = BLK_SIZE;
                const old = it.cur_blk;
                it.cur_blk = it.self.b.items[it.cur_blk].pred;
                check_empty(it.self, old, it.cur_blk);

                if (it.cur_blk == NoRef) return null;
            }

            const myidx = it.idx - 1;

            const ref = it.self.b.items[it.cur_blk].i[myidx];
            if (advance or ref == NoRef) {
                it.idx = myidx;
            }
            if (ref != NoRef) {
                return .{ .i = &it.self.i.items[ref], .ref = ref, .blk = it.cur_blk, .idx_in_blk = myidx };
            }
        }
    }
};

pub fn ins_iterator(self: *Self, first_blk: u16) InsIterator {
    return .{ .self = self, .cur_blk = first_blk, .idx = 0 };
}

pub fn ins_iterator_rev(self: *Self, last_blk: u16) InsIterator {
    return .{ .self = self, .cur_blk = last_blk, .idx = BLK_SIZE };
}

pub fn delete_raw(self: *Self, ref: u16) void {
    self.i.items[ref].tag = .freelist;
    self.i.items[ref].op1 = self.i_free;
    self.i_free = ref;
}

// delet item from iterator, but keep iterating safely (both fwd and rev)
pub fn delete_itersafe(self: *Self, item: InsIterator.IYtem) void {
    self.delete_raw(item.ref);
    self.b.items[item.blk].i[item.idx_in_blk] = NoRef;
}

// TODO: not yet sure if ABI should be comptime or runtime struct. this works for now
pub fn test_analysis(self: *Self, comptime ABI: type, comptime check: bool) !void {
    if (check) {
        self.check_ir_valid() catch |err| {
            self.debug_print();
            return err;
        };
    }

    // TODO: this "optmization order" is not optimal but enough to legalize
    // the IR, i e no (locally) constant ops, no dead succs

    // TODO: missed opportunity: some branches are already trivial
    try self.calc_preds();

    try self.resolve_ssa();

    if (check) try self.check_ir_valid();
    if (@TypeOf(options) != @TypeOf(null) and options.dbg_ssa_ir) {
        self.debug_print();
    }

    // Just to get rid of trivial ops that codegen expect not to see.
    try self.const_fold_legalize();

    // unreachable nodes here, cannot check:p
    // if (check) try self.check_ir_valid();

    // modified reverse post-order where all loops
    // are emitted contigously
    try self.calc_loop(); // also fills node.dfnum
    try self.reorder_nodes();

    if (check) try self.check_ir_valid();
    if (@TypeOf(options) != @TypeOf(null) and options.dbg_raw_reorder_ir) {
        self.debug_print();
    }

    try self.set_abi(ABI);

    if (check) try self.check_ir_valid();

    try self.calc_live();
    if (check) try self.check_ir_valid();
    if (check) try self.check_vregs();

    try self.scan_alloc(ABI);
    try self.resolve_moves(); // GLYTTIT
    if (check) try self.check_ir_valid();

    try self.mark_empty();
}

pub fn find_nonempty(self: *Self, ni_0: u16) u16 {
    var ni = ni_0;
    while (true) {
        const node = self.n.items[ni];
        if (!node.is_empty) break;
        ni = node.s[0];
        assert(ni > 0);
    }
    return ni;
}

pub fn mark_empty(self: *Self) !void {
    for (self.n.items, 0..) |*n, ni| {
        if (self.empty(uv(ni), true)) {
            n.is_empty = true;
        }
    }
}

pub fn empty(self: *Self, ni: u16, allow_succ: bool) bool {
    // entry point is implicitly non-empty (might contain prologue code)
    if (ni == 0) return false;
    const node = &self.n.items[ni];
    if (!allow_succ and node.s[0] != 0) return false;
    var it = self.ins_iterator(node.firstblk);
    if (it.next() != null) return false;
    assert(node.s[1] == 0);
    return true;
}

pub fn get_varname(self: *Self, idx: u16) ?[]const u8 {
    return if (self.var_names.items.len > idx) self.var_names.items[idx] else null;
}
