const std = @import("std");
const math = std.math;
const mem = std.mem;
const Allocator = mem.Allocator;
const Self = @This();
const print = std.debug.print;
const CFO = @import("./CFO.zig");
const SSA_GVN = @import("./SSA_GVN.zig");
const builtin = @import("builtin");

const options = if (!builtin.is_test) &@import("root").options else null;

// this currently causes a curious bug as of zig nightly 26dec2022.
// recursive method calls like self.rpo_visit() within itself do not work!
pub usingnamespace @import("./verify_ir.zig");

const ArrayList = std.ArrayList;
const assert = std.debug.assert;

const IPReg = CFO.IPReg;
const VMathOp = CFO.VMathOp;
const VCmpOp = CFO.VCmpOp;
const FMode = CFO.FMode;
const ISize = CFO.ISize;

a: Allocator,
// TODO: unmanage all these:
n: ArrayList(Node),
b: ArrayList(Block),
preorder: ArrayList(u16),
blkorder: ArrayList(u16),
// This only handles int and float64 constants,
// we need something else for "address into string table"
constvals: ArrayList(u64), // TODO: make constants typed?
// This is only used for preds currently, but use it for more stuff??
refs: ArrayList(u16),
narg: u16 = 0,

// variables are references which do not fullfill the SSA-property.
// we eleminate these early on in the ssa_gvn pass
nvar: u16 = 0,

// variables 2.0: virtual registero
// vregs is any result which is live outside of its defining node, that's it.
nvreg: u16 = 0,
vregs: ArrayList(u16),

// 8-byte slots in stack frame
nslots: u8 = 0,
nsave: u8 = 0,
ndf: u16 = 0,

// filler value for unintialized refs. not a sentinel for
// actually invalid refs!
pub const DEAD: u16 = 0xFEFF;
// For blocks: we cannot have more than 2^14 blocks anyway
// for vars: don't allocate last block!
pub const NoRef: u16 = 0xFFFF;
// 255 constants should be enough for everyone
// could be smarter, like dedicated Block type for constants!
pub const ConstOff: u16 = 0xFF00;
// 255 vrefs should be enough for everyone
// could be smarter, like dedicated Block type for vrefs!
pub const VRefOff: u16 = 0xFE00;

pub fn uv(s: usize) u16 {
    return @intCast(u16, s);
}

pub const Node = struct {
    s: [2]u16 = .{ 0, 0 }, // sucessors
    dfnum: u16 = 0,

    predref: u16 = 0,
    npred: u16 = 0,
    // NB: might be NoRef if the node was deleted,
    // a reachable node must have at least one block even if empty!
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
};

pub const EMPTY: Inst = .{ .tag = .empty, .op1 = 0, .op2 = 0 };

pub const BLK_SIZE = 4;
pub const BLK_SHIFT = 2;
pub const Block = struct {
    node: u16,
    succ: u16 = NoRef,
    pred: u16 = NoRef,
    i: [BLK_SIZE]Inst = .{EMPTY} ** BLK_SIZE,

    pub fn next(self: @This()) ?u16 {
        return if (self.succ != NoRef) self.succ else null;
    }

    pub fn prev(self: @This()) ?u16 {
        return if (self.pred != NoRef) self.pred else null;
    }
};

// TODO: integrate properly with SpecType
pub const ValType = enum(u4) {
    intptr = 0,
    avxval,

    pub fn spec(self: @This()) u4 {
        return @enumToInt(self);
    }
};

// union of integer sizes and Fmode
// not very well thought out but works for now to merge the intptry (eiri) and AVX:y aspects of FLIR
// currently u4 is enough but expand later?
pub const SpecType = union(ValType) {
    intptr: ISize,
    avxval: FMode,
    const INT_SPEC_OFF: u4 = 8;
    pub fn val_type(self: @This()) ValType {
        return if (@enumToInt(self) >= INT_SPEC_OFF) .intptr else .avxval;
    }
    pub fn from(val: u8) SpecType {
        if (val >= INT_SPEC_OFF) {
            return .{ .intptr = @intToEnum(ISize, val - INT_SPEC_OFF) };
        } else {
            return .{ .avxval = @intToEnum(FMode, val) };
        }
    }
    pub fn into(self: SpecType) u5 {
        return switch (self) {
            .intptr => |i| INT_SPEC_OFF + @enumToInt(i),
            .avxval => |a| @enumToInt(a),
        };
    }
};

// looks cute, might delete later
pub fn intspec(s: ISize) SpecType {
    return .{ .intptr = s };
}

pub fn avxspec(s: FMode) SpecType {
    return .{ .avxval = s };
}

test "sizey" {
    // @compileLog(@sizeOf(Inst));
    // @compileLog(@sizeOf(Block));
    assert(@sizeOf(Block) <= 64);
}

pub const Inst = struct {
    tag: Tag,
    spec: u8 = 0,
    op1: u16,
    op2: u16,
    mckind: MCKind = .unallocated_raw,
    mcidx: u8 = undefined,
    vreg: u16 = NoRef,
    f: packed struct {
        // note: if the reference is a vreg, it might
        // be alive in an other branch processed later
        kill_op1: bool = false,
        kill_op2: bool = false,
        killed: bool = false, // if false after calc_use, a non-vreg is never used
    } = .{},

    fn free(self: @This()) bool {
        return self.tag == .empty;
    }

    // TODO: handle spec being split between u4 type and u4 somethingelse?
    fn spec_type(self: Inst) ValType {
        return self.mem_type();
    }

    // TODO: handle spec being split between u4 type and u4 somethingelse?
    // for load/store insts that can handle both intptr and avxvalues
    pub fn mem_type(self: Inst) SpecType {
        return SpecType.from(self.low_spec());
    }

    pub fn scale(self: Inst) u2 {
        return @intCast(u2, self.high_spec());
    }

    const LOW_MASK: u8 = (1 << 5) - 1;
    const HIGH_MASK: u8 = ~LOW_MASK;

    fn high_spec(self: Inst) u3 {
        return @intCast(u3, (self.spec & HIGH_MASK) >> 5);
    }

    fn low_spec(self: Inst) u5 {
        return @intCast(u5, self.spec & LOW_MASK);
    }

    pub fn vmathop(self: Inst) VMathOp {
        return @intToEnum(VMathOp, self.low_spec());
    }

    pub fn vcmpop(self: Inst) VCmpOp {
        return @intToEnum(VCmpOp, self.low_spec());
    }

    // only valid for op instructions. otherwise use mem_type
    pub fn fmode_op(self: Inst) FMode {
        return @intToEnum(FMode, self.high_spec());
    }

    pub fn res_type(inst: Inst) ?ValType {
        return switch (inst.tag) {
            .empty => null,
            .arg => inst.mem_type(), // TODO: haIIIII
            .variable => inst.mem_type(), // gets preserved to the phis
            .putvar => null,
            .phi => inst.mem_type(),
            .putphi => null, // stated in the phi instruction
            .alloc => .intptr, // the type of .alloc is a pointer to it
            .renum => null, // should be removed at this point
            .load => inst.mem_type(),
            .lea => .intptr, // Lea? Who's Lea??
            .store => null,
            .ibinop => .intptr,
            .icmp => null, // technically the FLAG register but anyway
            .vmath => .avxval,
            .vcmpf => .avxval,
            .ret => null,
            .call => .intptr,
            .callarg => null,
        };
    }

    pub fn has_res(inst: Inst) bool {
        return inst.res_type() != null;
    }

    // number of op:s which are inst references.
    // otherwise they can store whatever data
    pub fn n_op(inst: Inst, rw: bool) u2 {
        return switch (inst.tag) {
            .empty => 0,
            .arg => 0,
            .variable => 0,
            // really only one, but we will get rid of this lie
            // before getting into any serious analysis.
            .putvar => 2, // TODO: if (rw) 2 else 1, FAST ÅT ANDRA HÅLLET
            .phi => 0,
            .putphi => if (rw) 2 else 1,
            .renum => 1,
            .load => 2, // base, idx
            .lea => 2, // base, idx. elided when only used for a store!
            .store => 2, // addr, val
            .ibinop => 2,
            .icmp => 2,
            .vmath => 2,
            .vcmpf => 2,
            .ret => 1,
            .callarg => 1,
            .call => 0, // could be for funptr/dynamic syscall?
            .alloc => 0,
        };
    }

    pub fn ops(i: *Inst, rw: bool) []u16 {
        assert(@ptrToInt(&i.op2) - @ptrToInt(&i.op1) == @sizeOf(u16));
        return @ptrCast([*]u16, &i.op1)[0..i.n_op(rw)];
    }

    pub fn ipreg(i: Inst) ?IPReg {
        return if (i.mckind == .ipreg) @intToEnum(IPReg, i.mcidx) else null;
    }

    pub fn avxreg(i: Inst) ?u4 {
        return if (i.mckind == .vfreg) @intCast(u4, i.mcidx) else null;
    }
};

pub const IPMCVal = union(enum) {
    ipreg: IPReg,
    constval: u64, // bitcast to i64 for signed
    frameslot: u8,

    // TODO: this is not a builtin? (or maybe meta)
    pub fn as_ipreg(self: @This()) ?IPReg {
        return switch (self) {
            .ipreg => |reg| reg,
            else => null,
        };
    }
};

pub fn constval(self: *Self, ref: u16) ?u64 {
    if (ref == NoRef) return null;
    if (ref >= ConstOff) {
        return self.constvals.items[ref - ConstOff];
    }
    return null;
}

pub fn ipval(self: *Self, ref: u16) ?IPMCVal {
    if (self.constval(ref)) |c| return .{ .constval = c };
    const i = self.iref(ref) orelse return null;
    if (i.ipreg()) |reg| return .{ .ipreg = reg };
    if (i.mckind == .frameslot) return .{ .frameslot = i.mcidx };
    return null;
}
pub fn ipreg(self: *Self, ref: u16) ?IPReg {
    return (self.iref(ref) orelse return null).ipreg();
}
pub fn avxreg(self: *Self, ref: u16) ?u4 {
    return (self.iref(ref) orelse return null).avxreg();
}

pub const Tag = enum(u8) {
    empty = 0, // empty slot. must not be refered to!
    alloc,
    arg,
    variable,
    putvar, // non-phi assignment op1 := op2
    phi,
    /// put op1 into phi op2 of (only) successor
    putphi,
    renum,
    load,
    lea,
    store,
    ibinop,
    icmp,
    vmath,
    vcmpf,
    ret,
    call,
    callarg,
};

pub const IntBinOp = enum(u6) {
    add,
    sub,
    @"or",
    @"and", // det finns en and
    xor,
    mul,
    shl,
    sar,
    shr,

    pub fn asAOP(self: @This()) ?CFO.AOp {
        return switch (self) {
            .add => .add,
            .sub => .sub,
            .@"or" => .bor,
            .@"and" => .band,
            .xor => .xor,
            else => null,
        };
    }

    pub fn asShift(self: @This()) ?CFO.ShiftOp {
        return switch (self) {
            .shl => .hl,
            .sar => .ar,
            .shr => .hr,
            else => null,
        };
    }
};

pub const CallKind = enum(u8) {
    /// op1 is relative start of pointer, must be a constant
    near,
    /// op1 is function pointer, use a constant
    fun_ptr,
    /// platform dependent. syscall index in op1
    /// directly encodes the linux syscall number of the target
    /// on EPBF this will be "helper" number?
    syscall,
};

pub const MCKind = enum(u8) {
    // not yet allocated, or Inst that trivially produces no value
    unallocated_raw = 0,
    // general purpose register like rax, r12, etc
    ipreg,
    // SSE/AVX registers, ie xmm0/ymm0-15
    vfreg,

    // unallocated, but has a ipreg hint
    unallocated_ipreghint,
    // unallocated, but has a vfreg hint
    unallocated_vfreghint,

    // TODO: support non-uniform sizes of spilled value
    frameslot,
    // unused value, perhaps should have been deleted before alloc
    dead,
    // not stored as such, will be emitted togheter with the next inst
    // example "lea" and then "store", or "load" and then ibinop/vmath
    fused,

    pub fn unallocated(self: @This()) bool {
        return switch (self) {
            .unallocated_raw => true,
            .unallocated_ipreghint => true,
            .unallocated_vfreghint => true,
            else => false,
        };
    }
};

pub fn init(n: u16, allocator: Allocator) !Self {
    var constvals = try ArrayList(u64).initCapacity(allocator, 8);
    constvals.appendAssumeCapacity(0);
    constvals.appendAssumeCapacity(1);
    return Self{
        .a = allocator,
        .n = try ArrayList(Node).initCapacity(allocator, n),
        .vregs = ArrayList(u16).init(allocator),
        .blkorder = ArrayList(u16).init(allocator),
        .preorder = ArrayList(u16).init(allocator),
        .refs = try ArrayList(u16).initCapacity(allocator, 4 * n),
        .constvals = constvals,
        .b = try ArrayList(Block).initCapacity(allocator, 2 * n),
    };
}

pub fn reinit(self: *Self) void {
    const info = @typeInfo(Self).Struct;

    // 1. reset fields with initializers to their initial value (most int members)
    // 2. set ArrayList members to empty (but keep existing allocations)
    inline for (info.fields) |field| {
        if (field.default_value) |default_value_ptr| {
            const default_value = @ptrCast(*align(1) const field.type, default_value_ptr).*;
            @field(self, field.name) = default_value;
        } else if (@typeInfo(field.type) == .Struct) {
            if (@hasField(field.type, "items")) {
                @field(self, field.name).items.len = 0;
            }
        }
    }

    // FUBBIT
    self.constvals.appendAssumeCapacity(0);
    self.constvals.appendAssumeCapacity(1);
}

pub fn deinit(self: *Self) void {
    self.n.deinit();
    self.blkorder.deinit();
    self.preorder.deinit();
    self.refs.deinit();
    self.constvals.deinit();
    self.b.deinit();
    self.vregs.deinit();
}

pub fn toref(blkid: u16, idx: u16) u16 {
    assert(idx < BLK_SIZE);
    return (blkid << BLK_SHIFT) | idx;
}

fn fromref(ref: u16) struct { block: u16, idx: u16 } {
    const IDX_MASK: u16 = BLK_SIZE - 1;
    const BLK_MASK: u16 = ~IDX_MASK;
    return .{
        .block = (ref & BLK_MASK) >> BLK_SHIFT,
        .idx = ref & IDX_MASK,
    };
}

const BIREF = struct { n: u16, i: *Inst };
pub fn biref(self: *Self, ref: u16) ?BIREF {
    if (ref >= ConstOff) {
        return null;
    }
    const blkref = if (ref >= VRefOff) self.vregs.items[ref - VRefOff] else ref;
    if (blkref >= VRefOff) @panic("NOT LIKE THIS");
    const r = fromref(blkref);
    const blk = &self.b.items[r.block];
    return BIREF{ .n = blk.node, .i = &blk.i[r.idx] };
}

pub fn iref(self: *Self, ref: u16) ?*Inst {
    return if (self.biref(ref)) |bi| bi.i else null;
}

fn sphigh(high: u3, low: u5) u8 {
    return @as(u8, high) << 5 | low;
}

pub fn vmathspec(vop: VMathOp, fmode: FMode) u8 {
    return sphigh(@enumToInt(fmode), @intCast(u5, vop.off()));
}

pub fn vcmpfspec(vcmp: VCmpOp, fmode: FMode) u8 {
    return sphigh(@enumToInt(fmode), vcmp.val());
}

pub fn addNode(self: *Self) !u16 {
    const n = try self.n.addOne();
    const b = try self.b.addOne();
    var nodeid = uv(self.n.items.len - 1);
    var blkid = uv(self.b.items.len - 1);
    n.* = .{ .firstblk = blkid, .lastblk = blkid };
    b.* = .{ .node = nodeid };
    return nodeid;
}

// add inst to the end of block
pub fn addInst(self: *Self, node: u16, inst: Inst) !u16 {
    const n = &self.n.items[node];
    // must exist:
    var blkid = n.lastblk;
    var blk = &self.b.items[blkid];

    // TODO: later we can add more constraints for where "empty" ins can be
    var lastfree: u8 = BLK_SIZE;
    var i: u8 = BLK_SIZE - 1;
    while (true) : (i -= 1) {
        if (blk.i[@intCast(u8, i)].free()) {
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
        blkid = uv(self.b.items.len);
        blk.succ = blkid;
        blk = try self.b.addOne();
        blk.* = .{ .node = node, .pred = prevblk };
        n.lastblk = blkid;
        lastfree = 0;
    }

    blk.i[lastfree] = inst;
    return toref(blkid, lastfree);
}

// add inst to the beginning of the block, _without_ renumbering any exiting instruction
pub fn preInst(self: *Self, node: u16, inst: Inst) !u16 {
    const n = &self.n.items[node];
    var blkid = n.firstblk;
    var blk = &self.b.items[blkid];

    var firstfree: i8 = -1;
    for (0..BLK_SIZE) |i| {
        if (blk.i[i].free()) {
            firstfree = @intCast(i8, i);
        } else {
            break;
        }
    }

    if (firstfree == -1) {
        const nextblk = blkid;
        blkid = uv(self.b.items.len);
        blk.pred = blkid;
        blk = try self.b.addOne();
        blk.* = .{ .node = node, .succ = nextblk };
        n.firstblk = blkid;
        firstfree = BLK_SIZE - 1;
    }

    const free = @intCast(u8, firstfree);

    blk.i[free] = inst;
    return toref(blkid, free);
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
    return const_uint(self, @bitCast(u32, val));
}

pub fn binop(self: *Self, node: u16, tag: Tag, op1: u16, op2: u16) !u16 {
    return self.addInst(node, .{ .tag = tag, .op1 = op1, .op2 = op2 });
}

pub fn load(self: *Self, node: u16, kind: SpecType, base: u16, idx: u16, scale: u2) !u16 {
    return self.addInst(node, .{ .tag = .load, .op1 = base, .op2 = idx, .spec = sphigh(scale, kind.into()) });
}
pub fn store(self: *Self, node: u16, kind: SpecType, base: u16, idx: u16, scale: u2, val: u16) !u16 {
    // FUBBIT: all possible instances of fusing should be detected in analysis anyway
    const addr = if (idx != NoRef) try self.addInst(node, .{ .tag = .lea, .op1 = base, .op2 = idx, .mckind = .fused, .spec = sphigh(scale, 0) }) else base;
    return self.addInst(node, .{ .tag = .store, .op1 = addr, .op2 = val, .spec = sphigh(0, kind.into()) });
}

pub fn vmath(self: *Self, node: u16, vop: VMathOp, fmode: FMode, op1: u16, op2: u16) !u16 {
    // TODO: somewhere, typecheck that FMode matches fmode of args..
    return self.addInst(node, .{ .tag = .vmath, .spec = vmathspec(vop, fmode), .op1 = op1, .op2 = op2 });
}

pub fn vcmpf(self: *Self, node: u16, vop: VCmpOp, fmode: FMode, op1: u16, op2: u16) !u16 {
    return self.addInst(node, .{ .tag = .vcmpf, .spec = vcmpfspec(vop, fmode), .op1 = op1, .op2 = op2 });
}

pub fn ibinop(self: *Self, node: u16, op: IntBinOp, op1: u16, op2: u16) !u16 {
    return self.addInst(node, .{ .tag = .ibinop, .spec = @enumToInt(op), .op1 = op1, .op2 = op2 });
}

pub fn icmp(self: *Self, node: u16, cond: CFO.Cond, op1: u16, op2: u16) !u16 {
    return self.addInst(node, .{ .tag = .icmp, .spec = cond.off(), .op1 = op1, .op2 = op2 });
}

pub fn putvar(self: *Self, node: u16, op1: u16, op2: u16) !void {
    _ = try self.binop(node, .putvar, op1, op2);
}

pub fn ret(self: *Self, node: u16, val: u16) !void {
    _ = try self.addInst(node, .{ .tag = .ret, .op1 = val, .op2 = 0 });
}

pub fn callarg(self: *Self, node: u16, num: u8, ref: u16) !void {
    // TODO: add a separate "abi" analysis step for this, like QBE does
    const callregs: [6]IPReg = .{ .rdi, .rsi, .rdx, .rcx, .r8, .r9 };
    if (num >= callregs.len) return error.FLIRError;
    _ = try self.addInst(node, .{
        .tag = .callarg,
        .spec = num,
        .op1 = ref,
        .op2 = 0,
        .mckind = .ipreg,
        .mcidx = @enumToInt(callregs[num]),
    });
}

pub fn call(self: *Self, node: u16, kind: CallKind, num: u16) !u16 {
    return try self.addInst(node, .{
        .tag = .call,
        .spec = @enumToInt(kind),
        .op1 = num,
        .op2 = NoRef,
        .mckind = .ipreg,
        // TODO: add a separate "abi" analysis step for this, like QBE does
        .mcidx = @enumToInt(IPReg.rax),
    });
}

pub fn prePhi(self: *Self, node: u16, vref: u16) !u16 {
    const v = self.iref(vref) orelse return error.FLIRError;
    return self.preInst(node, .{ .tag = .phi, .op1 = vref, .op2 = 0, .spec = v.spec });
}

// TODO: maintain wf of block 0: first all args, then all vars.

pub fn arg(self: *Self) !u16 {
    if (self.n.items.len == 0) return error.FLIRError;
    const inst = try self.addInst(0, .{ .tag = .arg, .op1 = self.narg, .op2 = 0, .spec = intspec(.dword).into() });
    self.narg += 1;
    return inst;
}

pub fn variable(self: *Self) !u16 {
    if (self.n.items.len == 0) return error.FLIRError;
    const inst = try self.addInst(0, .{ .tag = .variable, .op1 = self.nvar, .op2 = 0, .spec = intspec(.dword).into() });
    self.nvar += 1;
    return inst;
}

pub fn preds(self: *Self, i: u16) []u16 {
    const v = self.n.items[i];
    return self.refs.items[v.predref..][0..v.npred];
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
        try self.predlink(@intCast(u16, i), 0, split);
        try self.predlink(@intCast(u16, i), 1, split);
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
    mem.set(u16, newlink, NoRef);
    const oldlink = try self.a.alloc(u16, self.n.items.len);
    defer self.a.free(oldlink);
    mem.set(u16, oldlink, NoRef);
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
        n.loop = newlink[n.loop];

        var cur_blk: ?u16 = n.firstblk;
        while (cur_blk) |blk| {
            var b = &self.b.items[blk];
            b.node = uv(ni);

            cur_blk = b.next();
        }
    }
}

// ni = node id of user
pub fn adduse(self: *Self, ni: u16, user: u16, used: u16) u16 {
    _ = user;
    const ref = self.biref(used) orelse return used;
    //ref.i.n_use += 1;
    // it leaks to another block: give it a virtual register number
    if (ref.n != ni) {
        if (ref.i.vreg == NoRef) {
            ref.i.vreg = self.nvreg;
            self.nvreg += 1;
            self.vregs.appendAssumeCapacity(used);
        }
        return uv(VRefOff + ref.i.vreg);
    }
    return used;
}

// TODO: not idempotent! does not reset n_use=0 first.
// NB: requires reorder_nodes()
pub fn calc_use(self: *Self) !void {
    // TODO: NOT LIKE THIS
    try self.vregs.ensureTotalCapacity(64);
    for (self.n.items, 0..) |*n, ni| {
        var it = self.ins_iterator(n.firstblk);
        while (it.next()) |item| {
            const i = item.i;
            for (i.ops(false)) |*op| {
                op.* = self.adduse(uv(ni), item.ref, op.*);
            }
        }
    }

    var ni: u16 = uv(self.n.items.len - 1);
    // TODO: at this point the number of vregs is known. so a bitset for
    // node X vreg can be allocated here.

    if (self.nvreg > 64) return error.DoTheWork;

    while (true) : (ni -= 1) {
        const n = &self.n.items[ni];
        var live: u64 = 0;
        for (n.s) |s| {
            if (s != 0) {
                live |= self.n.items[s].live_in;
            }
        }
        // print("LIVEUT {}: {x} (dvs {})\n", .{ ni, live, @popCount(live) });

        var cur_blk: ?u16 = n.lastblk;
        while (cur_blk) |blk| {
            var b = &self.b.items[blk];
            var idx: usize = BLK_SIZE;
            while (idx > 0) {
                idx -= 1;
                const i = &b.i[idx];

                if (i.vreg != NoRef) {
                    live &= ~(@as(usize, 1) << @intCast(u6, i.vreg));
                }

                for (i.ops(false)) |op| {
                    if (self.iref(op)) |ref| {
                        if (ref.vreg != NoRef) live |= (@as(usize, 1) << @intCast(u6, ref.vreg));
                    }
                }
            }

            cur_blk = b.prev();
        }

        n.live_in = live;
        // print("LIVEIN {}: {x} (dvs {})\n", .{ ni, n.live_in, @popCount(n.live_in) });

        if (n.is_header) {
            // TODO: make me a loop membership bitset globally
            if (self.n.items.len > 64) unreachable;
            var loop_set: u64 = @as(u64, 1) << @intCast(u6, ni);
            for (ni + 1..self.n.items.len) |ch_i| {
                const ch_n = &self.n.items[ch_i];
                const ch_loop: u64 = @as(u64, 1) << @intCast(u6, ch_i);
                if (((@as(u64, 1) << @intCast(u6, ch_n.loop)) & loop_set) != 0) {
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

    for (self.n.items) |*n| {
        var live_out: u64 = 0;
        for (n.s) |s| {
            live_out |= self.n.items[s].live_in;
        }
        var killed = n.live_in & ~live_out;

        var cur_blk: ?u16 = n.lastblk;
        while (cur_blk) |blk| {
            var b = &self.b.items[blk];

            var idx: usize = BLK_SIZE;
            while (idx > 0) {
                idx -= 1;
                const i = &b.i[idx];
                for (i.ops(false), 0..) |op, i_op| {
                    var kill: bool = false;
                    if (self.iref(op)) |ref| {
                        if (ref.vreg != NoRef) {
                            const bit = (@as(u64, 1) << @intCast(u6, ref.vreg));
                            if ((killed & bit) != 0) {
                                killed = killed & ~bit;
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
            cur_blk = b.prev();
        }
    }
}

pub fn alloc_arg(self: *Self, inst: *Inst) !void {
    _ = self;
    const regs: [6]IPReg = .{ .rdi, .rsi, .rdx, .rcx, .r8, .r9 };
    if (inst.op1 >= regs.len) return error.ARA;
    inst.mckind = .ipreg;
    inst.mcidx = regs[inst.op1].id();
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
    const r = fromref(after);
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
        mem.copy(Inst, newblk.i[r.idx + 1 ..], blk.i[r.idx + 1 ..]);
        mem.set(Inst, blk.i[r.idx + 1 ..], EMPTY);
        for (r.idx + 1..BLK_SIZE) |i| {
            self.renumber(blkid, toref(r.block, uv(i)), toref(blkid, uv(i)));
            if (newblk.i[i].vreg != NoRef) {
                self.vregs.items[newblk.i[i].vreg] = toref(blkid, uv(i));
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
    const regs: [8]IPReg = .{ .rdi, .rsi, .rdx, .rcx, .r8, .r9, .r10, .r11 };
    var used: usize = self.narg;
    var avxused: u8 = 0;
    for (self.n.items) |*n| {
        var cur_blk: ?u16 = n.firstblk;
        while (cur_blk) |blk| {
            var b = &self.b.items[blk];
            for (b.i, 0..) |*i, idx| {
                const ref = toref(blk, uv(idx));

                if (i.tag == .arg) {
                    try self.alloc_arg(i);
                } else if (i.has_res() and i.mckind.unallocated()) {
                    const regkind: MCKind = if (i.res_type() == ValType.avxval) .vfreg else .ipreg;
                    const op1 = if (i.n_op(false) > 0) self.iref(i.op1) else null;
                    if (op1) |o| {
                        if (o.mckind == regkind and o.vreg == NoRef and o.last_use == ref) {
                            i.mckind = regkind;
                            i.mcidx = o.mcidx;
                            continue;
                        }
                    }
                    if (i.res_type() == ValType.avxval) {
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

// canonical order of callee saved registers. nsave>0 means
// the first nsave items needs to be saved and restored
pub const callee_saved: [5]IPReg = .{ .rbx, .r12, .r13, .r14, .r15 };

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
pub fn scan_alloc(self: *Self) !void {

    // first 9 caller saved and then 5 calle saved regs
    // as allocation is greedy (currently) we will only use the latter when the 8 first are all filled
    const reg_order: [14]IPReg = .{ .rax, .rcx, .rdx, .rsi, .rdi, .r8, .r9, .r10, .r11, .rbx, .r12, .r13, .r14, .r15 };
    const reg_first_save = 9;
    var highest_used: u8 = 0;

    for (self.n.items, 0..) |*n, ni| {
        var it = self.ins_iterator(n.firstblk);
        // registers currently free.
        var free_regs_ip: [16]bool = .{true} ** 16;
        var free_regs_avx: [16]bool = .{true} ** 16;

        free_regs_ip[IPReg.rsp.id()] = false;
        // just say NO to -fomiting the framepointer!
        free_regs_ip[IPReg.rbp.id()] = false;

        // any vreg which is "live in" should already be allocated. mark these as non-free
        for (self.vregs.items, 0..) |vref, vi| {
            const vr = self.iref(vref).?;
            const flag = @as(u64, 1) << @intCast(u6, vi);
            if ((flag & n.live_in) != 0) {
                if (vr.mckind == .ipreg) free_regs_ip[vr.mcidx] = false;
                if (vr.mckind == .vfreg) free_regs_avx[vr.mcidx] = false;
            }
        }

        while (it.next()) |item| {
            const i = item.i;
            // const ref = item.ref;
            //
            if (i.tag == .arg) {
                try self.alloc_arg(i);
                // TODO: FUBBIK, we need to break this up as thiss will
                // conflict with nested calls
                continue;
            }

            if (i.tag == .putphi) {
                // TODO: self.iref_reg() or something
                const from = if (self.iref(i.op1)) |f| f.ipreg() else null;
                if (from) |reg| {
                    const to = self.iref(i.op2).?;
                    if (to.mckind == .unallocated_raw) {
                        to.mckind = .unallocated_ipreghint;
                        to.mcidx = reg.id();
                    }
                }
            }

            const is_avx = (i.res_type() == ValType.avxval);

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

            if (!(i.has_res() and i.mckind.unallocated())) {
                // TODO: handle vregs with a pre-allocated register
                continue;
            }

            var usable_regs: [16]bool = undefined;
            var free_regs = if (is_avx) &free_regs_avx else &free_regs_ip;
            var reg_kind: MCKind = if (is_avx) .vfreg else .ipreg;

            mem.copy(bool, &usable_regs, free_regs);
            if (i.vreg != NoRef) {
                const myflag = @as(u64, 1) << @intCast(u6, i.vreg);
                for (self.vregs.items, 0..) |vref, vi| {
                    const vr = self.iref(vref).?;
                    if (vr.mckind != reg_kind) continue;
                    // NOTE: This does purposefully exclude current node. free_regs[]
                    // will already track vregs active at the current position (precisely)
                    // NOTE2: this should be made more efficient by transposing the
                    // [Node X VReg] bitset, so we can just do (me_livein & other_livein) over all nodes
                    const otherflag = @as(u64, 1) << @intCast(u6, vi);
                    const flagmask = myflag | otherflag;
                    for (ni + 1..self.n.items.len) |ni2| {
                        if (self.n.items[ni2].live_in & flagmask == flagmask) {
                            // mckind checked above
                            usable_regs[vr.mcidx] = false;
                            break;
                        }
                    }
                }
            }

            var chosen_reg: ?u8 = null;
            if (i.mckind == .unallocated_ipreghint) {
                if (i.res_type() != ValType.intptr) unreachable;
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
                            chosen_reg = @intCast(u8, reg);
                            break;
                        }
                    }
                } else {
                    for (reg_order, 0..) |reg_try, reg_i| {
                        if (usable_regs[reg_try.id()]) {
                            chosen_reg = reg_try.id();
                            if (reg_i > highest_used) {
                                highest_used = @intCast(u8, reg_i);
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
    }

    if (@TypeOf(options) != @TypeOf(null) and options.dbg_vregs) {
        print("used {} general purpose regs\n", .{highest_used + 1});
    }

    if (highest_used >= reg_first_save) {
        self.nsave = highest_used - reg_first_save + 1;
    }
}

pub fn resolve_phi(self: *Self) !void {
    for (self.n.items) |*n| {
        // putphi are at the end blocks before a join node, and
        // cannot be in a split node
        if (n.s[1] != 0) continue;
        const first_putphi = first: {
            var iter = self.ins_iterator(n.firstblk);
            var peek = iter;
            while (peek.next()) |it| {
                if (it.i.tag == .putphi) break :first iter;
                iter = peek; // TODO: be smarter than this (or not)
            }
            // no putphi, skip the node
            continue;
        };

        var phi_1 = first_putphi;
        while (phi_1.next()) |p1| {
            const before = p1.i;
            if (before.tag != .putphi) continue;

            var phi_2 = phi_1; // starts after phi_1
            while (phi_2.next()) |p2| {
                // print("considering: {} {}\n", .{ p1.ref, p2.ref });

                const after = p2.i;
                if (after.tag != .putphi) continue;
                if (self.conflict(before, after)) {
                    // print("DID\n", .{});
                    mem.swap(Inst, before, after);
                }
                if (self.conflict(before, after)) {
                    self.debug_print();
                    print("cycles detected: {} {}\n", .{ p1.ref, p2.ref });
                    return error.FLIRError;
                }
            }
        }
    }
}

pub fn conflict(self: *Self, before: *Inst, after: *Inst) bool {
    const written = self.iref(before.op2) orelse return false;
    const read = self.iref(after.op1) orelse return false;
    if (written.mckind == read.mckind and written.mcidx == read.mcidx) return true;
    return false;
}

const InsIterator = struct {
    self: *Self,
    cur_blk: u16,
    idx: u16,

    pub const IYtem = struct { i: *Inst, ref: u16 };
    pub fn next(it: *InsIterator) ?IYtem {
        while (true) {
            if (it.cur_blk == NoRef) return null;
            const retval = IYtem{ .i = &it.self.b.items[it.cur_blk].i[it.idx], .ref = toref(it.cur_blk, it.idx) };
            it.idx += 1;
            if (it.idx == BLK_SIZE) {
                it.idx = 0;
                it.cur_blk = it.self.b.items[it.cur_blk].succ;
            }
            if (retval.i.tag != .empty) {
                return retval;
            }
        }
    }
};

pub fn ins_iterator(self: *Self, first_blk: u16) InsIterator {
    return .{ .self = self, .cur_blk = first_blk, .idx = 0 };
}

pub fn test_analysis(self: *Self, comptime check: bool) !void {
    if (check) {
        self.check_ir_valid() catch |err| {
            self.debug_print();
            return err;
        };
    }
    try self.calc_preds();

    // modified reverse post-order where all loops
    // are emitted contigously
    try self.calc_loop(); // also fills node.dfnum

    try self.reorder_nodes();
    if (check) try self.check_ir_valid();
    try SSA_GVN.ssa_gvn(self);
    if (check) try self.check_ir_valid();

    if (check) try self.check_ir_valid();
    try self.calc_use();

    if (check) try self.check_vregs();

    try self.scan_alloc();
    try self.resolve_phi(); // GLYTTIT
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

pub fn trivial_ins(self: *Self, i: *Inst) bool {
    switch (i.tag) {
        .empty => return true,
        .putphi => {
            // TODO: src being Undef is trivial, strictly speaking
            const src = self.iref(i.op1) orelse return false;
            const dst = self.iref(i.op2).?;
            return (src.mckind == dst.mckind and src.mcidx == dst.mcidx);
        },
        else => return false,
    }
}

pub fn empty(self: *Self, ni: u16, allow_succ: bool) bool {
    // entry point is implicitly non-empty (might contain prologue code)
    if (ni == 0) return false;
    const node = &self.n.items[ni];
    if (!allow_succ and node.s[0] != 0) return false;
    var it = self.ins_iterator(node.firstblk);
    while (it.next()) |item| {
        if (!self.trivial_ins(item.i)) return false;
    }
    assert(node.s[1] == 0);
    return true;
}

pub const codegen = @import("./codegen.zig").codegen;

// force tests to run:
test {
    std.testing.refAllDecls(@import("./TestFLIR.zig"));
    std.testing.refAllDecls(@import("./TestAOC.zig"));
}
