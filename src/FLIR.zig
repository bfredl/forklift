const std = @import("std");
const math = std.math;
const mem = std.mem;
const Allocator = mem.Allocator;
const Self = @This();
const print = std.debug.print;
const CFO = @import("./CFO.zig");
const SSA_GVN = @import("./SSA_GVN.zig");

const builtin = @import("builtin");
// const stage2 = builtin.zig_backend != .stage1;
const ArrayList = std.ArrayList;
const assert = std.debug.assert;

const IPReg = CFO.IPReg;
const VMathOp = CFO.VMathOp;
const FMode = CFO.FMode;
const ISize = CFO.ISize;
const AOp = CFO.AOp;
const Cond = CFO.Cond;

a: Allocator,
// TODO: unmanage all these:
n: ArrayList(Node),
b: ArrayList(Block),
dfs: ArrayList(u16),
sccorder: ArrayList(u16),
refs: ArrayList(u16),
narg: u16 = 0,
nvar: u16 = 0,
// variables 2.0: virtual registero
nvreg: u16 = 0,
vregs: ArrayList(u16),

// 8-byte slots in stack frame
nslots: u8 = 0,

nsave: u8 = 0,

// canonical order of callee saved registers. nsave>0 means
// the first nsave items needs to be saved and restored
pub const callee_saved: [5]IPReg = .{ .rbx, .r12, .r13, .r14, .r15 };

// filler value for unintialized refs. not a sentinel for
// actually invalid refs!
pub const DEAD: u16 = 0xFEFF;
// For blocks: we cannot have more than 2^14 blocks anyway
// for vars: don't allocate last block!
pub const NoRef: u16 = 0xFFFF;

pub fn uv(s: usize) u16 {
    return @intCast(u16, s);
}

pub const Node = struct {
    s: [2]u16 = .{ 0, 0 }, // sucessors
    dfnum: u16 = 0,
    idom: u16 = 0,
    predref: u16 = 0,
    npred: u16 = 0,
    // NB: might be NoRef if the node was deleted,
    // a reachable node must have at least one block even if empty!
    firstblk: u16,
    lastblk: u16,
    dfs_parent: u16 = 0, // TODO: unused
    lowlink: u16 = 0,
    scc: u16 = 0, // XXX: not a topological index, just an identidifer
    live_in: u64 = 0, // TODO: globally allocate a [n_nodes*nvreg] multibitset
};

pub const EMPTY: Inst = .{ .tag = .empty, .op1 = 0, .op2 = 0 };

pub const BLK_SIZE = 4;
pub const BLK_SHIFT = 2;
pub const Block = struct {
    node: u16,
    succ: u16 = NoRef,
    i: [BLK_SIZE]Inst = .{EMPTY} ** BLK_SIZE,

    pub fn next(self: @This()) ?u16 {
        return if (self.succ != NoRef) self.succ else null;
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
    pub fn into(self: SpecType) u4 {
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
    // reindex: u16 = 0,
    mckind: MCKind = .unallocated_raw,
    mcidx: u8 = undefined,
    // n_use: u16 = 0,
    last_use: u16 = NoRef,
    vreg: u16 = NoRef,

    fn free(self: @This()) bool {
        return self.tag == .empty;
    }

    // TODO: handle spec being split between u4 type and u4 somethingelse?
    pub fn spec_type(self: Inst) ValType {
        return self.mem_type();
    }

    // TODO: handle spec being split between u4 type and u4 somethingelse?
    // for load/store insts that can handle both intptr and avxvalues
    pub fn mem_type(self: Inst) SpecType {
        return SpecType.from(self.spec & TYPE_MASK);
    }

    const TYPE_MASK: u8 = (1 << 4) - 1;
    const HIGH_MASK: u8 = ~TYPE_MASK;

    pub fn high_spec(self: Inst) u4 {
        return @intCast(u4, (self.spec & HIGH_MASK) >> 4);
    }

    pub fn vop(self: Inst) VMathOp {
        return @intToEnum(VMathOp, self.high_spec());
    }

    pub fn fmode(self: Inst) FMode {
        return @intToEnum(FMode, self.spec & TYPE_MASK);
    }

    pub fn res_type(inst: Inst) ?ValType {
        return switch (inst.tag) {
            .empty => null,
            .arg => inst.spec_type(), // TODO: haIIIII
            .variable => inst.spec_type(), // gets preserved to the phis
            .putvar => null,
            .phi => inst.spec_type(),
            .putphi => null, // stated in the phi instruction
            .constant => inst.spec_type(),
            .alloc => .intptr, // the type of .alloc is a pointer to it
            .renum => null, // should be removed at this point
            .load => inst.spec_type(),
            .lea => .intptr, // Lea? Who's Lea??
            .store => null,
            .iop => .intptr,
            .icmp => null, // technically the FLAG register but anyway
            .imul => .intptr,
            .shr => .intptr,
            .vmath => .avxval,
            .ret => null,
            .call => .intptr,
            .callarg => null,
        };
    }

    pub fn has_res(inst: Inst) bool {
        return inst.res_type() != null;
    }

    pub fn ipreg(i: Inst) ?IPReg {
        return if (i.mckind == .ipreg) @intToEnum(IPReg, i.mcidx) else null;
    }

    pub fn avxreg(i: Inst) ?u4 {
        return if (i.mckind == .vfreg) @intCast(u4, i.mcidx) else null;
    }
};
pub const Tag = enum(u8) {
    empty = 0, // empty slot. must not be refered to!
    alloc,
    arg,
    variable,
    putvar, // non-phi assignment
    phi,
    /// assign to phi of (only) successor
    /// note: despite swearing in the intel church.
    /// op1 is source and op2 is dest, to simplify stuff
    /// i e n_op(putphi) == 1 for the most part
    putphi,
    renum,
    constant,
    load,
    imul,
    shr, // TODO: all shifts to a family
    lea,
    store,
    iop, // imath group?
    icmp,
    vmath,
    ret,
    call,
    callarg,
};

pub const MCKind = enum(u8) {
    // not yet allocated, or Inst that trivially produces no value
    unallocated_raw,
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
    // example "lea" and then "store", or "load" and then iop/vmath
    fused,

    fn unallocated(self: @This()) bool {
        return switch (self) {
            .unallocated_raw => true,
            .unallocated_ipreghint => true,
            .unallocated_vfreghint => true,
            else => false,
        };
    }
};

// number of op:s which are inst references.
// otherwise they can store whatever data
pub fn n_op(tag: Tag, rw: bool) u2 {
    return switch (tag) {
        .empty => 0,
        .arg => 0,
        .variable => 0,
        // really only one, but we will get rid of this lie
        // before getting into any serious analysis.
        .putvar => 2,
        .phi => 0,
        // works on stage1:
        // .putphi => @as(u2, if (rw) 2 else 1),
        // works on stage2:
        // .putphi => if (rw) 2 else 1,
        // works on both: (clown_emoji)
        .putphi => if (rw) @as(u2, 2) else @as(u2, 1), // TODO: booooooo
        .constant => 0,
        .renum => 1,
        .load => 2, // base, idx
        .lea => 2, // base, idx. elided when only used for a store!
        .store => 2, // addr, val
        .iop => 2,
        .icmp => 2,
        .imul => 2,
        .shr => 2,
        .vmath => 2,
        .ret => 1,
        .callarg => 1,
        .call => 0, // could be for funptr/dynamic syscall?
        .alloc => 0,
    };
}

pub fn init(n: u16, allocator: Allocator) !Self {
    return Self{
        .a = allocator,
        .n = try ArrayList(Node).initCapacity(allocator, n),
        .dfs = ArrayList(u16).init(allocator),
        .vregs = ArrayList(u16).init(allocator),
        .sccorder = ArrayList(u16).init(allocator),
        .refs = try ArrayList(u16).initCapacity(allocator, 4 * n),
        .b = try ArrayList(Block).initCapacity(allocator, 2 * n),
    };
}

pub fn deinit(self: *Self) void {
    self.n.deinit();
    self.dfs.deinit();
    self.sccorder.deinit();
    self.refs.deinit();
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
    if (ref == NoRef) {
        return null;
    }
    const r = fromref(ref);
    const blk = &self.b.items[r.block];
    return BIREF{ .n = blk.node, .i = &blk.i[r.idx] };
}

pub fn iref(self: *Self, ref: u16) ?*Inst {
    return if (self.biref(ref)) |bi| bi.i else null;
}

pub fn vspec(vop: VMathOp, fmode: FMode) u8 {
    return (vop.off() << 4) | @as(u8, @enumToInt(fmode));
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
        blkid = uv(self.b.items.len);
        blk.succ = blkid;
        blk = try self.b.addOne();
        blk.* = .{ .node = node };
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
    var i: i8 = 0;
    while (i < BLK_SIZE) : (i += 1) {
        if (blk.i[@intCast(u8, i)].free()) {
            firstfree = i;
        } else {
            break;
        }
    }

    if (firstfree == -1) {
        const nextblk = blkid;
        blkid = uv(self.b.items.len);
        blk = try self.b.addOne();
        blk.* = .{ .node = node, .succ = nextblk };
        n.firstblk = blkid;
        firstfree = BLK_SIZE - 1;
    }

    const free = @intCast(u8, firstfree);

    blk.i[free] = inst;
    return toref(blkid, free);
}

pub fn const_int(self: *Self, node: u16, val: u16) !u16 {
    // TODO: actually store constants in a buffer, or something
    return self.addInst(node, .{ .tag = .constant, .op1 = val, .op2 = 0, .spec = intspec(.dword).into() });
}

pub fn binop(self: *Self, node: u16, tag: Tag, op1: u16, op2: u16) !u16 {
    return self.addInst(node, .{ .tag = tag, .op1 = op1, .op2 = op2 });
}

pub fn sphigh(high: u4, low: u4) u8 {
    return @as(u8, high) << 4 | low;
}

pub fn load(self: *Self, node: u16, kind: SpecType, base: u16, idx: u16, scale: u2) !u16 {
    return self.addInst(node, .{ .tag = .load, .op1 = base, .op2 = idx, .spec = sphigh(scale, kind.into()) });
}
pub fn store(self: *Self, node: u16, kind: SpecType, base: u16, idx: u16, scale: u2, val: u16) !u16 {
    // FUBBIT: all possible instances of fusing should be detected in analysis anyway
    const addr = if (idx != NoRef) try self.addInst(node, .{ .tag = .lea, .op1 = base, .op2 = idx, .mckind = .fused, .spec = sphigh(scale, 0) }) else base;
    return self.addInst(node, .{ .tag = .store, .op1 = addr, .op2 = val, .spec = kind.into() });
}

pub fn vmath(self: *Self, node: u16, vop: VMathOp, fmode: FMode, op1: u16, op2: u16) !u16 {
    // TODO: somewhere, typecheck that FMode matches fmode of args..
    return self.addInst(node, .{ .tag = .vmath, .spec = vspec(vop, fmode), .op1 = op1, .op2 = op2 });
}

pub fn iop(self: *Self, node: u16, vop: AOp, op1: u16, op2: u16) !u16 {
    return self.addInst(node, .{ .tag = .iop, .spec = vop.opx(), .op1 = op1, .op2 = op2 });
}

pub fn icmp(self: *Self, node: u16, cond: Cond, op1: u16, op2: u16) !u16 {
    return self.addInst(node, .{ .tag = .icmp, .spec = cond.off(), .op1 = op1, .op2 = op2 });
}

// TODO: fold into iop, no need to special case x86 weirdology here
pub fn imul(self: *Self, node: u16, op1: u16, op2: u16) !u16 {
    return self.addInst(node, .{ .tag = .imul, .op1 = op1, .op2 = op2 });
}

pub fn shr(self: *Self, node: u16, op1: u16, op2: u16) !u16 {
    return self.addInst(node, .{ .tag = .shr, .op1 = op1, .op2 = op2 });
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

pub fn call(self: *Self, node: u16, num: u16) !u16 {
    return try self.addInst(node, .{
        .tag = .call,
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

pub fn p(self: *Self, s1: u16, s2: u16) void {
    var z1: u16 = s1;
    var z2: u16 = s2;
    if (true and s2 != 0) {
        z1 = s2;
        z2 = s1;
    }
    // TODO: this is INVALID
    self.n.appendAssumeCapacity(.{ .s = .{ z1, z2 }, .firstblk = NoRef, .lastblk = NoRef });
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
    const n = self.n.items;
    // TODO: policy for rebuilding refs from scratch?
    if (self.refs.items.len > 0) unreachable;
    for (n) |v| {
        if (v.s[0] > 0) {
            n[v.s[0]].npred += 1;
        }
        if (v.s[1] > 0 and v.s[1] != v.s[0]) {
            n[v.s[1]].npred += 1;
        }
    }
    for (n) |v, i| {
        const shared = v.s[1] > 0 and v.s[1] == v.s[0];
        if (shared) return error.NotSureAboutThis;
        const split = v.s[1] > 0;
        try self.predlink(@intCast(u16, i), 0, split);
        try self.predlink(@intCast(u16, i), 1, split);
    }
}

pub fn calc_dfs(self: *Self) !void {
    const n = self.n.items;
    var stack = try ArrayList(u16).initCapacity(self.a, n.len);
    try self.dfs.ensureTotalCapacity(n.len);
    defer stack.deinit();
    stack.appendAssumeCapacity(0);
    while (stack.items.len > 0) {
        const v = stack.pop();
        if (n[v].dfnum > 0) {
            // already visited
            continue;
        }
        if (false) print("dfs[{}] = {};\n", .{ self.dfs.items.len, v });
        n[v].dfnum = uv(self.dfs.items.len);
        self.dfs.appendAssumeCapacity(v);

        for (n[v].s) |si| {
            // origin cannot be revisited anyway
            if (si > 0 and n[si].dfnum == 0) {
                n[si].dfs_parent = v;
                stack.appendAssumeCapacity(si);
            }
        }
    }
}

pub fn calc_scc(self: *Self) !void {
    const n = self.n.items;
    try self.dfs.ensureTotalCapacity(n.len);
    var stack = try ArrayList(u16).initCapacity(self.a, n.len);
    defer stack.deinit();
    try self.sccorder.ensureTotalCapacity(n.len);
    self.scc_connect(&stack, 0);
}

pub fn scc_connect(self: *Self, stack: *ArrayList(u16), v: u16) void {
    const n = self.n.items;
    n[v].dfnum = uv(self.dfs.items.len);
    self.dfs.appendAssumeCapacity(v);

    stack.appendAssumeCapacity(v);
    n[v].lowlink = n[v].dfnum;

    for (n[v].s) |w| {
        // origin cannot be revisited anyway
        if (w > 0) {
            if (n[w].dfnum == 0) {
                n[w].dfs_parent = v;
                self.scc_connect(stack, w);
                n[v].lowlink = math.min(n[v].lowlink, n[w].lowlink);
            } else if (n[w].dfnum < n[v].dfnum and n[w].scc == 0) { // or whatever
                n[v].lowlink = math.min(n[v].lowlink, n[w].dfnum);
            }
        }
    }

    if (n[v].lowlink == n[v].dfnum) {
        while (true) {
            const w = stack.pop();
            self.sccorder.appendAssumeCapacity(w);
            // XXX: not topologically sorted, just enables the check: n[i].scc == n[j].scc
            n[w].scc = v;
            if (w == v) break;
        }
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

    var last_scc: u16 = NoRef;
    var cur_scc: u16 = NoRef;

    var sci = self.sccorder.items.len - 1;
    while (true) : (sci -= 1) {
        const old_ni = self.sccorder.items[sci];
        const ni = if (oldlink[old_ni] != NoRef) oldlink[old_ni] else old_ni;
        const n = &self.n.items[ni];

        newlink[old_ni] = newpos;

        if (ni != newpos) {
            const oldval = if (oldlink[newpos] != NoRef) oldlink[newpos] else newpos;
            oldlink[ni] = oldval;

            oldlink[oldval] = ni;
        }

        if (n.scc != last_scc) {
            last_scc = n.scc;
            cur_scc = newpos;
        }
        n.scc = cur_scc;

        if (ni != newpos) {
            mem.swap(Node, n, &self.n.items[newpos]);
        }
        newpos += 1;

        if (sci == 0) break;
    }

    assert(newpos <= self.n.items.len);
    // oopsie woopsie, we killed some dead nodes!
    self.n.items.len = newpos;

    // fixup references:
    for (self.n.items) |*n, ni| {
        for (n.s) |*s| {
            if (s.* != NoRef) {
                s.* = newlink[s.*];
            }
        }

        for (self.preds(uv(ni))) |*pi| {
            pi.* = newlink[pi.*];
        }

        var cur_blk: ?u16 = n.firstblk;
        while (cur_blk) |blk| {
            var b = &self.b.items[blk];
            b.node = uv(ni);

            cur_blk = b.next();
        }
    }
}

// assumes already reorder_nodes !
pub fn reorder_inst(self: *Self) !void {
    const newlink = try self.a.alloc(u16, self.b.items.len * BLK_SIZE);
    mem.set(u16, newlink, NoRef);
    const newblkpos = try self.a.alloc(u16, self.b.items.len);
    mem.set(u16, newblkpos, NoRef);
    defer self.a.free(newlink);
    defer self.a.free(newblkpos);
    var newpos: u16 = 0;

    // already in scc order
    for (self.n.items) |*n| {
        var cur_blk: ?u16 = n.firstblk;
        var blklink: ?u16 = null;

        while (cur_blk) |old_blk| {
            // TRICKY: we might have swapped out the block
            const newblk = newpos >> BLK_SHIFT;
            const blk = if (newblkpos[old_blk] != NoRef) newblkpos[old_blk] else old_blk;

            var b = &self.b.items[blk];
            // TODO: RUNDA UPP
            if (blklink) |link| {
                self.b.items[link].succ = newblk;
            } else {
                n.firstblk = newblk;
            }
            blklink = newblk;

            for (b.i) |_, idx| {
                // TODO: compact away .empty, later when opts is punching holes and stuff
                newlink[toref(old_blk, uv(idx))] = newpos;
                newpos += 1;
            }

            if (blk != newblk) {
                const oldval = if (newblkpos[newblk] != NoRef) newblkpos[newblk] else newblk;
                newblkpos[blk] = oldval;
                newblkpos[oldval] = blk;
            }

            cur_blk = b.next();

            mem.swap(Block, b, &self.b.items[newblk]);
            if (cur_blk == null) {
                n.lastblk = newblk;
            }
        }
    }

    // order irrelevant here, just fixing up broken refs
    for (self.n.items) |*n, ni| {
        if (n.dfnum == 0 and ni > 0) {
            // He's dead, Jim!
            n.firstblk = NoRef;
            n.lastblk = NoRef;
            continue;
        }
        var it = self.ins_iterator(n.firstblk);
        while (it.next()) |item| {
            const nops = n_op(item.i.tag, true);
            if (nops > 0) {
                item.i.op1 = newlink[item.i.op1];
                if (nops > 1) {
                    item.i.op2 = newlink[item.i.op2];
                }
            }
        }
    }
}

// ni = node id of user
pub fn adduse(self: *Self, ni: u16, user: u16, used: u16) void {
    const ref = self.biref(used).?;
    //ref.i.n_use += 1;
    ref.i.last_use = user;
    // it leaks to another block: give it a virtual register number
    if (ref.n != ni) {
        if (ref.i.vreg == NoRef) {
            ref.i.vreg = self.nvreg;
            self.nvreg += 1;
            self.vregs.appendAssumeCapacity(used);
        }
    }
}

// TODO: not idempotent! does not reset n_use=0 first.
// NB: requires reorder_nodes() [scc] and reorder_inst()
pub fn calc_use(self: *Self) !void {
    // TODO: NOT LIKE THIS
    try self.vregs.ensureTotalCapacity(64);

    for (self.n.items) |*n, ni| {
        var cur_blk: ?u16 = n.firstblk;
        while (cur_blk) |blk| {
            var b = &self.b.items[blk];
            for (b.i) |*i, idx| {
                const ref = toref(blk, uv(idx));
                const nops = n_op(i.tag, false);
                if (nops > 0) {
                    self.adduse(uv(ni), ref, i.op1);
                    if (nops > 1) {
                        self.adduse(uv(ni), ref, i.op2);
                    }
                }
            }
            cur_blk = b.next();
        }
    }

    var ni: u16 = uv(self.n.items.len - 1);
    // TODO: at this point the number of vregs is known. so a bitset for
    // node X vreg can be allocated here.

    if (self.nvreg > 64) return error.DoTheWork;

    var scc_end: ?u16 = null;
    var scc_last: bool = false;

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
        if (scc_end == null) {
            // end exclusive
            scc_end = toref(cur_blk.?, BLK_SIZE - 1);
            scc_last = true;
        }
        while (cur_blk) |blk| {
            var b = &self.b.items[blk];
            var idx: usize = BLK_SIZE;
            while (idx > 0) {
                idx -= 1;
                const i = &b.i[idx];

                if (i.vreg != NoRef) {
                    live &= ~(@as(usize, 1) << @intCast(u6, i.vreg));
                }

                const nops = n_op(i.tag, false);
                if (nops > 0) {
                    const ref = self.iref(i.op1).?;
                    if (ref.vreg != NoRef) live |= (@as(usize, 1) << @intCast(u6, ref.vreg));
                    if (nops > 1) {
                        const ref2 = self.iref(i.op2).?;
                        if (ref2.vreg != NoRef) live |= (@as(usize, 1) << @intCast(u6, ref2.vreg));
                    }
                }
            }

            cur_blk = if (blk != n.firstblk) blk - 1 else null;
        }

        n.live_in = live;
        // print("LIVEIN {}: {x} (dvs {})\n", .{ ni, n.live_in, @popCount(n.live_in) });

        if (n.scc == ni) {
            // if scc was not a singleton, we are now at the first node
            // at a SCC which might be a loop. For values live
            // at the entry, extend the liveness interval to the end
            if (!scc_last) {
                var ireg: u16 = 0;
                while (ireg < self.nvreg) : (ireg += 1) {
                    if ((live & (@as(usize, 1) << @intCast(u6, ireg))) != 0) {
                        const i = self.iref(self.vregs.items[ireg]).?;
                        i.last_use = math.max(i.last_use, scc_end.?);
                    }
                }
            }
            scc_end = null;
        }
        scc_last = false;
        if (ni == 0) break;
    }
}

pub fn check_vregs(self: *Self) !void {
    if (self.n.items[0].live_in != 0) return error.HOPPSANSA;
    var err = false;
    for (self.n.items) |*n, ni| {
        var live_out: u64 = 0;
        // hack: if n.s[i] == 0 then no bits will be added anyway
        live_out |= self.n.items[n.s[0]].live_in;
        live_out |= self.n.items[n.s[1]].live_in;

        const born = live_out & ~n.live_in;
        if (born != 0) {
            print("BIRTH {}: {x} which is {}\n", .{ ni, born, @popCount(born) });
            var ireg: u16 = 0;
            while (ireg < self.nvreg) : (ireg += 1) {
                if ((born & (@as(usize, 1) << @intCast(u6, ireg))) != 0) {
                    const i = self.vregs.items[ireg];
                    print(" %{}", .{i});
                    if (self.biref(i).?.n != ni) {
                        print("!!", .{});
                        err = true;
                    }
                }
            }
            print("\n", .{});
        }
    }
    if (err) return error.DoYouEvenLoopAnalysis;
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
    newblk.* = .{ .node = blk.node, .succ = old_succ };
    node.lastblk = blkid;

    if (r.idx < BLK_SIZE - 1) {
        mem.copy(Inst, newblk.i[r.idx + 1 ..], blk.i[r.idx + 1 ..]);
        mem.set(Inst, blk.i[r.idx + 1 ..], EMPTY);
        var i = r.idx + 1;
        while (i < BLK_SIZE) : (i += 1) {
            self.renumber_sloow(toref(r.block, i), toref(blkid, i));
        }
        return toref(r.block, r.idx + 1);
    } else {
        return toref(blkid, 0);
    }
}

pub fn renumber_sloow(self: *Self, from: u16, to: u16) void {
    for (self.b.items) |*blk| {
        for (blk.i) |*i| {
            const nop = n_op(i.tag, false);
            if (nop > 0) {
                if (i.op1 == from) i.op1 = to;
                if (nop > 1) {
                    if (i.op2 == from) i.op2 = to;
                }
            }
        }
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
            for (b.i) |*i, idx| {
                const ref = toref(blk, uv(idx));

                if (i.tag == .arg) {
                    try self.alloc_arg(i);
                } else if (i.has_res() and i.mckind.unallocated()) {
                    const regkind: MCKind = if (i.res_type() == ValType.avxval) .vfreg else .ipreg;
                    const op1 = if (n_op(i.tag, false) > 0) self.iref(i.op1) else null;
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

pub fn scan_alloc(self: *Self) !void {
    var active_avx: [16]u16 = ([1]u16{0}) ** 16;
    var active_ipreg: [16]u16 = ([1]u16{0}) ** 16;
    active_ipreg[IPReg.rsp.id()] = NoRef;
    // just say NO to -fomiting the framepointer!
    active_ipreg[IPReg.rbp.id()] = NoRef;
    // TODO: handle auxilary register properly (by explicit load/spill?)
    active_ipreg[IPReg.rax.id()] = NoRef;

    // callee saved registers, can be allocated on demand
    active_ipreg[IPReg.rbx.id()] = NoRef;
    active_ipreg[IPReg.r12.id()] = NoRef;
    active_ipreg[IPReg.r13.id()] = NoRef;
    active_ipreg[IPReg.r14.id()] = NoRef;
    active_ipreg[IPReg.r15.id()] = NoRef;

    for (self.n.items) |*n| {
        var it = self.ins_iterator(n.firstblk);
        while (it.next()) |item| {
            const i = item.i;
            const ref = item.ref;
            if (i.tag == .arg) {
                try self.alloc_arg(i);
                assert(active_ipreg[i.mcidx] <= ref);
                active_ipreg[i.mcidx] = i.last_use;
            } else if (i.tag == .constant and i.mckind.unallocated()) {
                // TODO: check as needed
                i.mckind = .fused;
            } else if (i.tag == .putphi) {
                const from = self.iref(i.op1).?;
                if (from.ipreg()) |reg| {
                    const to = self.iref(i.op2).?;
                    if (to.mckind == .unallocated_raw) {
                        to.mckind = .unallocated_ipreghint;
                        to.mcidx = reg.id();
                    }
                }
            } else if (i.has_res() and i.mckind.unallocated()) {
                const is_avx = (i.res_type() == ValType.avxval);
                const regkind: MCKind = if (is_avx) .vfreg else .ipreg;
                const the_active = if (is_avx) &active_avx else &active_ipreg;

                if (i.tag == .constant and i.spec == 0) {
                    i.mckind = .fused;
                    continue;
                }

                var regid: ?u4 = null;
                if (i.tag == .iop) {
                    if (self.iref(i.op1).?.ipreg()) |reg| {
                        if (the_active[@enumToInt(reg)] <= ref) {
                            regid = @enumToInt(reg);
                        }
                    }
                }
                if (regid == null and i.mckind == .unallocated_ipreghint) {
                    if (i.res_type() != ValType.intptr) unreachable;
                    if (the_active[i.mcidx] <= ref) {
                        regid = @intCast(u4, i.mcidx);
                    }
                }

                // TODO: reghint for avx!
                if (regid == null) {
                    for (the_active) |l, ri| {
                        if (l <= ref) {
                            regid = @intCast(u4, ri);
                            break;
                        }
                    }
                }

                if (regid == null and regkind == .ipreg) {
                    if (self.nsave < callee_saved.len) {
                        regid = callee_saved[self.nsave].id();
                        self.nsave += 1;
                    }
                }

                if (regid) |ri| {
                    i.mckind = regkind;
                    i.mcidx = ri;
                    the_active[ri] = i.last_use;
                } else {
                    i.mckind = .frameslot;
                    if (self.nslots == 255) {
                        return error.UDunGoofed;
                    }
                    i.mcidx = self.nslots;
                    // TODO: lol reuse slots
                    self.nslots += 1;
                }
            }
        }
    }
    try self.resolve_phi();
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

pub fn check_phi(self: *Self, worklist: *ArrayList(u16), pred: u16, succ: u16) !void {
    const pn = self.n.items[pred];
    var iter = self.ins_iterator(pn.firstblk);
    var saw_putphi = false;

    while (iter.next()) |it| {
        if (it.i.tag == .putphi) {
            saw_putphi = true;
            // it.op1 == NoRef is techy allowed ( %thephi := undefined )
            if (it.i.op2 == NoRef) return error.InvalidCFG;
            try worklist.append(it.i.op2); // TODO: check duplicate
        } else {
            // error: regular instruction after putphi
            if (saw_putphi) return error.InvalidCFG;
        }
    }

    var left: u16 = uv(worklist.items.len);

    var siter = self.ins_iterator(self.n.items[succ].firstblk);
    while (siter.next()) |it| {
        if (it.i.tag == .phi) {
            const idx = mem.indexOfScalar(u16, worklist.items, it.ref) orelse return error.InvalidCFG;
            left -= 1;
            worklist.items[idx] = NoRef;
        } else {
            // TODO: check for phi in the wild if we memoize this before going thu preds
            break;
        }
    }
    if (left > 0) return error.InvalidCFG;
    worklist.items.len = 0; // ALL RESET
    // RETURN
}

pub fn conflict(self: *Self, before: *Inst, after: *Inst) bool {
    const written = self.iref(before.op2) orelse return false;
    const read = self.iref(after.op1) orelse return false;
    if (written.mckind == read.mckind and written.mcidx == read.mcidx) return true;
    return false;
}

pub fn debug_print(self: *Self) void {
    print("\n", .{});
    for (self.n.items) |*n, i| {
        print("node {} (npred {}, scc {}):", .{ i, n.npred, n.scc });
        if (n.live_in != 0) {
            print(" LIVEIN", .{});
            var ireg: u16 = 0;
            while (ireg < self.nvreg) : (ireg += 1) {
                const live = (n.live_in & (@as(usize, 1) << @intCast(u6, ireg))) != 0;
                if (live) {
                    print(" %{}", .{self.vregs.items[ireg]});
                }
            }
        }

        if (n.firstblk == NoRef) {
            print(" VERY DEAD\n", .{});
            continue;
        }

        print("\n", .{});

        self.print_blk(n.firstblk);

        // only print liveout if we have more than one sucessor, otherwise it is BOOORING
        if (n.s[1] != 0) {
            var live_out: u64 = 0;
            live_out |= self.n.items[n.s[0]].live_in;
            live_out |= self.n.items[n.s[1]].live_in;
            if (live_out != 0) {
                print("LIVE_OUT:", .{});
                var ireg: u16 = 0;
                while (ireg < self.nvreg) : (ireg += 1) {
                    const live = (live_out & (@as(usize, 1) << @intCast(u6, ireg))) != 0;
                    if (live) {
                        print(" %{}", .{self.vregs.items[ireg]});
                    }
                }
            }
            print("\n", .{});
        }

        if (n.s[1] == 0) {
            if (n.s[0] == 0) {
                print("  diverge\n", .{});
            } else if (n.s[0] != i + 1) {
                print("  jump {}\n", .{n.s[0]});
            }
        } else {
            print("  split: {any}\n", .{n.s});
        }
    }
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

fn print_blk(self: *Self, firstblk: u16) void {
    var it = self.ins_iterator(firstblk);
    while (it.next()) |item| {
        const i = item.i.*;
        const chr: u8 = if (i.has_res()) '=' else ' ';
        print("  %{} {c} {s}", .{ item.ref, chr, @tagName(i.tag) });

        if (i.tag == .variable) {
            print(" {s}", .{@tagName(i.spec_type())});
        }

        if (i.tag == .vmath) {
            print(".{s}", .{@tagName(i.vop())});
        } else if (i.tag == .iop) {
            print(".{s}", .{@tagName(@intToEnum(AOp, i.spec))});
        } else if (i.tag == .icmp) {
            print(".{s}", .{@tagName(@intToEnum(Cond, i.spec))});
        } else if (i.tag == .constant) {
            print(" c[{}]", .{i.op1});
        } else if (i.tag == .putphi) {
            print(" %{} <-", .{i.op2});
        }
        const nop = n_op(i.tag, false);
        if (nop > 0) {
            print(" %{}", .{i.op1});
            if (nop > 1) {
                print(", %{}", .{i.op2});
            }
        }
        print_mcval(i);
        if (i.last_use != NoRef) {
            // this is a compiler bug ("*" emitted for NoRef)
            //print(" <{}{s}>", .{ i.n_use, @as([]const u8, if (i.vreg != NoRef) "*" else "") });
            // this is getting ridiculous
            if (i.vreg != NoRef) {
                print(" |{}=>%{}|", .{ i.vreg, i.last_use });
            } else {
                print(" <%{}>", .{i.last_use});
            }
            // print(" <{}{s}>", .{ i.last_use, marker });
            //print(" <{}:{}>", .{ i.n_use, i.vreg });
        }
        if (i.tag == .putphi) {
            if (self.iref(i.op2).?.ipreg()) |reg| {
                const regsrc = self.iref(i.op1).?.ipreg();
                print(" [{s} <- {s}] ", .{ @tagName(reg), if (regsrc) |r| @tagName(r) else "XX" });
            }
        }
        print("\n", .{});
    }
}

fn print_mcval(i: Inst) void {
    if (i.tag != .phi and i.tag != .arg and !i.mckind.unallocated() and i.mckind != .fused) {
        print(" =>", .{});
    }
    switch (i.mckind) {
        .frameslot => print(" [rbp-8*{}]", .{i.mcidx}),
        .ipreg => print(" ${s}", .{@tagName(@intToEnum(IPReg, i.mcidx))}),
        .vfreg => print(" $ymm{}", .{i.mcidx}),
        else => {
            if (i.tag == .load or i.tag == .phi or i.tag == .arg) {
                if (i.res_type()) |t| {
                    print(" {s}", .{@tagName(t)});
                }
            }
        },
    }
}

const test_allocator = std.testing.allocator;

pub fn test_analysis(self: *Self, comptime check: bool) !void {
    if (check) {
        self.check_cfg_valid() catch |err| {
            self.debug_print();
            return err;
        };
    }
    try self.calc_preds();

    //try self.calc_dfs();
    try self.calc_scc(); // also provides dfs
    try self.reorder_nodes();
    if (check) try self.check_cfg_valid();
    try SSA_GVN.ssa_gvn(self);
    if (check) try self.check_cfg_valid();

    try self.reorder_inst();
    if (check) try self.check_cfg_valid();
    try self.calc_use();

    if (check) try self.check_vregs();

    // NB: breaks the critical-edge invariant.
    // move after scan_alloc in case it needs it?
    try self.remove_empty();
    if (check) try self.check_cfg_valid();
}

pub fn trivial_succ(self: *Self, ni: u16) ?u16 {
    const node = &self.n.items[ni];
    if (!self.empty(ni, true)) return null;
    return node.s[0];
}

pub fn remove_empty(self: *Self) !void {
    for (self.n.items) |*n, ni| {
        for (n.s) |*s| {
            if (s.* == 0) continue;
            // TODO: could potentially skip through multiple
            // empty blocks!
            const fallthrough = self.trivial_succ(s.*);
            if (fallthrough) |f| {
                const b = &self.n.items[s.*];
                b.npred = 0;
                s.* = f;
                try self.addpred(f, @intCast(u16, ni));
            }
        }
    }
}

pub fn empty(self: *Self, ni: u16, allow_succ: bool) bool {
    const node = &self.n.items[ni];
    if (!allow_succ and node.s[0] != 0) return false;
    if (node.firstblk == node.lastblk) {
        const blk = self.b.items[node.firstblk];
        for (blk.i) |i| {
            if (i.tag != .empty) return false;
        }
        assert(node.s[1] == 0);
        return true;
    } else {
        // we assume reorder_inst will kasta empty blocks, true??
        return false;
    }
}
pub fn get_jmp_or_last(self: *Self, n: *Node) !?Tag {
    var last_inst: ?Tag = null;
    var iter = self.ins_iterator(n.firstblk);
    while (iter.next()) |it| {
        if (last_inst) |l| if (l == .icmp or l == .ret) return error.InvalidCFG;
        last_inst = it.i.tag;
    }
    return last_inst;
}

/// does not use or verify node.npred
pub fn check_cfg_valid(self: *Self) !void {
    const reached = try self.a.alloc(bool, self.n.items.len);
    defer self.a.free(reached);
    mem.set(bool, reached, false);

    var worklist = ArrayList(u16).init(self.a);
    defer worklist.deinit();
    for (self.n.items) |*n, ni| {
        for (n.s) |s| {
            if (s > self.n.items.len) return error.InvalidCFG;
            reached[s] = true;
        }
        // TODO: explicit condition for this
        if (self.refs.items.len > 0) {
            for (self.preds(@intCast(u16, ni))) |pred| {
                const pn = self.n.items[pred];
                if (pn.s[0] != ni and pn.s[1] != ni) {
                    return error.InvalidCFG;
                }
                try self.check_phi(&worklist, pred, uv(ni));
            }
        }
    }
    for (self.n.items) |*n, ni| {
        const last = try self.get_jmp_or_last(n);
        if ((last == Tag.icmp) != (n.s[1] != 0)) return error.InvalidCFG;
        if (last == Tag.ret and n.s[0] != 0) return error.InvalidCFG;
        if (n.s[0] == 0 and (last != Tag.ret and reached[ni])) return error.InvalidCFG;
        // TODO: also !reached and n.s[0] != 0 (not verified by remove_empty)
        if (!reached[ni] and (last != null)) return error.InvalidCFG;
    }
}

// force tests to run:
test {
    std.testing.refAllDecls(@import("./TestFLIR.zig"));
    std.testing.refAllDecls(@import("./TestAOC.zig"));
}
