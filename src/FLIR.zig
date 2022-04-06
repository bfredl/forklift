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
const AOp = CFO.AOp;

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
        // TODO: jesus this is terrible
        return if (self.spec >= TODO_INT_SPEC) .intptr else .avxval;
    }

    const TODO_INT_SPEC: u8 = 8;

    const FMODE_MASK: u8 = (1 << 4) - 1;
    const VOP_MASK: u8 = ~FMODE_MASK;

    pub fn vop(self: Inst) VMathOp {
        return @intToEnum(VMathOp, (self.spec & VOP_MASK) >> 4);
    }

    pub fn fmode(self: Inst) FMode {
        return @intToEnum(FMode, self.spec & FMODE_MASK);
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
            .renum => null, // should be removed at this point
            .load => inst.spec_type(),
            .lea => .intptr, // Lea? Who's Lea??
            .store => null,
            .iop => .intptr,
            .ilessthan => null, // technically the FLAG register but anyway
            .vmath => .avxval,
            .ret => null,
        };
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
    lea,
    store,
    iop, // imath group?
    ilessthan, // icmp group?
    vmath,
    ret,
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
        .ilessthan => 2,
        .vmath => 2,
        .ret => 1,
    };
}

// TODO: expand into precise types, like "dword" or "4 packed doubles"
const ValType = enum(u4) {
    intptr = 0,
    avxval,

    pub fn spec(self: @This()) u4 {
        return @enumToInt(self);
    }
};

// TODO: refactor these to an array of InstMetadata structs
// or this is res_type != null?
pub fn has_res(tag: Tag) bool {
    return switch (tag) {
        .empty => false,
        .arg => true,
        .variable => true, // ASCHUALLY no, but looks like yes
        .putvar => false,
        .phi => true,
        .putphi => false, // storage location is stated in the phi instruction
        .constant => true,
        .renum => true, // TODO: removed at this point
        .load => true,
        .lea => true, // Lea? Who's Lea??
        .store => false,
        .iop => true,
        .ilessthan => false, // technically yes, but no
        .vmath => true,
        .ret => false,
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
    return self.addInst(node, .{ .tag = .constant, .op1 = val, .op2 = 0, .spec = Inst.TODO_INT_SPEC });
}

pub fn binop(self: *Self, node: u16, tag: Tag, op1: u16, op2: u16) !u16 {
    return self.addInst(node, .{ .tag = tag, .op1 = op1, .op2 = op2 });
}

// TODO: better abstraction for types (once we have real types)
pub fn vbinop(self: *Self, node: u16, tag: Tag, fmode: FMode, op1: u16, op2: u16) !u16 {
    return self.addInst(node, .{ .tag = tag, .op1 = op1, .op2 = op2, .spec = @enumToInt(fmode) });
}

pub fn vmath(self: *Self, node: u16, vop: VMathOp, fmode: FMode, op1: u16, op2: u16) !u16 {
    // TODO: somewhere, typecheck that FMode matches fmode of args..
    return self.addInst(node, .{ .tag = .vmath, .spec = vspec(vop, fmode), .op1 = op1, .op2 = op2 });
}

pub fn iop(self: *Self, node: u16, vop: AOp, op1: u16, op2: u16) !u16 {
    return self.addInst(node, .{ .tag = .iop, .spec = vop.opx(), .op1 = op1, .op2 = op2 });
}

pub fn putvar(self: *Self, node: u16, op1: u16, op2: u16) !void {
    _ = try self.binop(node, .putvar, op1, op2);
}

pub fn store(self: *Self, node: u16, base: u16, idx: u16, val: u16) !u16 {
    // FUBBIT: all possible instances of fusing should be detected in analysis anyway
    const addr = try self.addInst(node, .{ .tag = .lea, .op1 = base, .op2 = idx, .mckind = .fused });
    return self.addInst(node, .{ .tag = .store, .op1 = addr, .op2 = val, .spec = self.iref(val).?.spec });
}

pub fn ret(self: *Self, node: u16, val: u16) !void {
    _ = try self.addInst(node, .{ .tag = .ret, .op1 = val, .op2 = 0 });
}

pub fn prePhi(self: *Self, node: u16, v: Inst) !u16 {
    return self.preInst(node, .{ .tag = .phi, .op1 = v.op1, .op2 = 0, .spec = v.spec });
}

// TODO: maintain wf of block 0: first all args, then all vars.

pub fn arg(self: *Self) !u16 {
    if (self.n.items.len == 0) return error.EEEEE;
    const inst = try self.addInst(0, .{ .tag = .arg, .op1 = self.narg, .op2 = 0, .spec = Inst.TODO_INT_SPEC });
    self.narg += 1;
    return inst;
}

pub fn variable(self: *Self) !u16 {
    if (self.n.items.len == 0) return error.EEEEE;
    const inst = try self.addInst(0, .{ .tag = .variable, .op1 = self.nvar, .op2 = 0, .spec = Inst.TODO_INT_SPEC });
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
        addpred(self, s, inter);
        addpred(self, inter, i);
    } else {
        addpred(self, s, i);
    }
}

fn addpred(self: *Self, s: u16, i: u16) void {
    const n = self.n.items;
    // tricky: build the reflist per node backwards,
    // so the end result is the start index
    if (n[s].predref == 0) {
        self.refs.appendNTimesAssumeCapacity(DEAD, n[s].npred);
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
        const ni = if (old_ni < newpos) oldlink[old_ni] else old_ni;
        const n = &self.n.items[ni];

        oldlink[newpos] = ni;
        newlink[old_ni] = newpos;

        if (n.scc != last_scc) {
            last_scc = n.scc;
            cur_scc = newpos;
        }
        n.scc = cur_scc;

        mem.swap(Node, n, &self.n.items[newpos]);
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
    // not needed but for debug:
    // mem.set(u16, newblkpos, NoRef);
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
            const blk = if (old_blk < newblk) newblkpos[old_blk] else old_blk;

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

            newblkpos[newblk] = blk;
            // newblkpos[blk] = newblk;

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
        var cur_blk: ?u16 = n.firstblk;
        while (cur_blk) |blk| {
            var b = &self.b.items[blk];
            for (b.i) |*i| {
                const nops = n_op(i.tag, true);
                if (nops > 0) {
                    i.op1 = newlink[i.op1];
                    if (nops > 1) {
                        i.op2 = newlink[i.op2];
                    }
                }
            }
            cur_blk = b.next();
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

    var scc_end: ?u16 = null;
    var scc_last: bool = false;

    while (true) : (ni -= 1) {
        const n = &self.n.items[ni];
        var live: u64 = 0;
        for (n.s) |s| {
            if (s != NoRef) {
                live |= self.n.items[s].live_in;
            }
        }

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
                _ = i;

                if (i.vreg != NoRef) {
                    live &= ~(@as(usize, 1) << @intCast(u6, i.vreg));
                }

                const nops = n_op(i.tag, true);
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

pub fn alloc_arg(self: *Self, inst: *Inst) !void {
    _ = self;
    const regs: [6]IPReg = .{ .rdi, .rsi, .rdx, .rcx, .r8, .r9 };
    if (inst.op1 >= regs.len) return error.ARA;
    inst.mckind = .ipreg;
    inst.mcidx = regs[inst.op1].id();
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
                } else if (has_res(i.tag) and i.mckind.unallocated()) {
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

    // TODO: allocate callee-saved registers
    active_ipreg[IPReg.rbx.id()] = NoRef;
    active_ipreg[IPReg.r12.id()] = NoRef;
    active_ipreg[IPReg.r13.id()] = NoRef;
    active_ipreg[IPReg.r14.id()] = NoRef;
    active_ipreg[IPReg.r15.id()] = NoRef;

    for (self.n.items) |*n| {
        var cur_blk: ?u16 = n.firstblk;
        while (cur_blk) |blk| {
            var b = &self.b.items[blk];
            for (b.i) |*i, idx| {
                const ref = toref(blk, uv(idx));
                if (i.tag == .arg) {
                    try self.alloc_arg(i);
                    assert(active_ipreg[i.mcidx] <= ref);
                    active_ipreg[i.mcidx] = i.last_use;
                } else if (has_res(i.tag) and i.mckind.unallocated()) {
                    const is_avx = (i.res_type() == ValType.avxval);
                    const regkind: MCKind = if (is_avx) .vfreg else .ipreg;
                    const the_active = if (is_avx) &active_avx else &active_ipreg;

                    if (i.tag == .constant and i.spec == 0) {
                        i.mckind = .fused;
                        continue;
                    }

                    // TODO: reghint
                    var regid: ?u4 = null;
                    for (the_active) |l, ri| {
                        if (l <= ref) {
                            regid = @intCast(u4, ri);
                            break;
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
            cur_blk = b.next();
        }
    }
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
                    print(" {}", .{ireg});
                }
            }
        }

        if (n.firstblk == NoRef) {
            print(" VERY DEAD\n", .{});
            continue;
        }

        print("\n", .{});

        self.print_blk(n.firstblk);

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

fn print_blk(self: *Self, firstblk: u16) void {
    var cur_blk: ?u16 = firstblk;
    while (cur_blk) |blk| {
        // print("THE BLOCK: {}\n", .{blk});
        var b = &self.b.items[blk];
        for (b.i) |i, idx| {
            if (i.tag == .empty) {
                continue;
            }
            const chr: u8 = if (has_res(i.tag)) '=' else ' ';
            print("  %{} {c} {s}", .{ toref(blk, uv(idx)), chr, @tagName(i.tag) });

            if (i.tag == .variable) {
                print(" {s}", .{@tagName(i.spec_type())});
            }

            if (i.tag == .vmath) {
                print(".{s}", .{@tagName(i.vop())});
            } else if (i.tag == .iop) {
                print(".{s}", .{@tagName(@intToEnum(AOp, i.spec))});
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
                // this is a compiler bug ("*" emitted for Noref)
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
            print("\n", .{});
        }
        cur_blk = b.next();
    }
}

fn print_mcval(i: Inst) void {
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
const expectEqual = std.testing.expectEqual;

pub fn test_analysis(self: *Self) !void {
    try self.calc_preds();

    //try self.calc_dfs();
    try self.calc_scc(); // also provides dfs
    try self.reorder_nodes();
    try SSA_GVN.ssa_gvn(self);

    try self.reorder_inst();
    try self.calc_use();
    try self.scan_alloc();
}
