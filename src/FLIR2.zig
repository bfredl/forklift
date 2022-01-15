const std = @import("std");
const math = std.math;
const Allocator = std.mem.Allocator;
const Self = @This();
const print = std.debug.print;
const CFO = @import("./CFO.zig");
const swap = std.mem.swap;
const SSA_GVN = @import("./SSA_GVN.zig");

const builtin = @import("builtin");
const stage2 = builtin.zig_is_stage2;
const ArrayList = @import("./fake_list.zig").ArrayList;
const assert = std.debug.assert;

const VMathOp = CFO.VMathOp;

a: Allocator,
// TODO: unmanage all these:
n: ArrayList(Node),
b: ArrayList(Block),
dfs: []u16,
// number of reachable blocks (and thus valid entries in dfs)
// non-reachables might not have been pruned yet, so use this
// instead of dfs.len!
// TODO: or just make dfs ArrayListUnmanagable with the rest..
n_dfs: u16 = 0,
refs: ArrayList(u16),
narg: u16 = 0,
nvar: u16 = 0,

pub fn uv(s: usize) u16 {
    return @intCast(u16, s);
}

pub const Node = struct {
    s: [2]u16 = .{ 0, 0 }, // sucessors
    dfnum: u16 = 0,
    idom: u16 = 0,
    predref: u16 = 0,
    npred: u16 = 0,
    firstblk: u16,
    lastblk: u16,
    foo: u16 = 0,
};

pub const Tag = enum(u8) {
    empty = 0, // empty slot. must not be refered to!
    arg,
    variable,
    putvar, // non-phi assignment
    phi,
    putphi, // assign to phi of (only) sucessor
    renum,
    constant,
    load,
    store,
    iadd, // imath group?
    ilessthan, // icmp group?
    vmath,
    ret,
};

pub const Inst = struct {
    tag: Tag,
    spec: u8 = 0,
    op1: u16,
    op2: u16,
    // maybe only for store which is not used otherwise?
    op3: u16 = 0,
    reindex: u16 = 0,

    fn free(self: @This()) bool {
        return self.tag == .empty;
    }
};

// number of op:s which are inst references.
// otherwise they can store whatever data
pub fn n_op(tag: Tag) u2 {
    return switch (tag) {
        .empty => 0,
        .arg => 0,
        .variable => 0,
        .putvar => 2,
        .phi => 0,
        .putphi => 2, // fast nej, bara en?
        .constant => 0,
        .renum => 1,
        .load => 2, // base, idx
        .store => 3, // base, idx, val
        .iadd => 2,
        .ilessthan => 2,
        .vmath => 2,
        .ret => 1,
    };
}

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

pub fn iref(self: *Self, ref: u16) ?*Inst {
    if (ref == NoRef) {
        return null;
    }
    const r = fromref(ref);
    const blk = &self.b.items[r.block];
    return &blk.i[r.idx];
}

test "sizey" {
    // @compileLog(@sizeOf(Block));
    assert(@sizeOf(Block) <= 64);
}

// filler value for unintialized refs. not a sentinel for
// actually invalid refs!
pub const DEAD: u16 = 0xFEFF;
// For blocks: we cannot have more than 2^14 blocks anyway
// for vars: don't allocate last block!
pub const NoRef: u16 = 0xFFFF;

pub fn init(n: u16, allocator: Allocator) !Self {
    return Self{
        .a = allocator,
        .n = try ArrayList(Node).initCapacity(allocator, n),
        .dfs = &.{},
        .refs = try ArrayList(u16).initCapacity(allocator, 4 * n),
        .b = try ArrayList(Block).initCapacity(allocator, 2 * n),
    };
}

pub fn deinit(self: *Self) void {
    self.n.deinit();
    self.a.free(self.dfs);
    self.refs.deinit();
    self.b.deinit();
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
    return self.addInst(node, .{ .tag = .constant, .op1 = val, .op2 = 0 });
}

pub fn binop(self: *Self, node: u16, tag: Tag, op1: u16, op2: u16) !u16 {
    return self.addInst(node, .{ .tag = tag, .op1 = op1, .op2 = op2 });
}

pub fn vBinop(self: *Self, node: u16, vop: VMathOp, op1: u16, op2: u16) !u16 {
    return self.addInst(node, .{ .tag = .vmath, .spec = vop.off(), .op1 = op1, .op2 = op2 });
}

pub fn putvar(self: *Self, node: u16, op1: u16, op2: u16) !void {
    _ = try self.binop(node, .putvar, op1, op2);
}

pub fn ret(self: *Self, node: u16, val: u16) !void {
    // TODO: actually store constants in a buffer, or something
    _ = try self.addInst(node, .{ .tag = .ret, .op1 = val, .op2 = 0 });
}

pub fn prePhi(self: *Self, node: u16, varid: u16) !u16 {
    return self.preInst(node, .{ .tag = .phi, .op1 = varid, .op2 = 0 });
}

// TODO: maintain wf of block 0: first all args, then all vars.

pub fn arg(self: *Self) !u16 {
    if (self.n.items.len == 0) return error.EEEEE;
    const inst = try self.addInst(0, .{ .tag = .arg, .op1 = self.narg, .op2 = 0 });
    self.narg += 1;
    return inst;
}

pub fn variable(self: *Self) !u16 {
    if (self.n.items.len == 0) return error.EEEEE;
    const inst = try self.addInst(0, .{ .tag = .variable, .op1 = self.nvar, .op2 = 0 });
    self.nvar += 1;
    return inst;
}

pub fn dforder(self: *Self) []u16 {
    return self.dfs[0..self.n_dfs];
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

pub fn debug_print(self: *Self) void {
    if (stage2) {
        return;
    }
    print("\n", .{});
    for (self.n.items) |*b, i| {
        print("node {} (npred {}):\n", .{ i, b.npred });

        self.print_blk(b.firstblk);

        if (b.s[1] == 0) {
            if (b.s[0] == 0) {
                print("  diverge\n", .{});
            } else if (b.s[0] != i + 1) {
                print("  jump {}\n", .{b.s[0]});
            }
        } else {
            print("  split: {any}\n", .{b.s});
        }
    }
}

fn print_blk(self: *Self, b: u16) void {
    const blk = self.b.items[b];

    for (blk.i) |i, idx| {
        if (i.tag == .empty) {
            continue;
        }
        print("  %{} = {s}", .{ toref(b, uv(idx)), @tagName(i.tag) });

        if (i.tag == .vmath) {
            print(".{s}", .{@tagName(@intToEnum(VMathOp, i.spec))});
        } else if (i.tag == .constant) {
            print(" c[{}]", .{i.op1});
        }
        const nop = n_op(i.tag);
        if (nop > 0) {
            print(" %{}", .{i.op1});
            if (nop > 1) {
                print(", %{}", .{i.op2});
                if (nop > 2) {
                    print(" <- %{}", .{i.op3});
                }
            }
        }
        print("\n", .{});
    }

    if (blk.next()) |next| {
        return self.print_blk(next);
    }
}

const test_allocator = std.testing.allocator;
const expectEqual = std.testing.expectEqual;

test "printa" {
    var self = try Self.init(8, test_allocator);
    defer self.deinit();

    const node = try self.addNode();
    try expectEqual(uv(0), node);

    const arg1 = try self.addInst(node, .{ .tag = .arg, .op1 = 0, .op2 = 0 });
    const arg2 = try self.addInst(node, .{ .tag = .arg, .op1 = 1, .op2 = 0 });

    const node2 = try self.addNode();
    try expectEqual(uv(1), node2);
    self.n.items[node].s[0] = node2;

    const add = try self.addInst(node2, .{ .tag = .iadd, .op1 = arg1, .op2 = arg2 });
    const zero = try self.addInst(node2, .{ .tag = .constant, .op1 = 0, .op2 = 0 });
    const load1 = try self.addInst(node2, .{ .tag = .load, .op1 = arg1, .op2 = zero });
    const load2 = try self.addInst(node2, .{ .tag = .load, .op1 = arg2, .op2 = zero });
    const vadd = try self.addInst(node2, .{ .tag = .vmath, .spec = VMathOp.add.off(), .op1 = load1, .op2 = load2 });
    _ = vadd;

    _ = try self.addInst(node2, .{ .tag = .ret, .op1 = add, .op2 = 0 });

    self.debug_print();
}

test "loopvar" {
    var self = try Self.init(8, test_allocator);
    defer self.deinit();

    const start = try self.addNode();

    const arg1 = try self.arg();
    const arg2 = try self.arg();
    const arg3 = try self.arg();

    const var_i = try self.variable();
    const const_0 = try self.const_int(start, 0);

    try self.putvar(start, var_i, const_0);

    const loop = try self.addNode();
    // NB: assumes count > 0, always do first iteration
    self.n.items[start].s[0] = loop;

    const val = try self.binop(loop, .load, arg1, var_i);
    const newval = try self.vBinop(loop, .mul, val, val);
    _ = try self.addInst(loop, .{ .tag = .store, .op1 = arg2, .op2 = var_i, .op3 = newval });

    const const_1 = try self.const_int(loop, 1);
    const iadd = try self.binop(loop, .iadd, var_i, const_1);
    try self.putvar(loop, var_i, iadd);
    _ = try self.binop(loop, .ilessthan, var_i, arg3);

    // if true
    self.n.items[loop].s[0] = loop;
    const end = try self.addNode();
    self.n.items[loop].s[1] = end;

    try self.ret(end, const_0);

    try self.calc_preds();

    // sometime later..
    _ = try self.prePhi(loop, var_i);

    self.debug_print();
}

test "diamondvar" {
    var self = try Self.init(8, test_allocator);
    defer self.deinit();

    const start = try self.addNode();
    const arg1 = try self.arg();
    const arg2 = try self.arg();
    const v = try self.variable();

    // const const_0 = try self.const_int(start, 0);
    const const_42 = try self.const_int(start, 42);
    try self.putvar(start, v, const_42);
    _ = try self.binop(start, .ilessthan, arg1, v);

    const left = try self.addNode();
    self.n.items[start].s[0] = left;
    const addl = try self.binop(left, .iadd, v, arg2);
    try self.putvar(left, v, addl);

    const right = try self.addNode();
    self.n.items[start].s[1] = right;
    const addr = try self.binop(right, .iadd, v, arg1);
    try self.putvar(right, v, addr);

    const end = try self.addNode();
    self.n.items[left].s[0] = end;
    self.n.items[right].s[0] = end;

    const const_77 = try self.const_int(end, 77);
    const adde = try self.binop(end, .iadd, v, const_77);
    try self.putvar(end, v, adde);

    try self.ret(end, v);

    try self.calc_preds();
    self.debug_print();
    // TODO: we only use the dfs search, break it out
    // as a separate step!
    try @import("./MiniDOM.zig").dominators(&self);
    try SSA_GVN.ssa_gvn(&self);
    self.debug_print();
}
