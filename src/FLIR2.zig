const std = @import("std");
const math = std.math;
const Allocator = std.mem.Allocator;
const Self = @This();
const print = std.debug.print;
const CFO = @import("./CFO.zig");
const swap = std.mem.swap;

const builtin = @import("builtin");
const stage2 = builtin.zig_is_stage2;
const ArrayList = @import("./fake_list.zig").ArrayList;

const VMathOp = CFO.VMathOp;

a: Allocator,
// TODO: unmanage all these:
n: ArrayList(Node),
b: ArrayList(Block),
dfs: []u16,
refs: ArrayList(u16),

pub fn uv(s: usize) u16 {
    return @intCast(u16, s);
}

pub const Node = struct {
    s: [2]u16, // sucessors
    dfnum: u16 = 0,
    idom: u16 = 0,
    predref: u16 = 0,
    npred: u16 = 0,
    firstblk: u16,
    foo: u16 = 0,
};

pub const Tag = enum(u8) {
    empty = 0, // empty slot. must not be refered to!
    arg,
    phi,
    constant,
    iadd, // imath group?
    ilessthan, // icmp group?
};

pub const Inst = struct {
    Tag: u8,
    spec: u8 = 0,
    op1: u16,
    op2: u16,
    reindex: u16 = 0,
};

pub const EMPTY: Inst = .{ .tag = .empty, .op1 = 0, .op2 = 0 };

pub const Block = struct {
    node: u16,
    succ: u16 = NoBlk,
    i: [4]Inst = .{EMPTY},
};

test "sizey" {
    // @compileLog(@sizeOf(Block));
    std.debug.assert(@sizeOf(Block) <= 64);
}

// filler value for unintialized refs. not a sentinel for
// actually invalid refs!
const DEAD: u16 = 0xFEFF;
// we cannot have more than 2^14 blocks anyway
const NoBlk: u16 = 0xFFFF;

pub fn init(n: u16, allocator: Allocator) !Self {
    return Self{
        .a = allocator,
        .n = try ArrayList(Node).initCapacity(allocator, n),
        .dfs = &.{},
        .refs = try ArrayList(u16).initCapacity(allocator, 4 * n),
        .blk = try ArrayList(Block).initCapacity(allocator, 2 * n),
    };
}

pub fn deinit(self: *Self) void {
    self.n.deinit();
    self.a.free(self.dfs);
    self.refs.deinit();
}

fn predlink(self: *Self, s: u16, i: u16) void {
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

pub fn calc_preds(self: *Self) void {
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
        if (v.s[0] > 0) {
            self.predlink(v.s[0], @intCast(u16, i));
        }
        if (v.s[1] > 0 and v.s[1] != v.s[0]) {
            self.predlink(v.s[1], @intCast(u16, i));
        }
    }
}

pub fn addBlk(self: *Self) !u16 {

}

pub fn addInst(self: Self, tag: Tag, op1: u16, op2: u16) !u16 {
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
    self.n.appendAssumeCapacity(.{ .s = .{ z1, z2 } });
}
