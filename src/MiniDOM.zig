const std = @import("std");
const math = std.math;
const Allocator = std.mem.Allocator;
const print = std.debug.print;

const Self = @This();

const ArrayList = @import("./fake_list.zig").ArrayList;

// filler value for unintialized refs. not a sentinel for
// actually invalid refs!
const DEAD: u16 = 0xFEFF;

fn uv(s: usize) u16 {
    return @intCast(u16, s);
}

pub const Node = struct {
    s: [2]u16, // sucessors
    dfnum: u16 = 0,
    idom: u16 = 0,
    predref: u16 = 0,
    npred: u16 = 0,
};

a: Allocator,
n: ArrayList(Node),
dfs: []u16,
refs: ArrayList(u16),

const DomState = struct {
    sdom: u16,
    ancestor: ?u16,
    parent: u16,
};

fn init(n: u16, allocator: Allocator) !Self {
    return Self{
        .a = allocator,
        .n = try ArrayList(Node).initCapacity(allocator, n),
        .dfs = &.{},
        .refs = try ArrayList(u16).initCapacity(allocator, 4 * n),
    };
}

fn deinit(self: *Self) void {
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

fn calc_preds(self: *Self) void {
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

fn preds(self: *Self, i: u16) []u16 {
    const v = self.n.items[i];
    return self.refs.items[v.predref..][0..v.npred];
}

fn dominators(self: *Self) !void {
    const n = self.n.items;
    const s = try self.a.alloc(DomState, n.len);
    defer self.a.free(s);

    self.dfs = try self.a.alloc(u16, n.len);

    var stack = try ArrayList(u16).initCapacity(self.a, n.len);
    defer stack.deinit();
    var qi: u16 = 0;
    stack.appendAssumeCapacity(0);
    print("\n", .{});
    while (stack.items.len > 0) : (qi += 1) {
        const v = stack.pop();
        n[v].dfnum = qi;
        self.dfs[qi] = v;
        s[v].sdom = v;
        s[v].ancestor = null;
        print("dfs[{}] = {};\n", .{ qi, v });

        for (n[v].s) |si| {
            // origin cannot be revisited anyway
            if (si > 0 and n[si].dfnum == 0) {
                s[si].parent = v;
                stack.appendAssumeCapacity(si);
            }
        }
    }

    var i = qi - 1;
    while (i >= 1) : (i -= 1) {
        var w = self.dfs[i];
        for (self.preds(w)) |v| {
            var u = eval(v);
            if (n[s[u].sdom].dfnum < n[s[w].sdom].dfnum) {
                s[w].sdom = s[u].sdom;
            }
        }
    }
}

fn eval(self: *Self, v: u16) u16 {
    _ = self;
    _ = v;
    return undefined;
}

const test_allocator = std.testing.allocator;

fn p(self: *Self, s1: u16, s2: u16) void {
    self.n.appendAssumeCapacity(.{ .s = .{ s1, s2 } });
}

test "aa" {
    var self = try init(8, test_allocator);
    defer self.deinit();
    self.p(1, 0);
    self.p(2, 3);
    self.p(3, 6);
    self.p(4, 0);
    self.p(5, 0);
    self.p(0, 0);
    self.p(7, 0);
    self.p(5, 0);

    self.calc_preds();

    print("preds:\n", .{});
    for (self.n.items) |_, i| {
        print("{} :", .{i});
        for (self.preds(uv(i))) |pred| {
            print(" {}", .{pred});
        }
        print("\n", .{});
    }

    try self.dominators();
}
