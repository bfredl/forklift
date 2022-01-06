const std = @import("std");
const math = std.math;
const Allocator = std.mem.Allocator;
const print = std.debug.print;

const Self = @This();

const ArrayList = @import("./fake_list.zig").ArrayList;

pub const Node = struct {
    s: [2]u16, // sucessors
    dfnum: u16 = 0,
    idom: u16 = 0,
};

a: Allocator,
n: ArrayList(Node),
dfs: []u16,

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
    };
}

fn deinit(self: *Self) void {
    self.n.deinit();
    self.a.free(self.dfs);
}

fn dominators(self: *Self, allocator: Allocator) !void {
    const n = self.n.items;
    const s = try allocator.alloc(DomState, n.len);
    defer allocator.free(s);

    self.dfs = try allocator.alloc(u16, n.len);

    var stack = try ArrayList(u16).initCapacity(allocator, n.len);
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

    try self.dominators(test_allocator);
}
