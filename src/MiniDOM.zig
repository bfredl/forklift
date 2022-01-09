const std = @import("std");
const math = std.math;
const Allocator = std.mem.Allocator;
const mem = std.mem;
const print = std.debug.print;

const FLIR = @import("./FLIR2.zig");

const ArrayList = @import("./fake_list.zig").ArrayList;

const uv = FLIR.uv;
const DomState = @This();

sdom: u16 = undefined,
ancestor: ?u16 = null,
parent: u16 = undefined,
bucket: u16 = 0,
bucklink: u16 = 0,
label: u16 = undefined,

fn dominators(self: *FLIR) !void {
    const n = self.n.items;
    const s = try self.a.alloc(DomState, n.len);
    defer self.a.free(s);
    mem.set(DomState, s, .{});

    self.dfs = try self.a.alloc(u16, n.len);

    var stack = try ArrayList(u16).initCapacity(self.a, n.len);
    defer stack.deinit();
    var qi: u16 = 0;
    stack.appendAssumeCapacity(0);
    while (stack.items.len > 0) {
        const v = stack.pop();
        if (n[v].dfnum > 0) {
            // already visited
            continue;
        }
        if (false) print("dfs[{}] = {};\n", .{ qi, v });
        n[v].dfnum = qi;
        self.dfs[qi] = v;
        qi += 1;
        s[v].sdom = v;
        s[v].label = v;

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
            var u = eval(self, s, v);
            if (n[s[u].sdom].dfnum < n[s[w].sdom].dfnum) {
                s[w].sdom = s[u].sdom;
            }
        }

        var wp = s[w].parent;

        if (s[w].sdom != s[w].parent) {
            if (s[w].bucklink != 0) return error.AAAAAA;
            s[w].bucklink = s[s[w].sdom].bucket;
            s[s[w].sdom].bucket = w;
        } else {
            n[w].idom = wp;
        }

        s[w].ancestor = s[w].parent;
        while (s[wp].bucket != 0) {
            var v = s[wp].bucket;
            s[wp].bucket = s[v].bucklink;
            s[v].bucklink = 0;
            var u = eval(self, s, v);
            if (n[s[u].sdom].dfnum < n[s[v].sdom].dfnum) {
                n[v].idom = u;
            } else {
                n[v].idom = wp;
            }
        }
    }

    i = 1;
    while (i < qi) : (i += 1) {
        var w = self.dfs[i];
        if (n[w].idom != s[w].sdom) {
            n[w].idom = n[n[w].idom].idom;
        }
    }
}

fn eval_slow(self: *FLIR, s: []DomState, v0: u16) u16 {
    const n = self.n.items;
    var v = v0;
    var u = v;
    while (s[v].ancestor) |a| {
        if (n[s[v].sdom].dfnum < n[s[u].sdom].dfnum) {
            u = v;
        }
        v = a;
    }
    return u;
}

fn eval(self: *FLIR, s: []DomState, v: u16) u16 {
    const n = self.n.items;
    if (s[v].ancestor) |a| {
        _ = eval(self, s, a);
        if (s[a].ancestor) |aa| {
            if (n[s[s[a].label].sdom].dfnum < n[s[s[v].label].sdom].dfnum) {
                s[v].label = s[a].label;
                s[v].ancestor = aa;
            }
        }
    }
    return s[v].label;
}

const test_allocator = std.testing.allocator;

const expectEqual = std.testing.expectEqual;

test "aa" {
    var self = try FLIR.init(8, test_allocator);
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

    if (false) {
        print("preds:\n", .{});
        for (self.n.items) |_, i| {
            print("{} :", .{i});
            for (self.preds(uv(i))) |pred| {
                print(" {}", .{pred});
            }
            print("\n", .{});
        }
    }

    try dominators(&self);

    var idoms: [8]u16 = .{
        0,
        0,
        1,
        1,
        3,
        1,
        2,
        6,
    };

    print("doms:\n", .{});
    for (self.n.items) |v, i| {
        expectEqual(idoms[i], v.idom) catch {
            print("index {}\n", .{i});
            return error.IDomFail;
        };
    }
}
