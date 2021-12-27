const std = @import("std");
const math = std.math;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const FLIR = @This();
const print = std.debug.print;
const CFO = @import("./CFO.zig");

const VMathOp = CFO.VMathOp;

pub const Tag = enum(u8) {
    arg,
    load,
    vmath,
    ret,
};

const ref = u16;

const Inst = struct {
    tag: Tag,
    opspec: u8 = 0,
    op1: ref,
    op2: ref = 0,
    alloc: ?u4,
    live: ?u16 = null,
};

narg: u16,
inst: ArrayList(Inst),

fn deinit(self: FLIR) void {
    self.inst.deinit();
}

fn rpn(narg: u4, str: []const u8, autoreg: bool, allocator: Allocator) !FLIR {
    var stack = try ArrayList(u16).initCapacity(allocator, 16);
    defer stack.deinit();
    var pos: usize = 0;
    var self: FLIR = .{ .narg = narg, .inst = try ArrayList(Inst).initCapacity(allocator, 16) };
    errdefer self.deinit();
    var iarg: u4 = 0;
    while (iarg < narg) : (iarg += 1) {
        try self.inst.append(.{ .tag = .arg, .op1 = iarg, .alloc = iarg });
    }
    while (pos < str.len) : (pos += 1) {
        var sp = stack.items.len;
        switch (str[pos]) {
            '+', '-', '/', '*', 'M', 'm' => {
                if (sp < 2) return error.InvalidSyntax;
                var reg = if (autoreg) @intCast(u4, narg + sp - 2) else null;
                var op: VMathOp = switch (str[pos]) {
                    '+' => .add,
                    '-' => .sub,
                    '*' => .mul,
                    '/' => .div,
                    'M' => .max,
                    'm' => .min,
                    else => unreachable,
                };

                try self.inst.append(.{ .tag = .vmath, .opspec = op.off(), .op1 = stack.items[sp - 2], .op2 = stack.items[sp - 1], .alloc = reg });
                _ = stack.pop();
                stack.items[sp - 2] = @intCast(u16, self.inst.items.len - 1);
            },
            'a'...'d' => {
                const arg = str[pos] - 'a';
                if (arg >= narg) return error.InvalidSyntax;
                try stack.append(arg);
            },
            ' ' => continue,
            else => return error.InvalidSyntax,
        }
    }
    if (stack.items.len != 1) return error.InvalidSyntax;
    try self.inst.append(.{
        .tag = .ret,
        .op1 = stack.items[0],
        .alloc = 0,
    });
    return self;
}

inline fn ninst(self: FLIR) u16 {
    return @intCast(u16, self.inst.items.len);
}

fn live(self: FLIR) void {
    var pos: u16 = 0;
    while (pos < self.ninst()) : (pos += 1) {
        const inst = &self.inst.items[pos];
        const nop: u2 = switch (inst.tag) {
            .arg => 0,
            .vmath => 2,
            .ret => 1,
            .load => undefined,
        };
        if (nop > 0) {
            self.set_live(inst.op1, pos);
            if (nop > 1) {
                self.set_live(inst.op2, pos);
            }
        }
    }
}

inline fn set_live(self: FLIR, used: u16, user: u16) void {
    const inst = &self.inst.items[used];
    inst.live = if (inst.live) |l| math.max(l, user) else user;
}

fn debug_print(self: FLIR) void {
    var pos: u16 = 0;
    print("\n", .{});
    while (pos < self.ninst()) : (pos += 1) {
        const inst = &self.inst.items[pos];
        const marker: u8 = if (inst.live) |_| ' ' else '!';
        print(" %{}{c}= {s}", .{ pos, marker, @tagName(inst.tag) });
        const nop: u2 = switch (inst.tag) {
            .arg => 1,
            .vmath => 2,
            .ret => 1,
            .load => undefined,
        };
        if (inst.tag == .vmath) {
            print(".{s}", .{@tagName(@intToEnum(VMathOp, inst.opspec))});
        }
        if (nop > 0) {
            print(" %{}", .{inst.op1});
            if (self.inst.items[inst.op1].live == pos) {
                print("!", .{});
            }
            if (nop > 1) {
                print(", %{}", .{inst.op2});
                if (self.inst.items[inst.op2].live == pos) {
                    print("!", .{});
                }
            }
        }
        print("\n", .{});
    }
}

fn codegen(self: FLIR, cfo: *CFO) !u32 {
    const target = cfo.get_target();

    try cfo.enter();
    var didret = false;

    var pos: usize = 0;
    while (pos < self.inst.items.len) : (pos += 1) {
        const inst = &self.inst.items[pos];
        switch (inst.tag) {
            .arg => {
                if (inst.op1 != @as(u16, inst.alloc.?)) return error.InvalidArgRegister;
            },
            .vmath => {
                const src1 = self.inst.items[inst.op1].alloc.?;
                const src2 = self.inst.items[inst.op2].alloc.?;
                const dst = inst.alloc.?;
                try cfo.vmathf(@intToEnum(VMathOp, inst.opspec), .sd, dst, src1, src2);
            },
            .ret => {
                const src = self.inst.items[inst.op1].alloc.?;
                if (src != 0) {
                    try cfo.vmovf(.sd, 0, src);
                }
                didret = true;
                break;
            },
            .load => unreachable,
        }
    }

    if (!didret) return error.Unreachable;

    try cfo.leave();
    try cfo.ret();
    return target;
}

const test_allocator = std.testing.allocator;
test "the rpn" {
    var flir = try rpn(3, "a b + c a * m", true, test_allocator);
    defer flir.deinit();

    print("\nIS LEN: {}\n", .{flir.inst.items.len});

    flir.live();
    flir.debug_print();

    var cfo = try CFO.init(test_allocator);
    defer cfo.deinit();
    _ = try flir.codegen(&cfo);
    try cfo.dbg_nasm(test_allocator);
}

pub fn main() !void {
    const arg1 = std.os.argv[1];
    var flir = try rpn(4, std.mem.span(arg1), true, test_allocator);
    defer flir.deinit();
    print("\nIS LEN: {}\n", .{flir.inst.items.len});

    var cfo = try CFO.init(test_allocator);
    defer cfo.deinit();
    _ = try flir.codegen(&cfo);
    try cfo.dbg_nasm(test_allocator);
}
