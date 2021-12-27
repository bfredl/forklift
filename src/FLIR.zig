const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const FLIR = @This();
const print = std.debug.print;
const CFO = @import("./CFO.zig");

pub const Tag = enum(u8) {
    arg,
    load,
    add,
    ret,
};

const ref = u16;

const Inst = struct {
    tag: Tag,
    op1: ref,
    op2: ref,
    alloc: ?u4,
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
    var iarg: u4 = 0;
    while (iarg < narg) : (iarg += 1) {
        try self.inst.append(.{ .tag = .arg, .op1 = iarg, .op2 = 0, .alloc = iarg });
    }
    while (pos < str.len) : (pos += 1) {
        var sp = stack.items.len;
        switch (str[pos]) {
            '+' => {
                if (sp < 2) return error.InvalidSyntax;
                var reg = if (autoreg) @intCast(u4, narg + sp - 2) else null;
                try self.inst.append(.{ .tag = .add, .op1 = stack.items[sp - 2], .op2 = stack.items[sp - 1], .alloc = reg });
                _ = stack.pop();
                stack.items[sp - 2] = @intCast(u16, self.inst.items.len - 1);
            },
            'a'...'d' => {
                const arg = str[pos] - 'a';
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
        .op2 = 0,
        .alloc = 0,
    });
    return self;
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
            .add => {
                const src1 = self.inst.items[inst.op1].alloc.?;
                const src2 = self.inst.items[inst.op2].alloc.?;
                const dst = inst.alloc.?;
                try cfo.vmathf(.add, .sd, dst, src1, src2);
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
    var flir = try rpn(2, "a b +", true, test_allocator);
    defer flir.deinit();

    print("\nIS LEN: {}\n", .{flir.inst.items.len});

    var cfo = try CFO.init(test_allocator);
    defer cfo.deinit();
    _ = try flir.codegen(&cfo);
    try cfo.dbg_nasm(test_allocator);
}
