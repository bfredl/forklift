const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const FLIR = @import("./FLIR.zig");
const CFO = @import("./CFO.zig");

fn rpn(narg: u4, str: []const u8, autoreg: bool, allocator: Allocator) !FLIR {
    var stack = try ArrayList(u16).initCapacity(allocator, 16);
    defer stack.deinit();
    var pos: usize = 0;
    var flir: FLIR = try FLIR.init(narg, allocator);
    errdefer flir.deinit();
    while (pos < str.len) : (pos += 1) {
        var sp = stack.items.len;
        switch (str[pos]) {
            '+', '-', '/', '*', 'M', 'm' => {
                if (sp < 2) return error.InvalidSyntax;
                var reg = if (autoreg) @intCast(u4, narg + sp - 2) else null;
                var op: CFO.VMathOp = switch (str[pos]) {
                    '+' => .add,
                    '-' => .sub,
                    '*' => .mul,
                    '/' => .div,
                    'M' => .max,
                    'm' => .min,
                    else => unreachable,
                };

                try flir.inst.append(.{ .tag = .vmath, .opspec = op.off(), .op1 = stack.items[sp - 2], .op2 = stack.items[sp - 1], .alloc = reg });
                _ = stack.pop();
                stack.items[sp - 2] = @intCast(u16, flir.inst.items.len - 1);
            },
            'a'...'d' => {
                const arg = str[pos] - 'a';
                if (arg >= narg) return error.InvalidSyntax;
                try stack.append(arg);
            },
            'x'...'z' => {
                const arg = str[pos] - 'x';
                var reg = if (autoreg) @intCast(u4, narg + sp) else null;
                if (arg >= narg) return error.InvalidSyntax;
                try flir.inst.append(.{ .tag = .load, .opspec = arg, .op1 = 0, .alloc = reg });
                try stack.append(@intCast(u16, flir.inst.items.len - 1));
            },
            ' ' => continue,
            else => return error.InvalidSyntax,
        }
    }
    if (stack.items.len != 1) return error.InvalidSyntax;
    try flir.inst.append(.{
        .tag = .ret,
        .op1 = stack.items[0],
        .alloc = 0,
    });
    return flir;
}

const test_allocator = std.testing.allocator;
test "the rpn" {
    var flir = try rpn(3, "a x + c a * m", false, test_allocator);
    defer flir.deinit();

    flir.live();
    flir.debug_print();
    try flir.scanreg();
    flir.debug_print();

    var cfo = try CFO.init(test_allocator);
    defer cfo.deinit();
    _ = try flir.codegen(&cfo);
    try cfo.dbg_nasm(test_allocator);
}

pub fn main() !void {
    const arg1 = std.os.argv[1];
    var flir = try rpn(4, std.mem.span(arg1), false, test_allocator);
    defer flir.deinit();

    flir.live();
    try flir.scanreg();
    flir.debug_print();

    var cfo = try CFO.init(test_allocator);
    defer cfo.deinit();
    _ = try flir.codegen(&cfo);
    try cfo.dbg_nasm(test_allocator);
}
