const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const test_allocator = std.testing.allocator;
const FLIR = @import("./FLIR.zig");
const CFO = @import("./CFO.zig");
const print = std.debug.print;

pub fn nonws(str: []const u8, pos: *usize) ?u8 {
    while (pos.* < str.len) : (pos.* += 1) {
        if (str[pos.*] != ' ') {
            return str[pos.*];
        }
    }
    return null;
}

pub fn expr_0(flir: *FLIR, str: []const u8, pos: *usize) !?u16 {
    const char = nonws(str, pos) orelse return null;
    switch (char) {
        'a'...'d' => {
            const arg = char - 'a';
            if (arg >= flir.narg) return error.InvalidSyntax;
            pos.* += 1;
            return arg;
        },
        'x'...'z' => {
            const arg = char - 'x';
            var inst: FLIR.Inst = .{ .tag = .load, .opspec = arg, .op1 = 0 };
            pos.* += 1;
            const c2 = nonws(str, pos) orelse ' ';
            if ('0' <= c2 and c2 < '9') {
                inst.op1 = c2 - '0';
                pos.* += 1;
            }
            return try flir.put(inst);
        },
        else => return null,
    }
}

pub fn expr_1(flir: *FLIR, str: []const u8, pos: *usize) !?u16 {
    var val = (try expr_0(flir, str, pos)) orelse return null;
    while (nonws(str, pos)) |char| {
        const theop: CFO.VMathOp = switch (char) {
            '*' => .mul,
            '/' => .div,
            else => return val,
        };
        pos.* += 1;
        const op = (try expr_0(flir, str, pos)) orelse return error.EXPR1;
        val = try flir.put(.{ .tag = .vmath, .opspec = theop.off(), .op1 = val, .op2 = op });
    }
    return val;
}

pub fn expr_2(flir: *FLIR, str: []const u8, pos: *usize) !?u16 {
    var val = (try expr_1(flir, str, pos)) orelse return null;
    while (nonws(str, pos)) |char| {
        const theop: CFO.VMathOp = switch (char) {
            '+' => .add,
            '-' => .sub,
            else => return val,
        };
        pos.* += 1;
        const op = (try expr_1(flir, str, pos)) orelse return error.EXPR2;
        val = try flir.put(.{ .tag = .vmath, .opspec = theop.off(), .op1 = val, .op2 = op });
    }
    return val;
}

pub fn stmt(flir: *FLIR, str: []const u8, pos: *usize) !?bool {
    const char = nonws(str, pos) orelse return null;
    var inst: FLIR.Inst = dest: {
        switch (char) {
            'r' => {
                pos.* += 1;
                break :dest .{ .tag = .ret, .op1 = undefined };
            },
            'x'...'z' => {
                pos.* += 1;
                const arg = char - 'x';
                const c2 = nonws(str, pos) orelse ' ';
                var op2: u16 = 0;
                if ('0' <= c2 and c2 < '9') {
                    op2 = c2 - '0';
                    _ = op2;
                    pos.* += 1;
                }
                break :dest .{ .tag = .store, .opspec = arg, .op1 = undefined, .op2 = op2 };
            },
            else => return error.SyntaxError,
        }
    };

    if (nonws(str, pos) != @as(u8, '=')) return error.SyntaxError;
    pos.* += 1;
    inst.op1 = (try expr_2(flir, str, pos)) orelse return error.EOFError;
    if (nonws(str, pos) != @as(u8, ';')) return error.SyntaxError;
    pos.* += 1;

    _ = try flir.put(inst);
    return (inst.tag == .ret);
}

pub fn parse(flir: *FLIR, str: []const u8) !bool {
    var pos: usize = 0;
    var didret = false;
    while (try stmt(flir, str, &pos)) |res| {
        if (res) {
            didret = true;
            break;
        }
    }
    if (nonws(str, &pos) != null) return error.SyntaxError;
    return didret;
}

pub fn main() !void {
    const arg1 = std.os.argv[1];
    var flir = try FLIR.init(4, test_allocator);
    defer flir.deinit();

    const ret = try parse(&flir, std.mem.span(arg1));
    if (!ret) print("gör du ens\n", .{});
    flir.live();
    try flir.scanreg();
    flir.debug_print();

    var cfo = try CFO.init(test_allocator);
    defer cfo.deinit();
    _ = try flir.codegen(&cfo);
    try cfo.dbg_nasm(test_allocator);
}
