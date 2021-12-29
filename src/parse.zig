const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const test_allocator = std.testing.allocator;
const FLIR = @import("./FLIR.zig");
const CFO = @import("./CFO.zig");

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
            const ret = try flir.put(.{ .tag = .load, .op1 = arg });
            pos.* += 1;
            return ret;
        },
        else => return null,
    }
}

pub fn parse(flir: *FLIR, str: []const u8) !u16 {
    var pos: usize = 0;
    const res = (try expr_0(flir, str, &pos)) orelse return error.EOFError;
    return res;
}

pub fn main() !void {
    const arg1 = std.os.argv[1];
    var flir = try FLIR.init(4, test_allocator);
    defer flir.deinit();

    const ret = try parse(&flir, std.mem.span(arg1));
    _ = try flir.put(.{ .tag = .ret, .op1 = ret });
    flir.live();
    try flir.scanreg();
    flir.debug_print();

    var cfo = try CFO.init(test_allocator);
    defer cfo.deinit();
    _ = try flir.codegen(&cfo);
    try cfo.dbg_nasm(test_allocator);
}
