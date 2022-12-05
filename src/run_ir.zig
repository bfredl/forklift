const FLIR = @import("./FLIR.zig");
const CFO = @import("./CFO.zig");
const print = std.debug.print;

const IRParse = @import("./IRParse.zig");
const std = @import("std");
const mem = std.mem;
const os = std.os;

pub fn usage() void {
    print("no.\n", .{});
}

pub fn readall(allocator: mem.Allocator, filename: []u8) ![]u8 {
    const fil = try std.fs.cwd().openFile(filename, .{});
    const stat = try os.fstat(fil.handle);
    const size = std.math.cast(usize, stat.size) orelse return error.FileTooBig;
    const buf = try allocator.alloc(u8, size);
    if (try fil.readAll(buf) < size) {
        return error.IOError;
    }
    return buf;
}

pub fn main() !void {
    const argv = std.os.argv;
    if (argv.len < 2) return usage();
    const firstarg = mem.span(argv[1]);
    var filearg = firstarg;

    const mode = @import("builtin").mode;
    var gpa = if (mode == .Debug)
        std.heap.GeneralPurposeAllocator(.{}){}
    else
        std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = gpa.allocator();

    const buf = try readall(allocator, filearg);
    defer allocator.free(buf);

    const inbuf = if (argv.len >= 3) try readall(allocator, mem.span(argv[2])) else null;
    defer if (inbuf) |b| allocator.free(b);

    var ir = try FLIR.init(16, allocator);
    defer ir.deinit();
    var parser = IRParse.init(buf);
    // try parser.fd_objs.put("count", map_count);
    parser.parse_func(&ir, allocator) catch |e| {
        print("error at pos {}:{} (byte {} of {})\n", .{ parser.lnum + 1, 1 + parser.pos - parser.lpos, parser.pos, buf.len });
        return e;
    };

    // ir.debug_print();
    try ir.test_analysis(true);
    try ir.scan_alloc();
    ir.debug_print();
    try ir.check_cfg_valid();

    var cfo = try CFO.init(allocator);

    _ = try @import("./codegen.zig").codegen(&ir, &cfo);
    try cfo.finalize();
    try cfo.dbg_nasm(allocator);

    if (inbuf) |b| {
        const SFunc = *const fn (arg1: [*]u8, arg2: usize) callconv(.C) usize;
        const fun = cfo.get_ptr(0, SFunc);
        print("res: {}\n", .{fun(b.ptr, b.len)});
    }
}
