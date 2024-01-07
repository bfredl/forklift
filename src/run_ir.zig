const FLIR = @import("./FLIR.zig");
const OSHA = @import("./OSHA.zig");
const CodeBuffer = @import("./CodeBuffer.zig");
const X86Asm = @import("./X86Asm.zig");
const print = std.debug.print;

const parse_mod = @import("./CFOScript.zig").parse_mod;
const common = @import("./common.zig");
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

pub var options: common.DebugOptions = .{};

pub fn main() !void {
    const argv = std.os.argv;
    if (argv.len < 2) return usage();
    var nextarg: u8 = 1;
    const firstarg = mem.span(argv[nextarg]);
    var inline_arg1 = false;

    if (firstarg[0] == '-') {
        if (argv.len < 3) return usage();
        nextarg += 1;
        for (firstarg[1..]) |a| {
            switch (a) {
                'i' => options.dbg_raw_ir = true,
                'I' => options.dbg_raw_reorder_ir = true,
                's' => options.dbg_ssa_ir = true,
                'a' => options.dbg_analysed_ir = true,
                'p' => options.dbg_exclude_trivial_put = true,
                'v' => options.dbg_vregs = true,
                'd' => options.dbg_disasm = true,
                't' => options.dbg_trap = true,
                'T' => options.dbg_trap_join_nodes = true,
                'm' => options.dbg_regmap = true,
                'o' => options.dbg_osha = true,
                'q' => inline_arg1 = true,
                else => return usage(),
            }
        }
    }

    const filearg = mem.span(argv[nextarg]);
    nextarg += 1;

    const mode = @import("builtin").mode;
    var gpa = if (mode == .Debug)
        std.heap.GeneralPurposeAllocator(.{}){}
    else
        std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = gpa.allocator();

    const buf = try readall(allocator, filearg);
    defer allocator.free(buf);

    const arg1 = firstarg: {
        if (argv.len <= nextarg) break :firstarg null;
        const span = mem.span(argv[nextarg]);
        nextarg += 1;
        break :firstarg if (inline_arg1) span else try readall(allocator, span);
    };
    defer if (!inline_arg1) if (arg1) |b| allocator.free(b);
    const inbuf2 = if (argv.len > nextarg) try readall(allocator, mem.span(argv[nextarg])) else null;
    defer if (inbuf2) |b| allocator.free(b);

    var module = try @import("./CFOModule.zig").init(allocator);

    // try parser.fd_objs.put("count", map_count);
    try parse_mod(&module, allocator, buf, false, false);

    if (arg1) |b| {
        try module.code.finalize();
        if (options.dbg_osha) {
            try OSHA.install(&module.code);
        }
        const SFunc = *const fn (arg1: [*]u8, arg2: usize, arg3: ?[*]u8, arg4: usize) callconv(.C) usize;
        const fun = try module.get_func_ptr("main", SFunc);
        const ptr2, const len2 = if (inbuf2) |b2| .{ b2.ptr, b2.len } else .{ null, 0 };
        print("res: {}\n", .{fun(b.ptr, b.len, ptr2, len2)});
    }
}
