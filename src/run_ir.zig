const FLIR = @import("./FLIR.zig");
const OSHA = @import("./OSHA.zig");
const CodeBuffer = @import("./CodeBuffer.zig");
const X86Asm = @import("./X86Asm.zig");
const print = std.debug.print;

const parse_mod = @import("./CFOScript.zig").parse_mod;
const defs = @import("./defs.zig");
const std = @import("std");
const mem = std.mem;
const os = std.os;
const posix = std.posix;

pub fn usage() void {
    print("no.\n", .{});
}

pub fn readall(io: std.Io, gpa: mem.Allocator, filename: []const u8) ![]u8 {
    const fil = try std.Io.Dir.cwd().openFile(io, filename, .{});
    const stat = try fil.stat(io);
    const size = std.math.cast(usize, stat.size) orelse return error.FileTooBig;
    const buf = try gpa.alloc(u8, size);
    if (try fil.readStreaming(io, &.{buf}) < size) {
        return error.IOError;
    }
    return buf;
}

pub fn main(init: std.process.Init) !void {
    const argv = init.minimal.args.vector;
    const io = init.io;
    if (argv.len < 2) return usage();
    var nextarg: u8 = 1;
    const firstarg = mem.span(argv[nextarg]);
    var inline_arg1 = false;

    var opts: defs.CFOOptions = .{};

    if (firstarg[0] == '-') {
        if (argv.len < 3) return usage();
        nextarg += 1;
        for (firstarg[1..]) |a| {
            switch (a) {
                'i' => opts.dbg_raw_ir = true,
                'I' => opts.dbg_raw_reorder_ir = true,
                's' => opts.dbg_ssa_ir = true,
                'a' => opts.dbg_analysed_ir = true,
                'p' => opts.dbg_exclude_trivial_put = @panic("putt"),
                'v' => opts.dbg_vregs = true,
                'd' => opts.dbg_disasm = true,
                't' => opts.dbg_trap = true,
                'T' => opts.dbg_trap_join_nodes = true,
                'm' => opts.dbg_regmap = true,
                'o' => opts.dbg_osha = true,
                'q' => inline_arg1 = true,
                else => return usage(),
            }
        }
    }

    const filearg = mem.span(argv[nextarg]);
    nextarg += 1;

    const mode = @import("builtin").mode;
    var gpa = if (mode == .Debug) init.gpa else init.arena.allocator();

    const buf = try readall(io, gpa, filearg);
    defer gpa.free(buf);

    const arg1 = firstarg: {
        if (argv.len <= nextarg) break :firstarg null;
        const span = mem.span(argv[nextarg]);
        nextarg += 1;
        break :firstarg if (inline_arg1) span else try readall(io, gpa, span);
    };
    defer if (!inline_arg1) if (arg1) |b| gpa.free(b);
    const inbuf2 = if (argv.len > nextarg) try readall(io, gpa, mem.span(argv[nextarg])) else null;
    defer if (inbuf2) |b| gpa.free(b);

    var module: @import("./CFOModule.zig") = try .init(gpa);
    defer module.deinit_mem();

    // try parser.fd_objs.put("count", map_count);
    try parse_mod(&module, gpa, buf, opts);

    if (arg1) |b| {
        try module.code.finalize();
        if (opts.dbg_osha) {
            try OSHA.install(&module.code);
        }
        const SFunc = *const fn (arg1: [*]u8, arg2: usize, arg3: ?[*]u8, arg4: usize) callconv(.c) usize;
        const fun = try module.get_func_ptr("main", SFunc);
        const ptr2, const len2 = if (inbuf2) |b2| .{ b2.ptr, b2.len } else .{ null, 0 };
        print("res: {}\n", .{fun(@constCast(b.ptr), b.len, ptr2, len2)});
    }
}
