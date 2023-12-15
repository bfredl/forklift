const FLIR = @import("./FLIR.zig");
const CodeBuffer = @import("./CodeBuffer.zig");
const X86Asm = @import("./X86Asm.zig");
const print = std.debug.print;

const Parser = @import("./Parser.zig");
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
    const buf = try allocator.alloc(u8, size + 1);
    if (try fil.readAll(buf) < size) {
        return error.IOError;
    }
    buf[size] = 0;
    return buf;
}

pub var options: common.DebugOptions = .{};

pub fn main() !void {
    const argv = std.os.argv;
    if (argv.len < 2) return usage();
    var nextarg: u8 = 1;
    const firstarg = mem.span(argv[nextarg]);

    if (firstarg[0] == '-') {
        if (argv.len < 3) return usage();
        nextarg += 1;
        for (firstarg[1..]) |a| {
            switch (a) {
                'i' => options.dbg_raw_ir = true,
                's' => options.dbg_ssa_ir = true,
                'a' => options.dbg_analysed_ir = true,
                'p' => options.dbg_exclude_trivial_put = true,
                'v' => options.dbg_vregs = true,
                'd' => options.dbg_disasm = true,
                't' => options.dbg_trap = true,
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

    const inbuf = if (argv.len > nextarg) try readall(allocator, mem.span(argv[nextarg])) else null;
    defer if (inbuf) |b| allocator.free(b);
    nextarg += 1;
    const inbuf2 = if (argv.len > nextarg) try readall(allocator, mem.span(argv[nextarg])) else null;
    defer if (inbuf2) |b| allocator.free(b);

    var module = try @import("./CFOModule.zig").init(allocator);
    var parser = try Parser.init(buf, allocator, &module);

    // try parser.fd_objs.put("count", map_count);
    parser.parse(false, false) catch |e| {
        parser.t.fail_pos();
        print("(byte {} of {})\n", .{ parser.t.pos, buf.len });
        return e;
    };

    if (inbuf) |b| {
        try module.code.finalize();
        const SFunc = *const fn (arg1: [*]u8, arg2: usize, arg3: ?[*]u8, arg4: usize) callconv(.C) usize;
        const fun = try module.get_func_ptr("main", SFunc);
        const ptr2, const len2 = if (inbuf2) |b2| .{ b2.ptr, b2.len } else .{ null, 0 };
        // NB: buffers contain an extra NUL byte past the end, don't inlude it in size
        print("res: {}\n", .{fun(b.ptr, b.len - 1, ptr2, len2 - 1)});
    }
}
