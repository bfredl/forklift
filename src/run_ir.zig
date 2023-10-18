const FLIR = @import("./FLIR.zig");
const CodeBuffer = @import("./CodeBuffer.zig");
const X86Asm = @import("./X86Asm.zig");
const print = std.debug.print;
const ForkScript = @import("./ForkScript.zig");

const Parser = @import("./Parser.zig");
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

pub var options = struct {
    dbg_raw_ir: bool = false,
    dbg_analysed_ir: bool = false,
    dbg_disasm: bool = false,
    dbg_vregs: bool = false,
    dbg_trap: bool = false,
}{};

pub fn main() !void {
    const argv = std.os.argv;
    if (argv.len < 2) return usage();
    var nextarg: u8 = 1;
    const firstarg = mem.span(argv[nextarg]);

    var script: bool = false;

    if (firstarg[0] == '-') {
        if (argv.len < 3) return usage();
        nextarg += 1;
        for (firstarg[1..]) |a| {
            switch (a) {
                's' => script = true,
                'i' => options.dbg_raw_ir = true,
                'a' => options.dbg_analysed_ir = true,
                'v' => options.dbg_vregs = true,
                'd' => options.dbg_disasm = true,
                't' => options.dbg_trap = true,
                else => return usage(),
            }
        }
    }

    var filearg = mem.span(argv[nextarg]);
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

    var parser = try Parser.init(buf, allocator);
    // TODO: this is a little backwards pwnership but will do for now
    // TODO: won't do with multiple functions!
    const ir = &parser.ir;

    if (script) {
        const didret = try ForkScript.parse(ir, buf, allocator);
        if (!didret) {
            @panic("must return");
        }
    } else {
        // try parser.fd_objs.put("count", map_count);
        _ = parser.parse_one_func() catch |e| {
            parser.t.fail_pos();
            print("(byte {} of {})\n", .{ parser.t.pos, buf.len });
            return e;
        };
    }

    if (options.dbg_raw_ir) ir.debug_print();
    try ir.test_analysis(FLIR.X86ABI, true);
    if (options.dbg_analysed_ir) ir.debug_print();

    if (options.dbg_vregs) ir.print_intervals();

    var code = try CodeBuffer.init(allocator);
    // TODO: BULL
    var cfo = X86Asm{ .code = &code };

    // emit trap instruction to invoke debugger
    if (options.dbg_trap) {
        try cfo.trap();
    }

    _ = try @import("./codegen.zig").codegen(ir, &code, options.dbg_disasm);
    try code.finalize();
    if (options.dbg_disasm) try cfo.dbg_nasm(allocator);

    if (inbuf) |b| {
        const SFunc = *const fn (arg1: [*]u8, arg2: usize) callconv(.C) usize;
        const fun = code.get_ptr(0, SFunc);
        print("res: {}\n", .{fun(b.ptr, b.len)});
    }
}
