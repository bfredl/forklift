const FLIR = @import("./FLIR.zig");
const CFO = @import("./CFO.zig");
const print = std.debug.print;

const IRParse = @import("./IRParse.zig");
const std = @import("std");
const mem = std.mem;
const os = std.os;

pub var options = struct {
    dbg_raw_ir: bool = false,
    dbg_analysed_ir: bool = false,
    dbg_disasm: bool = false,
    dbg_vregs: bool = false,
    dbg_trap: bool = false,
}{};

pub fn main() void {
    // var gpa = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    // const allocator = gpa.allocator();
    const allocator = std.heap.page_allocator;

    print("start\n", .{});

    var ir = FLIR.init(32, allocator) catch unreachable;
    print("irren\n", .{});
    defer ir.deinit();
    const buf =
        \\func diff
        \\  %x = arg
        \\  %y = arg
        \\  %d = sub %x %y
        \\  ret %d
        \\end
    ;
    var parser = IRParse.init(buf, allocator);
    // try parser.fd_objs.put("count", map_count);
    _ = parser.parse_func(&ir) catch {
        print("error at pos {}:{} (byte {} of {})\n", .{ parser.lnum + 1, 1 + parser.pos - parser.lpos, parser.pos, buf.len });
        @panic("le panik");
    };

    // if (options.dbg_raw_ir) ir.debug_print();
    ir.test_analysis(true) catch unreachable;
    // if (options.dbg_analysed_ir) ir.debug_print();

    // if (options.dbg_vregs) ir.print_intervals();

    var cfo = CFO.init(allocator) catch unreachable;

    // emit trap instruction to invoke debugger
    // if (options.dbg_trap) try cfo.trap();

    _ = @import("./codegen.zig").codegen(&ir, &cfo, false) catch unreachable;
    cfo.finalize() catch unreachable;
    // if (options.dbg_disasm) try cfo.dbg_nasm(allocator);
    //

    const b = "stringen";

    const SFunc = *const fn (arg1: [*]const u8, arg2: usize) callconv(.C) usize;
    const fun = cfo.get_ptr(0, SFunc);
    print("res: {}\n", .{fun(b, 8)});
}
