const std = @import("std");
const Self = @import("./CFO.zig");

const test_allocator = std.testing.allocator;
const expectEqual = std.testing.expectEqual;
const a = Self.a;
const bo = Self.bo;

test "return first argument" {
    var cfo = try Self.init(test_allocator);
    defer cfo.deinit();

    try cfo.mov(.rax, .rdi);
    try cfo.ret();
    try expectEqual(@as(usize, 4), try cfo.test_call2(4, 10));
}

test "return second argument" {
    var cfo = try Self.init(test_allocator);
    defer cfo.deinit();

    try cfo.mov(.rax, .rsi);
    try cfo.ret();
    try expectEqual(@as(usize, 10), try cfo.test_call2(4, 10));
}

test "read/write first arg as 64-bit pointer" {
    var cfo = try Self.init(test_allocator);
    defer cfo.deinit();

    try cfo.movrm(.rax, a(.rdi));
    try cfo.movmr(a(.rdi), .rsi);
    try cfo.ret();

    var someint: u64 = 33;
    var retval = try cfo.test_call2(@ptrToInt(&someint), 10);
    try expectEqual(@as(usize, 33), retval);
    try expectEqual(@as(usize, 10), someint);
}

test "read/write first arg as 64-bit pointer with offsett" {
    var cfo = try Self.init(test_allocator);
    defer cfo.deinit();

    try cfo.movrm(.rax, bo(.rdi, 0x08));
    try cfo.movmr(bo(.rdi, 0x10), .rsi);
    try cfo.ret();

    var someint: [2]u64 = .{ 33, 45 };
    var retval = try cfo.test_call2(@ptrToInt(&someint) - 8, 79);
    try expectEqual(@as(usize, 33), retval);
    try expectEqual(@as(usize, 33), someint[0]);
    try expectEqual(@as(usize, 79), someint[1]);
}

test "RIP-relative read" {
    var cfo = try Self.init(test_allocator);
    defer cfo.deinit();
    try cfo.wq(0x9090909090909090);
    const theconst = cfo.get_target();
    try cfo.wq(0x1122334455667788);
    // not needed, but put nasm back in style
    // try cfo.wb(0x00);
    const entry = cfo.get_target();
    try cfo.enter();
    try cfo.movrm(.rax, Self.rel(theconst));
    try cfo.leave();
    try cfo.ret();

    try cfo.finalize();
    const fun = cfo.get_ptr(entry, *const fn () callconv(.C) u64);
    try expectEqual(@as(u64, 0x1122334455667788), fun());
}

test "lealink" {
    var cfo = try Self.init(test_allocator);
    defer cfo.deinit();

    //const OSHA = @import("./OSHA.zig");
    //OSHA.install(&cfo);
    //defer OSHA.clear();

    const entry = cfo.get_target();
    try cfo.enter();
    const link = try cfo.lealink(.rdx);
    try cfo.movrm(.rax, a(.rdx));
    try cfo.movrm(.rcx, a(.rdx).o(8));
    try cfo.movmr(a(.rdi), .rcx);
    try cfo.leave();
    try cfo.ret();

    try cfo.set_align(8);
    cfo.set_lea_target(link);
    try cfo.wq(0x8822883344114422);
    try cfo.wq(0x0104050610405060);

    try cfo.finalize();
    const fun = cfo.get_ptr(entry, *const fn (*u64) callconv(.C) u64);
    var somemem: u64 = undefined;
    try expectEqual(@as(u64, 0x8822883344114422), fun(&somemem));
    try expectEqual(@as(u64, 0x0104050610405060), somemem);
}

test "return intermediate value" {
    var cfo = try Self.init(test_allocator);
    defer cfo.deinit();

    // try cfo.movri(.rbx, 20);
    // try cfo.movri(.r15, 7);
    try cfo.movri(.rax, 1337);
    try cfo.ret();

    var retval = try cfo.test_call2(7, 8);
    try expectEqual(@as(usize, 1337), retval);
}

test "write intermediate value to 64-bit pointer" {
    var cfo = try Self.init(test_allocator);
    defer cfo.deinit();

    try cfo.movmi(a(.rdi), 586);
    try cfo.ret();

    var someint: u64 = 33;

    _ = try cfo.test_call2(@ptrToInt(&someint), 8);
    try expectEqual(@as(usize, 586), someint);
}

test "use r12 for base address" {
    var cfo = try Self.init(test_allocator);
    defer cfo.deinit();

    // r12 is callee-saved. so save it
    try cfo.mov(.rcx, .r12);
    try cfo.mov(.r12, .rdi);

    try cfo.movmi(a(.r12), 389);

    try cfo.mov(.r12, .rcx);
    try cfo.ret();

    var someint: u64 = 33;

    _ = try cfo.test_call2(@ptrToInt(&someint), 8);
    try expectEqual(@as(usize, 389), someint);
}

test "add arguments" {
    var cfo = try Self.init(test_allocator);
    defer cfo.deinit();

    try cfo.mov(.rax, .rdi);
    try cfo.arit(.add, .rax, .rsi);
    try cfo.ret();

    var retval = try cfo.test_call2(1002, 560);
    try expectEqual(@as(usize, 1562), retval);
}

test "add arguments using lea" {
    var cfo = try Self.init(test_allocator);
    defer cfo.deinit();

    try cfo.lea(.rax, Self.bi(.rdi, .rsi));
    try cfo.ret();

    var retval = try cfo.test_call2(736, 121);
    try expectEqual(@as(usize, 857), retval);
}

test "add scaled arguments using lea" {
    var cfo = try Self.init(test_allocator);
    defer cfo.deinit();

    try cfo.lea(.rax, Self.qi(.rdi, .rsi));
    try cfo.ret();

    var retval = try cfo.test_call2(736, 121);
    try expectEqual(@as(usize, 1704), retval);
}

test "subtract arguments" {
    var cfo = try Self.init(test_allocator);
    defer cfo.deinit();

    try cfo.mov(.rax, .rdi);
    try cfo.arit(.sub, .rax, .rsi);
    try cfo.ret();

    var retval = try cfo.test_call2(1002, 560);
    try expectEqual(@as(usize, 442), retval);
}

test "add imm8 to argument" {
    var cfo = try Self.init(test_allocator);
    defer cfo.deinit();

    try cfo.mov(.rax, .rdi);
    try cfo.aritri(.add, .rax, 64);
    try cfo.ret();

    var retval = try cfo.test_call2(120, 9204);
    try expectEqual(@as(usize, 184), retval);
}

test "add immediate to argument" {
    var cfo = try Self.init(test_allocator);
    defer cfo.deinit();

    try cfo.mov(.rax, .rdi);
    try cfo.aritri(.add, .rax, 137);
    try cfo.ret();

    var retval = try cfo.test_call2(100, 560);
    try expectEqual(@as(usize, 237), retval);
}

test "get the maximum of two args" {
    var cfo = try Self.init(test_allocator);
    defer cfo.deinit();

    try cfo.mov(.rax, .rdi);
    try cfo.arit(.cmp, .rdi, .rsi);
    const jump = try cfo.jfwd(.g);
    try cfo.mov(.rax, .rsi);
    try cfo.set_target(jump);
    try cfo.ret();

    var retval = try cfo.test_call2(1002, 560);
    try expectEqual(@as(usize, 1002), retval);

    retval = try cfo.test_call2(460, 902);
    try expectEqual(@as(usize, 902), retval);
}

test "jump backwards in a loop" {
    var cfo = try Self.init(test_allocator);
    defer cfo.deinit();

    try cfo.arit(.xor, .rax, .rax);
    const loop = cfo.get_target();
    try cfo.arit(.add, .rax, .rdi);
    try cfo.aritri(.sub, .rdi, 1);
    // equal -> zero after the subtraction
    try cfo.jbck(.ne, loop);
    try cfo.ret();

    var retval = try cfo.test_call2(10, 560);
    try expectEqual(@as(usize, 55), retval);

    retval = try cfo.test_call2(20, 560);
    try expectEqual(@as(usize, 210), retval);
}

test "push/pop" {
    var cfo = try Self.init(test_allocator);
    defer cfo.deinit();

    try cfo.push(.rdi);
    try cfo.pop(.r13);
    try cfo.mov(.rax, .r13);

    try cfo.ret();
    var retval = try cfo.test_call2(9009, 560);
    try expectEqual(@as(usize, 9009), retval);
}

test "add scalar double" {
    var cfo = try Self.init(test_allocator);
    defer cfo.deinit();

    try cfo.vmathf(.add, .sd, 0, 0, 1);
    try cfo.ret();

    var retval = try cfo.test_call2f64(2.0, 0.5);
    try expectEqual(@as(f64, 2.5), retval);
}

test "max of scalar double" {
    var cfo = try Self.init(test_allocator);
    defer cfo.deinit();

    try cfo.vmathf(.max, .sd, 0, 0, 1);
    try cfo.ret();

    var retval = try cfo.test_call2f64(2.0, 5.5);
    try expectEqual(@as(f64, 5.5), retval);

    retval = try cfo.test_call2f64(10.0, 8.5);
    try expectEqual(@as(f64, 10.0), retval);
}

test "move scalar double" {
    var cfo = try Self.init(test_allocator);
    defer cfo.deinit();

    try cfo.vmovf(.sd, 0, 1);
    try cfo.ret();

    var retval = try cfo.test_call2f64(22.0, 0.75);
    try expectEqual(@as(f64, 0.75), retval);
}

test "read/write scalar double" {
    var cfo = try Self.init(test_allocator);
    defer cfo.deinit();

    // as we are swapping [rdi] and xmm0, use a temp
    try cfo.vmovurm(.sd, 1, a(.rdi));
    try cfo.vmovumr(.sd, a(.rdi), 0);
    try cfo.vmovf(.sd, 0, 1);
    try cfo.ret();

    var thefloat: f64 = 13.5;

    var retval = try cfo.test_call2x(f64, &thefloat, @as(f64, 0.25));
    try expectEqual(@as(f64, 13.5), retval);
    try expectEqual(@as(f64, 0.25), thefloat);
}

test "read/write aligned double vector" {
    var cfo = try Self.init(test_allocator);
    defer cfo.deinit();

    try cfo.vmovarm(.pd4, 0, a(.rdi));
    try cfo.vmathf(.mul, .pd4, 0, 0, 0);
    try cfo.vmovamr(.pd4, a(.rdi), 0);
    try cfo.ret();

    var thevec: [4]f64 align(32) = .{ 13.5, 25.125, 4552.0, -50.5 };

    try cfo.test_call2x(void, &thevec, @as(u64, 0));
    try expectEqual(@as(f64, 182.25), thevec[0]);
    try expectEqual(@as(f64, 2550.25), thevec[3]);
}

test "add scalar double from memory" {
    var cfo = try Self.init(test_allocator);
    defer cfo.deinit();
    errdefer cfo.dbg_nasm(test_allocator) catch unreachable;

    try cfo.vmathfrm(.add, .sd, 0, 0, a(.rdi));
    try cfo.ret();

    var thefloat: f64 = 6.5;

    var retval = try cfo.test_call2x(f64, &thefloat, @as(f64, 0.125));
    try expectEqual(@as(f64, 6.625), retval);
    try expectEqual(@as(f64, 6.5), thefloat);
}

test "shlx (shift left)" {
    var cfo = try Self.init(test_allocator);
    defer cfo.deinit();

    try cfo.sx(.hl, .rax, .rdi, .rsi);
    try cfo.ret();

    var retval = try cfo.test_call2(17, 3);
    try expectEqual(@as(usize, 136), retval);
}

fn multiplier(arg: u64) callconv(.C) u64 {
    return arg * 10 + 7;
}

test "indirect call" {
    var cfo = try Self.init(test_allocator);
    defer cfo.deinit();

    try cfo.mov(.r10, .rdi);
    try cfo.mov(.rdi, .rsi); // 2nd arg is now first
    try cfo.call_ptr(.r10);
    try cfo.ret();

    var retval = try cfo.test_call2(@ptrToInt(&multiplier), 14);

    try expectEqual(@as(usize, 147), retval);
}

test "direct call" {
    // std.heap.next_mmap_addr_hint = @intToPtr(@TypeOf(std.heap.next_mmap_addr_hint), @ptrToInt(&multiplier) & ~@as(usize, 0x0FFF));
    // std.debug.print("\nBRK: {}\n", .{@intToPtr(*u8, std.os.linux.syscall1(.brk, 0))});
    // Well I made it, despite your directions
    std.heap.next_mmap_addr_hint = @intToPtr(@TypeOf(std.heap.next_mmap_addr_hint), 0x01000000);
    var cfo = try Self.init(test_allocator);
    // std.debug.print("\nyes: {}\nbut: {*}\n", .{ &multiplier, cfo.code.items.ptr });

    defer cfo.deinit();

    try cfo.maybe_call_rel_abs(@ptrCast(*const u8, &multiplier)) orelse return error.BadDirections;
    try cfo.ret();

    var retval = try cfo.test_call2(4, 1337);
    try expectEqual(@as(usize, 47), retval);
}

test "local call" {
    // std.heap.next_mmap_addr_hint = @intToPtr(@TypeOf(std.heap.next_mmap_addr_hint), @ptrToInt(&multiplier) & ~@as(usize, 0x0FFF));
    // std.debug.print("\nBRK: {}\n", .{@intToPtr(*u8, std.os.linux.syscall1(.brk, 0))});
    // Well I made it, despite your directions
    std.heap.next_mmap_addr_hint = @intToPtr(@TypeOf(std.heap.next_mmap_addr_hint), 0x01000000);
    var cfo = try Self.init(test_allocator);
    // std.debug.print("\nyes: {}\nbut: {*}\n", .{ &multiplier, cfo.code.items.ptr });

    defer cfo.deinit();

    const entry_nested = cfo.get_target();
    try cfo.mov(.rax, .rdi);
    try cfo.sh_ri(.rax, .hl, 10);
    try cfo.arit(.add, .rax, .rsi);
    try cfo.ret();

    const entry = cfo.get_target();
    try cfo.movri(.rdi, 1000);
    try cfo.movri(.rsi, 237);
    try cfo.call_rel(entry_nested);
    try cfo.sh_ri(.rax, .hl, 1);
    try cfo.ret();

    try cfo.finalize();
    const fun = cfo.get_ptr(entry, *const fn () callconv(.C) u64);
    try expectEqual(@as(u64, 2048474), fun());
    // why not
    const fun2 = cfo.get_ptr(entry_nested, *const fn (usize, usize) callconv(.C) u64);
    try expectEqual(@as(u64, 10243), fun2(10, 3));
}
