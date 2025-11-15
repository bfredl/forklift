const forklift = @import("forklift");
const std = @import("std");
const X86Asm = forklift.X86Asm;

pub fn main() !void {
    var allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const gpa = allocator.allocator();

    var code = try forklift.CodeBuffer.init(gpa);
    var cfo = X86Asm{ .code = &code, .long_jump_mode = true };
    try cfo.vmovdq_iv(true, .rdx, 7);
    try cfo.vmovdq_iv(true, .rdx, 13);
    try cfo.vmovdq_iv(true, .r12, 13);
    try cfo.vmovdq_iv(false, .r12, 13);
    try cfo.ret();

    defer cfo.dbg_nasm(gpa) catch unreachable;
}
