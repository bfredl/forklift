const forklift = @import("forklift");
const std = @import("std");
const X86Asm = forklift.X86Asm;

pub fn main(init: std.process.Init) !void {
    var code = try forklift.CodeBuffer.init(init.gpa);
    var cfo = X86Asm{ .code = &code, .long_jump_mode = true };
    try cfo.pushi(-1);
    try cfo.ret();

    defer cfo.dbg_nasm(init.gpa) catch unreachable;
}
