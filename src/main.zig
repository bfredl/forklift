const std = @import("std");
const print = std.debug.print;
const CFO = @import("./CFO.zig");

pub fn main() !void {
    const allocator = std.testing.allocator;
    print("Yes, I am your CFO (certified forklift operator)\n", .{});

    var cfo = try CFO.init(allocator);
    // try cfo.mov(CFO.IPReg.rax, CFO.IPReg.rdi);
    // try cfo.mov(CFO.IPReg.rax, CFO.IPReg.rsi);
    try cfo.movrm(CFO.IPReg.rax, CFO.IPReg.rdi, 0);
    try cfo.movmr(CFO.IPReg.rdi, 0, CFO.IPReg.rsi);
    try cfo.ret();
    var fptr = try cfo.test_finalize();
    var someint: u64 = 33;
    var val = fptr(@ptrToInt(&someint), 10);
    print("did: {}\n", .{val});
    print("done it: {}\n", .{someint});
}
