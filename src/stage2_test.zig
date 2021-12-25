const CFO = @import("./CFO.zig");
const std = @import("std");
const expectEqual = std.testing.expectEqual;

test "return first argument" {
    var cfo = CFO.init_stage2();
    // defer cfo.deinit();

    try cfo.mov(.rax, .rdi);
    try cfo.ret();
    // try expectEqual(@as(usize, 4), try cfo.test_call2(4, 10));
}
