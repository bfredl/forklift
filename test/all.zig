const std = @import("std");
test {
    std.testing.refAllDecls(@import("./cfo.zig"));
    std.testing.refAllDecls(@import("./flir.zig"));
    std.testing.refAllDecls(@import("./aoc.zig"));
    std.testing.refAllDecls(@import("./bpf.zig"));
}
