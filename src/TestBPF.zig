const FLIR = @import("./FLIR.zig");
const print = std.debug.print;
const IRParse = @import("./IRParse.zig");
const codegen_bpf = @import("./codegen_bpf.zig");
const bpf = @import("./bpf.zig");

const std = @import("std");
const test_allocator = std.testing.allocator;

pub fn parse(self: *FLIR, ir: []const u8) !void {
    var parser = IRParse.init(ir, test_allocator);
    _ = parser.parse_func(self) catch |e| {
        print("fail at {}\n", .{parser.pos});
        return e;
    };
}

test "rong" {
    var ir = try FLIR.init(8, test_allocator);
    defer ir.deinit();
    try parse(&ir,
        \\ func returner
        \\ ret 5
        \\ end
    );
    var code = codegen_bpf.Code.init(test_allocator);
    defer code.deinit();
    try ir.test_analysis(FLIR.BPF_ABI, true);
    _ = try codegen_bpf.codegen(&ir, &code);

    var data = std.ArrayList(u8).init(std.testing.allocator);
    defer data.deinit();
    try bpf.dump(data.writer(), code.items);
    // std.debug.print("\n{s}\n", .{data.items});
    try std.testing.expectEqualSlices(u8,
        \\  0: b7 0 0  +0   +5 MOV64 r0, 5
        \\  1: 95 0 0  +0   +0 EXIT
        \\
    , data.items);
}
