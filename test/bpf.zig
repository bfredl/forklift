const forklift = @import("forklift");
const FLIR = forklift.FLIR;
const print = std.debug.print;
const IRParse = forklift.IRParse;
const codegen_bpf = forklift.codegen_bpf;
const bpf_rt = forklift.bpf_rt;

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
    var code = forklift.BPFCode.init(test_allocator);
    defer code.deinit();
    try ir.test_analysis(FLIR.BPF_ABI, true);
    _ = try codegen_bpf(&ir, &code);

    var data = std.ArrayList(u8).init(std.testing.allocator);
    defer data.deinit();
    try forklift.dump_bpf(data.writer(), code.items);
    // std.debug.print("\n{s}\n", .{data.items});
    try std.testing.expectEqualSlices(u8,
        \\  0: b7 0 0  +0   +5 MOV64 r0, 5
        \\  1: 95 0 0  +0   +0 EXIT
        \\
    , data.items);
}

test "run simple" {
    var ir = try FLIR.init(8, test_allocator);
    defer ir.deinit();
    try parse(&ir,
        \\ func returner
        \\ ret 5
        \\ end
    );
    var code = forklift.BPFCode.init(test_allocator);
    defer code.deinit();
    try ir.test_analysis(FLIR.BPF_ABI, true);
    _ = try codegen_bpf(&ir, &code);
    try forklift.dump_bpf(std.io.getStdErr().writer(), code.items);

    const BPF_F_SLEEPABLE = (1 << 4);

    const prog_fd = try bpf_rt.prog_load_test(.syscall, code.items, "MIT", BPF_F_SLEEPABLE);
    print("the prog: {}\n", .{prog_fd});
    const ret = try bpf_rt.prog_test_run(prog_fd, null);
    try std.testing.expectEqual(ret, 5);
}
