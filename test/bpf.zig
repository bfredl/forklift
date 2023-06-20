const forklift = @import("forklift");
const FLIR = forklift.FLIR;
const print = std.debug.print;
const IRParse = forklift.IRParse;
const codegen_bpf = forklift.codegen_bpf;
const bpf_rt = forklift.bpf_rt;
const BPF = std.os.linux.BPF;
const fd_t = std.os.linux.fd_t;
const mem = std.mem;

const std = @import("std");
const test_allocator = std.testing.allocator;

pub fn parse(self: *FLIR, ir: []const u8) !void {
    var parser = IRParse.init(ir, test_allocator);
    _ = parser.parse_func(self) catch |e| {
        print("fail at {}\n", .{parser.pos});
        return e;
    };
}

test "show code" {
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

fn test_ir(ir_text: []const u8) !fd_t {
    var ir = try FLIR.init(8, test_allocator);
    defer ir.deinit();

    try parse(&ir, ir_text);
    var code = forklift.BPFCode.init(test_allocator);
    defer code.deinit();
    try ir.test_analysis(FLIR.BPF_ABI, true);
    _ = try codegen_bpf(&ir, &code);

    std.debug.print("\nprogram: \n", .{});
    try forklift.dump_bpf(std.io.getStdErr().writer(), code.items);
    return bpf_rt.prog_load_test(.syscall, code.items, "MIT", BPF.F_SLEEPABLE);
}

test "run simple" {
    const prog_fd = try test_ir(
        \\ func returner
        \\ ret 5
        \\ end
    );
    const ret = try bpf_rt.prog_test_run(prog_fd, null);
    try std.testing.expectEqual(ret, 5);
}

test "ctx memory" {
    std.debug.print("\nkod: \n", .{});
    const prog_fd = try test_ir(
        \\ func returner
        \\ %ctx = arg
        \\ %data = load byte [%ctx 0]
        \\ ret %data
        \\ end
    );
    const data: u32 = 4;
    const ret = try bpf_rt.prog_test_run(prog_fd, mem.asBytes(&data));
    try std.testing.expectEqual(ret, 5);
}
