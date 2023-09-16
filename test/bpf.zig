const forklift = @import("forklift");
const FLIR = forklift.FLIR;
const print = std.debug.print;
const Parser = forklift.Parser;
const codegen_bpf = forklift.codegen_bpf;
const bpf_rt = forklift.bpf_rt;
const BPF = std.os.linux.BPF;
const fd_t = std.os.linux.fd_t;
const mem = std.mem;
const expectEqual = std.testing.expectEqual;
const BPFModule = bpf_rt.BPFModule;
const asBytes = mem.asBytes;

fn expect(comptime T: type, x: T, y: T) !void {
    return std.testing.expectEqual(x, y);
}

const std = @import("std");
const test_allocator = std.testing.allocator;

test "show code" {
    var mod = BPFModule.init(test_allocator);
    defer mod.deinit_mem();
    var parser = try Parser.init(
        \\ func returner
        \\ ret 5
        \\ end
    , test_allocator);
    defer parser.deinit();
    parser.bpf_module = &mod;
    _ = parser.parse_bpf(false) catch |e| {
        print("fail at {}\n", .{parser.pos});
        return e;
    };

    var data = std.ArrayList(u8).init(std.testing.allocator);
    defer data.deinit();
    try forklift.dump_bpf(data.writer(), mod.bpf_code.items);
    // std.debug.print("\n{s}\n", .{data.items});
    try std.testing.expectEqualSlices(u8,
        \\  0: b7 0 0  +0   +5 MOV64 r0, 5
        \\  1: 95 0 0  +0   +0 EXIT
        \\
    , data.items);
}

fn test_one_func(ir_text: []const u8) !fd_t {
    var mod = try parse_multi(false, ir_text);
    defer mod.deinit_mem();

    if (mod.objs.count() != 1) {
        return error.ExpectedOneFunction;
    }

    return switch (mod.objs.values()[0]) {
        .prog => |x| x.fd,
        else => error.ExpectedOneFunction,
    };
}

pub fn parse_multi(dbg: bool, ir: []const u8) !BPFModule {
    var parser = try Parser.init(ir, test_allocator);
    var mod = BPFModule.init(test_allocator);
    defer parser.deinit();
    errdefer mod.deinit_mem();
    parser.bpf_module = &mod;
    parser.parse_bpf(dbg) catch |e| {
        print("fail at {}\n", .{parser.pos});
        return e;
    };
    try mod.load();
    return mod;
}

test "run simple" {
    const prog_fd = try test_one_func(
        \\ func returner
        \\ ret 5
        \\ end
    );
    const ret = try bpf_rt.prog_test_run(prog_fd, null);
    try expectEqual(ret, 5);
}

test "load byte" {
    // std.debug.print("\nkod: \n", .{});
    const prog_fd = try test_one_func(
        \\ func returner
        \\ %ctx = arg
        \\ %data = load byte [%ctx 0]
        \\ ret %data
        \\ end
    );
    // yes its LE specific. sorry if you use a weirdo processor
    var data: u64 = 42 + 2 * 256;
    const ret = try bpf_rt.prog_test_run(prog_fd, mem.asBytes(&data));
    try expect(u64, 42, ret);
}

test "load dword" {
    // std.debug.print("\nkod: \n", .{});
    const prog_fd = try test_one_func(
        \\ func returner
        \\ %ctx = arg
        \\ %data = load dword [%ctx 0]
        \\ ret %data
        \\ end
    );
    var data: u64 = 0x222233331234abcd;
    const ret = try bpf_rt.prog_test_run(prog_fd, mem.asBytes(&data));
    try expect(u64, 0x1234abcd, ret);
}

test "load dword with offset" {
    // std.debug.print("\nkod: \n", .{});
    const prog_fd = try test_one_func(
        \\ func returner
        \\ %ctx = arg
        \\ %data = load dword [%ctx 4]
        \\ ret %data
        \\ end
    );
    var data: u64 = 0x222233331234abcd;
    const ret = try bpf_rt.prog_test_run(prog_fd, mem.asBytes(&data));
    try expect(u64, 0x22223333, ret);
}

// useful template for testing a raw BFPCode program
test "raw BPFCode test template" {
    const I = BPF.Insn;
    var code = forklift.BPFCode.init(test_allocator);
    try code.append(I.mov(.r0, .r1));
    try code.append(I.exit());
    defer code.deinit();
    const prog_fd = try bpf_rt.prog_load_test(.syscall, code.items, "MIT", BPF.F_SLEEPABLE);
    const ret = try bpf_rt.prog_test_run(prog_fd, null);
    try expect(u64, 0, ret);
}

test "just map" {
    var mod = try parse_multi(false,
        \\ map my_map .array 4 4 3
    );
    defer mod.deinit_mem();
    try mod.load();
    const fd = mod.get_fd("my_map") orelse unreachable;

    const key: u32 = 1;
    const put_val: u32 = 2;
    try BPF.map_update_elem(fd, asBytes(&key), asBytes(&put_val), 0);

    var get_val: u32 = 0xFFFFFFF;
    try BPF.map_lookup_elem(fd, asBytes(&key), asBytes(&get_val));

    try expect(u32, get_val, put_val);
}

test "map value" {
    var mod = try parse_multi(false,
        \\ map global_var .array 4 8 1
        \\ func setter
        \\   %var = map_value global_var
        \\   store quadword [%var 0] 17
        \\   ret 0
        \\ end
    );
    defer mod.deinit_mem();
    try mod.load();

    if (false) {
        for (mod.objs.keys(), mod.objs.values()) |k, v| {
            std.debug.print("dod {s} {}\n", .{ k, v });
        }
    }

    const prog_fd = mod.get_fd("setter") orelse unreachable;
    const ret = try bpf_rt.prog_test_run(prog_fd, null);
    try expect(u64, ret, 0);

    const fd = mod.get_fd("global_var") orelse unreachable;

    const key: u32 = 0;
    var get_val: u64 = 0xFFFFFFF;
    try BPF.map_lookup_elem(fd, asBytes(&key), asBytes(&get_val));
    try expect(u64, get_val, 17);
}
