const forklift = @import("forklift");
const FLIR = forklift.FLIR;
const print = std.debug.print;
const Parser = forklift.Parser;
const codegen_bpf = forklift.codegen_bpf;
const BPF = std.os.linux.BPF;
const bpf = forklift.bpf;
const fd_t = std.os.linux.fd_t;
const mem = std.mem;
const expectEqual = std.testing.expectEqual;
const CFOModule = forklift.CFOModule;
const asBytes = mem.asBytes;

fn expect(comptime T: type, x: T, y: T) !void {
    return std.testing.expectEqual(x, y);
}

const std = @import("std");
const test_allocator = std.testing.allocator;

test "show code" {
    var mod = try CFOModule.init(test_allocator);
    defer mod.deinit_mem();
    const code =
        \\ bpf_func returner() {
        \\   return 5;
        \\ }
    ;
    try Parser.parse(&mod, test_allocator, code, false, false);

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

fn parse_load(ir_text: []const u8) !CFOModule {
    var mod = try parse_multi(false, ir_text);
    try mod.load();
    return mod;
}

pub fn parse_multi(dbg: bool, ir: []const u8) !CFOModule {
    var mod = try CFOModule.init(test_allocator);
    errdefer mod.deinit_mem();
    try Parser.parse(&mod, test_allocator, ir, dbg, false);
    try mod.load();
    return mod;
}

test "run simple" {
    var mod = try parse_load(
        \\ bpf_func returner() {
        \\   return 5;
        \\ }
    );
    defer mod.deinit_mem();
    const ret = try mod.bpf_test_run("returner", null);
    try expectEqual(ret, 5);
}

test "load byte" {
    // std.debug.print("\nkod: \n", .{});
    var mod = try parse_load(
        \\ bpf_func returner(ctx) {
        \\   return @ctx[0];
        \\ }
    );
    defer mod.deinit_mem();
    // yes its LE specific. sorry if you use a weirdo processor
    var data: u64 = 42 + 2 * 256;
    const ret = try mod.bpf_test_run("returner", mem.asBytes(&data));
    try expect(u64, 42, ret);
}

test "load dword" {
    // std.debug.print("\nkod: \n", .{});
    var mod = try parse_load(
        \\ bpf_func returner(ctx) {
        \\   return @4u ctx[0];
        \\ }
    );
    defer mod.deinit_mem();
    var data: u64 = 0x222233331234abcd;
    const ret = try mod.bpf_test_run("returner", mem.asBytes(&data));
    try expect(u64, 0x1234abcd, ret);
}

test "load dword with offset" {
    // std.debug.print("\nkod: \n", .{});
    var mod = try parse_load(
        \\ bpf_func returner(ctx) {
        \\ return @4u ctx[4];
        \\ }
    );
    defer mod.deinit_mem();
    var data: u64 = 0x222233331234abcd;
    const ret = try mod.bpf_test_run("returner", mem.asBytes(&data));
    try expect(u64, 0x22223333, ret);
}

// useful template for testing a raw BFPCode program
test "raw BPFCode test template" {
    const I = BPF.Insn;
    var code = forklift.BPFCode.init(test_allocator);
    try code.append(I.mov(.r0, .r1));
    try code.append(I.exit());
    defer code.deinit();
    const prog_fd = try bpf.prog_load_test(.syscall, code.items, "MIT", BPF.F_SLEEPABLE);
    const ret = try bpf.prog_test_run(prog_fd, null);
    try expect(u64, 0, ret);
}

test "just map" {
    var mod = try parse_multi(false,
        \\ bpf_map my_map .array 4 4 3
    );
    defer mod.deinit_mem();
    try mod.load();
    const fd = mod.bpf_get_fd("my_map") orelse unreachable;

    const key: u32 = 1;
    const put_val: u32 = 2;
    try BPF.map_update_elem(fd, asBytes(&key), asBytes(&put_val), 0);

    var get_val: u32 = 0xFFFFFFF;
    try BPF.map_lookup_elem(fd, asBytes(&key), asBytes(&get_val));

    try expect(u32, get_val, put_val);
}

test "map value" {
    var mod = try parse_multi(false,
        \\ bpf_map global_var .array 4 8 1
        \\ bpf_func main() {
        \\   let v = %map_value global_var;
        \\   v[0] 4u= 17;
        \\   return 0;
        \\ }
    );
    defer mod.deinit_mem();
    try mod.load();

    const ret = try mod.bpf_test_run("main", null);
    try expect(u64, ret, 0);

    const fd = mod.bpf_get_fd("global_var") orelse unreachable;

    const key: u32 = 0;
    var get_val: u64 = 0xFFFFFFF;
    try BPF.map_lookup_elem(fd, asBytes(&key), asBytes(&get_val));
    try expect(u64, get_val, 17);
}

test "map + call" {
    var mod = try parse_multi(false,
        \\ bpf_map myhash .hash 4 8 16834
        \\ bpf_func main() {
        \\   let hash = %map myhash;
        \\   let key = %alloc 4;
        \\   key[0] 4u= 86;
        \\   let value = $bpf map_lookup_elem(hash, key);
        \\   value[0] 4u= 231;
        \\   return 0;
        \\ }
    );
    defer mod.deinit_mem();
    try mod.load();

    const ret = try mod.bpf_test_run("main", null);
    try expect(u64, ret, 0);

    const fd = mod.bpf_get_fd("global_var") orelse unreachable;

    const key: u32 = 0;
    var get_val: u64 = 0xFFFFFFF;
    try BPF.map_lookup_elem(fd, asBytes(&key), asBytes(&get_val));
    try expect(u64, get_val, 17);
}
