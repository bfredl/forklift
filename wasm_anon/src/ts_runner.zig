const std = @import("std");
const dbg = std.debug.print;

const util = @import("./util.zig");
const wasm_shelf = @import("wasm_shelf");
const StackValue = wasm_shelf.StackValue;
const Instance = wasm_shelf.Instance;

const c = @cImport({
    @cInclude("./ts_defs.h");
});

fn trap(args_ret: []StackValue, in: *Instance, data: *anyopaque) !void {
    _ = args_ret;
    _ = data;
    _ = in;
    return error.WASMTrap;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const argv = std.os.argv;
    if (argv.len < 3) return dbg("ts_runner mod.wasm language\n", .{});
    const filearg = std.mem.span(argv[1]);
    const langarg = std.mem.span(argv[2]);
    const buf = try util.readall(allocator, filearg);
    defer allocator.free(buf);

    var mod = try wasm_shelf.Module.parse(buf, allocator);
    defer mod.deinit();

    try mod.dbg_imports();
    try mod.dbg_exports();

    const dylink_info = try mod.get_dylink_info();

    var imports: wasm_shelf.ImportTable = .init(allocator);
    defer imports.deinit();

    if (dylink_info) |info| {
        dbg("DYLING: {}\n", .{info});
        const pages = (info.memory_size + wasm_shelf.page_size - 1) / wasm_shelf.page_size;
        imports.memory_size = pages;
        imports.func_table_size = info.table_size;
    }

    var stack_pointer: StackValue = .{ .i32 = 0 };
    var memory_base: StackValue = .{ .i32 = 0 };
    var table_base: StackValue = .{ .i32 = 0 };
    try imports.add_global("__stack_pointer", &stack_pointer, .i32);
    try imports.add_global("__memory_base", &memory_base, .i32);
    try imports.add_global("__table_base", &table_base, .i32);
    try imports.add_func("calloc", .{ .cb = &trap, .data = undefined, .n_args = 2, .n_res = 1 });
    try imports.add_func("towupper", .{ .cb = &trap, .data = undefined, .n_args = 1, .n_res = 1 });
    try imports.add_func("iswspace", .{ .cb = &trap, .data = undefined, .n_args = 1, .n_res = 1 });
    try imports.add_func("strlen", .{ .cb = &trap, .data = undefined, .n_args = 1, .n_res = 1 });
    try imports.add_func("memcmp", .{ .cb = &trap, .data = undefined, .n_args = 3, .n_res = 1 });
    try imports.add_func("free", .{ .cb = &trap, .data = undefined, .n_args = 1, .n_res = 0 });
    try imports.add_func("realloc", .{ .cb = &trap, .data = undefined, .n_args = 2, .n_res = 1 });
    try imports.add_func("malloc", .{ .cb = &trap, .data = undefined, .n_args = 1, .n_res = 1 });
    try imports.add_func("__assert_fail", .{ .cb = &trap, .data = undefined, .n_args = 4, .n_res = 0 });
    try imports.add_func("strncpy", .{ .cb = &trap, .data = undefined, .n_args = 3, .n_res = 1 });
    try imports.add_func("iswalnum", .{ .cb = &trap, .data = undefined, .n_args = 1, .n_res = 1 });

    var in = try Instance.init(&mod, &imports);
    defer in.deinit();

    var interpreter: wasm_shelf.Interpreter = .init(allocator);
    defer interpreter.deinit();

    // TODO: check how the order is really defined, this is just guesswork on the major scale
    const initializers = &[_][]const u8{ "__wasm_call_ctors", "__wasm_apply_data_relocs", "_initialize" };

    for (initializers) |init| {
        if (try mod.lookup_export(init)) |sym| {
            dbg("INIT: {s}\n", .{init});
            if (sym.kind != .func) @panic("nej");
            _ = try interpreter.execute(&in, sym.idx, &.{}, &.{}, true);
        }
    }

    var lang_func_name: std.ArrayList(u8) = .empty;
    try std.fmt.format(lang_func_name.writer(allocator), "tree_sitter_{s}", .{langarg});
    const sym = try mod.lookup_export(lang_func_name.items) orelse @panic("no such lang");
    if (sym.kind != .func) @panic("nej");
    var res: [1]StackValue = undefined;
    _ = try interpreter.execute(&in, sym.idx, &.{}, &res, true);

    dbg("HERE IS THE RESULT: {}\n", .{res[0].i32});

    var langinfo: c.LanguageInWasmMemory = undefined;
    const mem = try in.mem_get_bytes(res[0].u32(), @sizeOf(c.LanguageInWasmMemory));
    @memcpy(std.mem.asBytes(&langinfo), mem);
    dbg("info: {}\n", .{langinfo});
}
