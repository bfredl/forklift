const std = @import("std");
const dbg = std.debug.print;

const util = @import("./util.zig");
const wasm_shelf = @import("wasm_shelf");
const StackValue = wasm_shelf.StackValue;
const Instance = wasm_shelf.Instance;
const clap = @import("clap");

pub var options: @import("wasm_shelf").forklift.DebugOptions = .{};

const params = clap.parseParamsComptime(
    \\-h, --help             Display this help and exit.
    \\-f, --func <str>       call function
    \\-a, --arg <str>        int argument (TODO: restructure this)
    \\-i, --inspect          inspect imports and exports
    \\-s, --stats <str>      Dump some stats on exit
    \\-d, --disasm <str>     Disassemble block
    \\-c, --compile <str>... Compile block using ThunderLightning
    \\--dbg_func <str>       Heavy machine func debug tool
    \\-m, --heavy            Compile entire module using HeavyMachineTool
    \\--stdin <str>          override wasi stdin
    \\<str>
    \\
);

fn blkspec(spec: []const u8) !struct { u32, u32 } {
    const brk = std.mem.indexOfScalar(u8, spec, ':');
    const func = try std.fmt.parseInt(u32, if (brk) |b| spec[0..b] else spec, 10);
    const blk = if (brk) |b| try std.fmt.parseInt(u32, spec[b + 1 ..], 10) else 0;
    return .{ func, blk };
}

pub fn main(init: std.process.Init) !u8 {
    const allocator = init.gpa;

    var diag = clap.Diagnostic{};
    var p = clap.parse(clap.Help, &params, clap.parsers.default, init.minimal.args, .{
        .diagnostic = &diag,
        .allocator = allocator,
    }) catch |err| {
        // Report useful error and exit.
        try diag.reportToFile(init.io, .stderr(), err);
        return err;
    };
    defer p.deinit();

    const filearg = p.positionals[0] orelse @panic("usage");
    const buf = try util.readall(init.io, allocator, filearg);
    defer allocator.free(buf);

    var mod = try wasm_shelf.Module.parse(buf, allocator);
    defer mod.deinit();
    defer if (p.args.stats) |s| mod.dump_counts(s) catch unreachable;

    if (p.args.inspect > 0) {
        try mod.dbg_imports();
        try mod.dbg_exports();
        return 0;
    }

    if (p.args.disasm) |str| {
        const func, const blk = try blkspec(str);
        try mod.dbg_disasm(func, blk);
        return 0;
    }

    for (p.args.compile) |str| {
        const func, const blk = try blkspec(str);
        try mod.dbg_compile(func, blk);
    }

    var interpreter: wasm_shelf.Interpreter = .init(allocator);
    defer interpreter.deinit();
    var tool: wasm_shelf.HeavyMachineTool = try .init(allocator);
    tool.trap_verbose = true;

    const engine: wasm_shelf.Engine = if (p.args.heavy > 0) .{ .heavy = &tool } else .{ .interpreter = &interpreter };

    if (p.args.func) |func| {
        const callname = func;

        var in: Instance = try .init(&mod, null);
        defer in.deinit();

        const sym = try mod.lookup_export(callname) orelse {
            dbg("not found :pensive:\n", .{});
            return 1;
        };

        dbg("SYM: {}\n", .{sym});
        if (sym.kind != .func) {
            dbg("not a function :(\n", .{});
            return 1;
        }

        const num = try std.fmt.parseInt(i32, p.args.arg orelse @panic("gib --arg"), 10);
        var res: [1]StackValue = undefined;
        const n_res = try in.execute_either(engine, sym.idx, &.{.{ .i32 = num }}, &res, true, null);
        if (n_res != 1) dbg("TODO: n_res\n", .{});
        dbg("{s}({}) == {}\n", .{ callname, num, res[0].i32 });
    } else {
        if (p.args.stdin) |path| {
            const fil = try std.Io.Dir.cwd().openFile(init.io, path, .{});
            _ = std.posix.system.dup2(fil.handle, 0);
        }
        const status = try wasi_run(init.io, engine, &mod, allocator, p.args.dbg_func);
        return @intCast(@min(status, 255));
    }
    return 0;
}

const WASIState = struct {
    io: std.Io,
    start_time: std.Io.Timestamp,
    exit_status: ?u32 = null,
};

// NB: engine is a tagged pointer
fn wasi_run(io: std.Io, engine: wasm_shelf.Engine, mod: *wasm_shelf.Module, allocator: std.mem.Allocator, filter: ?[]const u8) !u32 {
    var imports: wasm_shelf.ImportTable = .init(allocator);
    defer imports.deinit();

    var state: WASIState = .{ .io = io, .start_time = klocka.now(io) };

    try imports.add_func("proc_exit", .{ .cb = &wasi_proc_exit, .cb_direct = &wasi_proc_exit_direct, .n_args = 1, .n_res = 0, .data = @ptrCast(&state) });
    try imports.add_func("fd_read", .{ .cb = &wasi_fd_read, .n_args = 4, .n_res = 1, .data = @ptrCast(&state) });
    try imports.add_func("fd_write", .{ .cb = &wasi_fd_write, .n_args = 4, .n_res = 1, .data = @ptrCast(&state) });
    try imports.add_func("clock_time_get", .{ .cb = &wasi_clock_time_get, .n_args = 3, .n_res = 1, .data = @ptrCast(&state) });

    var in = try wasm_shelf.Instance.init(mod, &imports);
    defer in.deinit();

    try in.maybe_compile(engine, filter);

    const sym = try mod.lookup_export("_start") orelse @panic("_start not found");

    if (sym.kind != .func) @panic("_start not a function :(");

    if (engine == .heavy) std.debug.print("DET HÄR BLIR FINEMANG:\n", .{});
    var err_ret: ?[]const u8 = null;
    _ = in.execute_either(engine, sym.idx, &.{}, &.{}, true, &err_ret) catch |err| {
        switch (err) {
            error.WASMTrap => {
                if (state.exit_status) |status| {
                    // TRAP was sent by wasi_proc_exit
                    return status;
                } else {
                    std.debug.print("UNHANDLED TRAP :P\n", .{});
                }
            },
            error.NotImplemented => {
                std.debug.print("NYI: {s}\n", .{err_ret orelse "???????????????"});
            },
            else => {},
        }
        return err;
    };

    return 0;
}

fn wasi_proc_exit_direct() callconv(.c) void {
    @panic("hello there");
}

fn wasi_proc_exit(args_ret: []StackValue, in: *Instance, data: *anyopaque) !void {
    const state: *WASIState = @ptrCast(@alignCast(data));
    _ = in;
    const arg = args_ret[0].u32();
    dbg("wasi exit: {}\n", .{arg});

    std.posix.system.exit(0); // for benchmarking which expect 0 ret..
    state.exit_status = arg;
    return error.WASMTrap;
}

fn wasi_fd_read(args_ret: []StackValue, in: *Instance, data: *anyopaque) !void {
    _ = data;
    const fd = args_ret[0].i32;
    const iovs: u32 = @intCast(args_ret[1].i32);
    const iovs_len: u32 = @intCast(args_ret[2].i32);
    const res_size_ptr: u32 = @intCast(args_ret[3].i32);

    if (fd != 0) return error.WASMTrap;

    const raw_iovec = try in.mem_get_bytes(iovs, iovs_len * 8);
    //const iovec = try in.mem_get_as([2]u32, iovs, iovs_len);
    var cumulative: u32 = 0;
    for (0..@as(usize, @intCast(iovs_len))) |i| {
        const pos = 8 * i;
        const iptr = std.mem.readInt(u32, raw_iovec[pos..][0..4], .little);
        const ilen = std.mem.readInt(u32, raw_iovec[pos + 4 ..][0..4], .little);

        // TODO: actually use ioKVÄCK of the underlying platform
        const ain = try in.mem_get_bytes(iptr, ilen);
        const rlen = std.posix.read(fd, ain) catch return error.WASMTrap;
        cumulative += @intCast(rlen);
    }

    const raw_ret = try in.mem_get_bytes(res_size_ptr, 4);
    std.mem.writeInt(u32, raw_ret[0..4], cumulative, .little);
    args_ret[0] = .{ .i32 = 0 }; // SUCCESS
}

fn wasi_fd_write(args_ret: []StackValue, in: *Instance, data: *anyopaque) !void {
    const state: *WASIState = @ptrCast(@alignCast(data));
    const fd = args_ret[0].i32;
    const iovs: u32 = @intCast(args_ret[1].i32);
    const iovs_len: u32 = @intCast(args_ret[2].i32);
    const res_size_ptr: u32 = @intCast(args_ret[3].i32);

    if (fd != 2 and fd != 1) return error.WASMTrap;

    dbg("print to {}:\n", .{fd});

    const raw_iovec = try in.mem_get_bytes(iovs, iovs_len * 8);
    //const iovec = try in.mem_get_as([2]u32, iovs, iovs_len);
    var cumulative: u32 = 0;
    for (0..@as(usize, @intCast(iovs_len))) |i| {
        const pos = 8 * i;
        const iptr = std.mem.readInt(u32, raw_iovec[pos..][0..4], .little);
        const ilen = std.mem.readInt(u32, raw_iovec[pos + 4 ..][0..4], .little);

        // TODO: actually use ioKVÄCK of the underlying platform
        const aout = try in.mem_get_bytes(iptr, ilen);
        if (fd == 1) {
            _ = std.Io.File.stdout().writeStreamingAll(state.io, aout) catch return error.WASMTrap;
        } else {
            std.debug.print("{s}", .{aout});
        }
        cumulative += ilen;
    }

    const raw_ret = try in.mem_get_bytes(res_size_ptr, 4);
    std.mem.writeInt(u32, raw_ret[0..4], cumulative, .little);
    args_ret[0] = .{ .i32 = 0 }; // SUCCESS
}

fn wasi_clock_time_get(args_ret: []StackValue, in: *Instance, data: *anyopaque) !void {
    const state: *WASIState = @ptrCast(@alignCast(data));
    const id = args_ret[0].i32;
    const res_timestamp = try in.mem_get_bytes(args_ret[2].u32(), 8);

    if (id == 1) {
        const time = klocka.now(state.io); // PRESENT DAY, PRESENT TIME
        const interval = state.start_time.durationTo(time);
        std.mem.writeInt(u64, res_timestamp[0..8], @intCast(interval.nanoseconds), .little);
    } else {
        dbg("hey guys check this out: {}\n", .{id});
        return error.WASMTrap;
    }
}

const klocka = std.Io.Clock.real;
