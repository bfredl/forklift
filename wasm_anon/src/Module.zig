const std = @import("std");
const testing = std.testing;

//pub const dbg = std.debug.print;
pub const dbg = nodbg;
pub fn nodbg(str: anytype, vals: anytype) void {
    _ = str;
    _ = vals;
}

const severe = std.debug.print;

const defs = @import("./defs.zig");
const Module = @This();

const ImportTable = @import("./ImportTable.zig");

pub const Limits = struct { min: u32, max: ?u32 };

const Reader = @import("./Reader.zig");
allocator: std.mem.Allocator,
raw: []const u8,
n_funcs_import: u32 = 0,
funcs_imported_types: []u32 = &.{},
// TODO: {.ptr = undefined, .size = 0} would be a useful idiom..
funcs_internal: []Function = &.{},
types: []u32 = undefined,

export_off: u32 = 0,
data_off: u32 = 0,

// TODO: maybe not even parse "imports" until justin time?
n_globals_import: u32 = 0,
n_globals_internal: u32 = 0,
globals_off: u32 = 0,

n_imports: u32 = 0,
imports_off: u32 = 0,

table_off: u32 = 0,
element_off: u32 = 0,

mem_limits: Limits = .{ .min = 0, .max = null },

custom_dylink0_off: u32 = 0,
custom_dylink0_end: u32 = 0,

start_func: u32 = defs.funcref_nil,

istat: [256]u32 = @splat(0),

// TODO: tricky. traces might wanna depend on per-instance state (like some
// imported functions being JIT-able, etc)
traces: std.ArrayListUnmanaged(LightningTrace) = .{},

const Function = @import("./Function.zig");
const Interpreter = @import("./Interpreter.zig");
const LightningTrace = @import("./ThunderLightning.zig").LightningTrace;

pub fn reader_at(self: Module, off: u32) Reader {
    return .{ .buffer = self.raw, .pos = off };
}

pub fn parse(module: []const u8, allocator: std.mem.Allocator) !Module {
    if (module.len < 8) return error.InvalidFormat;
    if (!std.mem.eql(u8, module[0..8], &.{ 0, 'a', 's', 'm', 1, 0, 0, 0 })) return error.InvalidFormat;

    var self: Module = .{ .raw = module, .allocator = allocator };

    var r0 = self.reader_at(8);
    const r = &r0; // DO YOU LIKE MY SILLY HAT?

    while (true) {
        const id = r.readByte() catch |err| switch (err) {
            error.EndOfStream => break,
            else => |e| return e,
        };
        const kind: defs.SectionKind = @enumFromInt(id);

        const pos = r.pos;
        const len = try r.readu();
        dbg("SECTION: {s} ({}) at {} with len {}\n", .{ @tagName(kind), id, pos, len });
        const end_pos = r.pos + len;
        switch (kind) {
            .type => try self.type_section(r),
            .function => try self.function_section(r),
            .memory => try self.memory_section(r),
            .global => try self.global_section(r),
            .import => try self.import_section(r),
            .export_ => self.export_off = r.pos,

            .code => try self.code_section(r),
            .table => self.table_off = r.pos,
            .element => self.element_off = r.pos,
            .data => self.data_off = r.pos,
            .custom => try self.custom_section(r, len),
            .start => return error.NotImplemented,
            else => {
                // severe("NO {s}!\n", .{@tagName(kind)});
            },
        }

        // TODO: this should be strict, but we are just fucking around and finding out for now
        if (r.pos > end_pos) {
            return error.InvalidFormat;
        }
        r.pos = end_pos;
    }

    return self;
}

pub fn deinit(self: *Module) void {
    // ?[]Thingie is annoying when you could use .{.data = static, .len = 0}
    // this shalt be a common pattern somehow. Perhaps a wrapping allocator unless Allocator wrapper is smart already.
    if (self.funcs_internal.len > 0) {
        self.allocator.free(self.funcs_internal);
    }
}

pub fn type_section(self: *Module, r: *Reader) !void {
    const len = try r.readu();
    dbg("TYPES: {}\n", .{len});
    self.types = try self.allocator.alloc(u32, len);
    for (0..len) |i| {
        self.types[i] = r.pos;
        const tag = try r.readByte();
        if (tag != 0x60) return error.InvalidFormat;
        const n_params = try r.readu();
        dbg("(", .{});
        for (0..n_params) |_| {
            const typ: defs.ValType = @enumFromInt(try r.readByte());
            dbg("{s}, ", .{@tagName(typ)});
        }
        dbg("): (", .{});
        const n_res = try r.readu();
        for (0..n_res) |_| {
            const typ: defs.ValType = @enumFromInt(try r.readByte());
            dbg("{s}, ", .{@tagName(typ)});
        }
        dbg(")\n", .{});
    }
}

pub fn dbg_type(self: *Module, typeidx: u32) !void {
    if (self.types.len <= typeidx) return severe("[OUT OF BOUNDS]", .{});
    var r = self.reader_at(self.types[typeidx]);
    const tag = try r.readByte();
    if (tag != 0x60) return error.InvalidFormat;
    const n_params = try r.readu();
    severe("[", .{});
    for (0..n_params) |i| {
        if (i > 0) severe(", ", .{});
        const typ: defs.ValType = @enumFromInt(try r.readByte());
        severe("{s}", .{@tagName(typ)});
    }
    severe("] => [", .{});
    const n_res = try r.readu();
    for (0..n_res) |i| {
        if (i > 0) severe(", ", .{});
        const typ: defs.ValType = @enumFromInt(try r.readByte());
        severe("{s}", .{@tagName(typ)});
    }
    severe("]\n", .{});
}

// currently we two-cycle the import section to first get the counts
pub fn import_section(self: *Module, r: *Reader) !void {
    self.imports_off = r.pos;
    const len = try r.readu();
    dbg("IMPORTS: {}\n", .{len});
    self.n_imports = len;
    // greedy: assume most imports are functions
    var func_types: std.ArrayList(u32) = try .initCapacity(self.allocator, len);
    errdefer func_types.deinit(self.allocator);
    for (0..len) |_| {
        _ = try r.readName();
        _ = try r.readName();
        const kind: defs.ImportExportKind = @enumFromInt(try r.readByte());
        switch (kind) {
            .func => {
                const idx = try r.readu();
                try func_types.append(self.allocator, idx);
                self.n_funcs_import += 1;
            },
            .table => {
                _ = try r.readByte();
                _ = try r.readLimits();
            },
            .mem => {
                _ = try r.readLimits();
            },
            .global => {
                _ = try r.readByte();
                _ = try r.readByte();
                self.n_globals_import += 1;
            },
        }
    }
    self.funcs_imported_types = try func_types.toOwnedSlice(self.allocator);

    // if (dbg == severe) try self.dbg_imports();
}

pub fn dbg_imports(self: *Module) !void {
    var r = self.reader_at(self.imports_off);
    const len = try r.readu();

    for (0..len) |_| {
        const mod = try r.readName();
        const name = try r.readName();
        severe("{s}:{s} = ", .{ mod, name });
        const kind: defs.ImportExportKind = @enumFromInt(try r.readByte());
        switch (kind) {
            .func => {
                const idx = try r.readu();
                severe("func ", .{});
                try self.dbg_type(idx);
            },
            .table => {
                const typ: defs.ValType = @enumFromInt(try r.readByte());
                const limits = try r.readLimits();
                severe("table {s} w {}:{?}\n", .{ @tagName(typ), limits.min, limits.max });
            },
            .mem => {
                const limits = try r.readLimits();
                severe("mem w {}:{?}\n", .{ limits.min, limits.max });
            },
            .global => {
                const typ: defs.ValType = @enumFromInt(try r.readByte());
                const mut = (try r.readByte()) > 0;
                severe("global {} {}\n", .{ typ, mut });
            },
        }
    }
}

pub fn dbg_exports(self: *Module) !void {
    var r = self.reader_at(self.export_off);
    const len = try r.readu();
    severe("EXPORTS: {}\n", .{len});
    for (0..len) |_| {
        const name = try r.readName();
        const kind: defs.ImportExportKind = @enumFromInt(try r.readByte());
        const idx = try r.readu();
        severe("{s} = {s} {} ", .{ name, @tagName(kind), idx });
        if (kind == .func) {
            // TODO: reexport of imports allowed??
            try self.dbg_type(self.funcs_internal[idx - self.n_funcs_import].typeidx);
        }
        severe("\n", .{});
    }
}

pub fn mark_exports(self: *Module) !void {
    var r = self.reader_at(self.export_off);
    const len = try r.readu();
    for (0..len) |_| {
        const name = try r.readName();
        const kind: defs.ImportExportKind = @enumFromInt(try r.readByte());
        const idx = try r.readu();
        if (kind == .func and idx >= self.n_funcs_import) {
            // TODO: reexport of imports allowed??
            self.funcs_internal[idx - self.n_funcs_import].exported = name;
        }
    }
}

pub const Export = struct { kind: defs.ImportExportKind, idx: u32 };

pub fn lookup_export(self: *Module, name: []const u8) !?Export {
    if (self.export_off == 0) return null;
    var r = self.reader_at(self.export_off);

    const len = try r.readu();
    for (0..len) |_| {
        const item_name = try r.readName();
        const kind: defs.ImportExportKind = @enumFromInt(try r.readByte());
        const idx = try r.readu();
        if (std.mem.eql(u8, name, item_name)) {
            return .{ .kind = kind, .idx = idx };
        }
    }
    return null;
}

fn function_section(self: *Module, r: *Reader) !void {
    const len = try r.readu();
    if (len > 0) self.funcs_internal = try self.allocator.alloc(Function, len);
    dbg("FUNCS: {}\n", .{len});
    for (0..len) |i| {
        const idx = try r.readu();
        self.funcs_internal[i] = .{ .typeidx = idx };
    }
    dbg("...\n", .{});
}

pub fn memory_section(self: *Module, r: *Reader) !void {
    const len = try r.readu();
    dbg("MEMORYS: {}\n", .{len});
    for (0..len) |i| {
        const lim = try r.readLimits();
        dbg("mem {}:{?}\n", .{ lim.min, lim.max });
        if (i == 0) {
            self.mem_limits = lim;
        } else {
            return error.NotImplemented;
        }
    }
}

pub fn init_data(self: *Module, mem: []u8, preglobals: []const defs.StackValue) !void {
    if (self.data_off == 0) return;

    var r = self.reader_at(self.data_off);

    const len = try r.readu();
    dbg("DATAS: {}\n", .{len});
    for (0..len) |_| {
        const typ = try r.readu();
        dbg("TYP: {}\n", .{typ});

        if (typ > 2 or typ == 1) return error.NotImplemented;
        const memidx = if (typ == 0) 0 else try r.readu();
        if (memidx > 0) return error.NotImplemented;

        const offset: usize = @intCast((try Interpreter.eval_constant_expr(&r, .i32, preglobals)).i32);
        const lenna = try r.readu();
        dbg("offsetta: {}, len: {}\n", .{ offset, lenna });

        if (offset + lenna > mem.len) return error.WASMTrap;
        @memcpy(mem[offset..][0..lenna], try r.readBytes(lenna));
    }
}

pub fn global_section(self: *Module, r: *Reader) !void {
    const len = try r.readu();
    dbg("GLOBALS: {}\n", .{len});
    self.n_globals_internal = len;
    self.globals_off = r.pos;
}

const Instance = @import("./Instance.zig");
pub fn init_imports(self: *Module, in: *Instance, imports: ?*ImportTable) !void {
    var r = self.reader_at(self.imports_off);
    const len = try r.readu();

    var func_counter: u32 = 0;
    var global_counter: u32 = 0;

    for (0..len) |_| {
        const mod = try r.readName();
        const name = try r.readName();
        dbg("{s}:{s} = \n", .{ mod, name });
        const kind: defs.ImportExportKind = @enumFromInt(try r.readByte());
        const imp = imports orelse return error.InvalidArgument;
        switch (kind) {
            .func => {
                const idx = try r.readu();
                const item = imp.funcs.get(name) orelse return error.MissingImport;
                const n_args, const n_res = try self.type_arity(idx);
                if (n_args != item.n_args or n_res != item.n_res) return error.ImportTypeMismatch;
                in.funcs_imported[func_counter] = item;
                func_counter += 1;
            },
            .table => {
                const typ: defs.ValType = @enumFromInt(try r.readByte());
                const limits = try r.readLimits();
                if (typ == .funcref) {
                    if (imp.func_table_size < limits.min) return error.ImportLimitsMismatch;
                    if (limits.max) |m| if (imp.func_table_size > m) return error.ImportLimitsMismatch;
                    try in.init_func_table(imp.func_table_size);
                }
            },
            .mem => {
                // TODO: we don't really support memory shared between instances.
                // an imported memory just works like a private one
                const limits = try r.readLimits();
                if (imp.memory_size < limits.min) return error.ImportLimitsMismatch;
                if (limits.max) |m| if (imp.memory_size > m) return error.ImportLimitsMismatch;
                try in.init_memory(imp.memory_size);
            },
            .global => {
                const typ: defs.ValType = @enumFromInt(try r.readByte());
                const mut = (try r.readByte()) > 0;
                _ = mut;

                const item = imp.globals.get(name) orelse return error.MissingImport;
                if (typ != item.typ) return error.ImportTypeMismatch;
                in.globals_maybe_indir[global_counter] = .{ .indir = item.ref };
                global_counter += 1;
            },
        }
    }

    r.pos = self.globals_off;
    for (0..self.n_globals_internal) |i| {
        const typ: defs.ValType = @enumFromInt(try r.readByte());
        _ = try r.readByte(); // WHO FUCKING CARES IF IT IS MUTABLE OR NOT

        in.globals_maybe_indir[self.n_globals_import + i] = try Interpreter.eval_constant_expr(&r, typ, in.preglobals());
    }
}

pub fn type_arity(self: *const Module, type_idx: u32) !struct { u16, u16 } {
    var r = self.reader_at(self.types[type_idx]);
    const tag = try r.readByte();
    if (tag != 0x60) return error.InvalidFormat;
    const n_params: u16 = @intCast(try r.readu());
    for (0..n_params) |_| {
        _ = try r.readByte(); // TEMP hack: while we don't validate runtime args
    }
    const n_res: u16 = @intCast(try r.readu());
    return .{ n_params, n_res };
}

pub fn type_params(self: *const Module, type_idx: u32, out_types: []defs.ValType) !?defs.ValType {
    var r = self.reader_at(self.types[type_idx]);
    const tag = try r.readByte();
    if (tag != 0x60) return error.InvalidFormat;
    const n_params: u16 = @intCast(try r.readu());
    for (0..n_params) |i| {
        out_types[i] = @enumFromInt(try r.readByte());
    }
    // TODO: multiple return values
    const n_results: u16 = @intCast(try r.readu());
    return if (n_results > 0) @enumFromInt(try r.readByte()) else null;
}

pub fn table_section(self: *Module, in: *Instance) !void {
    var r = self.reader_at(self.table_off);
    const len = try r.readu();
    dbg("Tables: {}\n", .{len});
    for (0..len) |_| {
        const typ: defs.ValType = @enumFromInt(try r.readByte());
        const limits = try r.readLimits();
        dbg("table {s} w {}:{?}\n", .{ @tagName(typ), limits.min, limits.max });
        if (typ == .funcref) {
            try in.init_func_table(limits.min);
        }
    }
}

pub fn element_section(self: *Module, in: *Instance) !void {
    var r = self.reader_at(self.element_off);
    const len = try r.readu();
    dbg("Elements: {}\n", .{len});
    for (0..len) |_| {
        const kinda = try r.readu();
        if (kinda == 0) {
            const offset: usize = @intCast((try Interpreter.eval_constant_expr(&r, .i32, in.preglobals())).i32);
            const elen = try r.readu();
            if (offset + len > in.funcref_table.len) {
                return error.InvalidFormat;
            }
            for (0..elen) |i| {
                const item = try r.readu();
                if (item >= self.funcs_internal.len + self.n_funcs_import) return error.InvalidFormat;
                in.funcref_table[offset + i] = item;
            }
        } else {
            severe("KINDA: {}\n", .{kinda});
            @panic("unhandled!");
        }
    }
}

pub fn code_section(self: *Module, r: *Reader) !void {
    const len = try r.readu();
    dbg("Codes: {}\n", .{len});
    for (0..len) |i| {
        const size = try r.readu();
        dbg("CODE with size {}\n", .{size});
        const endpos = r.pos + size;
        self.funcs_internal[i].codeoff = r.pos;

        // if(force_eager)
        // try self.funcs[i].parse(self, r);

        r.pos = endpos;
        dbg("\n", .{});
    }
}

pub fn custom_section(self: *Module, r: *Reader, sec_len: u32) !void {
    const end_pos = r.pos + sec_len;
    const name = try r.readName();
    dbg("CUSTOM: {s} as {}\n", .{ name, sec_len });
    if (std.mem.eql(u8, name, "name")) {
        // LAYERING! they could just have used separate custom sections like
        // name.function and so on but that would have been too simple.
        while (r.pos < end_pos) {
            const kinda = try r.readByte();
            const size = try r.readu();

            if (kinda == 1 and self.funcs_internal.len > 0) { // function names, woho!
                const map_len = try r.readu();
                for (0..map_len) |_| {
                    const idx = try r.readu();
                    const funcname = try r.readName();
                    // dbg("waa {} is {s}\n", .{ idx, funcname });
                    if (idx >= self.n_funcs_import) {
                        const internal_idx = idx - self.n_funcs_import;
                        if (internal_idx < self.funcs_internal.len) {
                            self.funcs_internal[internal_idx].name = funcname;
                        }
                    }
                }
            } else {
                dbg("NAMM {} w size {}\n", .{ kinda, size });
                r.pos = @min(end_pos, r.pos + size);
            }
        }
    } else if (std.mem.eql(u8, name, "dylink.0")) {
        self.custom_dylink0_off = r.pos;
        self.custom_dylink0_end = end_pos;
    }
}

pub const DylinkInfo = struct {
    memory_size: u32,
    memory_align: u32,
    table_size: u32,
    table_align: u32,
};

pub fn get_dylink_info(self: *Module) !?DylinkInfo {
    if (self.custom_dylink0_off == 0) return null;
    var r = self.reader_at(self.custom_dylink0_off);
    while (r.pos < self.custom_dylink0_end) {
        const kinda = try r.readByte();
        const size = try r.readu();
        if (kinda == 1) { // WASM_DYLINK_MEMORY_INFO
            const memory_size = try r.readu();
            const memory_align = try r.readu();
            const table_size = try r.readu();
            const table_align = try r.readu();
            return .{
                .memory_size = memory_size,
                .memory_align = memory_align,
                .table_size = table_size,
                .table_align = table_align,
            };
        } else {
            r.pos += size;
        }
    }
    return null;
}

fn flag(flags: []const u8, f: u8) bool {
    return std.mem.indexOfScalar(u8, flags, f) != null;
}

pub fn dump_counts(self: *Module, flags: []const u8) !void {
    const all = flag(flags, 'A');
    const fo = self.n_funcs_import;

    severe("\n\n", .{});
    if (all or flag(flags, 'f')) {
        for (self.funcs_internal, 0..) |i, fi| {
            if (i.call_count > 0) {
                severe("{} : {} {s}\n", .{ i.call_count, fo + fi, i.name orelse "???" });
            }
        }
        severe("\n\n", .{});
    }

    if (all or flag(flags, 'i')) {
        var summa: u64 = 0;
        for (self.istat, 0..) |count, i| {
            if (count > 0) {
                severe("{} : {} {s}\n", .{ count, i, @tagName(@as(defs.OpCode, @enumFromInt(i))) });
                summa +|= count;
            }
        }
        severe("{}: summa\n\n", .{summa});
    }

    var big_arena = std.heap.ArenaAllocator.init(self.allocator);
    defer big_arena.deinit();
    const Block = struct {
        count: usize,
        str: []u8,
        func: *Function,
        blk: u32,
        fn busierThan(ctx: void, a: @This(), b: @This()) bool {
            _ = ctx;
            return a.count > b.count;
        }
    };
    var loop_list: std.ArrayList(Block) = .empty;
    defer loop_list.deinit(self.allocator);

    if (all or flag(flags, 'l') or flag(flags, 'w') or flag(flags, 'W')) {
        for (self.funcs_internal, 0..) |*i, fi| {
            if (i.control) |c| {
                for (c, 0..) |ci, cidx| {
                    const inst: defs.OpCode = @enumFromInt(self.raw[ci.off]);
                    if (inst == .loop) {
                        if ((all or flag(flags, 'l') or flag(flags, 'W')) and ci.count > 0) {
                            const string = try std.fmt.allocPrint(big_arena.allocator(), "{} : {}:{} {s} \n", .{ ci.count, fo + fi, cidx, i.name orelse "???" });
                            try loop_list.append(self.allocator, .{ .count = ci.count, .str = string, .func = i, .blk = @intCast(cidx) });
                        }
                    }
                }
            }
        }
        severe("\n\n", .{});
        std.sort.heap(Block, loop_list.items, {}, Block.busierThan);

        const maxen = @min(loop_list.items.len, 10);
        if (all or flag(flags, 'W')) {
            for (0..maxen) |pi| {
                const i = maxen - 1 - pi;
                const bad = &loop_list.items[i];
                severe("\n\npretty bad: {s}\n", .{bad.str});
                self.disasm_block(bad.func, bad.blk) catch {};
            }
        }
    }
}
pub const disasm_block = @import("./disasm.zig").disasm_block;

pub fn dbg_disasm(self: *Module, func: u32, blk: u32) !void {
    if (func < self.n_funcs_import) {
        severe("IMPORTED :PPP\n", .{});
        return;
    } else if (func >= self.n_funcs_import + self.funcs_internal.len) {
        @panic("out of bounds!\n");
    }

    const f = &self.funcs_internal[func - self.n_funcs_import];
    const c = try f.ensure_parsed(self);

    if (blk >= c.len) @panic("label out of bounds");
    try self.disasm_block(f, blk);
}

pub fn dbg_compile(self: *Module, func: u32, blk: u32) !void {
    if (func < self.n_funcs_import) {
        severe("IMPORTED :PPP\n", .{});
        return;
    } else if (func >= self.n_funcs_import + self.funcs_internal.len) {
        @panic("out of bounds!\n");
    }

    const f = &self.funcs_internal[func - self.n_funcs_import];
    const c = try f.ensure_parsed(self);
    const compiled = try @import("./ThunderLightning.zig").compile_block(self, f, blk);
    try self.traces.append(self.allocator, compiled);
    c[blk].trace_idx = @intCast(self.traces.items.len - 1);
}

test "basic functionality" {
    try testing.expect(11 == 10);
}
