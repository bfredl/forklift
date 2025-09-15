const std = @import("std");
const Instance = @This();
const Module = @import("./Module.zig");
const defs = @import("./defs.zig");
const Function = @import("./Function.zig");
const Interpreter = @import("./Interpreter.zig");
const HeavyMachineTool = @import("./HeavyMachineTool.zig");
const ImportTable = @import("./ImportTable.zig");
const severe = std.debug.print;

mod: *Module,
mem: std.ArrayList(u8),
globals_maybe_indir: []defs.StackValue,
funcs_imported: []defs.HostFunc,

// funcs not directly imported, but might be visible in funcref_table
funcs_extra: []defs.HostFunc,

// this a bit of a hack, an instance can has multiple funcref tables
funcref_table: []u32 = &.{},

pub fn get_global(self: *Instance, idx: u32) *defs.StackValue {
    const g = &self.globals_maybe_indir[idx];
    return if (idx < self.mod.n_globals_import) g.indir else g;
}

pub fn init(mod: *Module, imports: ?*ImportTable) !Instance {
    var self = Instance{
        .mod = mod,
        .mem = .empty,
        .globals_maybe_indir = try mod.allocator.alloc(defs.StackValue, mod.n_globals_import + mod.n_globals_internal),
        .funcs_imported = try mod.allocator.alloc(defs.HostFunc, mod.n_funcs_import),
        .funcs_extra = try mod.allocator.alloc(defs.HostFunc, if (imports) |i| i.func_table_funcs.items.len else 0),
    };

    try mod.init_imports(&self, imports);
    try self.init_memory(mod.mem_limits.min);
    if (imports) |imp| {
        if (self.funcref_table.len > 0) {
            if (self.funcref_table.len < imp.func_table_funcs.items.len) return error.ImportLimitsMismatch;
            for (imp.func_table_funcs.items, 0..) |f, i| {
                self.funcs_extra[i] = f;
                // this is a bit of a hack, need a better pseudo-reified "store"
                self.funcref_table[i] = (1 << 31) + @as(u32, @intCast(i));
            }
        }
    }
    if (mod.table_off > 0) try mod.table_section(&self);
    if (mod.element_off > 0) try mod.element_section(&self);
    try mod.init_data(self.mem.items, self.preglobals());

    return self;
}

pub fn deinit(self: *Instance) void {
    self.mem.deinit(self.mod.allocator);
    self.mod.allocator.free(self.globals_maybe_indir);
    self.mod.allocator.free(self.funcs_imported);
}

pub fn preglobals(self: *Instance) []const defs.StackValue {
    return self.globals_maybe_indir[0..self.mod.n_globals_import];
}

pub fn init_memory(in: *Instance, n_pages: u32) !void {
    if (n_pages == 0) return;
    if (in.mem.items.len > 0) return error.@"whattaf?????";

    try in.mem.appendNTimes(in.mod.allocator, 0, defs.page_size * n_pages);
}

pub fn init_func_table(in: *Instance, len: u32) !void {
    if (len == 0) return;
    if (in.funcref_table.len > 0) return error.NotImplemented;
    in.funcref_table = try in.mod.allocator.alloc(u32, len);
    @memset(in.funcref_table, defs.funcref_nil); // null
}

pub fn mem_get_bytes(self: *Instance, pos: u32, len: u32) ![]u8 {
    if (pos + len > self.mem.items.len) return error.WASMTrap;
    return self.mem.items[pos..][0..len];
}

pub fn mem_get_as(self: *Instance, comptime item: type, pos: u32, count: u32) []align(1) item {
    return @ptrCast(self.mem_get_bytes(pos, count * @sizeOf(item)));
}

pub fn memmove(self: *Instance, d: u32, s: u32, n: u32) !void {
    const m = self.mem.items;
    if (@as(u64, s) + n > m.len or @as(u64, d) + n > m.len) return error.WASMTrap;
    if (d <= s) {
        std.mem.copyForwards(u8, m[d..][0..n], m[s..][0..n]);
    } else {
        std.mem.copyBackwards(u8, m[d..][0..n], m[s..][0..n]);
    }
}

pub const Engine = union(enum) {
    interpreter: *Interpreter,
    heavy: *HeavyMachineTool,
};

pub fn execute_either(in: *Instance, engine: Engine, idx: u32, params: []const defs.StackValue, ret: []defs.StackValue, logga: bool, err_ret: ?*?[]const u8) !u32 {
    return switch (engine) {
        .interpreter => |i| i.execute(in, idx, params, ret, logga),
        .heavy => |h| h.execute(in, idx, params, ret, logga, err_ret),
    };
}
