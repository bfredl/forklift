const Function = @This();
typeidx: u32,
n_params: u32 = undefined,
n_res: u32 = undefined,
codeoff: u32 = undefined,
control: ?[]ControlItem = null,
exported: ?[]const u8 = null,

name: ?[]const u8 = null,

call_count: Counter = 0,

local_types: []defs.ValType = &.{},

// for optimization. in case there is more than 64 args this will contain false positives..
args_mut: u64 = 0,

// need not be strict but can be an over-estimate
val_stack_max_height: u16 = 0,

// index into CFOModule.objs if using HeavyMachineTool
hmt_object: ?u32 = null,
// index for wrapper function, built for all exports
hmt_trampoline: ?u32 = null,
// NB: leaky lifeboat - no errors make no leaked error strings!
hmt_error: ?[]const u8 = null,
const Counter = u64;

const std = @import("std");
const defs = @import("./defs.zig");
const ops = @import("./ops.zig");
const Module = @import("./Module.zig");
const Instance = @import("./Instance.zig");
const dbg = Module.dbg;
const severe = std.debug.print;

const Reader = @import("./Reader.zig");

const ControlItem = struct {
    off: u32, // this is absolute (relative to mod.raw)
    jmp_t: u16,
    trace_idx: u16 = 0xFFFF,
    count: Counter = 0,
};

pub fn ensure_parsed(self: *Function, mod: *Module) ![]ControlItem {
    if (self.control == null) {
        var r = mod.reader_at(self.codeoff);
        try self.parse(mod, &r);
    }
    return self.control orelse @panic("how could you");
}

pub fn parse(self: *Function, mod: *Module, r: *Reader) !void {
    self.n_params, self.n_res = try mod.type_arity(self.typeidx);

    self.codeoff = r.pos;

    var n_locals: u32 = self.n_params;
    const n_local_defs = try r.readu();

    var local_types: std.ArrayList(defs.ValType) = try .initCapacity(mod.allocator, self.n_params + n_local_defs);
    _ = try mod.type_params(self.typeidx, local_types.addManyAsSliceAssumeCapacity(self.n_params));

    for (0..n_local_defs) |_| {
        const n_decl = try r.readu();
        n_locals += n_decl;
        const typ: defs.ValType = @enumFromInt(try r.readByte());
        try local_types.appendNTimes(mod.allocator, typ, n_decl);
        dbg("{} x {s}, ", .{ n_decl, @tagName(typ) });
    }
    dbg("\n", .{});
    self.local_types = try local_types.toOwnedSlice(mod.allocator);

    try self.parse_body(mod, r, n_locals);
}

pub fn parse_body(self: *Function, mod: *Module, r: *Reader, n_locals: u32) !void {
    var level: u32 = 1;

    var clist: std.ArrayList(ControlItem) = .empty;
    // these point to the entry point of each level. for if-else-end we put in else_ when we have seen it
    var cstack: std.ArrayList(struct { start: u16 }) = .empty;
    // TODO: this is a sentinel, might be eliminated (use jmp_t = 0xFFFF instead for "INVALID")
    // although having 0 as a "name" for the implicit entire-function block is useful..
    try clist.append(mod.allocator, .{ .off = r.pos, .jmp_t = 0 });
    try cstack.append(mod.allocator, .{ .start = 0 });

    var val_stack_level: u16 = 0;
    var val_stack_height: u16 = 0;

    while (level >= 1) {
        const pos = r.pos;
        const inst = try r.readOpCode();
        if (inst == .end or inst == .else_) level -= 1;

        dbg("{x:04}:", .{pos});
        for (0..level) |_| dbg("  ", .{});
        dbg("{s}", .{@tagName(inst)});
        switch (inst) {
            .block, .loop, .if_ => {
                level += 1;
                const typ = try r.blocktype();
                dbg(" typ={}", .{typ});
                // TODO: we're fine for now but reconsider with multi-value
                // if (typ.results() != 0) return error.NotImplemented;
                if (inst == .if_) val_stack_level -= 1;
                try clist.append(mod.allocator, .{ .off = pos, .jmp_t = 0 });
                try cstack.append(mod.allocator, .{ .start = @intCast(clist.items.len - 1) });
            },
            .end => {
                try clist.append(mod.allocator, .{ .off = pos, .jmp_t = 0 });
                const start = &clist.items[cstack.items[cstack.items.len - 1].start];
                const start_op: defs.OpCode = @enumFromInt(r.buffer[start.off]);
                dbg(" (for {s} at {x:04})", .{ @tagName(start_op), start.off });
                start.jmp_t = @intCast(clist.items.len - 1);
                cstack.items.len -= 1;
            },
            .else_ => {
                level += 1;
                try clist.append(mod.allocator, .{ .off = pos, .jmp_t = 0 });
                dbg(" (for if_ at {x:04})", .{clist.items[cstack.items[cstack.items.len - 1].start].off});
                // "if_" jumps to "else_", and "else_" jumps to end
                clist.items[cstack.items[cstack.items.len - 1].start].jmp_t = @intCast(clist.items.len - 1);
                cstack.items[cstack.items.len - 1].start = @intCast(clist.items.len - 1);
            },
            .br, .br_if => {
                // try clist.append(mod.allocator, .{ .off = pos, .jmp_t = 0 });
                const idx = try r.readu();
                dbg(" {}", .{idx});
                if (inst == .br_if) {
                    val_stack_level -= 1;
                    try clist.append(mod.allocator, .{ .off = pos, .jmp_t = 0 });
                }
            },
            .br_table => {
                // TODO: this is a bit of a crash-out. need to look into real-world compiler
                // output to figure out how to best represent this in profile/trace codegen
                val_stack_level -= 1;
                const n = try r.readu();
                for (0..n) |_| {
                    _ = try r.readu();
                }
                _ = try r.readu(); // default
            },
            .ret => {},
            .call => {
                const idx = try r.readu();
                dbg(" {}", .{idx});
                const typ = if (idx < mod.n_funcs_import)
                    mod.funcs_imported_types[idx]
                else if (idx >= mod.n_funcs_import + mod.funcs_internal.len)
                    return error.InvalidFormat
                else
                    mod.funcs_internal[idx - mod.n_funcs_import].typeidx;
                const n_args, const n_res = try mod.type_arity(typ);
                val_stack_level -= n_args;
                val_stack_level += n_res;
            },
            .call_indirect => {
                const typeidx = try r.readu();
                const tblidx = try r.readu();
                dbg(" {}:{}", .{ typeidx, tblidx });
                const n_args, const n_res = try mod.type_arity(typeidx);
                val_stack_level -= n_args;
                val_stack_level += n_res;
            },
            .nop, .unreachable_ => {},
            .i32_const => {
                const val = try r.readLeb(i32);
                dbg(" {}", .{val});
                val_stack_level += 1;
            },
            .i64_const => {
                const val = try r.readLeb(i64);
                dbg(" {}", .{val});
                val_stack_level += 1;
            },
            .f32_const => {
                const val = try r.readf(f32);
                dbg(" {}", .{val});
                val_stack_level += 1;
            },
            .f64_const => {
                const val = try r.readf(f64);
                dbg(" {}", .{val});
                val_stack_level += 1;
            },
            .local_get, .local_set, .local_tee => {
                const idx = try r.readu();
                dbg(" {}", .{idx});
                if (idx >= n_locals) return error.InvalidFormat;
                if (inst != .local_get and idx < self.n_params) {
                    self.args_mut |= @as(u64, 1) << @as(u6, @intCast(idx & 63));
                }
                if (inst == .local_get) {
                    val_stack_level += 1;
                } else if (inst == .local_set) {
                    val_stack_level -= 1;
                }
            },
            .global_get, .global_set => {
                const idx = try r.readu();
                dbg(" {}", .{idx});
                if (idx >= mod.n_globals_import + mod.n_globals_internal) return error.InvalidFormat;
                if (inst == .global_get) {
                    val_stack_level += 1;
                } else {
                    val_stack_level -= 1;
                }
            },
            .drop => val_stack_level -= 1,
            .select => val_stack_level -= 2,
            .select_t => {
                const num = try r.readu();
                if (num != 1) return error.InvalidFormat; // possible extension
                const typ: defs.ValType = @enumFromInt(try r.readByte());
                dbg(" {}", .{typ});
                val_stack_level -= 2;
            },
            .memory_size, .memory_grow => {
                if (try r.readByte() != 0) return error.InvalidFormat;
                if (inst == .memory_size) val_stack_level += 1;
            },
            .prefixed => {
                const code = try r.prefix();
                dbg(":{s}", .{@tagName(code)});
                switch (code) {
                    .memory_fill => {
                        if (try r.readByte() != 0) return error.InvalidFormat;
                        val_stack_level -= 3;
                    },
                    .memory_copy => {
                        if (try r.readByte() != 0) return error.InvalidFormat;
                        if (try r.readByte() != 0) return error.InvalidFormat;
                        val_stack_level -= 3;
                    },
                    else => {
                        if (@intFromEnum(code) < 8) {
                            // ok, converter
                        } else {
                            severe(" UNKNOWN: {}, aborting!", .{@intFromEnum(code)});
                            return error.InvalidFormat;
                        }
                    },
                }
            },
            else => {
                const idx = @intFromEnum(inst);
                // TODO: maybe just a single "adj : [265]i8" for a lot of the cases
                if (idx >= 0x45 and idx <= 0x66) {
                    if (idx != 0x45 and idx != 0x50) val_stack_level -= 1;
                } else if (idx >= 0x67 and idx <= 0x69) {
                    // ok, parameterless
                } else if (idx >= 0x6a and idx <= 0x78) {
                    val_stack_level -= 1;
                } else if (idx >= 0x79 and idx <= 0x7b) {
                    // ok, parameterless
                } else if (idx >= 0x7c and idx <= 0x8a) {
                    val_stack_level -= 1;
                } else if (idx >= 0x8b and idx <= 0x91) {
                    // ok, parameterless
                } else if (idx >= 0x92 and idx <= 0x98) {
                    val_stack_level -= 1;
                } else if (idx >= 0x99 and idx <= 0x9f) {
                    // ok, parameterless
                } else if (idx >= 0xa0 and idx <= 0xa6) {
                    val_stack_level -= 1;
                } else if (idx >= 0xa7 and idx <= 0xc4) {
                    // ok, parameterless
                } else if (idx >= 0x28 and idx <= 0x3e) {
                    const alignas = try r.readu();
                    const offset = try r.readu();
                    dbg(" a={} o={}", .{ alignas, offset });
                    if (idx >= 0x36) val_stack_level -= 1;
                } else {
                    severe("inst {s} TBD, aborting!\n", .{@tagName(inst)});
                    return error.NotImplemented;
                }
            },
        }

        val_stack_height = @max(val_stack_height, val_stack_level);
        dbg("\n", .{});
    }

    dbg("\n\n", .{});
    for (0.., clist.items) |i, c| {
        dbg("{:2}: {x:04} {}\n", .{ i, c.off, c.jmp_t });
    }

    self.control = try clist.toOwnedSlice(mod.allocator);
    self.val_stack_max_height = val_stack_height;
}
