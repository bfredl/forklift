const Module = @import("./Module.zig");
const defs = @import("./defs.zig");
const dbg = std.debug.print;
const std = @import("std");
const Reader = @import("./Reader.zig");
const Function = @import("./Function.zig");

pub fn disasm_block(mod: *Module, func: *Function, blk_idx: u32) !void {
    const c = try func.ensure_parsed(mod);
    const blk_off = c[blk_idx].off;

    // if this is a blok/loop/if statement, we will increment right back
    var c_ip: u32 = if (blk_idx > 0) blk_idx - 1 else blk_idx;
    var r = mod.reader_at(blk_off);
    const i: defs.OpCode = @enumFromInt(r.peekByte());
    var level: u32 = 0; // make it non-zero in case we disasm a "end" or "else_"
    if (blk_idx == 0 or (i != .block and i != .loop and i != .if_)) level += 1;
    while (true) {
        const pos = r.pos;
        const inst = try r.readOpCode();
        if (inst == .end or inst == .else_) level -= 1;

        dbg("{x:04}:", .{pos});
        for (0..level) |_| dbg("  ", .{});
        dbg("{s}", .{@tagName(inst)});
        switch (inst) {
            .block, .loop, .if_ => {
                c_ip += 1;
                if (c[c_ip].off != pos) @panic("naaaaje");
                level += 1;
                const typ = try r.blocktype();
                dbg(" typ={}", .{typ});
                if (inst != .block) {
                    dbg(" ({})", .{c[c_ip].count});
                }
            },
            .end => {
                c_ip += 1;
                if (c[c_ip].off != pos) @panic("naaaaje");
                dbg(" ({})", .{c[c_ip].count});
            },
            .ret => {},
            .else_ => {
                c_ip += 1;
                if (c[c_ip].off != pos) @panic("naaaaje");
                level += 1;
                dbg(" ({})", .{c[c_ip].count});
            },
            .br, .br_if => {
                const idx = try r.readu();
                dbg(" {}", .{idx});
                if (inst == .br_if) {
                    c_ip += 1;
                    if (c[c_ip].off != pos) @panic("naaaaje");
                    dbg(" ({})", .{c[c_ip].count});
                }
            },
            .br_table => {
                const n = try r.readu();
                for (0..n) |_| {
                    const idx = try r.readu();
                    dbg(" {}", .{idx});
                }
                const dflt = try r.readu(); // default
                dbg(": {}", .{dflt});
            },
            .call => {
                const idx = try r.readu();
                dbg(" {}", .{idx});
                if (idx < mod.n_funcs_import) {
                    dbg(" imported", .{});
                } else if (idx < mod.n_funcs_import + mod.funcs_internal.len) {
                    const f = mod.funcs_internal[idx - mod.n_funcs_import];
                    if (f.name) |nom| {
                        dbg(" {s}", .{nom});
                    }
                }
                // TODO: find the type
            },
            .call_indirect => {
                const typidx = try r.readu();
                const tblidx = try r.readu();
                dbg("table:{} typ:", .{tblidx});
                _ = typidx;
            },
            .nop, .unreachable_ => {},
            .i32_const => {
                const val = try r.readLeb(i32);
                dbg(" {}", .{val});
            },
            .i64_const => {
                const val = try r.readLeb(i64);
                dbg(" {}", .{val});
            },
            .f32_const => {
                const val = try r.readf(f32);
                dbg(" {}", .{val});
            },
            .f64_const => {
                const val = try r.readf(f64);
                dbg(" {}", .{val});
            },
            .local_get, .local_set, .local_tee => {
                const idx = try r.readu();
                dbg(" {}", .{idx});
            },
            .global_get, .global_set => {
                const idx = try r.readu();
                dbg(" {}", .{idx});
            },
            .drop, .select => {},
            .select_t => {
                const num = try r.readu();
                if (num != 1) return error.InvalidFormat; // possible extension
                const typ: defs.ValType = @enumFromInt(try r.readByte());
                dbg(" {}", .{typ});
            },
            .memory_size, .memory_grow => {
                if (try r.readByte() != 0) return error.InvalidFormat;
            },
            .prefixed => {
                const code = try r.prefix();
                dbg(":{s}", .{@tagName(code)});
                switch (code) {
                    .memory_fill => {
                        if (try r.readByte() != 0) return error.InvalidFormat;
                    },
                    .memory_copy => {
                        if (try r.readByte() != 0) return error.InvalidFormat;
                        if (try r.readByte() != 0) return error.InvalidFormat;
                    },
                    else => {
                        if (@intFromEnum(code) < 8) {
                            // ok, converter
                        } else {
                            dbg(" UNKNOWN: {}, aborting!", .{@intFromEnum(code)});
                            return error.InvalidFormat;
                        }
                    },
                }
            },
            else => {
                const idx = @intFromEnum(inst);
                if (idx >= 0x45 and idx <= 0xc4) {
                    // ok, parameterless
                } else if (idx >= 0x28 and idx <= 0x3e) {
                    const alignas = try r.readu();
                    const offset = try r.readu();
                    dbg(" a={} o={}", .{ alignas, offset });
                } else {
                    dbg("inst {s} TBD, aborting!\n", .{@tagName(inst)});
                    return error.NotImplemented;
                }
            },
        }
        dbg("\n", .{});

        if (level <= 0) break;
    }
}
