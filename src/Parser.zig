const FLIR = @import("./FLIR.zig");
const codegen = @import("./codegen.zig");
const codegen_bpf = @import("./codegen_bpf.zig").codegen;
const CodeBuffer = @import("./CodeBuffer.zig");
const X86Asm = @import("./X86Asm.zig");
const BPF = std.os.linux.BPF;
const common = @import("./common.zig");
const std = @import("std");
const print = std.debug.print;
const mem = std.mem;
const ParseError = error{ ParseError, OutOfMemory, FLIRError };
const meta = std.meta;

const CFOScript = @import("./CFOScript.zig");

const Allocator = mem.Allocator;
const CFOModule = @import("./CFOModule.zig");

const Tokenizer = @import("./Tokenizer.zig");

const options = common.debug_options;

const Chunk = Tokenizer.Chunk;

fn require(val: anytype, what: []const u8) ParseError!@TypeOf(val.?) {
    return val orelse {
        print("missing {s}\n", .{what});
        return error.ParseError;
    };
}

pub fn parse(mod: *CFOModule, allocator: Allocator, str: []const u8, dbg: bool, one: bool) !void {

    // small init size on purpose: must allow reallocations in place
    var ir = try FLIR.init(4, allocator);
    defer ir.deinit();

    var t = Tokenizer{ .str = str };
    errdefer t.fail_pos();

    while (t.nonws()) |_| {
        const kw = t.keyword() orelse return;
        if (mem.eql(u8, kw, "func")) {
            const name = try require(t.keyword(), "name");

            const obj_slot = try nonexisting(&mod.objs, name, "object");

            try CFOScript.parse(mod, &ir, &t, allocator);
            if (one) return;

            if (options.dbg_raw_ir or dbg) ir.debug_print();
            try ir.test_analysis(FLIR.X86ABI, true);
            if (options.dbg_analysed_ir) ir.debug_print();
            if (options.dbg_vregs) ir.print_intervals();

            const target = try codegen.codegen(&ir, &mod.code, false);
            obj_slot.* = .{ .func = .{ .code_start = target } };
            ir.reinit();
        } else if (mem.eql(u8, kw, "bpf_func")) {
            const name = try require(t.keyword(), "name");
            const obj_slot = try nonexisting(&mod.objs, name, "object");
            try CFOScript.parse(mod, &ir, &t, allocator);

            try ir.test_analysis(FLIR.BPF_ABI, true);
            if (dbg) ir.debug_print();
            const offset = try codegen_bpf(&ir, mod);
            const len = mod.bpf_code.items.len - offset;

            obj_slot.* = .{ .bpf_prog = .{ .fd = -1, .code_start = @intCast(offset), .code_len = @intCast(len) } };
            ir.reinit();
        } else if (mem.eql(u8, kw, "bpf_map")) {
            const name = try require(t.keyword(), "name");
            const obj_slot = try nonexisting(&mod.objs, name, "object");
            const kind = try require(try t.prefixed('.'), "kind");
            const key_size = try require(t.num(), "key_size");
            const val_size = try require(t.num(), "val_size");
            const n_entries = try require(t.num(), "n_entries");
            // print("map '{s}' of kind {s}, key={}, val={}\n", .{ name, kind, key_size, val_size });
            const map_kind = meta.stringToEnum(BPF.MapType, kind) orelse {
                print("unknown map kind: '{s}'\n", .{kind});
                return error.ParseError;
            };
            obj_slot.* = .{ .bpf_map = .{ .fd = -1, .kind = map_kind, .key_size = @intCast(key_size), .val_size = @intCast(val_size), .n_entries = @intCast(n_entries) } };
            try t.lbrk();
        } else {
            return error.ParseError;
        }
    }
    if (one) {
        return error.ParseError;
    }
}

pub fn nonexisting(map: anytype, key: []const u8, what: []const u8) ParseError!@TypeOf(map.getPtr(key).?) {
    const item = try map.getOrPut(key);
    if (item.found_existing) {
        print("duplicate {s}{s}!\n", .{ what, key });
        return error.ParseError;
    }
    // NON-EXIST-ENT!
    return item.value_ptr;
}
