const FLIR = @import("./FLIR.zig");
const codegen = @import("./codegen.zig");
const codegen_bpf = @import("./codegen_bpf.zig").codegen;
const CodeBuffer = @import("./CodeBuffer.zig");
const X86Asm = @import("./X86Asm.zig");
const BPF = std.os.linux.BPF;
const common = @import("./common.zig");
const Self = @This();
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

t: Tokenizer,

allocator: Allocator,

// reset for each function; reused just to keep allocated buffers
ir: FLIR,

mod: *CFOModule,

pub fn init(str: []const u8, allocator: Allocator, module: *CFOModule) !Self {
    return .{
        .t = .{ .str = str },
        .allocator = allocator,
        .mod = module,
        .ir = try FLIR.init(4, allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.ir.deinit();
}

const Chunk = Tokenizer.Chunk;

fn objname(self: *Self) ParseError!?Chunk {
    return self.t.prefixed('$');
}

fn enumname(self: *Self) ParseError!?Chunk {
    return self.t.prefixed('.');
}

fn require(val: anytype, what: []const u8) ParseError!@TypeOf(val.?) {
    return val orelse {
        print("missing {s}\n", .{what});
        return error.ParseError;
    };
}

pub fn parse(self: *Self, dbg: bool, one: bool) !void {
    const mod = self.mod;
    // small init size on purpose: must allow reallocations in place
    var flir = try FLIR.init(4, self.allocator);
    defer flir.deinit();
    while (self.t.nonws()) |_| {
        const kw = self.t.keyword() orelse return;
        if (mem.eql(u8, kw, "func")) {
            const name = try require(self.t.keyword(), "name");

            const obj_slot = try nonexisting(&mod.objs, name, "object");

            if (self.t.nonws() == '(') { // arg list
                try CFOScript.parse(self.mod, &self.ir, &self.t, self.allocator);
                try self.t.expect_char('}');
                try self.t.lbrk();
            } else {
                try self.t.lbrk();
            }
            if (one) {
                return;
            }

            if (options.dbg_raw_ir or dbg) self.ir.debug_print();
            try self.ir.test_analysis(FLIR.X86ABI, true);
            if (options.dbg_analysed_ir) self.ir.debug_print();
            if (options.dbg_vregs) self.ir.print_intervals();

            const target = try codegen.codegen(&self.ir, &mod.code, false);
            obj_slot.* = .{ .func = .{ .code_start = target } };
            self.ir.reinit();
        } else if (mem.eql(u8, kw, "bpf_func")) {
            const name = try require(self.t.keyword(), "name");
            const obj_slot = try nonexisting(&mod.objs, name, "object");
            if (self.t.nonws() == '(') { // arg list
                try CFOScript.parse(self.mod, &self.ir, &self.t, self.allocator);
                try self.t.expect_char('}');
                try self.t.lbrk();
            } else {
                try self.t.lbrk();
            }

            try self.ir.test_analysis(FLIR.BPF_ABI, true);
            if (dbg) flir.debug_print();
            const offset = try codegen_bpf(&self.ir, mod);
            const len = mod.bpf_code.items.len - offset;

            obj_slot.* = .{ .bpf_prog = .{ .fd = -1, .code_start = @intCast(offset), .code_len = @intCast(len) } };
            self.ir.reinit();
        } else if (mem.eql(u8, kw, "bpf_map")) {
            const name = try require(self.t.keyword(), "name");
            const obj_slot = try nonexisting(&mod.objs, name, "object");
            const kind = try require(try self.enumname(), "kind");
            const key_size = try require(self.t.num(), "key_size");
            const val_size = try require(self.t.num(), "val_size");
            const n_entries = try require(self.t.num(), "n_entries");
            // print("map '{s}' of kind {s}, key={}, val={}\n", .{ name, kind, key_size, val_size });
            const map_kind = meta.stringToEnum(BPF.MapType, kind) orelse {
                print("unknown map kind: '{s}'\n", .{kind});
                return error.ParseError;
            };
            obj_slot.* = .{ .bpf_map = .{ .fd = -1, .kind = map_kind, .key_size = @intCast(key_size), .val_size = @intCast(val_size), .n_entries = @intCast(n_entries) } };
            try self.t.lbrk();
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
