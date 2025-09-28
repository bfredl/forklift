const std = @import("std");
const defs = @import("./defs.zig");
const Module = @import("./Module.zig");
pub const Reader = @This();

buffer: []const u8,
pos: u32,

pub fn readLeb(r: *Reader, comptime T: type) !T {
    return switch (@typeInfo(T).int.signedness) {
        .signed => std.leb.readIleb128(T, r),
        .unsigned => std.leb.readUleb128(T, r),
    };
}

pub fn readu(r: *Reader) !u32 {
    return r.readLeb(u32);
}

pub fn readf(r: *Reader, comptime T: type) !T {
    const ival = try r.readInt(if (T == f32) u32 else u64, .little);
    return @bitCast(ival);
}

pub fn readName(r: *Reader) ![]const u8 {
    const len = try r.readu();
    return r.readBytes(len);
}

pub fn peekByte(r: Reader) u8 {
    return r.buffer[r.pos];
}

pub fn readByte(r: *Reader) !u8 {
    if (r.pos >= r.buffer.len) return error.EndOfStream;
    const b = r.peekByte();
    r.pos += 1;
    return b;
}

pub fn readOpCode(r: *Reader) !defs.OpCode {
    return @enumFromInt(try r.readByte());
}

pub fn peekOpCode(r: *Reader) defs.OpCode {
    return @enumFromInt(r.peekByte());
}

fn i(op: defs.OpCode) u8 {
    return @intFromEnum(op);
}

const MemInst = struct {
    wide: bool,
    sext: bool,
    memsize: defs.ISize,
    alignas: u32,
    offset: u32,
};

fn mem(r: *Reader, wide: bool, sext: bool, memsize: defs.ISize) !MemInst {
    // NOTE: "The alignment in load and store instructions does not affect the semantics."
    const alignas = try r.readu();
    const offset = try r.readu();
    return .{ .wide = wide, .sext = sext, .memsize = memsize, .alignas = alignas, .offset = offset };
}

// kinda a def but just the return type of readInst so eh
pub const Instruction = union(enum) {
    block: defs.BlockType,
    loop: defs.BlockType,
    if_: defs.BlockType,

    i32_binop: defs.BinOp,
    i64_binop: defs.BinOp,
    i32_relop: defs.RelOp,
    i64_relop: defs.RelOp,
    i32_unop: defs.UnOp,
    i64_unop: defs.UnOp,

    i32_sext: defs.ISize,
    i64_sext: defs.ISize,

    i_load: MemInst,
    i_store: MemInst,

    other__fixme: defs.OpCode, // not yet converted
};

pub fn readInst(r: *Reader) !Instruction {
    // const opcode = try readOpCode();
    const byte = try r.readByte();
    return switch (byte) {
        i(.block) => .{ .block = try r.blocktype() },
        i(.loop) => .{ .loop = try r.blocktype() },
        i(.if_) => .{ .if_ = try r.blocktype() },
        i(.i32_add)...i(.i32_rotr) => .{ .i32_binop = @enumFromInt(byte - i(.i32_add)) },
        i(.i64_add)...i(.i64_rotr) => .{ .i64_binop = @enumFromInt(byte - i(.i64_add)) },
        i(.i32_clz)...i(.i32_popcnt) => .{ .i32_unop = @enumFromInt(byte - i(.i32_clz)) },
        i(.i64_clz)...i(.i64_popcnt) => .{ .i64_unop = @enumFromInt(byte - i(.i64_clz)) },
        // TODO: likely eqz as well as a treat:
        i(.i32_eq)...i(.i32_ge_u) => .{ .i32_relop = @enumFromInt(byte - i(.i32_eq)) },
        i(.i64_eq)...i(.i64_ge_u) => .{ .i64_relop = @enumFromInt(byte - i(.i64_eq)) },
        else => {
            return .{ .other__fixme = @enumFromInt(byte) };
        },
        i(.i32_extend8_s), i(.i32_extend16_s) => .{ .i32_sext = @enumFromInt(byte - i(.i32_extend8_s)) },
        i(.i64_extend8_s)...i(.i64_extend32_s) => .{ .i64_sext = @enumFromInt(byte - i(.i64_extend8_s)) },
        i(.i32_load) => .{ .i_load = try r.mem(false, false, .dword) },
        i(.i64_load) => .{ .i_load = try r.mem(true, false, .quadword) },
        i(.i32_load8_s)...i(.i32_load16_u) => .{ .i_load = try r.mem(false, (byte % 2 == 0), @enumFromInt((byte & 2) >> 1)) },
        i(.i64_load8_s)...i(.i64_load32_u) => .{ .i_load = try r.mem(true, (byte % 2 == 0), @enumFromInt((byte & 6) >> 1)) },
        i(.i32_store) => .{ .i_store = try r.mem(false, false, .dword) },
        i(.i64_store) => .{ .i_store = try r.mem(true, false, .quadword) },
        i(.i32_store8)...i(.i32_store16) => .{ .i_store = try r.mem(false, false, @enumFromInt(byte & 1)) },
        i(.i64_store8)...i(.i64_store32) => .{ .i_store = try r.mem(true, false, @enumFromInt(byte & 3)) },
    };
}

pub fn readBytes(r: *Reader, len: u32) ![]const u8 {
    if (r.pos + len > r.buffer.len) return error.EndOfStream;
    const str = r.buffer[r.pos..][0..len];
    r.pos += len;
    return str;
}

pub inline fn readInt(self: *Reader, comptime T: type, endian: std.builtin.Endian) !T {
    const len = @divExact(@typeInfo(T).int.bits, 8);
    return std.mem.readInt(T, @ptrCast(try self.readBytes(len)), endian);
}

pub fn blocktype(r: *Reader) !defs.BlockType {
    // TODO: just readLeb(r, i33) directly and "interpret" negative values might be simpler?
    const nextByte = r.peekByte();
    if ((nextByte & 0xc0) == 0x40) {
        const t: defs.ValType = @enumFromInt(try r.readByte());
        return .{ .simple = t };
    } else {
        const tidx: u32 = @intCast(try readLeb(r, i33));
        return .{ .complex_idx = tidx };
    }
}

// throws on unknown prefix
pub fn prefix(r: *Reader) !defs.Prefixed {
    const inst = try r.readu();
    if (inst > defs.max_prefixed) return error.NotImplemented;
    return @enumFromInt(inst);
}

pub fn readLimits(r: *Reader) !Module.Limits {
    const kind = try r.readByte();
    const min = try r.readu();
    return .{ .min = min, .max = switch (kind) {
        0x00 => null,
        0x01 => try r.readu(),
        else => return error.InvalidFormat,
    } };
}
