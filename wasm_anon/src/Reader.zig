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

pub fn readOp(r: *Reader) !defs.Instruction {
    const opcode = try readOpCode();
    switch (opcode) {
        .block => return .{ .block = try r.blocktype() },
        .loop => return .{ .loop = try r.blocktype() },
        .if_ => return .{ .if_ = try r.blocktype() },
        else => {
            return .{ .other__fixme = opcode };
        },
    }
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

pub const BlockType = union(enum) {
    simple: defs.ValType,
    complex_idx: u32,

    // args, results
    pub fn arity(self: BlockType, mod: *const Module) !struct { u16, u16 } {
        return switch (self) {
            .simple => |vt| .{ 0, if (vt == .void) 0 else 1 },
            .complex_idx => |idx| mod.type_arity(idx),
        };
    }
};

pub fn blocktype(r: *Reader) !BlockType {
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
