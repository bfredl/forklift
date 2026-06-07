const std = @import("std");

/// Read a single unsigned LEB128 value from the given reader as type T,
/// or error.Overflow if the value cannot fit.
pub fn readUleb128(comptime T: type, reader: anytype) !T {
    const U = if (@typeInfo(T).int.bits < 8) u8 else T;
    const ShiftT = std.math.Log2Int(U);

    const max_group = (@typeInfo(U).int.bits + 6) / 7;

    var value: U = 0;
    var group: ShiftT = 0;

    while (group < max_group) : (group += 1) {
        const byte = try reader.readByte();

        const ov = @shlWithOverflow(@as(U, byte & 0x7f), group * 7);
        if (ov[1] != 0) return error.Overflow;

        value |= ov[0];
        if (byte & 0x80 == 0) break;
    } else {
        return error.Overflow;
    }

    // only applies in the case that we extended to u8
    if (U != T) {
        if (value > std.math.maxInt(T)) return error.Overflow;
    }

    return @as(T, @truncate(value));
}

/// Read a single signed LEB128 value from the given reader as type T,
/// or error.Overflow if the value cannot fit.
pub fn readIleb128(comptime T: type, reader: anytype) !T {
    const S = if (@typeInfo(T).int.bits < 8) i8 else T;
    const U = @Int(.unsigned, @typeInfo(S).int.bits);
    const ShiftU = std.math.Log2Int(U);

    const max_group = (@typeInfo(U).int.bits + 6) / 7;

    var value = @as(U, 0);
    var group = @as(ShiftU, 0);

    while (group < max_group) : (group += 1) {
        const byte = try reader.readByte();

        const shift = group * 7;
        const ov = @shlWithOverflow(@as(U, byte & 0x7f), shift);
        if (ov[1] != 0) {
            // Overflow is ok so long as the sign bit is set and this is the last byte
            if (byte & 0x80 != 0) return error.Overflow;
            if (@as(S, @bitCast(ov[0])) >= 0) return error.Overflow;

            // and all the overflowed bits are 1
            const remaining_shift = @as(u3, @intCast(@typeInfo(U).int.bits - @as(u16, shift)));
            const remaining_bits = @as(i8, @bitCast(byte | 0x80)) >> remaining_shift;
            if (remaining_bits != -1) return error.Overflow;
        } else {
            // If we don't overflow and this is the last byte and the number being decoded
            // is negative, check that the remaining bits are 1
            if ((byte & 0x80 == 0) and (@as(S, @bitCast(ov[0])) < 0)) {
                const remaining_shift = @as(u3, @intCast(@typeInfo(U).int.bits - @as(u16, shift)));
                const remaining_bits = @as(i8, @bitCast(byte | 0x80)) >> remaining_shift;
                if (remaining_bits != -1) return error.Overflow;
            }
        }

        value |= ov[0];
        if (byte & 0x80 == 0) {
            const needs_sign_ext = group + 1 < max_group;
            if (byte & 0x40 != 0 and needs_sign_ext) {
                const ones = @as(S, -1);
                value |= @as(U, @bitCast(ones)) << (shift + 7);
            }
            break;
        }
    } else {
        return error.Overflow;
    }

    const result = @as(S, @bitCast(value));
    // Only applies if we extended to i8
    if (S != T) {
        if (result > std.math.maxInt(T) or result < std.math.minInt(T)) return error.Overflow;
    }

    return @as(T, @truncate(result));
}
