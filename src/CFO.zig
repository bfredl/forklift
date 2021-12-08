const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

code: ArrayList(u8),

/// offset of each encoded instruction. Might not be needed
/// but useful for debugging.
inst_off: ArrayList(u32),

const Self = @This();

pub fn init(allocator: Allocator) !Self {
    // TODO: allocate consequtive mprotectable pages
    return Self{
        .code = try ArrayList(u8).initCapacity(allocator, 1024),
        .inst_off = ArrayList(u32).init(allocator),
    };
}

fn new_inst(self: *Self) !u32 {
    var size = @intCast(u32, self.code.items.len);
    try self.inst_off.append(size);

    return size;
}

// TODO: use appendAssumeCapacity in a smart way like arch/x86_64
pub fn inst_1byte(self: *Self, opcode: u8) !u32 {
    var pos = self.new_inst();
    try self.code.append(opcode);
    return pos;
}
