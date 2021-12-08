const std = @import("std");
const os = std.os;
const Allocator = std.mem.Allocator;
const page_allocator = std.heap.page_allocator;
const ArrayList = std.ArrayList;
const ArrayListAligned = std.ArrayListAligned;

code: ArrayListAligned(u8, 4096),

/// offset of each encoded instruction. Might not be needed
/// but useful for debugging.
inst_off: ArrayList(u32),

const Self = @This();

const FunPtr = fn (arg1: usize, arg2: usize) callconv(.C) usize;

pub fn init(allocator: Allocator) !Self {
    // TODO: allocate consequtive mprotectable pages
    return Self{
        .code = try ArrayListAligned(u8, 4096).initCapacity(page_allocator, 4096),
        .inst_off = ArrayList(u32).init(allocator),
    };
}

fn new_inst(self: *Self) !void {
    var size = @intCast(u32, self.code.items.len);
    try self.inst_off.append(size);
}

// TODO: use appendAssumeCapacity in a smart way like arch/x86_64
pub fn inst_1byte(self: *Self, opcode: u8) !void {
    try self.new_inst();
    try self.code.append(opcode);
}

pub fn ret(self: *Self) !void {
    try self.inst_1byte(0xC3);
}

pub fn test_finalize(self: *Self) !FunPtr {
    try os.mprotect(self.code.items.ptr[0..self.code.capacity], os.PROT.READ | os.PROT.EXEC);
    return @ptrCast(FunPtr, self.code.items.ptr);
}
