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

/// Registers for pointers and pointer-sized int
///
/// smaller sizes will be handled by separate modifiers
/// Question: support ah,ch,dh,bh at all? perhaps as separate pseudo-op.
pub const IPReg = enum(u4) {
    // 0 through 15, 64-bit registers. 8-15 are extended.
    // id is just the int value.
    rax,
    rcx,
    rdx,
    rbx,
    rsp,
    rbp,
    rsi,
    rdi,
    r8,
    r9,
    r10,
    r11,
    r12,
    r13,
    r14,
    r15,

    pub fn lowId(self: @This()) u3 {
        return @truncate(u3, @enumToInt(self));
    }
};

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
fn wb(self: *Self, opcode: u8) !void {
    try self.code.append(opcode);
}

pub fn inst_1byte(self: *Self, opcode: u8) !void {
    try self.new_inst();
    try self.wb(opcode);
}

pub fn ret(self: *Self) !void {
    try self.inst_1byte(0xC3);
}

pub fn mov(self: *Self, dst: IPReg, src: IPReg) !void {
    try self.new_inst();
    try self.rex_wrxb(true, false, false, false); // TODO: r8-r15 thx plz
    try self.wb(0x8b); // MOV \rm
    try self.modRm(0b11, dst.lowId(), src.lowId());
}

pub fn test_finalize(self: *Self) !FunPtr {
    try os.mprotect(self.code.items.ptr[0..self.code.capacity], os.PROT.READ | os.PROT.EXEC);
    return @ptrCast(FunPtr, self.code.items.ptr);
}

pub fn rex_wrxb(self: *Self, w: bool, r: bool, x: bool, b: bool) !void {
    var value: u8 = 0x40;

    if (w) value |= 0b1000;
    if (r) value |= 0b0100;
    if (x) value |= 0b0010;
    if (b) value |= 0b0001;

    if (value != 0x40) {
        try self.wb(value);
    }
}

pub fn modRm(self: *Self, mod: u2, reg_or_opx: u3, rm: u3) !void {
    try self.wb(@as(u8, mod) << 6 | @as(u8, reg_or_opx) << 3 | rm);
}
