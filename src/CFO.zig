const std = @import("std");
const os = std.os;
const fs = std.fs;
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

    pub fn ext(self: @This()) bool {
        return @enumToInt(self) >= 0x08;
    }
};

pub fn init(allocator: Allocator) !Self {
    // TODO: allocate consequtive mprotectable pages
    return Self{
        .code = try ArrayListAligned(u8, 4096).initCapacity(page_allocator, 4096),
        .inst_off = ArrayList(u32).init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    // TODO: only in debug mode (as clobbers the array, needs r/w)
    os.mprotect(self.code.items.ptr[0..self.code.capacity], os.PROT.READ | os.PROT.WRITE) catch unreachable;
    self.code.deinit();
    self.inst_off.deinit();
}

fn new_inst(self: *Self) !void {
    var size = @intCast(u32, self.code.items.len);
    try self.inst_off.append(size);
}

// TODO: use appendAssumeCapacity in a smart way like arch/x86_64
fn wb(self: *Self, opcode: u8) !void {
    try self.code.append(opcode);
}

fn wd(self: *Self, dword: i32) !void {
    std.mem.writeIntLittle(i32, try self.code.addManyAsArray(4), dword);
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
    try self.rex_wrxb(true, dst.ext(), false, src.ext());
    try self.wb(0x8b); // MOV reg, \rm
    try self.modRm(0b11, dst.lowId(), src.lowId());
}

pub fn movrm(self: *Self, dst: IPReg, srcbase: IPReg, srcoff: i32) !void {
    if (srcoff != 0) {
        return error.OOPSIE;
    }
    if (srcbase.lowId() == 0x04 or srcbase == IPReg.rbp) {
        return error.OHNOES;
    }
    try self.new_inst();
    try self.rex_wrxb(true, dst.ext(), false, srcbase.ext());
    try self.wb(0x8b); // MOV reg, \rm
    try self.modRm(0b00, dst.lowId(), srcbase.lowId());
}

pub fn movmr(self: *Self, dstbase: IPReg, dstoff: i32, src: IPReg) !void {
    if (dstoff != 0) {
        return error.OOPSIE;
    }
    if (dstbase.lowId() == 0x04 or dstbase == IPReg.rbp) {
        return error.OHNOES;
    }
    try self.new_inst();
    try self.rex_wrxb(true, src.ext(), false, dstbase.ext());
    try self.wb(0x89); // MOV \rm, reg
    try self.modRm(0b00, src.lowId(), dstbase.lowId());
}

pub fn movri(self: *Self, dst: IPReg, src: i32) !void {
    try self.new_inst();
    // TODO: w bit should be avoidable in a lot of cases
    // like "mov rax, 1337" is equivalent to "mov eax, 1337"
    try self.rex_wrxb(true, dst.ext(), false, false);
    try self.wb(0xc7); // MOV \rm, imm32
    try self.modRm(0b11, 0b000, dst.lowId());
    try self.wd(src);
}

pub fn dump(self: *Self) !FunPtr {
    try fs.cwd().writeFile("test.o", self.code.items);
}

pub fn dbg_nasm(self: *Self, allocator: Allocator) !void {
    var nasm = try std.ChildProcess.init(&[_][]const u8{ "ndisasm", "-b", "64", "-" }, allocator);
    defer nasm.deinit();
    nasm.stdin_behavior = .Pipe;
    _ = try std.io.getStdOut().write("\n");
    try nasm.spawn();
    _ = try nasm.stdin.?.write(self.code.items);
    _ = nasm.stdin.?.close();
    nasm.stdin = null;
    _ = try nasm.wait();
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

const test_allocator = std.testing.allocator;
const expectEqual = std.testing.expectEqual;

test "return first argument" {
    var cfo = try init(test_allocator);
    defer cfo.deinit();

    try cfo.mov(IPReg.rax, IPReg.rdi);
    try cfo.ret();
    const fptr = try cfo.test_finalize();
    try expectEqual(@as(usize, 4), fptr(4, 10));
}

test "return second argument" {
    var cfo = try init(test_allocator);
    defer cfo.deinit();

    try cfo.mov(IPReg.rax, IPReg.rsi);
    try cfo.ret();
    const fptr = try cfo.test_finalize();
    try expectEqual(@as(usize, 10), fptr(4, 10));
}

test "read/write first arg as 64-bit pointer" {
    var cfo = try init(test_allocator);
    defer cfo.deinit();

    try cfo.movrm(IPReg.rax, IPReg.rdi, 0);
    try cfo.movmr(IPReg.rdi, 0, IPReg.rsi);
    try cfo.ret();

    var someint: u64 = 33;
    const fptr = try cfo.test_finalize();

    var retval = fptr(@ptrToInt(&someint), 10);
    try expectEqual(@as(usize, 33), retval);
    try expectEqual(@as(usize, 10), someint);
}

test "return intermediate value" {
    var cfo = try init(test_allocator);
    defer cfo.deinit();
    errdefer cfo.dbg_nasm(test_allocator) catch unreachable;

    try cfo.movri(IPReg.rax, 1337);
    try cfo.ret();

    const fptr = try cfo.test_finalize();

    var retval = fptr(7, 8);
    try expectEqual(@as(usize, 1337), retval);
}
