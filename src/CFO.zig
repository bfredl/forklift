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

pub const AOp = enum(u3) {
    add,
    bor,
    adc,
    sbb,
    band, // There is no band!
    sub,
    xor,
    cmp,

    fn off(self: @This()) u8 {
        return @as(u8, @enumToInt(self)) * 8;
    }
};

pub const Cond = enum(u4) {
    o, // overflow
    no,
    b, // below
    nb,
    e, // equal
    ne,
    na,
    a, // above
    s, // sign
    ns,
    p, // parity
    np,
    l, // less
    nl,
    ng,
    g, // greater

    const C = @This();
    pub const c = C.b;
    pub const nc = C.nb;

    pub const ae = C.nb;
    pub const be = C.na;
    pub const ge = C.nl;
    pub const le = C.ng;
    fn off(self: @This()) u8 {
        return @as(u8, @enumToInt(self));
    }
};

const PP = enum(u4) {
    none,
    h66,
    F3,
    F2,
    fn val(self: @This()) u8 {
        return @as(u8, @enumToInt(self));
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

// encodings
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

pub fn tibflag(comptime T: type, flag: bool) u8 {
    return @as(T, @boolToInt(!flag));
}

// Note: implements inversion of r, vvvv
pub fn vex2(self: *Self, r: bool, vv: u4, l: bool, pp: PP) !void {
    try self.wb(0xC5);
    try self.wb(tibflag(u8, r) << 7 | @as(u8, ~vv) << 3 | @as(u8, @boolToInt(l)) << 2 | pp.val());
}

// Note: implements inversion of wrxb, vvvv
pub fn vex3(self: *Self, w: bool, r: bool, x: bool, b: bool, mm: u5, vv: u4, l: bool, pp: PP) !void {
    try self.wb(0xC4);
    try self.wb(tibflag(u8, r) << 7 | tibflag(u8, x) << 6 | tibflag(u8, b) << 5 | @as(u8, mm));
    try self.wb(tibflag(u8, w) << 7 | @as(u8, ~vv) << 3 | @as(u8, @boolToInt(l)) << 2 | pp.val());
}

pub fn vex0fwig(self: *Self, r: bool, x: bool, b: bool, vv: u4, l: bool, pp: PP) !void {
    try if (x or b) self.vex3(false, r, x, b, 1, vv, l, pp) else self.vex2(r, vv, l, pp);
}

// control flow
pub fn ret(self: *Self) !void {
    try self.inst_1byte(0xC3);
}

pub fn jfwd(self: *Self, cond: Cond) !u32 {
    try self.new_inst();
    try self.wb(0x70 + cond.off());
    var pos = @intCast(u32, self.code.items.len);
    try self.wb(0x00); // placeholder
    return pos;
}

pub fn set_target(self: *Self, pos: u32) !void {
    var off = @intCast(u32, self.code.items.len) - (pos + 1);
    if (off > 0x7f) {
        return error.InvalidNearJump;
    }
    self.code.items[pos] = @intCast(u8, off);
}

// mov and arithmetic
fn op_rr(self: *Self, opcode: u8, dst: IPReg, src: IPReg) !void {
    try self.new_inst();
    try self.rex_wrxb(true, dst.ext(), false, src.ext());
    try self.wb(opcode); // OP reg, \rm
    try self.modRm(0b11, dst.lowId(), src.lowId());
}

pub fn mov(self: *Self, dst: IPReg, src: IPReg) !void {
    try self.op_rr(0x8b, dst, src);
}

pub fn arit(self: *Self, op: AOp, dst: IPReg, src: IPReg) !void {
    try self.op_rr(op.off() + 0b11, dst, src);
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

pub fn movmi(self: *Self, dstbase: IPReg, dstoff: i32, src: i32) !void {
    if (dstoff != 0) {
        return error.OOPSIE;
    }
    if (dstbase.lowId() == 0x04 or dstbase == IPReg.rbp) {
        return error.OHNOES;
    }
    try self.new_inst();
    try self.rex_wrxb(true, false, false, false);
    try self.wb(0xc7); // MOV \rm, imm32
    try self.modRm(0b00, 0b000, dstbase.lowId());
    try self.wd(src);
}

// VEX instructions
// note: for now we use VEX for all xmm/ymm operations.
// old school SSE forms might be shorter for some 128/scalar ops?

pub fn vaddsd(self: *Self, dst: u4, src1: u4, src2: u4) !void {
    try self.new_inst();
    try self.vex0fwig(dst > 7, false, src2 > 7, src1, false, PP.F2);
    try self.wb(0x58);
    try self.modRm(0b11, @truncate(u3, dst), @truncate(u3, src2));
}

pub fn dump(self: *Self) !void {
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

pub fn finalize(self: *Self) !void {
    try os.mprotect(self.code.items.ptr[0..self.code.capacity], os.PROT.READ | os.PROT.EXEC);
}

pub fn get_ptr(self: *Self, comptime T: type) T {
    return @ptrCast(T, self.code.items.ptr);
}

pub fn test_call2(self: *Self, arg1: usize, arg2: usize) !usize {
    try self.finalize();
    const FunPtr = fn (arg1: usize, arg2: usize) callconv(.C) usize;
    return self.get_ptr(FunPtr)(arg1, arg2);
}

pub fn test_call2f64(self: *Self, arg1: f64, arg2: f64) !f64 {
    try self.finalize();
    const FunPtr = fn (arg1: f64, arg2: f64) callconv(.C) f64;
    return self.get_ptr(FunPtr)(arg1, arg2);
}

const test_allocator = std.testing.allocator;
const expectEqual = std.testing.expectEqual;

test "return first argument" {
    var cfo = try init(test_allocator);
    defer cfo.deinit();

    try cfo.mov(IPReg.rax, IPReg.rdi);
    try cfo.ret();
    try expectEqual(@as(usize, 4), try cfo.test_call2(4, 10));
}

test "return second argument" {
    var cfo = try init(test_allocator);
    defer cfo.deinit();

    try cfo.mov(IPReg.rax, IPReg.rsi);
    try cfo.ret();
    try expectEqual(@as(usize, 10), try cfo.test_call2(4, 10));
}

test "read/write first arg as 64-bit pointer" {
    var cfo = try init(test_allocator);
    defer cfo.deinit();

    try cfo.movrm(IPReg.rax, IPReg.rdi, 0);
    try cfo.movmr(IPReg.rdi, 0, IPReg.rsi);
    try cfo.ret();

    var someint: u64 = 33;
    var retval = try cfo.test_call2(@ptrToInt(&someint), 10);
    try expectEqual(@as(usize, 33), retval);
    try expectEqual(@as(usize, 10), someint);
}

test "return intermediate value" {
    var cfo = try init(test_allocator);
    defer cfo.deinit();

    try cfo.movri(IPReg.rax, 1337);
    try cfo.ret();

    var retval = try cfo.test_call2(7, 8);
    try expectEqual(@as(usize, 1337), retval);
}

test "write intermediate value to 64-bit pointer" {
    var cfo = try init(test_allocator);
    defer cfo.deinit();

    try cfo.movmi(IPReg.rdi, 0, 586);
    try cfo.ret();

    var someint: u64 = 33;

    _ = try cfo.test_call2(@ptrToInt(&someint), 8);
    try expectEqual(@as(usize, 586), someint);
}

test "add arguments" {
    var cfo = try init(test_allocator);
    defer cfo.deinit();

    try cfo.mov(IPReg.rax, IPReg.rdi);
    try cfo.arit(AOp.add, IPReg.rax, IPReg.rsi);
    try cfo.ret();

    var retval = try cfo.test_call2(1002, 560);
    try expectEqual(@as(usize, 1562), retval);
}

test "subtract arguments" {
    var cfo = try init(test_allocator);
    defer cfo.deinit();

    try cfo.mov(IPReg.rax, IPReg.rdi);
    try cfo.arit(AOp.sub, IPReg.rax, IPReg.rsi);
    try cfo.ret();

    var retval = try cfo.test_call2(1002, 560);
    try expectEqual(@as(usize, 442), retval);
}

test "get the maximum of two args" {
    var cfo = try init(test_allocator);
    defer cfo.deinit();

    try cfo.mov(IPReg.rax, IPReg.rdi);
    try cfo.arit(AOp.cmp, IPReg.rdi, IPReg.rsi);
    const jump = try cfo.jfwd(Cond.g);
    try cfo.mov(IPReg.rax, IPReg.rsi);
    try cfo.set_target(jump);
    try cfo.ret();

    var retval = try cfo.test_call2(1002, 560);
    try expectEqual(@as(usize, 1002), retval);

    retval = try cfo.test_call2(460, 902);
    try expectEqual(@as(usize, 902), retval);
}

test "add scalar double" {
    var cfo = try init(test_allocator);
    defer cfo.deinit();
    errdefer cfo.dbg_nasm(test_allocator) catch unreachable;

    try cfo.vaddsd(0, 0, 1);
    // try cfo.vaddsd(0, 8, 2);
    // try cfo.vaddsd(8, 3, 1);
    // try cfo.vaddsd(5, 3, 8);
    try cfo.ret();

    var retval = try cfo.test_call2f64(2.0, 0.5);
    try expectEqual(@as(f64, 2.5), retval);
}
