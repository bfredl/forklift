const std = @import("std");
const mem = std.mem;
const os = std.os;
const debug = std.debug;
const Allocator = mem.Allocator;

const common = @import("./common.zig");
const ISize = common.ISize;

code: *@import("./CodeBuffer.zig"),
long_jump_mode: bool = false,

fn new_inst(self: *Self, addr: usize) !void {
    try self.code.new_inst(addr);
}

// TODO: use appendAssumeCapacity in a smart way like arch/x86_64
pub fn wb(self: *Self, opcode: u8) !void {
    try self.code.buf.append(opcode);
}

pub fn wbi(self: *Self, imm: i8) !void {
    try self.wb(@as(u8, @bitCast(imm)));
}

pub fn wd(self: *Self, dword: i32) !void {
    std.mem.writeInt(i32, try self.code.buf.addManyAsArray(4), dword, .little);
}

pub fn wq(self: *Self, qword: u64) !void {
    std.mem.writeInt(u64, try self.code.buf.addManyAsArray(8), qword, .little);
}

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

    pub fn id(self: IPReg) u4 {
        return @intFromEnum(self);
    }

    pub fn into(self: IPReg) common.IPReg {
        return @enumFromInt(self.id());
    }

    pub fn from(self: common.IPReg) IPReg {
        return @enumFromInt(self.id());
    }

    pub fn lowId(self: IPReg) u3 {
        return @truncate(@intFromEnum(self));
    }

    pub fn ext(self: IPReg) bool {
        return @intFromEnum(self) >= 0x08;
    }

    // if register would confilct with AH,BH,CH,DH in byte mode
    // force a REX byte to avoid it.
    pub fn highlike(self: IPReg) bool {
        return IPReg.rsp.id() <= self.id() and self.id() < IPReg.r8.id();
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
        return @as(u8, @intFromEnum(self)) * 8;
    }

    pub fn opx(self: @This()) u3 {
        return @intFromEnum(self);
    }
};

pub const ShiftOp = enum {
    hl, // logically left
    ar, // arithmically right
    hr, // logically right
    const al = .hl; // arithmically left, same as logically left
    //
    pub fn to_pp(self: @This()) PP {
        return @enumFromInt(@intFromEnum(self) + 1);
    }

    pub fn to_rm(self: @This()) u3 {
        return switch (self) {
            .hl => 4,
            .ar => 7,
            .hr => 5,
        };
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
    pub fn off(self: @This()) u8 {
        return @intFromEnum(self);
    }
};

pub const VCmpOp = enum(u5) {
    eq,
    lt,
    le,
    unord,
    neq,
    nlt,
    nle,
    ord,
    eq_uq,
    nge,
    ngt,
    false,
    neq_oq,
    ge,
    gt,
    true,
    eq_os,
    lt_oq,
    le_oq,
    unord_s,
    neq_us,
    nlt_uq,
    nle_uq,
    ord_s,
    eq_us,
    nge_uq,
    ngt_uq,
    false_os,
    neq_os,
    ge_oq,
    gt_oq,
    true_us,
    pub fn val(self: @This()) u5 {
        return @intFromEnum(self);
    }
};

const PP = enum(u2) {
    none,
    h66,
    F3,
    F2,
    fn val(self: @This()) u8 {
        return @intFromEnum(self);
    }
};

// common floating-point modes of VEX instructions
pub const FMode = enum(u3) {
    ps4,
    pd2,
    ss,
    sd,
    ps8,
    pd4,

    fn pp(self: @This()) PP {
        return @enumFromInt(@as(u2, @truncate(@intFromEnum(self))));
    }

    fn l(self: @This()) bool {
        return @intFromEnum(self) >= 4;
    }

    pub fn scalar(self: @This()) bool {
        return self == @This().ss or self == @This().sd;
    }

    fn double(self: @This()) bool {
        return @as(u1, @truncate(@intFromEnum(self))) == 1;
    }
};

const MM = enum(u5) {
    h0F = 1,
    h0F38 = 2,
    h0F3A = 3,
    fn val(self: @This()) u8 {
        return @intFromEnum(self);
    }
};

// TODO: this is just ISize
pub const IMode = enum(u2) {
    b,
    w,
    d,
    q,

    fn off(self: @This()) u8 {
        return @intFromEnum(self);
    }
};

pub const VMathOp = enum(u3) {
    add = 0,
    mul = 1,
    sub = 4,
    min = 5,
    div = 6,
    max = 7,

    pub fn off(self: @This()) u8 {
        return @intFromEnum(self);
    }
};

pub fn set_align(self: *Self, alignment: u32) !void {
    var residue = self.code.get_target() & (alignment - 1);
    var padding = alignment - residue;
    if (padding != 0 and padding != alignment) {
        try self.code.buf.appendNTimes(0x90, padding);
    }
}

// encodings
pub fn rex_wrxb_force(self: *Self, w: bool, r: bool, x: bool, b: bool, force: bool) !void {
    var value: u8 = 0x40;

    if (w) value |= 0b1000;
    if (r) value |= 0b0100;
    if (x) value |= 0b0010;
    if (b) value |= 0b0001;

    if (value != 0x40 or force) {
        try self.wb(value);
    }
}
pub fn rex_wrxb(self: *Self, w: bool, r: bool, x: bool, b: bool) !void {
    return self.rex_wrxb_force(w, r, x, b, false);
}

pub fn modRm(self: *Self, mod: u2, reg_or_opx: u3, rm: u3) !void {
    try self.wb(@as(u8, mod) << 6 | @as(u8, reg_or_opx) << 3 | rm);
}

pub fn sib(self: *Self, scale: u2, index: u3, base: u3) !void {
    try self.wb(@as(u8, scale) << 6 | @as(u8, index) << 3 | base);
}

pub const EAddr = struct {
    base: ?IPReg, // null for rip[offset]
    index: ?IPReg = null,
    scale: u2 = 0,
    offset: i32 = 0,

    pub inline fn b(self: @This()) bool {
        return if (self.base) |base| base.ext() else false;
    }
    pub inline fn x(self: @This()) bool {
        return if (self.index) |index| index.ext() else false;
    }

    pub inline fn o(self: @This(), offset: i32) @This() {
        var newself = self;
        newself.offset += offset;
        return newself;
    }
};

pub fn a(reg: IPReg) EAddr {
    return .{ .base = reg };
}

pub fn bo(reg: IPReg, offset: i32) EAddr {
    return .{ .base = reg, .offset = offset };
}

pub fn rel(offset: u32) EAddr {
    return .{ .base = null, .offset = @bitCast(offset) };
}

// index quadword array
pub fn qi(base: IPReg, index: IPReg) EAddr {
    return .{ .base = base, .index = index, .scale = 3 };
}

pub fn bi(base: IPReg, index: IPReg) EAddr {
    return .{ .base = base, .index = index, .scale = 0 };
}

pub fn maybe_imm8(imm: i32) ?i8 {
    var imm8: i8 = @truncate(imm);
    return if (imm == imm8) imm8 else null;
}

// write modrm byte + optional SIB + optional offset
// caller needs to handle ea.x() and ea.b() for addresses
// with extended indices!
pub fn modRmEA(self: *Self, reg_or_opx: u3, ea: EAddr) !void {
    const offset8 = maybe_imm8(ea.offset);
    const mod: u2 = if (ea.base == null or (ea.offset == 0 and ea.base.? != .rbp))
        @as(u2, 0b00)
    else if (offset8 != null)
        @as(u2, 0b01)
    else
        @as(u2, 0b10);

    // we allow base == null, index = null to encode RIP+off32, but not
    // yet index without base ( i e scale*index+off )
    if (ea.base == null and ea.index != null) return error.NotImplemented;

    const rm = if (ea.base) |base| base.lowId() else 0x05;
    try self.modRm(mod, reg_or_opx, if (ea.index) |_| 0x04 else rm);
    if (ea.index == null and rm == 0x04) {
        // no index, but RSP/R12 as base
        // forces a SIB byte
        try self.sib(0b00, 0x04, 0x04);
    } else if (ea.index) |index| {
        if (index == .rsp) {
            return error.InvalidIndex;
        }
        try self.sib(ea.scale, index.lowId(), rm);
    }
    if (ea.base == null) {
        // rip+off32
        try self.wd(ea.offset - (@as(i32, @bitCast(self.code.get_target())) + 4));
    } else if (mod != 0b00) {
        try if (offset8) |off| self.wbi(off) else self.wd(ea.offset);
    }
}

pub fn tibflag(comptime T: type, flag: bool) T {
    return @intFromBool(!flag);
}

// Note: implements inversion of r, vvvv
pub fn vex2(self: *Self, r: bool, vv: u4, l: bool, pp: PP) !void {
    try self.wb(0xC5);
    try self.wb(tibflag(u8, r) << 7 | @as(u8, ~vv) << 3 | @as(u8, @intFromBool(l)) << 2 | pp.val());
}

// Note: implements inversion of wrxb, vvvv
pub fn vex3(self: *Self, w: bool, r: bool, x: bool, b: bool, mm: MM, vv: u4, l: bool, pp: PP) !void {
    try self.wb(0xC4);
    try self.wb(tibflag(u8, r) << 7 | tibflag(u8, x) << 6 | tibflag(u8, b) << 5 | mm.val());
    try self.wb(tibflag(u8, w) << 7 | @as(u8, ~vv) << 3 | @as(u8, @intFromBool(l)) << 2 | pp.val());
}

pub fn vex0fwig(self: *Self, r: bool, x: bool, b: bool, vv: u4, l: bool, pp: PP) !void {
    return self.vex0f(false, r, x, b, vv, l, pp);
}

pub fn vex0f(self: *Self, w: bool, r: bool, x: bool, b: bool, vv: u4, l: bool, pp: PP) !void {
    try if (w or x or b) self.vex3(w, r, x, b, .h0F, vv, l, pp) else self.vex2(r, vv, l, pp);
}

// control flow
pub fn ret(self: *Self) !void {
    try self.new_inst(@returnAddress());
    try self.wb(0xC3);
}

// for quick debugging change ret to retnasm
pub fn retnasm(self: *Self) !void {
    try self.ret();
    try self.dbg_nasm(std.testing.allocator);
}

pub fn enter(self: *Self) !void {
    try self.new_inst(@returnAddress());
    try self.wb(0x55); // PUSH rbp
    try self.mov(.rbp, .rsp);
}

pub fn leave(self: *Self) !void {
    try self.new_inst(@returnAddress());
    try self.mov(.rsp, .rbp);
    try self.wb(0x5D); // POP rbp
}

pub fn trap(self: *Self) !void {
    try self.new_inst(@returnAddress());
    // WHEEEEEEEE!
    try self.wb(0xCC); // INT 03h
}

pub fn syscall(self: *Self) !void {
    try self.new_inst(@returnAddress());
    // hello?
    try self.wb(0x0F);
    try self.wb(0x05);
}

pub fn call_rel(self: *Self, addr: u32) !void {
    try self.new_inst(@returnAddress());
    const rel_pos = self.code.get_target() + 5;
    // TODO: check bounds
    const diff = @as(i32, @intCast(addr)) - @as(i32, @intCast(rel_pos));
    try self.wb(0xE8);
    try self.wd(diff);
}

pub fn maybe_call_rel_abs(self: *Self, addr: *const u8) !?void {
    // TRICKY: this assumes code won't move.
    const rel_pos = @intFromPtr(self.code.buf.items.ptr) + self.code.get_target() + 5;
    // This should be safe if we stay in USER space (0 <= intptr < 2**47)
    const diff = @as(isize, @intCast(@intFromPtr(addr))) - @as(isize, @intCast(rel_pos));
    const short_diff = @as(i32, @truncate(diff));
    if (diff != short_diff) return null;

    try self.new_inst(@returnAddress());
    try self.wb(0xE8);
    try self.wd(short_diff);
    return {};
}

pub fn call_ptr(self: *Self, reg: IPReg) !void {
    try self.new_inst(@returnAddress());
    try self.rex_wrxb(false, false, false, reg.ext());
    try self.wb(0xFF);
    try self.modRm(0b11, 2, reg.lowId());
}

// there..
pub fn jfwd(self: *Self, cond: ?Cond) !u32 {
    try self.new_inst(@returnAddress());
    const long = self.long_jump_mode;
    if (cond) |c| {
        if (long) {
            try self.wb(0x0f);
            try self.wb(0x80 + c.off());
        } else {
            try self.wb(0x70 + c.off());
        }
    } else {
        try self.wb(if (long) 0xe9 else 0xeb);
    }
    const pos: u32 = @intCast(self.code.buf.items.len);
    if (long) {
        try self.wd(0x00); // placeholder
    } else {
        try self.wb(0x00); // placeholder
    }
    return pos;
}

pub fn set_target(self: *Self, pos: u32) !void {
    if (self.long_jump_mode) {
        var off = self.code.get_target() - (pos + 4);
        std.mem.writeInt(u32, self.code.buf.items[pos..][0..4], off, .little);
    } else {
        var off = self.code.get_target() - (pos + 1);
        if (off > 0x7f) {
            return error.InvalidNearJump;
        }
        self.code.buf.items[pos] = @as(u8, @intCast(off));
    }
}

pub fn set_lea_target(self: *Self, pos: u32) void {
    self.set_lea(pos, self.code.get_target());
}

pub fn set_lea(self: *Self, pos: u32, target: u32) void {
    var off = target - (pos + 4);
    self.code.buf.items[pos] = @intCast(off);
    std.mem.writeInt(u32, self.code.buf.items[pos..][0..4], off, .little);
}

// .. and back again
pub fn jbck(self: *Self, cond: ?Cond, target: u32) !void {
    try self.new_inst(@returnAddress());
    var off = @as(i32, @intCast(target)) - (@as(i32, @intCast(self.code.buf.items.len)) + 2);
    if (maybe_imm8(off)) |off8| {
        try self.wb(if (cond) |c| 0x70 + c.off() else 0xEB);
        try self.wbi(off8);
    } else {
        if (cond) |c| {
            try self.wb(0x0f);
            try self.wb(0x80 + c.off());
            try self.wd(off - 4); // FETING: offset is larger as the jump instruction is larger
        } else {
            try self.wb(0xe9);
            try self.wd(off - 3); // FETING: offset is larger as the jump instruction is larger
        }
    }
}

// stack management

pub fn push(self: *Self, src: IPReg) !void {
    // luring: 64-bit wide is already the default,
    // extension only needed for r8-r15 registers
    try self.rex_wrxb(false, false, false, src.ext());
    try self.wb(0x50 + @as(u8, src.lowId()));
}

pub fn pop(self: *Self, dst: IPReg) !void {
    try self.rex_wrxb(false, false, false, dst.ext());
    try self.wb(0x58 + @as(u8, dst.lowId()));
}

// mov and arithmetic

inline fn op_rr(self: *Self, opcode: u8, dst: IPReg, src: IPReg) !void {
    try self.new_inst(@returnAddress());
    try self.rex_wrxb(true, dst.ext(), false, src.ext());
    try self.wb(opcode); // OP reg, \rm
    try self.modRm(0b11, dst.lowId(), src.lowId());
}

pub fn mov(self: *Self, dst: IPReg, src: IPReg) !void {
    try self.op_rr(0x8b, dst, src);
}

pub fn zero(self: *Self, dst: IPReg) !void {
    try self.new_inst(@returnAddress());
    try self.rex_wrxb(dst.ext(), dst.ext(), false, dst.ext());
    try self.wb(0x33); // xor reg, \rm
    try self.modRm(0b11, dst.lowId(), dst.lowId());
}

pub fn arit(self: *Self, op: AOp, dst: IPReg, src: IPReg) !void {
    try self.op_rr(op.off() + 0b11, dst, src);
}

pub inline fn op_rm(self: *Self, opcode: u8, reg: IPReg, ea: EAddr) !void {
    try self.new_inst(@returnAddress());
    try self.rex_wrxb(true, reg.ext(), ea.x(), ea.b());
    try self.wb(opcode);
    try self.modRmEA(reg.lowId(), ea);
}

pub fn movrm(self: *Self, dst: IPReg, src: EAddr) !void {
    try self.op_rm(0x8b, dst, src); // MOV reg, \rm
}

// FIXME: all IPReg ops should take size!
pub fn movrm_byte(self: *Self, dst: IPReg, src: EAddr) !void {
    const reg = dst;
    const ea = src;
    try self.rex_wrxb(true, reg.ext(), ea.x(), ea.b());
    // MOVZX. TODO: indicate zero vs sign explicitly!
    try self.wb(0x0F);
    try self.wb(0xB6);
    try self.modRmEA(reg.lowId(), ea);
}

pub fn movmr(self: *Self, dst: EAddr, src: IPReg) !void {
    try self.op_rm(0x89, src, dst); // MOV \rm, reg
}

// FIXME: all IPReg ops should take size!
pub fn movmr_byte(self: *Self, dst: EAddr, src: IPReg) !void {
    const ea = dst;
    const reg = src;
    try self.new_inst(@returnAddress());
    // REX prefix is only needed for r4-r7 to avoid AH/BH/CD/DH
    try self.rex_wrxb_force(false, reg.ext(), ea.x(), ea.b(), reg.highlike());
    try self.wb(0x88);
    try self.modRmEA(reg.lowId(), ea);
}

pub fn xchg(self: *Self, lhs: IPReg, rhs: IPReg) !void {
    try self.op_rr(0x87, lhs, rhs); // XCHG reg, \rm
}

pub fn xchg_rm(self: *Self, lhs: IPReg, rhs: EAddr) !void {
    try self.op_rm(0x87, lhs, rhs); // XCHG reg, \rm
}

pub fn aritrm(self: *Self, op: AOp, dst: IPReg, src: EAddr) !void {
    try self.op_rm(op.off() + 0b11, dst, src);
}

pub fn lea(self: *Self, dst: IPReg, src: EAddr) !void {
    try self.op_rm(0x8d, dst, src); // LEA reg, \rm
}

// load adress of a latter target into a register
// this is useful i e to keep a pointer to section
// of constants in an ordinary register
pub fn lealink(self: *Self, dst: IPReg) !u32 {
    try self.new_inst(@returnAddress());
    try self.rex_wrxb(true, dst.ext(), false, false);
    try self.wb(0x8d);
    try self.modRm(0x00, dst.lowId(), 0x05);
    const pos = self.code.get_target();
    try self.wd(0); // placeholder
    return pos;
}

pub fn movri(self: *Self, dst: IPReg, src: i32) !void {
    try self.new_inst(@returnAddress());
    // TODO: w bit should be avoidable in a lot of cases
    // like "mov rax, 1337" is equivalent to "mov eax, 1337"
    try self.rex_wrxb(true, false, false, dst.ext());
    try self.wb(0xc7); // MOV \rm, imm32
    try self.modRm(0b11, 0b000, dst.lowId());
    try self.wd(src);
}

pub fn aritri(self: *Self, op: AOp, dst: IPReg, imm: i32) !void {
    const imm8 = maybe_imm8(imm);
    try self.new_inst(@returnAddress());
    try self.rex_wrxb(true, false, false, dst.ext());
    try self.wb(if (imm8 != null) 0x83 else 0x81);
    try self.modRm(0b11, op.opx(), dst.lowId());
    try if (imm8) |i| self.wbi(i) else self.wd(imm);
}

pub fn movmi(self: *Self, dst: EAddr, src: i32) !void {
    try self.new_inst(@returnAddress());
    try self.rex_wrxb(true, false, dst.x(), dst.b());
    try self.wb(0xc7); // MOV \rm, imm32
    try self.modRmEA(0b000, dst);
    try self.wd(src);
}

pub fn movmi_byte(self: *Self, dst: EAddr, src: u8) !void {
    try self.new_inst(@returnAddress());
    try self.wb(0xc6); // MOV \rm, imm32
    try self.modRmEA(0b000, dst);
    try self.wb(src);
}

// MUL/DIV instructions

// DST = DST * SRC
pub fn imulrr(self: *Self, dst: IPReg, src: IPReg) !void {
    try self.new_inst(@returnAddress());
    try self.rex_wrxb(true, dst.ext(), false, src.ext());
    try self.wb(0x0f); // IMUL reg, \rm
    try self.wb(0xaf);
    try self.modRm(0b11, dst.lowId(), src.lowId());
}

// DST = SRC * imm
pub fn imulrri(self: *Self, dst: IPReg, src: IPReg, factor: i32) !void {
    try self.new_inst(@returnAddress());
    try self.rex_wrxb(true, dst.ext(), false, src.ext());
    const small_factor: ?i8 = maybe_imm8(factor);

    try self.wb(if (small_factor) |_| 0x6b else 0x69); // IMUL reg, \rm, ib/id
    try self.modRm(0b11, dst.lowId(), src.lowId());
    if (small_factor) |f| {
        try self.wbi(f);
    } else {
        try self.wd(factor);
    }
}

// RDX:RAX = RAX * SRC
pub fn mulr(self: *Self, src: IPReg) !void {
    try self.new_inst(@returnAddress());
    try self.rex_wrxb(true, false, false, src.ext());
    try self.wb(0xf7); // MUL \rm
    try self.modRm(0b11, 4, src.lowId());
}

// bitshift instructions

// shift with immediate count. use shorthand version when count==1
pub fn sh_ri(self: *Self, dst: IPReg, op: ShiftOp, count: u8) !void {
    try self.rex_wrxb(true, false, false, dst.ext());
    try self.wb(if (count == 1) 0xD1 else 0xC1); // Sxx \rm, 1
    try self.modRm(0b11, op.to_rm(), dst.lowId());
    try if (count != 1) self.wb(count);
}

// VEX instructions
// note: for now we use VEX for all xmm/ymm operations.
// old school SSE forms might be shorter for some 128/scalar ops?

pub inline fn vop_rr(self: *Self, op: u8, fmode: FMode, dst: u4, src1: u4, src2: u4) !void {
    try self.new_inst(@returnAddress());
    try self.vex0fwig(dst > 7, false, src2 > 7, src1, fmode.l(), fmode.pp());
    try self.wb(op);
    try self.modRm(0b11, @truncate(dst), @truncate(src2));
}

pub inline fn vop_rm(self: *Self, op: u8, fmode: FMode, reg: u4, vreg: u4, ea: EAddr) !void {
    try self.new_inst(@returnAddress());
    try self.vex0fwig(reg > 7, ea.x(), ea.b(), vreg, fmode.l(), fmode.pp());
    try self.wb(op);
    try self.modRmEA(@truncate(reg), ea);
}

// dst[low] = src2[low]; dst[high] = src[high]
pub fn vmov2(self: *Self, fmode: FMode, dst: u4, src1: u4, src2: u4) !void {
    if (!fmode.scalar()) {
        return error.InvalidFMode;
    }
    try self.vop_rr(0x10, fmode, dst, src1, src2);
}

// pseudo-instruction for moving register
// vmovsd xmm1, xmm1, xmm2
// vmovupd xmm1, xmm2
pub fn vmovf(self: *Self, fmode: FMode, dst: u4, src: u4) !void {
    try self.vop_rr(0x10, fmode, dst, if (fmode.scalar()) dst else 0, src);
}

pub fn vmovurm(self: *Self, fmode: FMode, dst: u4, src: EAddr) !void {
    try self.vop_rm(0x10, fmode, dst, 0, src);
}

pub fn vmovumr(self: *Self, fmode: FMode, dst: EAddr, src: u4) !void {
    try self.vop_rm(0x11, fmode, src, 0, dst);
}

pub fn vmovarm(self: *Self, fmode: FMode, dst: u4, src: EAddr) !void {
    // scalar load/store cannot use vmova* encoding, so don't
    const op: u8 = if (fmode.scalar()) 0x10 else 0x28;
    try self.vop_rm(op, fmode, dst, 0, src);
}

pub fn vmovamr(self: *Self, fmode: FMode, dst: EAddr, src: u4) !void {
    const op: u8 = if (fmode.scalar()) 0x11 else 0x29;
    try self.vop_rm(op, fmode, src, 0, dst);
}

// note: not all fmodes makes sense (and pd2 is not mentioned in the manual)
pub fn vbroadcast(self: *Self, fmode: FMode, dst: u4, src: EAddr) !void {
    try self.new_inst(@returnAddress());
    try self.vex3(false, dst > 7, src.x(), src.b(), .h0F38, 0, fmode.l(), .h66);
    try self.wb(if (fmode.double()) 0x19 else 0x18);
    try self.modRmEA(@truncate(dst), src);
}

pub fn vmathf(self: *Self, op: VMathOp, fmode: FMode, dst: u4, src1: u4, src2: u4) !void {
    try self.vop_rr(0x58 + op.off(), fmode, dst, src1, src2);
}

pub fn vmathfrm(self: *Self, op: VMathOp, fmode: FMode, dst: u4, src1: u4, src2: EAddr) !void {
    try self.vop_rm(0x58 + op.off(), fmode, dst, src1, src2);
}

pub fn vcmpf(self: *Self, op: VCmpOp, fmode: FMode, dst: u4, src1: u4, src2: u4) !void {
    try self.vop_rr(0xC2, fmode, dst, src1, src2);
    try self.wb(op.val());
}

pub fn vcmpfrm(self: *Self, op: VCmpOp, fmode: FMode, dst: u4, src1: u4, src2: u4) !void {
    try self.vop_rr(0xC2, fmode, dst, src1, src2);
    try self.wb(op.val());
}

//conversion instructions

pub fn vcvtsi2s_rr(self: *Self, fmode: FMode, dst: u4, w: bool, src2: IPReg) !void {
    if (!fmode.scalar()) return error.InvalidFMode;
    // TODO: we get one free mix as part of the instruction, currently ignored
    // i e, we only do vcvtsi2sd xmmX, xmmX, rYY for the same X
    const src1 = dst;
    try self.vex0f(false, dst > 7, w, src2.ext(), src1, false, fmode.pp());
    try self.wb(0x2A);
    try self.modRm(0b11, @truncate(dst), src2.lowId());
}

pub fn vcvtsx2si_rr(self: *Self, fmode: FMode, w: bool, dst: IPReg, src: u4) !void {
    if (!fmode.scalar()) return error.InvalidFMode;
    try self.vex0f(false, dst.ext(), w, src > 8, 0, false, fmode.pp());
    try self.wb(0x2D);
    try self.modRm(0b11, dst.lowId(), @truncate(src));
}

// integer vector instructions
pub inline fn vop_i_rr(self: *Self, op: u8, wide: bool, pp: PP, dst: u4, src1: u4, src2: u4) !void {
    try self.new_inst(@returnAddress());
    try self.vex0fwig(dst > 7, false, src2 > 7, src1, wide, pp);
    try self.wb(op);
    try self.modRm(0b11, @truncate(dst), @truncate(src2));
}

pub inline fn vop_i_rm(self: *Self, op: u8, wide: bool, pp: PP, reg: u4, vreg: u4, ea: EAddr) !void {
    try self.new_inst(@returnAddress());
    try self.vex0fwig(reg > 7, ea.x(), ea.b(), vreg, wide, pp);
    try self.wb(op);
    try self.modRmEA(@truncate(reg), ea);
}

pub fn vmovdq(self: *Self, wide: bool, dst: u4, src: u4) !void {
    try self.vop_i_rr(0x6f, wide, .h66, dst, 0, src);
}

pub fn vmovdqarm(self: *Self, wide: bool, dst: u4, src: EAddr) !void {
    try self.vop_i_rm(0x6f, wide, .h66, dst, 0, src);
}

pub fn vmovdqamr(self: *Self, wide: bool, dst: EAddr, src: u4) !void {
    try self.vop_i_rm(0x7f, wide, .h66, src, 0, dst);
}

pub fn vmovdqurm(self: *Self, wide: bool, dst: u4, src: EAddr) !void {
    try self.vop_i_rm(0x6f, wide, .F3, dst, 0, src);
}

pub fn vmovdqumr(self: *Self, wide: bool, dst: EAddr, src: u4) !void {
    try self.vop_i_rm(0x7f, wide, .F3, src, 0, dst);
}

pub fn vaddi(self: *Self, wide: bool, imode: ISize, dst: u4, src1: u4, src2: u4) !void {
    const op = if (imode == .quadword) 0xD4 else (0xFC + @as(u8, @intFromEnum(imode)));
    try self.vop_i_rr(op, wide, .h66, dst, src1, src2);
}

pub fn vzeroupper(self: *Self) !void {
    try self.vex2(false, 0, false, PP.none);
    try self.wb(0x77);
}

pub fn vzeroall(self: *Self) !void {
    try self.vex2(false, 0, true, PP.none);
    try self.wb(0x77);
}

// BMI instructions: GPR operations coded with VEX

pub inline fn bmi_rr(self: *Self, op: u8, wide: bool, pp: PP, dst: IPReg, src1: IPReg, src2: IPReg) !void {
    try self.new_inst(@returnAddress());
    try self.vex3(wide, dst.ext(), src1.ext(), false, .h0F38, src2.id(), false, pp);
    try self.wb(op);
    try self.modRm(0b11, dst.lowId(), src1.lowId());
}

pub fn sx(self: *Self, op: ShiftOp, dst: IPReg, src1: IPReg, src2: IPReg) !void {
    try self.bmi_rr(0xf7, true, op.to_pp(), dst, src1, src2);
}

// output functions

pub fn dbg_nasm(self: *Self, allocator: Allocator) !void {
    var nasm = std.ChildProcess.init(&[_][]const u8{ "ndisasm", "-b", "64", "-" }, allocator);
    // defer nasm.deinit();
    nasm.stdin_behavior = .Pipe;
    _ = try std.io.getStdOut().write("\n");
    try nasm.spawn();
    _ = try nasm.stdin.?.write(self.code.buf.items);
    _ = nasm.stdin.?.close();
    nasm.stdin = null;
    _ = try nasm.wait();
}
