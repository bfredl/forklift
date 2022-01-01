const std = @import("std");
const mem = std.mem;
const os = std.os;
const debug = std.debug;
const fs = std.fs;
const Allocator = mem.Allocator;
const page_allocator = std.heap.page_allocator;
const ArrayList = std.ArrayList;
const ArrayListAligned = std.ArrayListAligned;

const builtin = @import("builtin");
const s2 = builtin.zig_is_stage2;

code: if (s2) []u8 else ArrayListAligned(u8, 4096),

s2_pos: if (s2) usize else void,

/// offset of each encoded instruction. Might not be needed
/// but useful for debugging.
inst_off: if (s2) void else ArrayList(u32),
inst_dbg: if (s2) void else ArrayList(usize),

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

    fn opx(self: @This()) u3 {
        return @enumToInt(self);
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

pub const VCmp = enum(u5) {
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
    @"false",
    neq_oq,
    ge,
    gt,
    @"true",
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
    fn val(self: @This()) u8 {
        return @as(u8, @enumToInt(self));
    }
};

const PP = enum(u2) {
    none,
    h66,
    F3,
    F2,
    fn val(self: @This()) u8 {
        return @as(u8, @enumToInt(self));
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
        return @intToEnum(PP, @truncate(u2, @enumToInt(self)));
    }

    fn l(self: @This()) bool {
        return @enumToInt(self) >= 4;
    }

    fn scalar(self: @This()) bool {
        return self == @This().ss or self == @This().sd;
    }
};

pub const IMode = enum(u2) {
    b,
    w,
    d,
    q,

    fn off(self: @This()) u8 {
        return @as(u8, @enumToInt(self));
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
        return @as(u8, @enumToInt(self));
    }
};

// cannot Error!Struct because:
// error: TODO coerce_result_ptr wrap_errunion_payload
pub fn init_stage2() Self {
    if (!s2) {
        return init(page_allocator) catch unreachable;
    }
    return Self{
        .code = page_allocator.alloc(u8, 4096) catch unreachable,
        .inst_off = {},
        .inst_dbg = {},
        .s2_pos = 0,
    };
}

pub fn init(allocator: Allocator) !Self {
    // TODO: allocate consequtive mprotectable pages
    return Self{
        .code = try ArrayListAligned(u8, 4096).initCapacity(page_allocator, 4096),
        .inst_off = ArrayList(u32).init(allocator),
        .inst_dbg = ArrayList(usize).init(allocator),
        .s2_pos = {},
    };
}

pub fn deinit(self: *Self) void {
    // TODO: only in debug mode (as clobbers the array, needs r/w)
    os.mprotect(self.code.items.ptr[0..self.code.capacity], os.PROT.READ | os.PROT.WRITE) catch unreachable;
    self.code.deinit();
    self.inst_off.deinit();
    self.inst_dbg.deinit();
}

fn new_inst(self: *Self, addr: usize) !void {
    if (!s2) {
        var size = @intCast(u32, self.code.items.len);
        try self.inst_off.append(size);
        try self.inst_dbg.append(addr);
    }
}

// TODO: use appendAssumeCapacity in a smart way like arch/x86_64
pub fn wb(self: *Self, opcode: u8) !void {
    if (s2) {
        self.code[self.s2_pos] = opcode;
        self.s2_pos += 1;
    } else {
        try self.code.append(opcode);
    }
}

fn wbi(self: *Self, imm: i8) !void {
    try self.wb(@bitCast(u8, imm));
}

fn wd(self: *Self, dword: i32) !void {
    if (s2) {
        std.mem.writeIntLittle(i32, self.code[self.s2_pos..][0..4], dword);
        self.s2_pos += 4;
    } else {
        std.mem.writeIntLittle(i32, try self.code.addManyAsArray(4), dword);
    }
}

fn wq(self: *Self, qword: u64) !void {
    if (s2) {
        std.mem.writeIntLittle(u64, self.code[self.s2_pos..][0..8], qword);
        self.s2_pos += 4;
    } else {
        std.mem.writeIntLittle(u64, try self.code.addManyAsArray(8), qword);
    }
}

fn set_align(self: *Self, alignment: u32) !void {
    var residue = self.get_target() & (alignment - 1);
    var padding = alignment - residue;
    if (padding != 0 and padding != alignment) {
        try self.code.appendNTimes(0x90, padding);
    }
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
    return .{ .base = null, .offset = @bitCast(i32, offset) };
}

// index quadword array
pub fn qi(base: IPReg, index: IPReg) EAddr {
    // stage2: lol no default fields
    // return .{ .base = base, .index = index, .scale = 3 };
    return .{ .base = base, .index = index, .scale = 3, .offset = 0 };
}

pub fn bi(base: IPReg, index: IPReg) EAddr {
    return .{ .base = base, .index = index, .scale = 0 };
}

pub fn maybe_imm8(imm: i32) ?i8 {
    var imm8 = @truncate(i8, imm);
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
        try self.wd(ea.offset - (@bitCast(i32, self.get_target()) + 4));
    } else if (mod != 0b00) {
        // TODO: stage2
        // try if (offset8) |off| self.wbi(off) else self.wd(ea.offset);
        if (offset8) |off| (try self.wbi(off)) else (try self.wd(ea.offset));
    }
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
    // TODO: stage2
    // try if (x or b) self.vex3(false, r, x, b, 1, vv, l, pp) else self.vex2(r, vv, l, pp);
    if (x or b) (try self.vex3(false, r, x, b, 1, vv, l, pp)) else (try self.vex2(r, vv, l, pp));
}

// control flow
pub fn ret(self: *Self) !void {
    try self.new_inst(@returnAddress());
    try self.wb(0xC3);
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

// there..
pub fn jfwd(self: *Self, cond: Cond) !u32 {
    try self.new_inst(@returnAddress());
    try self.wb(0x70 + cond.off());
    var pos = @intCast(u32, self.code.items.len);
    try self.wb(0x00); // placeholder
    return pos;
}

pub fn set_target(self: *Self, pos: u32) !void {
    var off = self.get_target() - (pos + 1);
    if (off > 0x7f) {
        return error.InvalidNearJump;
    }
    self.code.items[pos] = @intCast(u8, off);
}

pub fn set_lea_target(self: *Self, pos: u32) void {
    var off = self.get_target() - (pos + 4);
    self.code.items[pos] = @intCast(u8, off);
    std.mem.writeIntLittle(u32, self.code.items[pos..][0..4], off);
}

pub fn get_target(self: *Self) u32 {
    if (s2) {
        return @intCast(u32, self.s2_pos);
    } else {
        return @intCast(u32, self.code.items.len);
    }
}

// .. and back again
pub fn jbck(self: *Self, cond: Cond, target: u32) !void {
    if (s2) {
        var off = @intCast(i32, target) - (@intCast(i32, self.s2_pos) + 2);
        try self.wb(0x0f);
        try self.wb(0x80 + cond.off());
        try self.wd(off - 4); // FETING: offset is larger as the jump instruction is larger
    } else {
        try self.new_inst(@returnAddress());
        var off = @intCast(i32, target) - (@intCast(i32, self.code.items.len) + 2);
        if (maybe_imm8(off)) |off8| {
            try self.wb(0x70 + cond.off());
            try self.wbi(off8);
        } else {
            try self.wb(0x0f);
            try self.wb(0x80 + cond.off());
            try self.wd(off - 4); // FETING: offset is larger as the jump instruction is larger
        }
    }
}

// stack management

fn push(self: *Self, src: IPReg) !void {
    // luring: 64-bit wide is already the default,
    // extension only needed for r8-r15 registers
    try self.rex_wrxb(false, false, false, src.ext());
    try self.wb(0x50 + @as(u8, src.lowId()));
}

fn pop(self: *Self, dst: IPReg) !void {
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

pub fn movmr(self: *Self, dst: EAddr, src: IPReg) !void {
    try self.op_rm(0x89, src, dst); // MOV \rm, reg
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
    const pos = self.get_target();
    try self.wd(0); // placeholder
    return pos;
}

pub fn movri(self: *Self, dst: IPReg, src: i32) !void {
    try self.new_inst(@returnAddress());
    // TODO: w bit should be avoidable in a lot of cases
    // like "mov rax, 1337" is equivalent to "mov eax, 1337"
    try self.rex_wrxb(true, dst.ext(), false, false);
    try self.wb(0xc7); // MOV \rm, imm32
    try self.modRm(0b11, 0b000, dst.lowId());
    try self.wd(src);
}

pub fn aritri(self: *Self, op: AOp, dst: IPReg, imm: i32) !void {
    if (s2) {
        try self.new_inst(@returnAddress());
        try self.rex_wrxb(true, dst.ext(), false, false);
        try self.wb(0x81);
        try self.modRm(0b11, op.opx(), dst.lowId());
        try self.wd(imm);
    } else {
        const imm8 = maybe_imm8(imm);
        try self.new_inst(@returnAddress());
        try self.rex_wrxb(true, dst.ext(), false, false);
        try self.wb(if (imm8 != null) 0x83 else 0x81);
        try self.modRm(0b11, op.opx(), dst.lowId());
        try if (imm8) |i| self.wbi(i) else self.wd(imm);
    }
}

pub fn movmi(self: *Self, dst: EAddr, src: i32) !void {
    try self.new_inst(@returnAddress());
    try self.rex_wrxb(true, false, dst.x(), dst.b());
    try self.wb(0xc7); // MOV \rm, imm32
    try self.modRmEA(0b000, dst);
    try self.wd(src);
}

// VEX instructions
// note: for now we use VEX for all xmm/ymm operations.
// old school SSE forms might be shorter for some 128/scalar ops?

pub inline fn vop_rr(self: *Self, op: u8, fmode: FMode, dst: u4, src1: u4, src2: u4) !void {
    try self.new_inst(@returnAddress());
    try self.vex0fwig(dst > 7, false, src2 > 7, src1, fmode.l(), fmode.pp());
    try self.wb(op);
    try self.modRm(0b11, @truncate(u3, dst), @truncate(u3, src2));
}

pub inline fn vop_rm(self: *Self, op: u8, fmode: FMode, reg: u4, vreg: u4, ea: EAddr) !void {
    try self.new_inst(@returnAddress());
    try self.vex0fwig(reg > 7, ea.x(), ea.b(), vreg, fmode.l(), fmode.pp());
    try self.wb(op);
    try self.modRmEA(@truncate(u3, reg), ea);
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
    try self.vop_rm(0x28, fmode, dst, 0, src);
}

pub fn vmovamr(self: *Self, fmode: FMode, dst: EAddr, src: u4) !void {
    try self.vop_rm(0x29, fmode, src, 0, dst);
}
pub fn vmathf(self: *Self, op: VMathOp, fmode: FMode, dst: u4, src1: u4, src2: u4) !void {
    try self.vop_rr(0x58 + op.off(), fmode, dst, src1, src2);
}

pub fn vmathfrm(self: *Self, op: VMathOp, fmode: FMode, dst: u4, src1: u4, src2: EAddr) !void {
    try self.vop_rm(0x58 + op.off(), fmode, dst, src1, src2);
}

pub fn vcmp(self: *Self, op: VCmp, fmode: FMode, dst: u4, src1: u4, src2: EAddr) !void {
    if (fmode.scalar()) {
        return error.FEEEEL; // TODO:probably does something useful for scalars?
    }
    try self.vop_rr(0xC2, fmode, dst, src1, src2);
    try self.wb(op.val());
}

// integer vector instructions
pub inline fn vop_i_rr(self: *Self, op: u8, wide: bool, dst: u4, src1: u4, src2: u4) !void {
    try self.new_inst(@returnAddress());
    try self.vex0fwig(dst > 7, false, src2 > 7, src1, wide, .h66);
    try self.wb(op);
    try self.modRm(0b11, @truncate(u3, dst), @truncate(u3, src2));
}

pub inline fn vop_i_rm(self: *Self, op: u8, wide: bool, pp: PP, reg: u4, vreg: u4, ea: EAddr) !void {
    try self.new_inst(@returnAddress());
    try self.vex0fwig(reg > 7, ea.x(), ea.b(), vreg, wide, pp);
    try self.wb(op);
    try self.modRmEA(@truncate(u3, reg), ea);
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

pub fn vaddi(self: *Self, wide: bool, imode: IMode, dst: u4, src1: u4, src2: u4) !void {
    const op = if (imode == .q) 0xD4 else (0xFC + imode.off());
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
    if (s2) {
        if (os.linux.mprotect(self.code.ptr, self.code.len, os.PROT.READ | os.PROT.EXEC) != 0) {
            return error.ComputarSaysNo;
        }
    } else {
        try os.mprotect(self.code.items.ptr[0..self.code.capacity], os.PROT.READ | os.PROT.EXEC);
    }
}

pub fn get_ptr(self: *Self, target: u32, comptime T: type) T {
    if (s2) {
        return @ptrCast(T, self.code[target..].ptr);
    } else {
        return @ptrCast(T, self.code.items[target..].ptr);
    }
}

pub fn test_call2(self: *Self, arg1: usize, arg2: usize) !usize {
    try self.finalize();
    const FunPtr = fn (arg1: usize, arg2: usize) callconv(.C) usize;
    return self.get_ptr(0, FunPtr)(arg1, arg2);
}

pub fn test_call2f64(self: *Self, arg1: f64, arg2: f64) !f64 {
    try self.finalize();
    const FunPtr = fn (arg1: f64, arg2: f64) callconv(.C) f64;
    return self.get_ptr(0, FunPtr)(arg1, arg2);
}

pub fn test_call2x(self: *Self, comptime T: type, arg1: anytype, arg2: anytype) !T {
    try self.finalize();
    const FunPtr = fn (arg1: @TypeOf(arg1), arg2: @TypeOf(arg2)) callconv(.C) T;
    return self.get_ptr(0, FunPtr)(arg1, arg2);
}

const test_allocator = std.testing.allocator;
const expectEqual = std.testing.expectEqual;

// for quick debugging change ret to retnasm
pub fn retnasm(self: *Self) !void {
    try self.ret();
    try self.dbg_nasm(test_allocator);
}

pub fn dbg_test(self: *Self) !void {
    const stderr = std.io.getStdErr().writer();
    const dbginfo = try debug.getSelfDebugInfo();
    const tty_config = debug.detectTTYConfig();
    for (self.inst_dbg.items) |x, i| {
        debug.print("{} {}\n", .{ i, x });
        try debug.printSourceAtAddress(dbginfo, stderr, x, tty_config);
    }
}

pub fn lookup(self: *Self, addr: usize) usize {
    const startaddr: usize = @ptrToInt(self.code.items.ptr);
    const endaddr: usize = startaddr + self.code.items.len;
    if (startaddr <= addr and addr < endaddr) {
        const off = addr - startaddr;
        for (self.inst_dbg.items) |x, i| {
            if (i + 1 >= self.inst_off.items.len or off < self.inst_off.items[i + 1]) {
                return x;
            }
        }
    }
    return addr;
}

test "return first argument" {
    var cfo = try init(test_allocator);
    defer cfo.deinit();

    try cfo.mov(.rax, .rdi);
    try cfo.ret();
    try expectEqual(@as(usize, 4), try cfo.test_call2(4, 10));
}

test "return second argument" {
    var cfo = try init(test_allocator);
    defer cfo.deinit();

    try cfo.mov(.rax, .rsi);
    try cfo.ret();
    try expectEqual(@as(usize, 10), try cfo.test_call2(4, 10));
}

test "read/write first arg as 64-bit pointer" {
    var cfo = try init(test_allocator);
    defer cfo.deinit();

    try cfo.movrm(.rax, a(.rdi));
    try cfo.movmr(a(.rdi), .rsi);
    try cfo.ret();

    var someint: u64 = 33;
    var retval = try cfo.test_call2(@ptrToInt(&someint), 10);
    try expectEqual(@as(usize, 33), retval);
    try expectEqual(@as(usize, 10), someint);
}

test "read/write first arg as 64-bit pointer with offsett" {
    var cfo = try init(test_allocator);
    defer cfo.deinit();

    try cfo.movrm(.rax, bo(.rdi, 0x08));
    try cfo.movmr(bo(.rdi, 0x10), .rsi);
    try cfo.ret();

    var someint: [2]u64 = .{ 33, 45 };
    var retval = try cfo.test_call2(@ptrToInt(&someint) - 8, 79);
    try expectEqual(@as(usize, 33), retval);
    try expectEqual(@as(usize, 33), someint[0]);
    try expectEqual(@as(usize, 79), someint[1]);
}

test "RIP-relative read" {
    var cfo = try init(test_allocator);
    defer cfo.deinit();
    try cfo.wq(0x9090909090909090);
    const theconst = cfo.get_target();
    try cfo.wq(0x1122334455667788);
    // not needed, but put nasm back in style
    // try cfo.wb(0x00);
    const entry = cfo.get_target();
    try cfo.enter();
    try cfo.movrm(.rax, rel(theconst));
    try cfo.leave();
    try cfo.ret();

    try cfo.finalize();
    const fun = cfo.get_ptr(entry, fn () callconv(.C) u64);
    try expectEqual(@as(u64, 0x1122334455667788), fun());
}

test "lealink" {
    var cfo = try init(test_allocator);
    defer cfo.deinit();

    //const OSHA = @import("./OSHA.zig");
    //OSHA.install(&cfo);
    //defer OSHA.clear();

    const entry = cfo.get_target();
    try cfo.enter();
    const link = try cfo.lealink(.rdx);
    try cfo.movrm(.rax, a(.rdx));
    try cfo.movrm(.rcx, a(.rdx).o(8));
    try cfo.movmr(a(.rdi), .rcx);
    try cfo.leave();
    try cfo.ret();

    try cfo.set_align(8);
    cfo.set_lea_target(link);
    try cfo.wq(0x8822883344114422);
    try cfo.wq(0x0104050610405060);

    try cfo.finalize();
    const fun = cfo.get_ptr(entry, fn (*u64) callconv(.C) u64);
    var somemem: u64 = undefined;
    try expectEqual(@as(u64, 0x8822883344114422), fun(&somemem));
    try expectEqual(@as(u64, 0x0104050610405060), somemem);
}

test "return intermediate value" {
    var cfo = try init(test_allocator);
    defer cfo.deinit();

    try cfo.movri(.rax, 1337);
    try cfo.ret();

    var retval = try cfo.test_call2(7, 8);
    try expectEqual(@as(usize, 1337), retval);
}

test "write intermediate value to 64-bit pointer" {
    var cfo = try init(test_allocator);
    defer cfo.deinit();

    try cfo.movmi(a(.rdi), 586);
    try cfo.ret();

    var someint: u64 = 33;

    _ = try cfo.test_call2(@ptrToInt(&someint), 8);
    try expectEqual(@as(usize, 586), someint);
}

test "use r12 for base address" {
    var cfo = try init(test_allocator);
    defer cfo.deinit();

    // r12 is callee-saved. so save it
    try cfo.mov(.rcx, .r12);
    try cfo.mov(.r12, .rdi);

    try cfo.movmi(a(.r12), 389);

    try cfo.mov(.r12, .rcx);
    try cfo.ret();

    var someint: u64 = 33;

    _ = try cfo.test_call2(@ptrToInt(&someint), 8);
    try expectEqual(@as(usize, 389), someint);
}

test "add arguments" {
    var cfo = try init(test_allocator);
    defer cfo.deinit();

    try cfo.mov(.rax, .rdi);
    try cfo.arit(.add, .rax, .rsi);
    try cfo.ret();

    var retval = try cfo.test_call2(1002, 560);
    try expectEqual(@as(usize, 1562), retval);
}

test "add arguments using lea" {
    var cfo = try init(test_allocator);
    defer cfo.deinit();

    try cfo.lea(.rax, bi(.rdi, .rsi));
    try cfo.ret();

    var retval = try cfo.test_call2(736, 121);
    try expectEqual(@as(usize, 857), retval);
}

test "add scaled arguments using lea" {
    var cfo = try init(test_allocator);
    defer cfo.deinit();

    try cfo.lea(.rax, qi(.rdi, .rsi));
    try cfo.ret();

    var retval = try cfo.test_call2(736, 121);
    try expectEqual(@as(usize, 1704), retval);
}

test "subtract arguments" {
    var cfo = try init(test_allocator);
    defer cfo.deinit();

    try cfo.mov(.rax, .rdi);
    try cfo.arit(.sub, .rax, .rsi);
    try cfo.ret();

    var retval = try cfo.test_call2(1002, 560);
    try expectEqual(@as(usize, 442), retval);
}

test "add imm8 to argument" {
    var cfo = try init(test_allocator);
    defer cfo.deinit();

    try cfo.mov(.rax, .rdi);
    try cfo.aritri(.add, .rax, 64);
    try cfo.ret();

    var retval = try cfo.test_call2(120, 9204);
    try expectEqual(@as(usize, 184), retval);
}

test "add immediate to argument" {
    var cfo = try init(test_allocator);
    defer cfo.deinit();

    try cfo.mov(.rax, .rdi);
    try cfo.aritri(.add, .rax, 137);
    try cfo.ret();

    var retval = try cfo.test_call2(100, 560);
    try expectEqual(@as(usize, 237), retval);
}

test "get the maximum of two args" {
    var cfo = try init(test_allocator);
    defer cfo.deinit();

    try cfo.mov(.rax, .rdi);
    try cfo.arit(.cmp, .rdi, .rsi);
    const jump = try cfo.jfwd(.g);
    try cfo.mov(.rax, .rsi);
    try cfo.set_target(jump);
    try cfo.ret();

    var retval = try cfo.test_call2(1002, 560);
    try expectEqual(@as(usize, 1002), retval);

    retval = try cfo.test_call2(460, 902);
    try expectEqual(@as(usize, 902), retval);
}

test "jump backwards in a loop" {
    var cfo = try init(test_allocator);
    defer cfo.deinit();

    try cfo.arit(.xor, .rax, .rax);
    const loop = cfo.get_target();
    try cfo.arit(.add, .rax, .rdi);
    try cfo.aritri(.sub, .rdi, 1);
    // equal -> zero after the subtraction
    try cfo.jbck(.ne, loop);
    try cfo.ret();

    var retval = try cfo.test_call2(10, 560);
    try expectEqual(@as(usize, 55), retval);

    retval = try cfo.test_call2(20, 560);
    try expectEqual(@as(usize, 210), retval);
}

test "push/pop" {
    var cfo = try init(test_allocator);
    defer cfo.deinit();

    try cfo.push(.rdi);
    try cfo.pop(.r13);
    try cfo.mov(.rax, .r13);

    try cfo.ret();
    var retval = try cfo.test_call2(9009, 560);
    try expectEqual(@as(usize, 9009), retval);
}

test "add scalar double" {
    var cfo = try init(test_allocator);
    defer cfo.deinit();

    try cfo.vmathf(.add, .sd, 0, 0, 1);
    try cfo.ret();

    var retval = try cfo.test_call2f64(2.0, 0.5);
    try expectEqual(@as(f64, 2.5), retval);
}

test "max of scalar double" {
    var cfo = try init(test_allocator);
    defer cfo.deinit();

    try cfo.vmathf(.max, .sd, 0, 0, 1);
    try cfo.ret();

    var retval = try cfo.test_call2f64(2.0, 5.5);
    try expectEqual(@as(f64, 5.5), retval);

    retval = try cfo.test_call2f64(10.0, 8.5);
    try expectEqual(@as(f64, 10.0), retval);
}

test "move scalar double" {
    var cfo = try init(test_allocator);
    defer cfo.deinit();

    try cfo.vmovf(.sd, 0, 1);
    try cfo.ret();

    var retval = try cfo.test_call2f64(22.0, 0.75);
    try expectEqual(@as(f64, 0.75), retval);
}

test "read/write scalar double" {
    var cfo = try init(test_allocator);
    defer cfo.deinit();

    // as we are swapping [rdi] and xmm0, use a temp
    try cfo.vmovurm(.sd, 1, a(.rdi));
    try cfo.vmovumr(.sd, a(.rdi), 0);
    try cfo.vmovf(.sd, 0, 1);
    try cfo.ret();

    var thefloat: f64 = 13.5;

    var retval = try cfo.test_call2x(f64, &thefloat, @as(f64, 0.25));
    try expectEqual(@as(f64, 13.5), retval);
    try expectEqual(@as(f64, 0.25), thefloat);
}

test "read/write aligned double vector" {
    var cfo = try init(test_allocator);
    defer cfo.deinit();

    try cfo.vmovarm(.pd4, 0, a(.rdi));
    try cfo.vmathf(.mul, .pd4, 0, 0, 0);
    try cfo.vmovamr(.pd4, a(.rdi), 0);
    try cfo.ret();

    var thevec: [4]f64 align(32) = .{ 13.5, 25.125, 4552.0, -50.5 };

    try cfo.test_call2x(void, &thevec, @as(u64, 0));
    try expectEqual(@as(f64, 182.25), thevec[0]);
    try expectEqual(@as(f64, 2550.25), thevec[3]);
}

test "add scalar double from memory" {
    var cfo = try init(test_allocator);
    defer cfo.deinit();
    errdefer cfo.dbg_nasm(test_allocator) catch unreachable;

    try cfo.vmathfrm(.add, .sd, 0, 0, a(.rdi));
    try cfo.ret();

    var thefloat: f64 = 6.5;

    var retval = try cfo.test_call2x(f64, &thefloat, @as(f64, 0.125));
    try expectEqual(@as(f64, 6.625), retval);
    try expectEqual(@as(f64, 6.5), thefloat);
}
