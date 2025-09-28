// declarations common to all archs

pub const DebugOptions = struct {
    dbg_raw_ir: bool = false,
    dbg_raw_reorder_ir: bool = false,
    dbg_ssa_ir: bool = false,
    dbg_analysed_ir: bool = false,
    dbg_exclude_trivial_put: bool = false,
    dbg_disasm: bool = false,
    dbg_vregs: bool = false,
    dbg_trap: bool = false,
    dbg_disasm_ir: bool = false,
    dbg_regmap: bool = false,
    dbg_osha: bool = false,
    dbg_trap_join_nodes: bool = false, // trap every loop header and join
};
// TODO: set these for tests, somehow..
pub const debug_options = if (!builtin.is_test) &@import("root").options else &DebugOptions{};

// size for integer operations
pub const ISize = enum(u2) {
    byte,
    word,
    dword,
    quadword,

    pub fn wide(self: ISize) bool {
        return self == .quadword;
    }
};

// TODO: integrate properly with SpecType
pub const ValType = enum(u4) {
    intptr = 0,
    avxval,

    pub fn spec(self: @This()) u4 {
        return @intFromEnum(self);
    }
};

// union of integer sizes and Fmode
// not very well thought out but works for now to merge the intptry (eiri) and AVX:y aspects of FLIR
// currently u4 is enough but expand later?
pub const SpecType = union(ValType) {
    intptr: ISize,
    avxval: X86Asm.FMode,
    const INT_SPEC_OFF: u4 = 8;
    pub fn val_type(self: @This()) ValType {
        return if (@intFromEnum(self) >= INT_SPEC_OFF) .intptr else .avxval;
    }
    pub fn from(val: u8) SpecType {
        if (val >= INT_SPEC_OFF) {
            return .{ .intptr = @enumFromInt(val - INT_SPEC_OFF) };
        } else {
            return .{ .avxval = @enumFromInt(val) };
        }
    }
    pub fn into(self: SpecType) u5 {
        return switch (self) {
            .intptr => |i| INT_SPEC_OFF + @intFromEnum(i),
            .avxval => |a| @intFromEnum(a),
        };
    }
};

pub const IPReg = enum(u4) {
    _,
    pub fn id(self: IPReg) u4 {
        return @intFromEnum(self);
    }
};

pub const IntUnOp = enum(u5) {
    popcount,
    ctz,
    clz,
    sign_extend8,
    sign_extend16,
    sign_extend32,

    pub fn is_bitop(self: IntUnOp) bool {
        return @intFromEnum(self) < 3;
    }

    pub fn is_sign_extend(self: IntUnOp) ?ISize {
        return switch (self) {
            .sign_extend8 => .byte,
            .sign_extend16 => .word,
            .sign_extend32 => .dword,
            else => null,
        };
    }
};

// could be u6 but then we need special spec packing (2+6)
pub const IntBinOp = enum(u5) {
    // SILLY: these must have the same order as the 15 basic WASM int binops:
    add,
    sub,
    mul,
    sdiv, // signed division
    udiv, // unsigned division
    srem, // signed reminder
    urem, // unsigned reminder
    @"and", // det finns en and
    @"or",
    xor,
    shl,
    sar,
    shr,
    rotl,
    rotr,
    // but afterwards we can add more (IF WE HAD ANY):

    pub fn symmetric(self: IntBinOp) bool {
        return switch (self) {
            .add, .@"or", .@"and", .xor, .mul => true,
            else => false,
        };
    }

    pub fn asAOP(self: IntBinOp) ?X86Asm.AOp {
        return switch (self) {
            .add => .add,
            .sub => .sub,
            .@"or" => .bor,
            .@"and" => .band,
            .xor => .xor,
            else => null,
        };
    }

    pub fn asShift(self: IntBinOp) ?X86Asm.ShiftOp {
        return switch (self) {
            .shl => .hl,
            .sar => .ar,
            .shr => .hr,
            else => null,
        };
    }

    pub fn asBpfAluOp(self: IntBinOp) ?BPF.Insn.AluOp {
        return switch (self) {
            .add => .add,
            .sub => .sub,
            .mul => .mul,
            .@"or" => .alu_or,
            .@"and" => .alu_and,
            .xor => .xor,
            .shl => .lsh,
            .sar => .arsh,
            .shr => .rsh,
            .sdiv => .div, // signed, unsigned, schmigned
            .udiv => .div,
            .srem, .urem, .rotl, .rotr => null,
        };
    }
};

pub const IntCond = enum(u4) {
    // same here: very wasm (except eqz)
    eq,
    neq,
    lt,
    b, // beloved: unsigned less than
    gt,
    a, // abode: unsigned greather than
    le,
    na, // uN-Atainable: unsigned less-equal
    ge,
    nb, // non-bidenary: unsigned greater-equal

    pub fn asX86Cond(self: IntCond) ?X86Asm.Cond {
        return switch (self) {
            .eq => .e,
            .neq => .ne,
            .gt => .g,
            .ge => .nl,
            .lt => .l,
            .le => .ng,
            .a => .a,
            .na => .na,
            .b => .b,
            .nb => .nb,
        };
    }

    pub fn asBpfJmpOp(self: IntCond) ?BPF.Insn.JmpOp {
        return switch (self) {
            .eq => .jeq,
            .neq => .jne,
            .gt => .jgt,
            .ge => .jge,
            .lt => .jlt,
            .le => .jle,
            else => null,
        };
    }

    pub fn off(self: IntCond) u4 {
        return @intFromEnum(self);
    }

    pub fn invert(self: IntCond) IntCond {
        return switch (self) {
            .eq => .neq,
            .neq => .eq,
            .gt => .le,
            .ge => .lt,
            .lt => .ge,
            .le => .gt,
            .a => .na,
            .na => .a,
            .b => .nb,
            .nb => .b,
        };
    }

    // after peeping "cmp X, Y" into "cmp Y, X"
    pub fn argswap(self: IntCond) IntCond {
        return switch (self) {
            .eq => .eq,
            .neq => .neq,
            .gt => .lt,
            .lt => .gt,
            .ge => .le,
            .le => .ge,
            .a => .b,
            .b => .a,
            .na => .nb,
            .nb => .na,
        };
    }
};

pub const MemoryIntrinsic = enum(u8) {
    memset, // dest [rsi], what [rax], count [rcx]
};

pub const IPMCVal = union(enum) {
    ipreg: IPReg,
    constval: i32, //sign extended in 64-bit context
    frameslot: u8,
    // both these adresses constant data stored in memory
    // "constref" implies loading the actual value
    // while "constptr" loads a pointer to the constant memory
    constref: u16,
    constptr: u16,
    // frameslot_ptr: u8,

    // TODO: this is not a builtin? (or maybe meta)
    pub fn as_ipreg(self: IPMCVal) ?IPReg {
        return switch (self) {
            .ipreg => |reg| reg,
            else => null,
        };
    }
};

// op1 is always a ref, use a const ref when a constant is required
pub const CallKind = enum(u8) {
    /// op1 is relative start of pointer, must be a constant
    near,
    /// op1 is function pointer
    fun_ptr,
    /// platform dependent. syscall index in op1
    /// directly encodes the linux syscall number of the target
    syscall,
    /// op1: bpf helper index
    bpf_helper,
    /// spec is like memtype, op1 is index into MemoryIntrinsic
    memory_intrinsic,
};

pub const MCKind = enum(u8) {
    // not yet allocated, or Inst that trivially produces no value
    unallocated_raw = 0,
    // general purpose register like rax, r12, etc
    ipreg,
    // SSE/AVX registers, ie xmm0/ymm0-15
    vfreg,

    // unallocated, but has a ipreg hint
    unallocated_ipreghint,
    // unallocated, but has a vfreg hint
    unallocated_vfreghint,

    // TODO: support non-uniform sizes of spilled value
    frameslot,
    // unused value, perhaps should have been deleted before alloc
    dead,
    // not stored as such, will be emitted togheter with the next inst
    // example "lea" and then "store", or "load" and then ibinop/vmath
    fused,

    constval, // constvals[i] interpreted as "data"
    constptr, // pointer to a constvals[i] emitted value

    pub fn unallocated(self: MCKind) bool {
        return switch (self) {
            .unallocated_raw => true,
            .unallocated_ipreghint => true,
            .unallocated_vfreghint => true,
            else => false,
        };
    }
};

const builtin = @import("builtin");
const std = @import("std");
const BPF = std.os.linux.BPF;
const X86Asm = @import("./X86Asm.zig");
