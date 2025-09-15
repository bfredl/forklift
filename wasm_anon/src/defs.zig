// type punning is NOT safe, this assumes validaded WASM code
pub const StackValue = extern union {
    i32: i32,
    i64: i64,
    f32: f32,
    f64: f64,
    ref: u32,
    indir: *StackValue,

    pub fn @"u32"(val: StackValue) u32 {
        return @bitCast(val.i32);
    }

    pub fn default(valtype: ValType) ?StackValue {
        return switch (valtype) {
            .i32 => .{ .i32 = 0 },
            .i64 => .{ .i64 = 0 },
            .f32 => .{ .f32 = 0 },
            .f64 => .{ .f64 = 0 },
            .externref => .{ .ref = 0 },
            else => null, // a bit of a type mismatch but whatever
        };
    }
};

pub const WASMError = error{WASMTrap};

pub const SectionKind = enum(u8) {
    custom = 0,
    type = 1,
    import = 2,
    function = 3,
    table = 4,
    memory = 5,
    global = 6,
    export_ = 7,
    start = 8,
    element = 9,
    code = 10,
    data = 11,
    data_count = 12,
    _,
};

pub const ImportExportKind = enum(u8) {
    func = 0,
    table = 1,
    mem = 2,
    global = 3,
};

pub const ValType = enum(u8) {
    void = 0x40,
    i32 = 0x7F,
    i64 = 0x7E,
    f32 = 0x7D,
    f64 = 0x7C,
    vec128 = 0x7B,
    funcref = 0x70,
    externref = 0x6f,
    _,
};

pub const BlockType = union(enum) {
    simple: ValType,
    complex_idx: u32,

    pub fn results(self: BlockType) !u16 {
        return switch (self) {
            .simple => |vt| if (vt == .void) 0 else 1,
            .complex_idx => error.NotImplemented,
        };
    }
};

pub const OpCode = enum(u8) {
    unreachable_ = 0x00,
    nop = 0x01,
    block = 0x02,
    loop = 0x03,
    if_ = 0x04,
    else_ = 0x05,
    end = 0x0B,
    br = 0x0C,
    br_if = 0x0D,
    br_table = 0x0E,
    ret = 0x0F,
    call = 0x10,
    call_indirect = 0x11,

    drop = 0x1A,
    select = 0x1B,
    select_t = 0x1C,

    local_get = 0x20,
    local_set = 0x21,
    local_tee = 0x22,
    global_get = 0x23,
    global_set = 0x24,

    table_get = 0x25,
    table_set = 0x26,

    i32_load = 0x28,
    i64_load = 0x29,
    f32_load = 0x2A,
    f64_load = 0x2B,
    i32_load8_s = 0x2C,
    i32_load8_u = 0x2D,
    i32_load16_s = 0x2E,
    i32_load16_u = 0x2F,
    i64_load8_s = 0x30,
    i64_load8_u = 0x31,
    i64_load16_s = 0x32,
    i64_load16_u = 0x33,
    i64_load32_s = 0x34,
    i64_load32_u = 0x35,

    i32_store = 0x36,
    i64_store = 0x37,
    f32_store = 0x38,
    f64_store = 0x39,
    i32_store8 = 0x3A,
    i32_store16 = 0x3B,
    i64_store8 = 0x3C,
    i64_store16 = 0x3D,
    i64_store32 = 0x3E,

    memory_size = 0x3F,
    memory_grow = 0x40,

    i32_const = 0x41,
    i64_const = 0x42,
    f32_const = 0x43,
    f64_const = 0x44,

    i32_eqz = 0x45,
    i32_eq = 0x46,
    i32_ne = 0x47,
    i32_lt_s = 0x48,
    i32_lt_u = 0x49,
    i32_gt_s = 0x4A,
    i32_gt_u = 0x4B,
    i32_le_s = 0x4C,
    i32_le_u = 0x4D,
    i32_ge_s = 0x4E,
    i32_ge_u = 0x4F,

    i64_eqz = 0x50,
    i64_eq = 0x51,
    i64_ne = 0x52,
    i64_lt_s = 0x53,
    i64_lt_u = 0x54,
    i64_gt_s = 0x55,
    i64_gt_u = 0x56,
    i64_le_s = 0x57,
    i64_le_u = 0x58,
    i64_ge_s = 0x59,
    i64_ge_u = 0x5A,

    f32_eq = 0x5B,
    f32_ne = 0x5C,
    f32_lt = 0x5D,
    f32_gt = 0x5E,
    f32_le = 0x5F,
    f32_ge = 0x60,

    f64_eq = 0x61,
    f64_ne = 0x62,
    f64_lt = 0x63,
    f64_gt = 0x64,
    f64_le = 0x65,
    f64_ge = 0x66,

    i32_clz = 0x67,
    i32_ctz = 0x68,
    i32_popcnt = 0x69,

    i32_add = 0x6A,
    i32_sub = 0x6B,
    i32_mul = 0x6C,
    i32_div_s = 0x6D,
    i32_div_u = 0x6E,
    i32_rem_s = 0x6F,
    i32_rem_u = 0x70,
    i32_and = 0x71,
    i32_or = 0x72,
    i32_xor = 0x73,
    i32_shl = 0x74,
    i32_shr_s = 0x75,
    i32_shr_u = 0x76,
    i32_rotl = 0x77,
    i32_rotr = 0x78,

    i64_clz = 0x79,
    i64_ctz = 0x7A,
    i64_popcnt = 0x7B,

    i64_add = 0x7C,
    i64_sub = 0x7D,
    i64_mul = 0x7E,
    i64_div_s = 0x7F,
    i64_div_u = 0x80,
    i64_rem_s = 0x81,
    i64_rem_u = 0x82,
    i64_and = 0x83,
    i64_or = 0x84,
    i64_xor = 0x85,
    i64_shl = 0x86,
    i64_shr_s = 0x87,
    i64_shr_u = 0x88,
    i64_rotl = 0x89,
    i64_rotr = 0x8A,

    f32_abs = 0x8B,
    f32_neg = 0x8C,
    f32_ceil = 0x8D,
    f32_floor = 0x8E,
    f32_trunc = 0x8F,
    f32_nearest = 0x90,
    f32_sqrt = 0x91,

    f32_add = 0x92,
    f32_sub = 0x93,
    f32_mul = 0x94,
    f32_div = 0x95,
    f32_min = 0x96,
    f32_max = 0x97,
    f32_copysign = 0x98,

    f64_abs = 0x99,
    f64_neg = 0x9A,
    f64_ceil = 0x9B,
    f64_floor = 0x9C,
    f64_trunc = 0x9D,
    f64_nearest = 0x9E,
    f64_sqrt = 0x9F,

    f64_add = 0xA0,
    f64_sub = 0xA1,
    f64_mul = 0xA2,
    f64_div = 0xA3,
    f64_min = 0xA4,
    f64_max = 0xA5,
    f64_copysign = 0xA6,

    i32_wrap_i64 = 0xA7,
    i32_trunc_f32_s = 0xA8,
    i32_trunc_f32_u = 0xA9,
    i32_trunc_f64_s = 0xAA,
    i32_trunc_f64_u = 0xAB,
    i64_extend_i32_s = 0xAC,
    i64_extend_i32_u = 0xAD,
    i64_trunc_f32_s = 0xAE,
    i64_trunc_f32_u = 0xAF,
    i64_trunc_f64_s = 0xB0,
    i64_trunc_f64_u = 0xB1,
    f32_convert_i32_s = 0xB2,
    f32_convert_i32_u = 0xB3,
    f32_convert_i64_s = 0xB4,
    f32_convert_i64_u = 0xB5,
    f32_demote_f64 = 0xB6,
    f64_convert_i32_s = 0xB7,
    f64_convert_i32_u = 0xB8,
    f64_convert_i64_s = 0xB9,
    f64_convert_i64_u = 0xBA,
    f64_promote_f32 = 0xBB,
    i32_reinterpret_f32 = 0xBC,
    i64_reinterpret_f64 = 0xBD,
    f32_reinterpret_i32 = 0xBE,
    f64_reinterpret_i64 = 0xBF,

    i32_extend8_s = 0xC0,
    i32_extend16_s = 0xC1,
    i64_extend8_s = 0xC2,
    i64_extend16_s = 0xC3,
    i64_extend32_s = 0xC4,

    ref_null = 0xD0,

    prefixed = 0xFC,
};

pub const BinOp = enum(u8) {
    add,
    sub,
    mul,
    div_s,
    div_u,
    rem_s,
    rem_u,
    @"and",
    @"or",
    xor,
    shl,
    shr_s,
    shr_u,
    rotl,
    rotr,
};

pub const RelOp = enum(u8) {
    eq,
    ne,
    lt_s,
    lt_u,
    gt_s,
    gt_u,
    le_s,
    le_u,
    ge_s,
    ge_u,
};

pub fn memtype(comptime op: OpCode) type {
    return switch (op) {
        .i32_load => i32,
        .i64_load => i64,
        .f32_load => f32,
        .f64_load => f64,
        .i32_load8_s => i8,
        .i32_load8_u => u8,
        .i32_load16_s => i16,
        .i32_load16_u => u16,
        .i64_load8_s => i8,
        .i64_load8_u => u8,
        .i64_load16_s => i16,
        .i64_load16_u => u16,
        .i64_load32_s => i32,
        .i64_load32_u => u32,

        .i32_store => i32,
        .i64_store => i64,
        .f32_store => f32,
        .f64_store => f64,
        .i32_store8 => i8,
        .i32_store16 => i16,
        .i64_store8 => i8,
        .i64_store16 => i16,
        .i64_store32 => i32,
        else => unreachable,
    };
}

pub const Category = enum {
    i32_unop,
    i32_binop,
    i32_relop,
    i64_unop,
    i64_binop,
    i64_relop,
    f32_unop,
    f32_binop,
    f32_relop,
    f64_unop,
    f64_binop,
    f64_relop,
    convert,
    load,
    store,
    other,
};

// to start with this is only used at comptime
pub fn category(comptime op: OpCode) Category {
    const numval = @intFromEnum(op);
    if (numval >= 0x28 and numval <= 0x35) return .load;
    if (numval >= 0x36 and numval <= 0x3E) return .store;
    if (numval >= 0x46 and numval <= 0x4F) return .i32_relop;
    if (numval >= 0x51 and numval <= 0x5A) return .i64_relop;
    if (numval >= 0x5B and numval <= 0x60) return .f32_relop;
    if (numval >= 0x61 and numval <= 0x66) return .f64_relop;
    if (numval >= 0x67 and numval <= 0x69) return .i32_unop;
    if (numval >= 0x6A and numval <= 0x78) return .i32_binop;
    if (numval >= 0x79 and numval <= 0x7B) return .i64_unop;
    if (numval >= 0x7C and numval <= 0x8A) return .i64_binop;
    if (numval >= 0x8B and numval <= 0x91) return .f32_unop;
    if (numval >= 0x92 and numval <= 0x98) return .f32_binop;
    if (numval >= 0x99 and numval <= 0x9F) return .f64_unop;
    if (numval >= 0xA0 and numval <= 0xA6) return .f64_binop;
    if (numval >= 0xA7 and numval <= 0xBF) return .convert;
    if (numval >= 0xC0 and numval <= 0xC1) return .i32_unop;
    if (numval >= 0xC2 and numval <= 0xC4) return .i64_unop;
    return .other;
}

// don't use _ to not blow up dispatch tables
pub const Prefixed = enum(u32) {
    i32_trunc_sat_f32_s = 0,
    i32_trunc_sat_f32_u = 1,
    i32_trunc_sat_f64_s = 2,
    i32_trunc_sat_f64_u = 3,
    i64_trunc_sat_f32_s = 4,
    i64_trunc_sat_f32_u = 5,
    i64_trunc_sat_f64_s = 6,
    i64_trunc_sat_f64_u = 7,

    memory_init = 8,
    data_drop = 9,
    memory_copy = 10,
    memory_fill = 11,

    table_init = 12,
    elem_drop = 13,
    table_copy = 14,
    table_grow = 15,
    table_size = 16,
    table_fill = 17,
};
pub const max_prefixed = 17;

pub const funcref_nil: u32 = 0xffffffff;

pub const page_size: u32 = 0x10000;

pub const HostFunc = struct {
    cb: *const fn (args_ret: []StackValue, in: *@import("./Instance.zig"), data: *anyopaque) WASMError!void,
    data: *anyopaque = undefined,
    n_args: u16,
    n_res: u16,
};

pub const Instruction = union(enum) {
    block: BlockType,
    loop: BlockType,
    if_: BlockType,

    other__fixme: OpCode, // not yet converted
};
