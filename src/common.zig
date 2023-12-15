// declarations common to all archs
//
// size for integer operations
pub const ISize = enum(u2) {
    byte,
    word,
    dword,
    quadword,
};

pub const IPReg = enum(u4) {
    _,
    pub fn id(self: IPReg) u4 {
        return @intFromEnum(self);
    }
};

pub const DebugOptions = struct {
    dbg_raw_ir: bool = false,
    dbg_ssa_ir: bool = false,
    dbg_analysed_ir: bool = false,
    dbg_exclude_trivial_put: bool = false,
    dbg_disasm: bool = false,
    dbg_vregs: bool = false,
    dbg_trap: bool = false,
    dbg_disasm_ir: bool = false,
};

const builtin = @import("builtin");

// TODO: set these for tests, somehow..
pub const debug_options = if (!builtin.is_test) &@import("root").options else &DebugOptions{};
