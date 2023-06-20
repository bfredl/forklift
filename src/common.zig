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
