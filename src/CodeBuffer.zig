const std = @import("std");
const mem = std.mem;
const os = std.os;
const debug = std.debug;
const ArrayListAligned = std.ArrayListAligned;
const page_size = std.mem.page_size;
const ArrayList = std.ArrayList;

const print = std.debug.print;
const Self = @This();
const common = @import("./common.zig");

buf: ArrayListAligned(u8, page_size),

/// offset of each encoded instruction. Might not be needed
/// but useful for debugging.
inst_off: ArrayList(u32),
inst_dbg: ArrayList(usize),

relocations: ArrayList(Relocation),

value_map: ArrayList(ValueDebugInfo),

// currently only for constant data past "ret", should also be
// for calls to unemitted functions etc
pub const Relocation = struct {
    pos: u32,
    idx: u16,
    is_ptr: bool,
};

// at address &buf.items[pos] register "reg" will store value with "name"
pub const ValueDebugInfo = struct {
    pos: u32,
    reg: common.IPReg,
    name: []const u8,
};

pub fn get_target(self: *Self) u32 {
    return @intCast(self.buf.items.len);
}

pub fn new_inst(self: *Self, addr: usize) !void {
    const size = @as(u32, @intCast(self.get_target()));
    try self.inst_off.append(size);
    try self.inst_dbg.append(addr);
}

pub fn init(allocator: mem.Allocator) !Self {
    // TODO: allocate consequtive mprotectable pages
    return Self{
        .buf = try ArrayListAligned(u8, page_size).initCapacity(std.heap.page_allocator, page_size),
        .inst_off = ArrayList(u32).init(allocator),
        .inst_dbg = ArrayList(usize).init(allocator),
        .relocations = ArrayList(Relocation).init(allocator),
        .value_map = ArrayList(ValueDebugInfo).init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    // TODO: only in debug mode (as clobbers the array, needs r/w)
    os.mprotect(self.buf.items.ptr[0..self.buf.capacity], os.PROT.READ | os.PROT.WRITE) catch unreachable;
    self.buf.deinit();
    self.inst_off.deinit();
    self.inst_dbg.deinit();
    self.relocations.deinit();
    self.value_map.deinit();
}

pub fn dump(self: *Self) !void {
    try std.fs.cwd().writeFile("test.o", self.buf.items);
}

pub fn lookup(self: *Self, addr: usize) usize {
    const startaddr: usize = @intFromPtr(self.code.items.ptr);
    const endaddr: usize = startaddr + self.code.items.len;
    if (startaddr <= addr and addr < endaddr) {
        const off = addr - startaddr;
        for (self.inst_dbg.items, 0..) |x, i| {
            if (i + 1 >= self.inst_off.items.len or off < self.inst_off.items[i + 1]) {
                return x;
            }
        }
    }
    return addr;
}

pub fn dbg_test(self: *Self) !void {
    const stderr = std.io.getStdErr().writer();
    const dbginfo = try debug.getSelfDebugInfo();
    const tty_config = debug.detectTTYConfig();
    for (self.inst_dbg.items, 0..) |x, i| {
        print("{} {}\n", .{ i, x });
        try debug.printSourceAtAddress(dbginfo, stderr, x, tty_config);
    }
}

pub fn finalize(self: *Self) !void {
    try os.mprotect(self.buf.items.ptr[0..self.buf.capacity], os.PROT.READ | os.PROT.EXEC);
}

pub fn get_ptr(self: *Self, target: u32, comptime T: type) T {
    return @ptrCast(self.buf.items[target..].ptr);
}
