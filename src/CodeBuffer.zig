const zig_version = @import("builtin").zig_version;
const std = @import("std");
const posix = std.posix;
const debug = std.debug;
const page_size_min = std.heap.page_size_min;

const page_alignment = if (zig_version.major == 0 and zig_version.minor <= 14) page_size_min else std.mem.Alignment.fromByteUnits(page_size_min);

const print = std.debug.print;
const Self = @This();
const defs = @import("./defs.zig");

buf: std.ArrayListAligned(u8, page_alignment),

/// offset of each encoded instruction. Might not be needed
/// but useful for debugging.
inst_off: std.ArrayList(u32),
inst_dbg: std.ArrayList(usize),

relocations: std.ArrayList(Relocation),

value_map: std.ArrayList(ValueDebugInfo),

// currently only for constant data past "ret", should also be
// for calls to unemitted functions etc
pub const Relocation = struct {
    pos: u32,
    idx: u16,
};

// at address &buf.items[pos] register "reg" will store value with "name"
pub const ValueDebugInfo = struct {
    pos: u32,
    reg: defs.IPReg,
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

pub fn init(allocator: std.mem.Allocator) !Self {
    // TODO: allocate consequtive mprotectable pages
    return Self{
        .buf = try .initCapacity(std.heap.page_allocator, page_size_min),
        .inst_off = .init(allocator),
        .inst_dbg = .init(allocator),
        .relocations = .init(allocator),
        .value_map = .init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    // TODO: only in debug mode (as clobbers the array, needs r/w)
    posix.mprotect(self.buf.items.ptr[0..self.buf.capacity], posix.PROT.READ | posix.PROT.WRITE) catch unreachable;
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
    try posix.mprotect(self.buf.items.ptr[0..self.buf.capacity], posix.PROT.READ | posix.PROT.EXEC);
}

pub fn get_ptr(self: *Self, target: u32, comptime T: type) T {
    return @ptrCast(self.buf.items[target..].ptr);
}
