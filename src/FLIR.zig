const std = @import("std");
const math = std.math;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const FLIR = @This();
const print = std.debug.print;
const CFO = @import("./CFO.zig");

const VMathOp = CFO.VMathOp;

pub const Tag = enum(u8) {
    arg,
    load,
    vmath,
    ret,
};

const ref = u16;

const Inst = struct {
    tag: Tag,
    opspec: u8 = 0,
    op1: ref,
    op2: ref = 0,
    alloc: ?u4 = null,
    live: ?u16 = null,
};

narg: u16,
inst: ArrayList(Inst),

pub fn init(narg: u4, allocator: Allocator) !FLIR {
    var self: FLIR = .{ .narg = narg, .inst = try ArrayList(Inst).initCapacity(allocator, 16) };
    var iarg: u4 = 0;
    while (iarg < narg) : (iarg += 1) {
        try self.inst.append(.{ .tag = .arg, .op1 = iarg, .alloc = iarg });
    }
    return self;
}

pub fn deinit(self: FLIR) void {
    self.inst.deinit();
}

pub inline fn ninst(self: FLIR) u16 {
    return @intCast(u16, self.inst.items.len);
}

pub fn put(self: *FLIR, inst: Inst) !u16 {
    try self.inst.append(inst);
    return @intCast(u16, self.inst.items.len - 1);
}

pub fn live(self: FLIR) void {
    var pos: u16 = 0;
    while (pos < self.ninst()) : (pos += 1) {
        const inst = &self.inst.items[pos];
        const nop: u2 = switch (inst.tag) {
            .arg => 0,
            .vmath => 2,
            .ret => 1,
            .load => 0, // of course this will be more when we track GPRs..
        };
        if (nop > 0) {
            self.set_live(inst.op1, pos);
            if (nop > 1) {
                self.set_live(inst.op2, pos);
            }
        }
    }
}

inline fn set_live(self: FLIR, used: u16, user: u16) void {
    const inst = &self.inst.items[used];
    inst.live = if (inst.live) |l| math.max(l, user) else user;
}

pub fn scanreg(self: FLIR) !void {
    var active: [16]?u16 = .{null} ** 16;
    var pos: u16 = 0;
    while (pos < self.ninst()) : (pos += 1) {
        const inst = &self.inst.items[pos];
        if (inst.live) |end| {
            const reg = inst.alloc orelse found: {
                for (active) |v, i| {
                    if (v == null or v.? <= pos) {
                        break :found @intCast(u4, i);
                    }
                } else {
                    return error.TooManyLiveRanges;
                }
            };
            inst.alloc = reg;
            active[reg] = end;
        } else if (inst.tag == .ret) {
            // not used but it looks good\tm
            inst.alloc = 0;
        }
    }
}

pub fn debug_print(self: FLIR) void {
    var pos: u16 = 0;
    print("\n", .{});
    while (pos < self.ninst()) : (pos += 1) {
        const inst = &self.inst.items[pos];
        if (inst.alloc) |reg| {
            print(" xmm{} ", .{reg});
        } else {
            print(" ---- ", .{});
        }
        const marker: u8 = if (inst.live) |_| ' ' else '!';
        print("%{}{c}= {s}", .{ pos, marker, @tagName(inst.tag) });
        const nop: u2 = switch (inst.tag) {
            .arg => 1,
            .vmath => 2,
            .ret => 1,
            .load => 0,
        };
        if (inst.tag == .vmath) {
            print(".{s}", .{@tagName(@intToEnum(VMathOp, inst.opspec))});
        } else if (inst.tag == .load) {
            print(" [{}]", .{inst.op1});
        }
        if (nop > 0) {
            print(" %{}", .{inst.op1});
            if (self.inst.items[inst.op1].live == pos) {
                print("!", .{});
            }
            if (nop > 1) {
                print(", %{}", .{inst.op2});
                if (self.inst.items[inst.op2].live == pos) {
                    print("!", .{});
                }
            }
        }
        print("\n", .{});
    }
}

pub fn codegen(self: FLIR, cfo: *CFO) !u32 {
    const target = cfo.get_target();

    try cfo.enter();
    var didret = false;

    var pos: usize = 0;
    while (pos < self.inst.items.len) : (pos += 1) {
        const inst = &self.inst.items[pos];
        switch (inst.tag) {
            .arg => {
                if (inst.op1 != @as(u16, inst.alloc.?)) return error.InvalidArgRegister;
            },
            .vmath => {
                const src1 = self.inst.items[inst.op1].alloc.?;
                const src2 = self.inst.items[inst.op2].alloc.?;
                const dst = inst.alloc.?;
                try cfo.vmathf(@intToEnum(VMathOp, inst.opspec), .sd, dst, src1, src2);
            },
            .ret => {
                const src = self.inst.items[inst.op1].alloc.?;
                if (src != 0) {
                    try cfo.vmovf(.sd, 0, src);
                }
                didret = true;
                break;
            },
            .load => {
                const dst = inst.alloc.?;
                const reg: CFO.IPReg = switch (inst.op1) {
                    0 => .rdi,
                    1 => .rsi,
                    2 => .rdx,
                    else => unreachable,
                };
                const src = CFO.qi(reg, .rcx);
                try cfo.vmovurm(.sd, dst, src);
            },
        }
    }

    if (!didret) return error.Unreachable;

    try cfo.leave();
    try cfo.ret();
    return target;
}
