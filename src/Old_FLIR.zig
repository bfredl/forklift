const std = @import("std");
const math = std.math;
const Allocator = std.mem.Allocator;
const FLIR = @This();
const print = std.debug.print;
const CFO = @import("./CFO.zig");
const swap = std.mem.swap;

const builtin = @import("builtin");
const s2 = builtin.zig_backend != .stage1;
const ArrayList = @import("./fake_list.zig").ArrayList;

const VMathOp = CFO.VMathOp;

pub const Tag = enum(u8) {
    arg,
    load,
    store,
    constant,
    vmath,
    ret,
    loop_start,
    loop_end,
};

const ref = u16;

pub const Inst = struct {
    tag: Tag,
    opspec: u8 = 0,
    op1: ref = 0,
    op2: ref = 0,
    alloc: ?u4 = null,
    live: ?u16 = null,
    tmp: u16 = 0,
};

narg: u16,
theinst: Inst = undefined,
inst: ArrayList(Inst),
constants: ArrayList(f64),
pos_loop_start: u16 = 0,
pos_loop_end: u16 = 0,

pub fn init(narg: u4, allocator: Allocator) !FLIR {
    var self: FLIR = .{
        .narg = narg,
        .inst = try ArrayList(Inst).initCapacity(allocator, 16),
        .constants = try ArrayList(f64).initCapacity(allocator, 16),
    };
    try self.initialize();
    return self;
}

pub fn init_stage2(narg: u4, allocator: Allocator) !FLIR {
    _ = allocator;
    var self: FLIR = .{
        .narg = narg,
        .inst = try ArrayList(Inst).initCapacity(allocator, 16),
        .constants = try ArrayList(f64).initCapacity(allocator, 16),
        .pos_loop_start = 0,
        .pos_loop_end = 0,
        .theinst = undefined,
    };
    self.initialize() catch unreachable;
    return self;
}

fn initialize(self: *FLIR) !void {
    var iarg: u4 = 0;
    while (iarg < self.narg) : (iarg += 1) {
        // stage2: cannot initialize in place
        // try self.inst.append(.{ .tag = .arg, .op1 = iarg, .alloc = iarg });
        const inst: Inst = .{ .tag = .arg, .op1 = iarg, .alloc = iarg };
        _ = try self.put(inst);
    }
}

pub fn deinit(self: FLIR) void {
    self.inst.deinit();
    self.constants.deinit();
}

pub inline fn ninst(self: FLIR) u16 {
    return @intCast(u16, self.inst.items.len);
}

pub fn put(self: *FLIR, inst: Inst) !u16 {
    try self.inst.append(inst);
    return @intCast(u16, self.inst.items.len - 1);
}

pub fn loop_start(self: *FLIR) !void {
    _ = try self.put(Inst{ .tag = .loop_start });
}

pub fn loop_end(self: *FLIR) !void {
    _ = try self.put(Inst{ .tag = .loop_end });
}

pub fn live(self: *FLIR, arglive: bool) void {
    var pos: u16 = 0;
    while (pos < self.ninst()) : (pos += 1) {
        const inst = &self.inst.items[pos];
        inst.live = null;
        const nop: u2 = switch (inst.tag) {
            .arg => 0,
            .vmath => 2,
            .ret => 1,
            .load => 0, // of course this will be more when we track GPRs..
            .store => 1, // of course this will be more when we track GPRs..
            .constant => 0,
            .loop_start => 0,
            .loop_end => 0,
        };
        if (nop > 0) {
            self.set_live(inst.op1, pos);
            if (nop > 1) {
                self.set_live(inst.op2, pos);
            }
        }
        switch (inst.tag) {
            .loop_start => {
                self.pos_loop_start = pos;
            },
            .loop_end => {
                self.pos_loop_end = pos;
            },
            else => {},
        }
    }
    // TODO: lol what is loop analysis
    if (arglive) {
        pos = 0;
        while (pos < self.pos_loop_start) : (pos += 1) {
            const inst = &self.inst.items[pos];
            if (inst.live) |l| if (l > self.pos_loop_start and l < self.pos_loop_end) {
                inst.live = self.pos_loop_end;
            };
        }
    }
}

inline fn set_live(self: FLIR, used: u16, user: u16) void {
    const inst = &self.inst.items[used];
    inst.live = if (inst.live) |l| math.max(l, user) else user;
}

pub fn scanreg(self: FLIR, doit: bool) !u5 {
    var active: [16]?u16 = ([1]?u16{null}) ** 16;
    var pos: u16 = 0;
    var maxpressure: u5 = 0;
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
            if (doit) inst.alloc = reg;
            active[reg] = end;
            var pressure: u5 = 0;
            for (active) |v| {
                if (!(v == null or v.? <= pos)) {
                    pressure += 1;
                }
            }
            maxpressure = math.max(maxpressure, pressure);
        } else if (inst.tag == .ret) {
            // not used but it looks good\tm
            if (doit) inst.alloc = 0;
        }
    }
    return maxpressure;
}

// must have run self.live() !
// pressure can be calculated by self.scanreg(false)
pub fn hoist_loopy(self: FLIR, pressure: u5) !void {
    var available: u5 = 16 - pressure;
    if (available == 0) return;
    var pos: u16 = 0;
    var newpos: u16 = 0;
    while (pos < self.pos_loop_start) : (pos += 1) {
        const inst = &self.inst.items[pos];
        inst.tmp = newpos;
        newpos += 1;
    }
    if (self.inst.items[pos].tag != .loop_start) return error.FEEL;
    pos += 1;
    while (pos < self.pos_loop_end) : (pos += 1) {
        const inst = &self.inst.items[pos];
        if (inst.tag == .constant) {
            inst.tmp = newpos;
            newpos += 1;
            available -= 1;
            if (available == 0) {
                break;
            }
        }
    }
    self.inst.items[self.pos_loop_start].tmp = newpos;
    newpos += 1;
    pos = self.pos_loop_start + 1;
    while (pos < self.pos_loop_end) : (pos += 1) {
        const inst = &self.inst.items[pos];
        if (inst.tmp == 0) {
            inst.tmp = newpos;
            newpos += 1;
        }
    }

    if (pos != newpos) return error.youDunGoofed;
    while (pos < self.ninst()) : (pos += 1) {
        const inst = &self.inst.items[pos];
        inst.tmp = pos;
    }

    self.debug_print(true);

    pos = 0;
    while (pos < self.ninst()) : (pos += 1) {
        const inst = &self.inst.items[pos];
        const nop = n_op(inst.tag);
        if (nop > 0) {
            inst.op1 = self.inst.items[inst.op1].tmp;
            if (nop > 1) {
                inst.op2 = self.inst.items[inst.op2].tmp;
            }
        }
    }

    pos = 0;
    while (pos < self.ninst()) : (pos += 1) {
        const inst = &self.inst.items[pos];
        while (inst.tmp != pos) {
            swap(Inst, inst, &self.inst.items[inst.tmp]);
        }
    }
}

fn n_op(tag: Tag) u2 {
    return switch (tag) {
        .arg => 1,
        .vmath => 2,
        .ret => 1,
        .load => 0,
        .constant => 0,
        .store => 1,
        .loop_start => 0,
        .loop_end => 0,
    };
}

pub fn debug_print(self: FLIR, tmp: bool) void {
    if (s2) {
        return;
    }
    var pos: u16 = 0;
    print("\n", .{});
    while (pos < self.ninst()) : (pos += 1) {
        const inst = &self.inst.items[pos];
        if (inst.alloc) |reg| {
            print(" xmm{} ", .{reg});
        } else {
            print(" ---- ", .{});
        }
        if (tmp) {
            print("{:3}  ", .{inst.tmp});
        }
        const marker: u8 = if (inst.live) |_| ' ' else '!';
        print("%{}{c}= {s}", .{ pos, marker, @tagName(inst.tag) });
        const nop = n_op(inst.tag);
        if (inst.tag == .vmath) {
            print(".{s}", .{@tagName(@intToEnum(VMathOp, inst.opspec))});
        } else if (inst.tag == .load) {
            const i: []const u8 = if (inst.opspec > 0xF) "i+" else "";
            print(" p{}[{s}{}]", .{ inst.opspec & 0xF, i, inst.op1 });
        } else if (inst.tag == .store) {
            const i: []const u8 = if (inst.opspec > 0xF) "i+" else "";
            print(" p{}[{s}{}] <-", .{ inst.opspec & 0xF, i, inst.op2 });
        } else if (inst.tag == .constant) {
            print(" c[{}]", .{inst.op1});
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

pub fn codegen(self: FLIR, cfo: *CFO, simd: bool) !u32 {
    const target = cfo.get_target();
    const idx = .rcx;

    var pos: usize = 0;
    var loop_pos: ?u32 = null;
    const stride: u8 = if (simd) 4 else 1;
    const fm: CFO.FMode = if (simd) .pd4 else .sd;

    while (pos < self.inst.items.len) : (pos += 1) {
        const inst = &self.inst.items[pos];
        switch (inst.tag) {
            .loop_start => {
                try cfo.mov(.r10, .rcx);
                try cfo.arit(.xor, idx, idx);
                loop_pos = cfo.get_target();
            },
            .loop_end => {
                try cfo.aritri(.add, idx, stride);
                try cfo.arit(.cmp, idx, .r10);
                try cfo.jbck(.l, loop_pos orelse return error.UW0TM8);
            },
            .arg => {
                if (inst.op1 != @as(u16, inst.alloc.?)) return error.InvalidArgRegister;
                if (inst.live != null and simd) return error.AAA_AAA_AAA;
            },
            .vmath => {
                // TODO: kill dead instructions completely instead of leaving alloc blank
                const dst = inst.alloc orelse continue;
                const src1 = self.inst.items[inst.op1].alloc.?;
                const src2 = self.inst.items[inst.op2].alloc.?;
                try cfo.vmathf(@intToEnum(VMathOp, inst.opspec), fm, dst, src1, src2);
            },
            .ret => {
                const src = self.inst.items[inst.op1].alloc.?;
                if (src != 0) {
                    try cfo.vmovf(fm, 0, src);
                }
                break;
            },
            .load => {
                const dst = inst.alloc.?;
                const reg: CFO.IPReg = switch (inst.opspec & 0xF) {
                    0 => .rdi,
                    1 => .rsi,
                    2 => .rdx,
                    else => unreachable,
                };
                const base = if (inst.opspec > 0xF) CFO.qi(reg, idx) else CFO.a(reg);
                const src = base.o(inst.op1);
                // TODO: use align!
                if (simd and base.index == null) {
                    try cfo.vbroadcast(.pd4, dst, src);
                } else {
                    try cfo.vmovarm(fm, dst, src);
                }
            },
            .constant => {
                const dst = inst.alloc.?;
                if (simd) {
                    try cfo.vbroadcast(.pd4, dst, CFO.a(.rax).o(8 * inst.op1));
                } else {
                    try cfo.vmovurm(.sd, dst, CFO.a(.rax).o(8 * inst.op1));
                }
            },
            .store => {
                const src = self.inst.items[inst.op1].alloc.?;
                const reg: CFO.IPReg = switch (inst.opspec & 0xF) {
                    0 => .rdi,
                    1 => .rsi,
                    2 => .rdx,
                    else => unreachable,
                };
                const base = if (inst.opspec > 0xF) CFO.qi(reg, .rcx) else CFO.a(reg);
                const dst = base.o(inst.op2);
                try cfo.vmovamr(fm, dst, src);
            },
        }
    }

    return target;
}

pub fn add_constant(self: *FLIR, k: f64) !u16 {
    try self.constants.append(k);
    return @intCast(u16, self.constants.items.len - 1);
}

// TRICKY: we might want to share a single constant block across multiple kernels
pub fn emit_constants(self: *FLIR, cfo: *CFO) !u32 {
    try cfo.set_align(8);
    const target = cfo.get_target();
    for (self.constants.items) |c| {
        try cfo.wq(@bitCast(u64, c));
    }
    return target;
}
