const std = @import("std");
const math = std.math;
const mem = std.mem;
const Allocator = mem.Allocator;
const Self = @This();
const print = std.debug.print;
const CFO = @import("./CFO.zig");
const SSA_GVN = @import("./SSA_GVN.zig");
const IPReg = CFO.IPReg;

const builtin = @import("builtin");
const stage2 = builtin.zig_backend != .stage1;
const ArrayList = @import("./fake_list.zig").ArrayList;
const assert = std.debug.assert;

const VMathOp = CFO.VMathOp;

a: Allocator,
// TODO: unmanage all these:
n: ArrayList(Node),
b: ArrayList(Block),
dfs: ArrayList(u16),
refs: ArrayList(u16),
narg: u16 = 0,
nvar: u16 = 0,

// 8-byte slots in stack frame
nslots: u8 = 0,

pub fn uv(s: usize) u16 {
    return @intCast(u16, s);
}

pub const Node = struct {
    s: [2]u16 = .{ 0, 0 }, // sucessors
    dfnum: u16 = 0,
    idom: u16 = 0,
    predref: u16 = 0,
    npred: u16 = 0,
    firstblk: u16,
    lastblk: u16,
    dfs_parent: u16 = 0,

    genlink: u16 = 0,
};

pub const Tag = enum(u8) {
    empty = 0, // empty slot. must not be refered to!
    arg,
    variable,
    putvar, // non-phi assignment
    phi,
    /// assign to phi of (only) successor
    /// note: despite swearing in the intel church.
    /// op1 is source and op2 is dest, to simplify stuff
    /// i e n_op(putphi) == 1 for the most part
    putphi,
    renum,
    constant,
    load,
    lea,
    store,
    iadd, // imath group?
    ilessthan, // icmp group?
    vmath,
    ret,
};

pub const MCKind = enum(u8) {
    // not yet allocated, or Inst that trivially produces no value
    unallocated,
    // general purpose register like rax, r12, etc
    ipreg,
    // SSE/AVX registers, ie xmm0/ymm0-15
    vfreg,
    // TODO: support non-uniform sizes of spilled value
    frameslot,
    // unused value, perhaps should have been deleted before alloc
    dead,
    // not stored as such, will be emitted togheter with the next inst
    // example "lea" and then "store", or "load" and then iadd/vmath
    fused,
};

pub const Inst = struct {
    tag: Tag,
    spec: u8 = 0,
    op1: u16,
    op2: u16,
    reindex: u16 = 0,
    mckind: MCKind = .unallocated,
    mcidx: u8 = undefined,
    n_use: u16 = 0,

    fn free(self: @This()) bool {
        return self.tag == .empty;
    }

    pub fn res_type(inst: Inst) ?ValType {
        return switch (inst.tag) {
            .empty => null,
            .arg => spec_type(inst.spec), // TODO: haIIIII
            .variable => spec_type(inst.spec), // gets preserved to the phis
            .putvar => null,
            .phi => spec_type(inst.spec),
            .putphi => null, // stated in the phi instruction
            .constant => spec_type(inst.spec),
            .renum => null, // should be removed at this point
            .load => spec_type(inst.spec),
            .lea => .intptr, // Lea? Who's Lea??
            .store => null,
            .iadd => .intptr,
            .ilessthan => null, // technically the FLAG register but anyway
            .vmath => .avxval,
            .ret => null,
        };
    }

    pub fn ipreg(i: Inst) ?IPReg {
        return if (i.mckind == .ipreg) @intToEnum(IPReg, i.mcidx) else null;
    }

    pub fn avxreg(i: Inst) ?u4 {
        return if (i.mckind == .vfreg) @intCast(u4, i.mcidx) else null;
    }
};

// number of op:s which are inst references.
// otherwise they can store whatever data
pub fn n_op(tag: Tag) u2 {
    return switch (tag) {
        .empty => 0,
        .arg => 0,
        .variable => 0,
        // really only one, but we will get rid of this lie
        // before getting into any serious analysis.
        .putvar => 2,
        .phi => 0,
        .putphi => 1,
        .constant => 0,
        .renum => 1,
        .load => 2, // base, idx
        .lea => 2, // base, idx. elided when only used for a store!
        .store => 2, // addr, val
        .iadd => 2,
        .ilessthan => 2,
        .vmath => 2,
        .ret => 1,
    };
}

// TODO: expand into precise types, like "dword" or "4 packed doubles"
const ValType = enum(u4) {
    intptr = 0,
    avxval,

    pub fn spec(self: @This()) u4 {
        return @enumToInt(self);
    }
};

// TODO: handle spec being split between u4 type and u4 somethingelse?
fn spec_type(spec: u8) ValType {
    return @intToEnum(ValType, spec);
}

// TODO: refactor these to an array of InstMetadata structs
// or this is res_type != null?
pub fn has_res(tag: Tag) bool {
    return switch (tag) {
        .empty => false,
        .arg => true,
        .variable => true, // ASCHUALLY no, but looks like yes
        .putvar => false,
        .phi => true,
        .putphi => false, // storage location is stated in the phi instruction
        .constant => true,
        .renum => true, // TODO: removed at this point
        .load => true,
        .lea => true, // Lea? Who's Lea??
        .store => false,
        .iadd => true,
        .ilessthan => false, // technically yes, but no
        .vmath => true,
        .ret => false,
    };
}

pub const EMPTY: Inst = .{ .tag = .empty, .op1 = 0, .op2 = 0 };

pub const BLK_SIZE = 4;
pub const BLK_SHIFT = 2;
pub const Block = struct {
    node: u16,
    succ: u16 = NoRef,
    i: [BLK_SIZE]Inst = .{EMPTY} ** BLK_SIZE,

    pub fn next(self: @This()) ?u16 {
        return if (self.succ != NoRef) self.succ else null;
    }
};

test "sizey" {
    // @compileLog(@sizeOf(Inst));
    // @compileLog(@sizeOf(Block));
    assert(@sizeOf(Block) <= 64);
}

// filler value for unintialized refs. not a sentinel for
// actually invalid refs!
pub const DEAD: u16 = 0xFEFF;
// For blocks: we cannot have more than 2^14 blocks anyway
// for vars: don't allocate last block!
pub const NoRef: u16 = 0xFFFF;

pub fn init(n: u16, allocator: Allocator) !Self {
    return Self{
        .a = allocator,
        .n = try ArrayList(Node).initCapacity(allocator, n),
        .dfs = ArrayList(u16).init(allocator),
        .refs = try ArrayList(u16).initCapacity(allocator, 4 * n),
        .b = try ArrayList(Block).initCapacity(allocator, 2 * n),
    };
}

pub fn deinit(self: *Self) void {
    self.n.deinit();
    self.dfs.deinit();
    self.refs.deinit();
    self.b.deinit();
}

pub fn toref(blkid: u16, idx: u16) u16 {
    assert(idx < BLK_SIZE);
    return (blkid << BLK_SHIFT) | idx;
}

fn fromref(ref: u16) struct { block: u16, idx: u16 } {
    const IDX_MASK: u16 = BLK_SIZE - 1;
    const BLK_MASK: u16 = ~IDX_MASK;
    return .{
        .block = (ref & BLK_MASK) >> BLK_SHIFT,
        .idx = ref & IDX_MASK,
    };
}

pub fn iref(self: *Self, ref: u16) ?*Inst {
    if (ref == NoRef) {
        return null;
    }
    const r = fromref(ref);
    const blk = &self.b.items[r.block];
    return &blk.i[r.idx];
}

pub fn addNode(self: *Self) !u16 {
    const n = try self.n.addOne();
    const b = try self.b.addOne();
    var nodeid = uv(self.n.items.len - 1);
    var blkid = uv(self.b.items.len - 1);
    n.* = .{ .firstblk = blkid, .lastblk = blkid };
    b.* = .{ .node = nodeid };
    return nodeid;
}

// add inst to the end of block
pub fn addInst(self: *Self, node: u16, inst: Inst) !u16 {
    const n = &self.n.items[node];
    // must exist:
    var blkid = n.lastblk;
    var blk = &self.b.items[blkid];

    // TODO: later we can add more constraints for where "empty" ins can be
    var lastfree: u8 = BLK_SIZE;
    var i: u8 = BLK_SIZE - 1;
    while (true) : (i -= 1) {
        if (blk.i[@intCast(u8, i)].free()) {
            lastfree = i;
        } else {
            break;
        }
        if (i == 0) {
            break;
        }
    }

    if (lastfree == BLK_SIZE) {
        blkid = uv(self.b.items.len);
        blk.succ = blkid;
        blk = try self.b.addOne();
        blk.* = .{ .node = node };
        n.lastblk = blkid;
        lastfree = 0;
    }

    blk.i[lastfree] = inst;
    return toref(blkid, lastfree);
}

// add inst to the beginning of the block, _without_ renumbering any exiting instruction
pub fn preInst(self: *Self, node: u16, inst: Inst) !u16 {
    const n = &self.n.items[node];
    var blkid = n.firstblk;
    var blk = &self.b.items[blkid];

    var firstfree: i8 = -1;
    var i: i8 = 0;
    while (i < BLK_SIZE) : (i += 1) {
        if (blk.i[@intCast(u8, i)].free()) {
            firstfree = i;
        } else {
            break;
        }
    }

    if (firstfree == -1) {
        const nextblk = blkid;
        blkid = uv(self.b.items.len);
        blk = try self.b.addOne();
        blk.* = .{ .node = node, .succ = nextblk };
        n.firstblk = blkid;
        firstfree = BLK_SIZE - 1;
    }

    const free = @intCast(u8, firstfree);

    blk.i[free] = inst;
    return toref(blkid, free);
}

pub fn const_int(self: *Self, node: u16, val: u16) !u16 {
    // TODO: actually store constants in a buffer, or something
    return self.addInst(node, .{ .tag = .constant, .op1 = val, .op2 = 0 });
}

pub fn binop(self: *Self, node: u16, tag: Tag, op1: u16, op2: u16) !u16 {
    return self.addInst(node, .{ .tag = tag, .op1 = op1, .op2 = op2 });
}

// TODO: better abstraction for types (once we have real types)
pub fn vbinop(self: *Self, node: u16, tag: Tag, op1: u16, op2: u16) !u16 {
    return self.addInst(node, .{ .tag = tag, .op1 = op1, .op2 = op2, .spec = ValType.avxval.spec() });
}

pub fn vmath(self: *Self, node: u16, vop: VMathOp, op1: u16, op2: u16) !u16 {
    return self.addInst(node, .{ .tag = .vmath, .spec = vop.off(), .op1 = op1, .op2 = op2 });
}

pub fn putvar(self: *Self, node: u16, op1: u16, op2: u16) !void {
    _ = try self.binop(node, .putvar, op1, op2);
}

pub fn store(self: *Self, node: u16, base: u16, idx: u16, val: u16) !u16 {
    // FUBBIT: all possible instances of fusing should be detected in analysis
    // anyway
    const addr = try self.addInst(node, .{ .tag = .lea, .op1 = base, .op2 = idx, .mckind = .fused });
    return self.addInst(node, .{ .tag = .store, .op1 = addr, .op2 = val });
}

pub fn ret(self: *Self, node: u16, val: u16) !void {
    // TODO: actually store constants in a buffer, or something
    _ = try self.addInst(node, .{ .tag = .ret, .op1 = val, .op2 = 0 });
}

pub fn prePhi(self: *Self, node: u16, v: Inst) !u16 {
    return self.preInst(node, .{ .tag = .phi, .op1 = v.op1, .op2 = 0, .spec = v.spec });
}

// TODO: maintain wf of block 0: first all args, then all vars.

pub fn arg(self: *Self) !u16 {
    if (self.n.items.len == 0) return error.EEEEE;
    const inst = try self.addInst(0, .{ .tag = .arg, .op1 = self.narg, .op2 = 0 });
    self.narg += 1;
    return inst;
}

pub fn variable(self: *Self) !u16 {
    if (self.n.items.len == 0) return error.EEEEE;
    const inst = try self.addInst(0, .{ .tag = .variable, .op1 = self.nvar, .op2 = 0 });
    self.nvar += 1;
    return inst;
}

pub fn preds(self: *Self, i: u16) []u16 {
    const v = self.n.items[i];
    return self.refs.items[v.predref..][0..v.npred];
}

pub fn p(self: *Self, s1: u16, s2: u16) void {
    var z1: u16 = s1;
    var z2: u16 = s2;
    if (true and s2 != 0) {
        z1 = s2;
        z2 = s1;
    }
    // TODO: this is INVALID
    self.n.appendAssumeCapacity(.{ .s = .{ z1, z2 }, .firstblk = NoRef, .lastblk = NoRef });
}

fn predlink(self: *Self, i: u16, si: u1, split: bool) !void {
    var n = self.n.items;
    const s = n[i].s[si];
    if (s == 0) return;

    if (split and n[s].npred > 1) {
        const inter = try self.addNode();
        n = self.n.items; // haii
        n[inter].npred = 1;
        n[i].s[si] = inter;
        n[inter].s[0] = s;
        n[inter].genlink = n[i].genlink;
        n[i].genlink = inter;
        addpred(self, s, inter);
        addpred(self, inter, i);
    } else {
        addpred(self, s, i);
    }
}

fn addpred(self: *Self, s: u16, i: u16) void {
    const n = self.n.items;
    // tricky: build the reflist per node backwards,
    // so the end result is the start index
    if (n[s].predref == 0) {
        self.refs.appendNTimesAssumeCapacity(DEAD, n[s].npred);
        n[s].predref = uv(self.refs.items.len);
    }
    n[s].predref -= 1;
    self.refs.items[n[s].predref] = i;
}

pub fn calc_preds(self: *Self) !void {
    const n = self.n.items;
    // TODO: policy for rebuilding refs from scratch?
    if (self.refs.items.len > 0) unreachable;
    for (n) |v| {
        if (v.s[0] > 0) {
            n[v.s[0]].npred += 1;
        }
        if (v.s[1] > 0 and v.s[1] != v.s[0]) {
            n[v.s[1]].npred += 1;
        }
    }
    for (n) |v, i| {
        const shared = v.s[1] > 0 and v.s[1] == v.s[0];
        if (shared) return error.NotSureAboutThis;
        const split = v.s[1] > 0;
        try self.predlink(@intCast(u16, i), 0, split);
        try self.predlink(@intCast(u16, i), 1, split);
    }
}

// TODO: not idempotent! does not reset n_use=0 first.
pub fn calc_use(self: *Self) !void {
    // TODO: stop abusing dfs for reachable blocks and just kill
    // unreachable blocks whenever they are/become unreachable
    for (self.dfs.items) |ni| {
        var n = &self.n.items[ni];
        var cur_blk: ?u16 = n.firstblk;
        while (cur_blk) |blk| {
            var b = &self.b.items[blk];
            for (b.i) |*i| {
                const nops = n_op(i.tag);
                if (nops > 0) {
                    const ref = self.iref(i.op1).?;
                    ref.n_use += 1;
                    if (nops > 1) {
                        const ref2 = self.iref(i.op2).?;
                        ref2.n_use += 1;
                    }
                }
            }
            cur_blk = b.next();
        }
    }
}

pub fn calc_dfs(self: *Self) !void {
    const n = self.n.items;
    var stack = try ArrayList(u16).initCapacity(self.a, n.len);
    try self.dfs.ensureTotalCapacity(n.len);
    defer stack.deinit();
    stack.appendAssumeCapacity(0);
    while (stack.items.len > 0) {
        const v = stack.pop();
        if (n[v].dfnum > 0) {
            // already visited
            continue;
        }
        if (false) print("dfs[{}] = {};\n", .{ self.dfs.items.len, v });
        n[v].dfnum = uv(self.dfs.items.len);
        self.dfs.appendAssumeCapacity(v);

        for (n[v].s) |si| {
            // origin cannot be revisited anyway
            if (si > 0 and n[si].dfnum == 0) {
                n[si].dfs_parent = v;
                stack.appendAssumeCapacity(si);
            }
        }
    }
}

pub fn alloc_arg(self: *Self, inst: *Inst) !void {
    _ = self;
    const regs: [6]IPReg = .{ .rdi, .rsi, .rdx, .rcx, .r8, .r9 };
    if (inst.op1 >= regs.len) return error.ARA;
    inst.mckind = .ipreg;
    inst.mcidx = regs[inst.op1].id();
}

pub fn trivial_stack_alloc(self: *Self) !void {
    const regs: [5]IPReg = .{ .r12, .r13, .r14, .r15, .rbx };
    const usereg = 5;
    var used: usize = 0;
    var avxused: u8 = 0;
    for (self.dfs.items) |ni| {
        var n = &self.n.items[ni];
        var cur_blk: ?u16 = n.firstblk;
        while (cur_blk) |blk| {
            var b = &self.b.items[blk];
            for (b.i) |*i| {
                if (i.tag == .arg) {
                    try self.alloc_arg(i);
                } else if (has_res(i.tag) and i.mckind == .unallocated) {
                    if (i.res_type() == ValType.avxval) {
                        if (avxused == 16) {
                            return error.GOOOF;
                        }
                        i.mckind = .vfreg;
                        i.mcidx = avxused;
                        avxused += 1;
                    } else if (used < usereg) {
                        i.mckind = .ipreg;
                        i.mcidx = regs[used].id();
                        used += 1;
                    } else {
                        i.mckind = .frameslot;
                        if (self.nslots == 255) {
                            return error.UDunGoofed;
                        }
                        i.mcidx = self.nslots;
                        self.nslots += 1;
                    }
                }
            }
            cur_blk = b.next();
        }
    }
}

pub fn debug_print(self: *Self) void {
    if (stage2) {
        return;
    }
    print("\n", .{});
    for (self.n.items) |*b, i| {
        print("node {} (npred {}):\n", .{ i, b.npred });

        self.print_blk(b.firstblk);

        if (b.s[1] == 0) {
            if (b.s[0] == 0) {
                print("  diverge\n", .{});
            } else if (b.s[0] != i + 1) {
                print("  jump {}\n", .{b.s[0]});
            }
        } else {
            print("  split: {any}\n", .{b.s});
        }
    }
}

fn print_blk(self: *Self, firstblk: u16) void {
    var cur_blk: ?u16 = firstblk;
    while (cur_blk) |blk| {
        var b = &self.b.items[blk];
        for (b.i) |i, idx| {
            if (i.tag == .empty) {
                continue;
            }
            const chr: u8 = if (has_res(i.tag)) '=' else ' ';
            print("  %{} {c} {s}", .{ toref(blk, uv(idx)), chr, @tagName(i.tag) });

            if (i.tag == .variable) {
                print(" {s}", .{@tagName(spec_type(i.spec))});
            }

            if (i.tag == .vmath) {
                print(".{s}", .{@tagName(@intToEnum(VMathOp, i.spec))});
            } else if (i.tag == .constant) {
                print(" c[{}]", .{i.op1});
            } else if (i.tag == .putphi) {
                print(" %{} <-", .{i.op2});
            }
            const nop = n_op(i.tag);
            if (nop > 0) {
                print(" %{}", .{i.op1});
                if (nop > 1) {
                    print(", %{}", .{i.op2});
                }
            }
            print_mcval(i);
            if (i.n_use > 0) {
                // this is getting ridiculous
                print(" <{}>", .{i.n_use});
            }
            print("\n", .{});
        }
        cur_blk = b.next();
    }
}

fn print_mcval(i: Inst) void {
    switch (i.mckind) {
        .frameslot => print(" [rbp-8*{}]", .{i.mcidx}),
        .ipreg => print(" ${s}", .{@tagName(@intToEnum(IPReg, i.mcidx))}),
        .vfreg => print(" $ymm{}", .{i.mcidx}),
        .unallocated => {
            if (i.tag == .load or i.tag == .phi or i.tag == .arg) {
                if (i.res_type()) |t| {
                    print(" {s}", .{@tagName(t)});
                }
            }
        },
        else => {},
    }
}

fn regmovmc(cfo: *CFO, dst: IPReg, src: Inst) !void {
    switch (src.mckind) {
        .frameslot => try cfo.movrm(dst, CFO.a(.rbp).o(-8 * @as(i32, src.mcidx))),
        .ipreg => {
            const reg = @intToEnum(IPReg, src.mcidx);
            if (dst != reg) try cfo.mov(dst, reg);
        },
        else => return error.AAA_AA_A,
    }
}

fn regaritmc(cfo: *CFO, op: CFO.AOp, dst: IPReg, i: Inst) !void {
    switch (i.mckind) {
        .frameslot => try cfo.aritrm(op, dst, CFO.a(.rbp).o(-8 * @as(i32, i.mcidx))),
        .ipreg => {
            const reg = @intToEnum(IPReg, i.mcidx);
            try cfo.arit(op, dst, reg);
        },
        .fused => {
            if (i.tag != .constant) return error.GetLostHeIsNeverComingBack;
            try cfo.aritri(op, dst, i.op1); // TODO: proper constval

        },
        else => return error.AAA_AA_A,
    }
}

fn mcmovreg(cfo: *CFO, dst: Inst, src: IPReg) !void {
    switch (dst.mckind) {
        .frameslot => try cfo.movmr(CFO.a(.rbp).o(-8 * @as(i32, dst.mcidx)), .rax),
        .ipreg => {
            const reg = @intToEnum(IPReg, dst.mcidx);
            if (reg != src) try cfo.mov(reg, src);
        },
        else => return error.AAA_AA_A,
    }
}

fn mcmovi(cfo: *CFO, i: Inst) !void {
    switch (i.mckind) {
        .frameslot => try cfo.movmi(CFO.a(.rbp).o(-8 * @as(i32, i.mcidx)), i.op1),
        .ipreg => {
            const reg = @intToEnum(IPReg, i.mcidx);
            if (i.op1 != 0) {
                try cfo.movri(reg, i.op1);
            } else {
                // THANKS INTEL
                try cfo.arit(.xor, reg, reg);
            }
        },
        .fused => {}, // let user lookup value
        else => return error.AAA_AA_A,
    }
}

// TODO: obviously better handling of scratch register
fn movmcs(cfo: *CFO, dst: Inst, src: Inst, scratch: IPReg) !void {
    if (dst.mckind == src.mckind and dst.mcidx == src.mcidx) {
        return;
    }
    if (dst.mckind == .ipreg) {
        try regmovmc(cfo, @intToEnum(IPReg, dst.mcidx), src);
    } else {
        const reg = if (src.mckind == .ipreg)
            @intToEnum(IPReg, src.mcidx)
        else reg: {
            try regmovmc(cfo, scratch, src);
            break :reg scratch;
        };
        try mcmovreg(cfo, dst, reg);
    }
}

pub fn makejmp(self: *Self, cfo: *CFO, cond: ?CFO.Cond, ni: u16, si: u1, labels: []u32, targets: [][2]u32) !void {
    const succ = self.n.items[ni].s[si];
    // NOTE: we assume blk 0 always has the prologue (push rbp; mov rbp, rsp)
    // at least, so that even if blk 0 is empty, blk 1 has target larger than 0x00
    if (labels[succ] != 0) {
        try cfo.jbck(cond, labels[succ]);
    } else {
        targets[ni][si] = try cfo.jfwd(cond);
    }
}

pub fn codegen(self: *Self, cfo: *CFO) !u32 {
    var labels = try self.a.alloc(u32, self.dfs.items.len);
    var targets = try self.a.alloc([2]u32, self.dfs.items.len);
    defer self.a.free(labels);
    defer self.a.free(targets);
    mem.set(u32, labels, 0);
    mem.set([2]u32, targets, .{ 0, 0 });

    const target = cfo.get_target();
    try cfo.enter();
    const stacksize = 8 * @as(i32, self.nslots);
    if (stacksize > 0) {
        const padding = (-stacksize) & 0xF;
        // print("size: {}, extrasize: {}\n", .{ stacksize, padding });
        try cfo.aritri(.sub, .rsp, stacksize + padding);
    }

    var ni: u16 = 0;
    while (ni != NoRef) : (ni = self.n.items[ni].genlink) {
        const n = &self.n.items[ni];
        if (n.dfnum == 0 and ni > 0) {
            // non-entry block not reached by df search is dead.
            // TODO: these should already been cleaned up at this point
            continue;
        }
        labels[ni] = cfo.get_target();
        print("LABEL: {x} {}\n", .{ labels[ni], ni });
        for (self.preds(uv(ni))) |pred| {
            const pr = &self.n.items[pred];
            const si: u1 = if (pr.s[0] == ni) 0 else 1;
            if (targets[pred][si] != 0) {
                try cfo.set_target(targets[pred][si]);
                targets[pred][si] = 0;
            }
        }

        var cur_blk: ?u16 = n.firstblk;
        var ea_fused: CFO.EAddr = undefined;
        var fused_inst: ?*Inst = null;
        while (cur_blk) |blk| {
            var b = &self.b.items[blk];
            for (b.i) |*i| {
                if (i.tag == .empty) continue;

                var was_fused: bool = false;
                switch (i.tag) {
                    // empty doesn't flush fused value
                    .empty => continue,
                    .ret => try regmovmc(cfo, .rax, self.iref(i.op1).?.*),
                    .iadd => {
                        const dst = i.ipreg() orelse .rax;
                        try regmovmc(cfo, dst, self.iref(i.op1).?.*);
                        try regaritmc(cfo, .add, dst, self.iref(i.op2).?.*);
                        try mcmovreg(cfo, i.*, dst); // elided if dst is register
                    },
                    .constant => try mcmovi(cfo, i.*),
                    .ilessthan => {
                        const firstop = self.iref(i.op1).?.ipreg() orelse .rax;
                        try regmovmc(cfo, firstop, self.iref(i.op1).?.*);
                        try regaritmc(cfo, .cmp, firstop, self.iref(i.op2).?.*);
                    },
                    .putphi => {
                        // TODO: actually check for parallell-move conflicts
                        // either here or as an extra deconstruction step
                        try movmcs(cfo, self.iref(i.op2).?.*, self.iref(i.op1).?.*, .rax);
                    },
                    .load => {
                        // TODO: spill spall supllit?
                        const base = self.iref(i.op1).?.ipreg() orelse unreachable;
                        const idx = self.iref(i.op2).?.ipreg() orelse unreachable;
                        const eaddr = CFO.qi(base, idx);
                        if (spec_type(i.spec) == .intptr) {
                            const dst = i.ipreg() orelse .rax;
                            try cfo.movrm(dst, eaddr);
                            try mcmovreg(cfo, i.*, dst); // elided if dst is register
                        } else {
                            const dst = i.avxreg() orelse unreachable;
                            try cfo.vmovurm(.sd, dst, eaddr);
                        }
                    },
                    .lea => {
                        // TODO: spill spall supllit?
                        const base = self.iref(i.op1).?.ipreg() orelse unreachable;
                        const idx = self.iref(i.op2).?.ipreg() orelse unreachable;
                        const eaddr = CFO.qi(base, idx);
                        if (i.mckind == .fused) {
                            ea_fused = eaddr;
                            was_fused = true;
                        } else {
                            const dst = i.ipreg() orelse .rax;
                            try cfo.lea(dst, CFO.qi(base, idx));
                            try mcmovreg(cfo, i.*, dst); // elided if dst is register
                        }
                    },
                    .store => {
                        // TODO: fuse lea with store
                        const addr = self.iref(i.op1).?;
                        const eaddr = if (addr == fused_inst)
                            ea_fused
                        else
                            CFO.a(self.iref(i.op1).?.ipreg() orelse unreachable);
                        const val = self.iref(i.op2).?;
                        if (val.res_type().? == .intptr) {
                            unreachable;
                        } else {
                            const src = val.avxreg() orelse unreachable;
                            try cfo.vmovumr(.sd, eaddr, src);
                        }
                    },
                    .vmath => {
                        const x = self.iref(i.op1).?.avxreg() orelse unreachable;
                        const y = self.iref(i.op2).?.avxreg() orelse unreachable;
                        const dst = i.avxreg() orelse unreachable;
                        try cfo.vmathf(@intToEnum(VMathOp, i.spec), .sd, dst, x, y);
                    },

                    else => {},
                }
                fused_inst = if (was_fused) i else null;
            }
            cur_blk = b.next();
        }
        const fallthru = n.genlink;
        if (n.s[0] == fallthru and n.s[1] != 0) {
            try self.makejmp(cfo, .nl, uv(ni), 1, labels, targets);
        } else {
            const default: u1 = default: {
                if (n.s[1] != 0) {
                    try self.makejmp(cfo, .l, uv(ni), 0, labels, targets);
                    break :default 1;
                } else break :default 0;
            };

            if (n.s[default] != fallthru and n.s[default] != 0) {
                try self.makejmp(cfo, null, uv(ni), default, labels, targets);
            }
        }
    }

    try cfo.leave();
    try cfo.ret();
    return target;
}

const test_allocator = std.testing.allocator;
const expectEqual = std.testing.expectEqual;

fn test_analysis(self: *Self) !void {
    // TODO: do a proper block ordering for codegen,
    // like a proper DAG of SCC order.
    // this just ensures declaration order is preserved
    for (self.n.items) |*v, i| {
        v.genlink = if (i < self.n.items.len - 1)
            uv(i + 1)
        else
            NoRef;
    }

    try self.calc_preds();
    self.debug_print();

    try self.calc_dfs();
    try SSA_GVN.ssa_gvn(self);
    try self.calc_use();
    try self.trivial_stack_alloc();
}

test "printa" {
    var self = try Self.init(8, test_allocator);
    defer self.deinit();

    const node = try self.addNode();
    try expectEqual(uv(0), node);

    const arg1 = try self.addInst(node, .{ .tag = .arg, .op1 = 0, .op2 = 0 });
    const arg2 = try self.addInst(node, .{ .tag = .arg, .op1 = 1, .op2 = 0 });

    const node2 = try self.addNode();
    try expectEqual(uv(1), node2);
    self.n.items[node].s[0] = node2;

    const add = try self.addInst(node2, .{ .tag = .iadd, .op1 = arg1, .op2 = arg2 });
    const zero = try self.addInst(node2, .{ .tag = .constant, .op1 = 0, .op2 = 0 });
    const load1 = try self.addInst(node2, .{ .tag = .load, .op1 = arg1, .op2 = zero });
    const load2 = try self.addInst(node2, .{ .tag = .load, .op1 = arg2, .op2 = zero });
    const vadd = try self.addInst(node2, .{ .tag = .vmath, .spec = VMathOp.add.off(), .op1 = load1, .op2 = load2 });
    _ = vadd;

    _ = try self.addInst(node2, .{ .tag = .ret, .op1 = add, .op2 = 0 });

    self.debug_print();
}

test "loopvar" {
    var self = try Self.init(8, test_allocator);
    defer self.deinit();

    const start = try self.addNode();

    const arg1 = try self.arg();
    const arg2 = try self.arg();
    const arg3 = try self.arg();

    const var_i = try self.variable();
    const const_0 = try self.const_int(start, 0);

    try self.putvar(start, var_i, const_0);

    const loop = try self.addNode();
    // NB: assumes count > 0, always do first iteration
    self.n.items[start].s[0] = loop;

    const valx = try self.vbinop(loop, .load, arg1, var_i);
    const valy = try self.vbinop(loop, .load, arg2, var_i);
    const newval = try self.vmath(loop, .add, valx, valy);
    _ = try self.store(loop, arg1, var_i, newval);

    const const_1 = try self.const_int(loop, 1);
    // TODO: analysis should of course do this:
    self.iref(const_1).?.mckind = .fused;
    const iadd = try self.binop(loop, .iadd, var_i, const_1);
    try self.putvar(loop, var_i, iadd);
    _ = try self.binop(loop, .ilessthan, var_i, arg3);

    // if true
    self.n.items[loop].s[0] = loop;
    const end = try self.addNode();
    self.n.items[loop].s[1] = end;

    try self.ret(end, const_0);

    try self.test_analysis();

    self.debug_print();

    var cfo = try CFO.init(test_allocator);
    defer cfo.deinit();

    _ = try self.codegen(&cfo);
    try cfo.dbg_nasm(test_allocator);
}

test "diamondvar" {
    var self = try Self.init(8, test_allocator);
    defer self.deinit();

    const start = try self.addNode();
    const arg1 = try self.arg();
    const arg2 = try self.arg();
    const v = try self.variable();

    // const const_0 = try self.const_int(start, 0);
    const const_42 = try self.const_int(start, 42);
    try self.putvar(start, v, const_42);
    _ = try self.binop(start, .ilessthan, arg1, v);

    const left = try self.addNode();
    self.n.items[start].s[0] = left;
    const addl = try self.binop(left, .iadd, v, arg2);
    try self.putvar(left, v, addl);

    const right = try self.addNode();
    self.n.items[start].s[1] = right;
    const addr = try self.binop(right, .iadd, v, arg1);
    try self.putvar(right, v, addr);

    const end = try self.addNode();
    self.n.items[left].s[0] = end;
    self.n.items[right].s[0] = end;

    const const_77 = try self.const_int(end, 77);
    const adde = try self.binop(end, .iadd, v, const_77);
    try self.putvar(end, v, adde);

    try self.ret(end, v);

    try self.test_analysis();
    var phii = self.iref(adde).?.op1;
    var thephi = self.iref(phii).?;
    thephi.mckind = .ipreg;
    thephi.mcidx = 8; // r8

    self.debug_print();

    var cfo = try CFO.init(test_allocator);
    defer cfo.deinit();

    _ = try self.codegen(&cfo);
    try cfo.dbg_nasm(test_allocator);
}
