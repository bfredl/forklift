const std = @import("std");
const math = std.math;
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const Self = @This();

const FLIR = @import("./FLIR.zig");

f: *FLIR,
vardef: []u16,

// Simple and Eﬃcient Construction of Static Single Assignment Form
// Matthias Braun et al, 2013
pub fn ssa_gvn(flir: *FLIR) !void {
    const vardef = try flir.a.alloc(u16, flir.n.items.len * flir.nvar);
    defer flir.a.free(vardef);
    std.mem.set(u16, vardef, FLIR.NoRef);

    const self: Self = .{ .f = flir, .vardef = vardef };
    try self.ssa();
}

fn ssa(self: Self) !void {
    for (self.f.n.items) |n| {
        try self.fill_blk(n.firstblk);
    }

    // at this point all nodes have been _filled_ but join nodes (npred > 1)
    // have not been _sealed_, in the terminology of Braun 2013
    // TODO: keep a worklist of unfinished phi nodes, more effective +
    // otherwise might need multiple passes until a fix point
    for (self.f.n.items) |n| {
        try self.resolve_blk(n.firstblk);
    }

    try self.delete_vars(self.f.n.items[0].firstblk);
}

fn fill_blk(self: Self, first_blk: u16) !void {
    var cur_blk: ?u16 = first_blk;
    while (cur_blk) |blk| {
        var b = &self.f.b.items[blk];
        const n = b.node;

        for (b.i) |*i| {
            if (i.tag == .putvar) {
                const ivar = self.f.iref(i.op1) orelse return error.UW0tM8;
                self.vdi(n, ivar.op1).* = i.op2;
                // TODO: store debug info, or some shit
                i.tag = .empty;
            } else if (.tag == .phi) {
                // TODO: likely we'll never need to consider an existing
                // phi node here but verify this!
            } else {
                const nop = FLIR.n_op(i.tag, false);
                if (nop > 0) {
                    i.op1 = try self.read_ref(n, i.op1);
                    if (nop > 1) {
                        i.op2 = try self.read_ref(n, i.op2);
                    }
                }
            }
        }

        cur_blk = b.next();
    }
}

fn vdi(self: Self, node: u16, v: u16) *u16 {
    return &self.vardef[self.f.nvar * node + v];
}

fn read_ref(self: Self, node: u16, ref: u16) !u16 {
    const i = self.f.iref(ref) orelse return FLIR.NoRef;
    if (i.tag == .variable) {
        return self.read_var(node, i.*);
    } else {
        // already on SSA-form, nothing to do
        return ref;
    }
}

const MaybePhi = @typeInfo(@TypeOf(FLIR.prePhi)).Fn.return_type.?;
fn read_var(self: Self, node: u16, v: FLIR.Inst) MaybePhi {
    // It matters where you are
    const vd = self.vdi(node, v.op1);
    if (vd.* != FLIR.NoRef) {
        return vd.*;
    }

    const n = self.f.n.items[node];
    const def = thedef: {
        if (n.npred == 0) {
            unreachable; // TODO: error for undefined var
        } else if (n.npred == 1) {
            const pred = self.f.refs.items[n.predref];
            // assert recursion eventually terminates
            assert(self.f.n.items[pred].dfnum < n.dfnum);
            break :thedef try self.read_var(pred, v);
        } else {
            // as an optimization, we could check if all predecessors
            // are filled (pred[i].dfnum < n.dfnum), and in that case
            // fill the phi node already;
            break :thedef try self.f.prePhi(node, v);
        }
    };
    vd.* = def;
    return def;
}

fn resolve_blk(self: Self, first_blk: u16) !void {
    var cur_blk: ?u16 = first_blk;
    while (cur_blk) |blk| {
        var b = &self.f.b.items[blk];
        for (b.i) |*i, idx| {
            if (i.tag == .phi) {
                try self.resolve_phi(blk, FLIR.uv(idx));
            }
        }
        cur_blk = b.next();
    }
}
fn resolve_phi(self: Self, b: u16, idx: u16) !void {
    const blk = &self.f.b.items[b];
    const i = &blk.i[idx];
    if (i.op2 == 1) return;
    const ivar = self.f.iref(i.op1) orelse return error.GLUGG;
    var onlyref: ?u16 = null;
    for (self.f.preds(blk.node)) |v| {
        const ref = try self.read_var(v, ivar.*);
        _ = try self.f.binop(v, .putphi, ref, FLIR.toref(b, idx));
        onlyref = if (onlyref) |only| if (only == ref) only else FLIR.NoRef else ref;
    }
    i.op1 = 0;
    i.op2 = 1; // flag for "phi already resolved"
}

fn delete_vars(self: Self, first_blk: u16) !void {
    var cur_blk: ?u16 = first_blk;
    while (cur_blk) |blk| {
        var b = &self.f.b.items[blk];
        for (b.i) |*i| {
            if (i.tag == .variable) {
                i.tag = .empty;
            }
        }
        cur_blk = b.next();
    }
}
