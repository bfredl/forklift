const std = @import("std");
const math = std.math;
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const Self = @This();

const FLIR = @import("./FLIR2.zig");

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
    const n = self.f.n.items;

    for (self.f.dforder()) |i| {
        try self.fill_blk(n[i].firstblk);
    }

    // at this point all nodes have been _filled_ but join nodes (npred > 1)
    // have not been _sealed_, in the terminology of Braun 2013
    // TODO: keep a worklist of unfinished phi nodes, more effective +
    // otherwise will need multiple passes until a fix point
    for (self.f.dforder()) |i| {
        try self.resolve_blk(n[i].firstblk);
    }
}

fn fill_blk(self: Self, b: u16) !void {
    const blk = &self.f.b.items[b];
    const n = blk.node;

    for (blk.i) |*i| {
        if (i.tag == .putvar) {
            const ivar = self.f.iref(i.op1) orelse return error.UW0tM8;
            self.vdi(n, ivar.op1).* = i.op2;
            // print("PUTTA: [{},{}] := {}\n", .{ n, ivar.op1, i.op2 });
        } else if (.tag == .phi) {
            // TODO: likely we'll never need to consider an existing
            // phi node here but verify this!
        } else {
            const nop = FLIR.n_op(i.tag);
            if (nop > 0) {
                i.op1 = try self.read_ref(n, i.op1);
                if (nop > 1) {
                    i.op2 = try self.read_ref(n, i.op2);
                    // TODO: delet this:
                    if (nop > 2) {
                        i.op3 = try self.read_ref(n, i.op3);
                    }
                }
            }
        }
    }

    // TODO: more like a loop?
    if (blk.next()) |next| {
        return self.fill_blk(next);
    }
}

fn vdi(self: Self, node: u16, v: u16) *u16 {
    return &self.vardef[self.f.nvar * node + v];
}

fn read_ref(self: Self, node: u16, ref: u16) !u16 {
    const i = self.f.iref(ref) orelse return FLIR.NoRef;
    if (i.tag == .variable) {
        return self.read_var(node, i.op1);
    } else {
        // already on SSA-form, nothing to do
        return ref;
    }
}

const MaybePhi = @typeInfo(@TypeOf(FLIR.prePhi)).Fn.return_type.?;
fn read_var(self: Self, node: u16, v: u16) MaybePhi {
    // It matters where you are
    const vd = self.vdi(node, v);
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
            break :thedef try self.f.prePhi(node, self.f.narg + v);
        }
    };
    vd.* = def;
    // print("CACHEA: [{},{}] := {}\n", .{ node, v, def });
    return def;
}

fn resolve_blk(self: Self, b: u16) !void {
    const blk = &self.f.b.items[b];

    for (blk.i) |*i, idx| {
        if (i.tag == .phi) {
            try self.resolve_phi(b, FLIR.uv(idx));
        }
    }
    // TODO: more like a loop?
    if (blk.next()) |next| {
        return self.resolve_blk(next);
    }
}
fn resolve_phi(self: Self, b: u16, idx: u16) !void {
    const blk = &self.f.b.items[b];
    const i = &blk.i[idx];
    if (i.spec == 1) return;
    const ivar = self.f.iref(i.op1) orelse return error.GLUGG;
    var onlyref: ?u16 = null;
    for (self.f.preds(blk.node)) |v| {
        const ref = try self.read_var(v, ivar.op1);
        _ = try self.f.binop(v, .putphi, FLIR.toref(b, idx), ref);
        onlyref = if (onlyref) |only| if (only == ref) only else FLIR.NoRef else ref;
    }
    i.spec = 1;
}
