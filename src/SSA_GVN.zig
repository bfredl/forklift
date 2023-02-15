const std = @import("std");
const math = std.math;
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const Self = @This();
const print = std.debug.print;

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
    for (self.f.n.items) |n, ni| {
        try self.fill_blk(@intCast(u16, ni), n.firstblk);
    }

    // at this point all nodes have been _filled_ but join nodes (npred > 1)
    // have not been _sealed_, in the terminology of Braun 2013
    // TODO: keep a worklist of unfinished phi nodes, more effective
    while (true) {
        var did_phi: bool = false;
        for (self.f.n.items) |n, ni| {
            did_phi = did_phi or try self.resolve_blk(@intCast(u16, ni), n.firstblk);
        }
        if (!did_phi) break;
    }

    try self.delete_vars(self.f.n.items[0].firstblk);
}

fn fill_blk(self: Self, n: u16, first_blk: u16) !void {
    var it = self.f.ins_iterator(first_blk);
    while (it.next()) |item| {
        const i = item.i;
        if (i.tag == .putvar) {
            const ivar = self.f.iref(i.op1) orelse return error.UW0tM8;
            self.vdi(n, ivar.op1).* = i.op2;
            // TODO: store debug info, or some shit
            i.tag = .empty;
        } else if (.tag == .phi) {
            // TODO: likely we'll never need to consider an existing
            // phi node here but verify this!
        } else {
            const nop = i.n_op(false);
            var ival = i.*;
            if (nop > 0) {
                ival.op1 = try self.read_ref(n, ival.op1);
                if (nop > 1) {
                    ival.op2 = try self.read_ref(n, ival.op2);
                }
            }
            // read_ref might invalidate pointers
            self.f.iref(item.ref).?.* = ival;
        }
    }
}

fn vdi(self: Self, node: u16, v: u16) *u16 {
    return &self.vardef[self.f.nvar * node + v];
}

fn read_ref(self: Self, node: u16, ref: u16) !u16 {
    const i = self.f.iref(ref) orelse return ref;
    if (i.tag == .variable) {
        return try self.read_var(node, ref, i.*);
    } else {
        // already on SSA-form, nothing to do
        return ref;
    }
}

const MaybePhi = @typeInfo(@TypeOf(FLIR.prePhi)).Fn.return_type.?;
fn read_var(self: Self, node: u16, vref: u16, v: FLIR.Inst) MaybePhi {
    // It matters where you are
    const vd = self.vdi(node, v.op1);
    if (vd.* != FLIR.NoRef) {
        return vd.*;
    }

    const n = self.f.n.items[node];
    const def = thedef: {
        if (n.npred == 0) {
            return error.FLIRError; // undefined var
        } else if (n.npred == 1) {
            const pred = self.f.refs.items[n.predref];
            // assert recursion eventually terminates
            assert(self.f.n.items[pred].dfnum < n.dfnum);
            break :thedef try self.read_var(pred, vref, v);
        } else {
            // as an optimization, we could check if all predecessors
            // are filled (pred[i].dfnum < n.dfnum), and in that case
            // fill the phi node already;
            break :thedef try self.f.prePhi(node, vref);
        }
    };
    vd.* = def;
    return def;
}

fn resolve_blk(self: Self, node: u16, first_blk: u16) !bool {
    var it = self.f.ins_iterator(first_blk);
    var any = false;
    while (it.next()) |item| {
        if (item.i.tag == .phi) {
            any = any or try self.resolve_phi(node, item.i.*, item.ref);
        }
    }
    return any;
}

fn resolve_phi(self: Self, n: u16, i: FLIR.Inst, iref: u16) !bool {
    if (i.op2 == 1) return false;
    const ivar = (self.f.iref(i.op1) orelse return error.GLUGG).*;
    var onlyref: ?u16 = null;
    for (self.f.preds(n)) |v| {
        const ref = try self.read_var(v, i.op1, ivar);

        _ = try self.f.binop(v, .putphi, ref, iref);
        onlyref = if (onlyref) |only| if (only == ref) only else FLIR.NoRef else ref;
    }

    // lookup again in case self.f.b.items changed
    const i_ref = self.f.iref(iref).?;
    i_ref.op1 = 0;
    i_ref.op2 = 1; // flag for "phi already resolved"
    return true;
}

fn delete_vars(self: Self, first_blk: u16) !void {
    var it = self.f.ins_iterator(first_blk);
    while (it.next()) |item| {
        if (item.i.tag == .variable) {
            item.i.tag = .empty; // this is explicitly allowed
        }
    }
}
