const std = @import("std");
const math = std.math;
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const Self = @This();
const print = std.debug.print;

const FLIR = @import("./FLIR.zig");
const NoRef = FLIR.NoRef;

f: *FLIR,
vardef: []u16,
predbuf: []u16,

// Simple and Eﬃcient Construction of Static Single Assignment Form
// Matthias Braun et al, 2013
pub fn ssa_gvn(flir: *FLIR) !void {
    const vardef = try flir.a.alloc(u16, flir.n.items.len * flir.nvar);
    const predbuf = try flir.a.alloc(u16, flir.n.items.len);
    defer flir.a.free(vardef);
    defer flir.a.free(predbuf);
    @memset(vardef, NoRef);

    const self: Self = .{ .f = flir, .vardef = vardef, .predbuf = predbuf };
    try self.ssa();
}

fn ssa(self: Self) !void {
    for (self.f.n.items, 0..) |n, ni| {
        try self.fill_blk(@intCast(ni), n.firstblk);
    }

    // at this point all nodes have been _filled_ but join nodes (npred > 1)
    // have not been _sealed_, in the terminology of Braun 2013
    // TODO: keep a worklist of unfinished phi nodes, more effective
    while (true) {
        var did_phi: bool = false;
        for (self.f.n.items, 0..) |n, ni| {
            did_phi = did_phi or try self.resolve_blk(@intCast(ni), n.firstblk);
        }
        if (!did_phi) break;
    }

    try self.delete_vars(self.f.n.items[0].firstblk);
    try self.cleanup_trivial_phi();
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
    if (vd.* != NoRef) {
        return self.check_trivial(vd.*);
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
    // not useful yet:
    // def = check_trivial(def);
    vd.* = def;
    return def;
}

fn check_trivial(self: Self, ref: u16) u16 {
    if (self.f.iref(ref)) |i| {
        if (i.tag == .phi and i.op2 == 2) {
            return self.check_trivial(i.op1);
        }
    }
    return ref;
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
    var changed: bool = false;
    if (i.op2 == 0) {
        // not resolved
        changed = true;
        // NoRef: no values seen yet, null: contradiction seen
        var onlyref: ?u16 = NoRef;
        const ivar = (self.f.iref(i.op1) orelse return error.GLUGG).*;
        const op1 = i.op1; // TRICKY: i might be hidden pointer to f.b.items
        for (self.f.preds(n), 0..) |v, vi| {
            const ref = try self.read_var(v, op1, ivar);
            // XX: In principle we could already handle "Undefined" being represented
            // as NoRef, but we don't support undefined yets and we likely want
            // another sentinel value than Noref, to catch mistakes easier.
            onlyref = if (onlyref) |only| (if (only == ref or only == NoRef) ref else null) else null;
            self.predbuf[vi] = ref;
        }

        if (onlyref) |ref| {
            // lookup again in case self.f.b.items changed
            const i_ref = self.f.iref(iref).?;
            i_ref.op1 = ref;
            i_ref.op2 = 2; // flag for "trivial phi"
        } else {
            for (self.f.preds(n), 0..) |v, vi| {
                _ = try self.f.binop(v, .putphi, self.predbuf[vi], iref);
            }
            const i_ref = self.f.iref(iref).?;
            i_ref.op1 = 0;
            i_ref.op2 = 1; // flag for "resolved"
        }
    } else if (i.op2 == 1) {
        // TODO: check if was trivialized
    }

    return changed;
}

fn delete_vars(self: Self, first_blk: u16) !void {
    var it = self.f.ins_iterator(first_blk);
    while (it.next()) |item| {
        if (item.i.tag == .variable) {
            item.i.tag = .empty;
        }
    }
}

// TODO: this can be merged with a general copy-propagation pass (IF WE HAD ONE!!)
fn cleanup_trivial_phi(self: Self) !void {
    var ni = self.f.n.items.len - 1;

    while (true) : (ni -= 1) {
        if (ni == 0) break;
        const n = &self.f.n.items[ni];

        // TODO: archetypical example of an insn _backwards_ iterator
        var cur_blk: ?u16 = n.lastblk;
        while (cur_blk) |blk| {
            var b = &self.f.b.items[blk];
            var idx: u16 = FLIR.BLK_SIZE;
            while (idx > 0) {
                idx -= 1;
                const i = &b.i[idx];

                // phi reading phi was already handled in resolve_phi()
                if (i.tag != .phi) {
                    const nop = i.n_op(false);
                    if (nop > 0) {
                        i.op1 = self.check_trivial(i.op1);
                        if (nop > 1) {
                            i.op2 = self.check_trivial(i.op2);
                        }
                    }
                } else {
                    if (i.op2 == 2) {
                        // by effect by reverse traversal, have already fixed any uses
                        i.tag = .empty;
                    }
                }
            }

            cur_blk = b.prev();
        }
    }
}
