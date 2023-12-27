const std = @import("std");
const math = std.math;
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const print = std.debug.print;

const FLIR = @import("./FLIR.zig");
const NoRef = FLIR.NoRef;

// Simple and Eﬃcient Construction of Static Single Assignment Form
// Matthias Braun et al, 2013
pub fn resolve_ssa(self: *FLIR) !void {
    const predbuf = try self.a.alloc(u16, self.n.items.len);
    defer self.a.free(predbuf);

    self.unsealed = false;

    // at this point all nodes have been _filled_ but join nodes (npred > 1)
    // have not been _sealed_, in the terminology of Braun 2013
    // TODO: keep a worklist of unfinished phi nodes, more effective
    while (true) {
        var did_phi: bool = false;
        for (self.n.items, 0..) |n, ni| {
            did_phi = did_phi or try self.resolve_blk(@intCast(ni), n.firstblk, predbuf);
        }
        if (!did_phi) break;
    }

    try self.delete_vars();
    try self.cleanup_trivial_phi();
}

fn fill_blk(self: *FLIR, n: u16, first_blk: u16) !void {
    var it = self.ins_iterator(first_blk);
    while (it.next()) |item| {
        const i = item.i;
        if (i.tag == .putvar) {
            // TODO: store debug info, or some shit
            // careful with pointers, i.* is valid before read_ref():
            const op1 = i.op1;
            const readval = try self.read_ref(n, i.op2);
            const ivar = self.iref(op1) orelse return error.UW0tM8;
            self.vdi(n, ivar.op1).* = readval;
            self.delete_itersafe(item);
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
            self.iref(item.ref).?.* = ival;
        }
    }
}

pub fn read_ref(self: *FLIR, node: u16, ref: u16) !u16 {
    const i = self.iref(ref) orelse return ref;
    if (i.tag == .variable) {
        // return try self.read_var(node, ref, i.op1);
        return try self.read_var(node, ref, i.op1);
    } else {
        // already on SSA-form, nothing to do
        return ref;
    }
}

const MaybePhi = @typeInfo(@TypeOf(FLIR.prePhi)).Fn.return_type.?;

pub fn read_var(self: *FLIR, node: u16, vref: u16, vop1: u16) MaybePhi {
    // It matters where you are
    const n = self.n.items[node];

    // HOW CAN VARIABLES BE REAL IF PHI:s AREN'T REAL?
    // first check if the value was assigned a value in this block.
    // Then check if it already has a phi-definition
    var put_iter = n.putphi_list;
    while (put_iter != NoRef) {
        const p = &self.i.items[put_iter];
        if (p.tag == .putvar and p.op2 == vref) {
            return self.check_trivial(p.op1);
        }
        put_iter = p.next;
    }

    var phi_iter = n.phi_list;
    while (phi_iter != NoRef) {
        const p = &self.i.items[phi_iter];
        if (p.op1 == vref) {
            return phi_iter;
        }
        phi_iter = p.next;
    }

    const def: u16 = thedef: {
        if (self.unsealed or n.npred > 1) {
            // as an optimization, we could check if all predecessors
            // are filled (pred[i].dfnum < n.dfnum), and in that case
            // fill the phi node already;
            break :thedef try FLIR.prePhi(self, node, vref);
        } else if (n.npred == 1) {
            const pred = self.refs.items[n.predref];
            // assert recursion eventually terminates
            assert(self.n.items[pred].dfnum < n.dfnum);
            // self.read_var() doesn't work (no recursive mixin methods :P)
            break :thedef try read_var(self, pred, vref, vop1);
        } else { // n.npred == 0
            return error.FLIRError; // undefined var
        }
    };

    // not useful yet:
    // vd.* = def;
    return def;
}

pub fn check_trivial(self: *FLIR, ref: u16) u16 {
    if (self.iref(ref)) |i| {
        if (i.tag == .phi and i.op2 != NoRef) {
            return check_trivial(self, i.op2);
        }
    }
    return ref;
}

pub fn resolve_blk(self: *FLIR, node: u16, first_blk: u16, pred_buf: []u16) !bool {
    var it = self.ins_iterator(first_blk);
    var any = false;
    while (it.next()) |item| {
        if (item.i.tag == .phi) {
            any = any or try self.resolve_phi(node, item.i, item.ref, pred_buf);
        }
    }
    return any;
}

pub fn resolve_phi(self: *FLIR, n: u16, i: *FLIR.Inst, iref: u16, pred_buf: []u16) !bool {
    var changed: bool = false;
    if (i.f.kill_op1) {
        // not resolved
        changed = true;
        // NoRef: no values seen yet, null: contradiction seen
        var onlyref: ?u16 = NoRef;
        const vref = i.op1; // TRICKY: i pointer might get invalidated by read_var()
        const vop1 = (self.iref(vref) orelse return error.GLUGG).op1;
        for (self.preds(n), 0..) |v, vi| {
            const ref = try self.read_var(v, vref, vop1);
            // XX: In principle we could already handle "Undefined" being represented
            // as NoRef, but we don't support undefined yets and we likely want
            // another sentinel value than Noref, to catch mistakes easier.
            if (ref != iref) {
                onlyref = if (onlyref) |only| (if (only == ref or only == NoRef) ref else null) else null;
            }
            pred_buf[vi] = ref;
        }

        if (onlyref) |ref| {
            // lookup again in case self.b.items changed
            const i_ref = self.iref(iref).?;
            i_ref.f.kill_op1 = false; // flag for "resolved". techy we don't depend on op1 anymore :zany_face:
            i_ref.op2 = ref; // if set, this is a trivial alias to phi_i.op2

            // i_phi.op1 is not used anymore. But the dense variable index is more useful for debugging, later.
            // I forgor why don't we store it directly :P
            i_ref.op1 = vop1;
        } else {
            for (self.preds(n), 0..) |v, vi| {
                _ = try self.binop(v, .putphi, pred_buf[vi], iref);
            }
            const i_ref = self.iref(iref).?;
            i_ref.f.kill_op1 = false; // flag for "resolved"
            i_ref.op1 = vop1;
            // initialized as: i_ref.op2 = NoRef; // sentinel for no trivial reference.
        }
    } else if (i.op2 == NoRef) {
        const onlyref: ?u16 = theref: {
            var seen_ref: ?u16 = null;
            for (self.preds(n)) |v| {
                const phiref = self.read_putphi(v, iref) orelse return error.FLIRError;
                const ref = self.check_trivial(phiref);
                if (ref != iref) {
                    if (seen_ref) |seen| {
                        if (seen != ref) break :theref null;
                    } else {
                        seen_ref = ref;
                    }
                }
            }
            break :theref seen_ref;
        };
        if (onlyref) |ref| {
            i.op2 = ref; // found trivial reference
            changed = true;
        }
    }

    return changed;
}

pub fn delete_vars(self: *FLIR) !void {
    while (self.var_list != NoRef) {
        const ref = self.var_list;
        self.var_list = self.i.items[ref].next;
        self.delete_raw(ref);
    }
}

pub fn read_putphi(self: *FLIR, ni: u16, phiref: u16) ?u16 {
    const n = &self.n.items[ni];
    var it = self.ins_iterator_rev(n.lastblk);
    while (it.next_rev()) |item| {
        if (item.i.tag != .putphi) break;
        if (item.i.op2 == phiref) return item.i.op1;
    }
    return null;
}

// TODO: this can be merged with a general copy-propagation pass (IF WE HAD ONE!!)
pub fn cleanup_trivial_phi(self: *FLIR) !void {
    var ni = self.n.items.len - 1;

    while (true) : (ni -= 1) {
        const n = &self.n.items[ni];
        var it = self.ins_iterator_rev(n.lastblk);
        while (it.next_rev()) |item| {
            const i = item.i;
            if (i.tag == .phi) {
                // phi reading phi was already handled in resolve_phi()
                if (i.op2 != NoRef) {
                    // by effect by reverse traversal, have already fixed any uses
                    self.delete_itersafe(item);
                }
            } else if (i.tag == .putphi) {
                const ref = self.iref(i.op2) orelse return error.FLIRError;
                // TODO: accessing "freelist" NOT ok, but soon we are gonna change over all this anyway
                if (ref.tag == .freelist or ref.op2 != NoRef) {
                    self.delete_itersafe(item);
                } else {
                    i.op1 = self.check_trivial(i.op1);
                }
            } else {
                const nop = i.n_op(false);
                if (nop > 0) {
                    i.op1 = self.check_trivial(i.op1);
                    if (nop > 1) {
                        i.op2 = self.check_trivial(i.op2);
                    }
                }
            }
        }
        if (ni == 0) break;
    }
}
