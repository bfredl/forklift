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
    const pred_buf = try self.gpa.alloc(PredItem, self.n.items.len);
    defer self.gpa.free(pred_buf);

    self.unsealed = false;

    // at this point all nodes have been _filled_ but join nodes (npred > 1)
    // have not been _sealed_, in the terminology of Braun 2013
    // TODO: keep a worklist of unfinished phi nodes, more effective
    while (true) {
        var did_phi: bool = false;
        for (self.n.items, 0..) |*n, ni| {
            did_phi = did_phi or try resolve_node(self, @intCast(ni), n, pred_buf);
        }
        if (!did_phi) break;
    }

    try cleanup_trivial_phi_and_vars(self);
}

pub fn read_ref(self: *FLIR, node: u16, ref: u16) !u16 {
    const i = self.iref(ref) orelse return ref;
    if (i.tag == .variable) {
        return try read_var(self, node, ref, i.spec, null);
    } else {
        // already on SSA-form, nothing to do
        return ref;
    }
}

fn read_var(self: *FLIR, node: u16, vref: u16, vspec: u8, direct_putvar: ?*u16) !u16 {
    // It matters where you are
    const n = self.n.items[node];

    // HOW CAN VARIABLES BE REAL IF PHI:s AREN'T REAL?
    // first check if the value was assigned a value in this block.
    // Then check if it already has a phi-definition
    var put_iter = n.putphi_list;
    while (put_iter != NoRef) {
        const p = &self.i.items[put_iter];
        if (p.tag == .putvar and p.op2 == vref) {
            if (direct_putvar) |put| {
                put.* = put_iter;
            }
            return check_trivial(self, p.op1);
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
        if (node == 0) {
            // NB: might be overriden by a putvar above
            const vvar = self.iref(vref) orelse return error.FLIRError;
            if (vvar.op2 != NoRef) {
                break :thedef vvar.op2;
            } else {
                return error.FLIRError;
            }
        } else if (self.unsealed or n.npred > 1) {
            // as an optimization, we could check if all predecessors
            // are filled (pred[i].dfnum < n.dfnum), and in that case
            // fill the phi node already;
            break :thedef try FLIR.addPhi(self, node, vref, vspec);
        } else if (n.npred == 1) {
            const pred = n.predref;
            // assert recursion eventually terminates
            // TODO: still want this check somehow, we just doesn't fill dfnum yet..
            // assert(self.n.items[pred].dfnum < n.dfnum);

            // no longer direct, discard direct_putvar
            break :thedef try read_var(self, pred, vref, vspec, null);
        } else { // n.npred == 0
            return error.FLIRError; // undefined var
        }
    };

    // not useful yet:
    // vd.* = def;
    return def;
}

fn check_trivial(self: *FLIR, ref: u16) u16 {
    if (self.iref(ref)) |i| {
        if (i.tag == .phi and i.op2 != NoRef) {
            return check_trivial(self, i.op2);
        }
    }
    return ref;
}

fn resolve_node(self: *FLIR, ni: u16, n: *FLIR.Node, pred_buf: []PredItem) !bool {
    var any = false;
    var it = n.phi_list;
    while (it != NoRef) {
        const i = &self.i.items[it];
        const next = i.next;
        if (i.tag != .phi) @panic("GRAAAK");
        any = any or try resolve_phi(self, ni, i, it, pred_buf);
        it = next;
    }
    return any;
}

const PredItem = struct { ref: u16, direct_putvar: u16 };
fn resolve_phi(self: *FLIR, n: u16, i: *FLIR.Inst, iref: u16, pred_buf: []PredItem) !bool {
    var changed: bool = false;
    if (i.f.kill_op1) {
        // not resolved
        changed = true;
        // NoRef: no values seen yet, null: contradiction seen
        var onlyref: ?u16 = NoRef;
        const vref = i.op1;
        const vspec = i.spec; // TRICKY: i pointer might get invalidated by read_var()
        for (self.preds(n), 0..) |p, pi| {
            var direct_putvar: u16 = NoRef;
            const ref = try read_var(self, p, vref, vspec, &direct_putvar);
            // XX: In principle we could already handle "Undefined" being represented
            // as NoRef, but we don't support undefined yets and we likely want
            // another sentinel value than Noref, to catch mistakes easier.
            if (ref != iref) {
                onlyref = if (onlyref) |only| (if (only == ref or only == NoRef) ref else null) else null;
            }
            pred_buf[pi] = .{ .ref = ref, .direct_putvar = direct_putvar };
        }

        if (onlyref) |ref| {
            // lookup again in case self.b.items changed
            const i_ref = self.iref(iref).?;
            i_ref.f.kill_op1 = false; // flag for "resolved". techy we don't depend on op1 anymore :zany_face:
            i_ref.op2 = ref; // if set, this is a trivial alias to phi_i.op2
        } else {
            for (self.preds(n), 0..) |p, pi| {
                if (pred_buf[pi].direct_putvar != NoRef) {
                    const put = &self.i.items[pred_buf[pi].direct_putvar];
                    put.tag = .putphi;
                    put.op1 = pred_buf[pi].ref;
                    put.op2 = iref;
                } else {
                    const vn = &self.n.items[p];
                    vn.putphi_list = try self.addRawInst(.{ .tag = .putphi, .op1 = pred_buf[pi].ref, .op2 = iref, .next = vn.putphi_list, .node_delete_this = p });
                }
            }
            const i_ref = self.iref(iref).?;
            i_ref.f.kill_op1 = false; // flag for "resolved"
            // initialized as: i_ref.op2 = NoRef; // sentinel for no trivial reference.
        }
    } else if (i.op2 == NoRef) {
        const onlyref: ?u16 = theref: {
            var seen_ref: ?u16 = null;
            for (self.preds(n)) |v| {
                const phiref = try read_putphi(self, v, iref) orelse return error.FLIRError;
                const ref = check_trivial(self, phiref);
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

fn read_putphi(self: *FLIR, ni: u16, phiref: u16) !?u16 {
    const n = &self.n.items[ni];
    var item = n.putphi_list;
    while (item != NoRef) {
        const i = self.iref(item) orelse return error.FLIRError;
        if (i.tag == .putphi and i.op2 == phiref) return i.op1;
        item = i.next;
    }
    return null;
}

fn cleanup_trivial_phi_and_vars(self: *FLIR) !void {
    // Pass 1: eleminate usages of trivial phi:s (including dead puts)
    // TODO: this can be merged with a general copy-propagation pass (IF WE HAD ONE!!)
    for (self.n.items) |*n| {
        var it = self.ins_iterator(n.firstblk);
        while (it.next()) |item| {
            const i = item.i;
            for (i.ops(false)) |*op| {
                op.* = check_trivial(self, op.*);
            }
            var opnext = i.next_as_op();
            while (opnext != NoRef) {
                const ii = self.iref(opnext) orelse return error.FLIRError;
                ii.op1 = check_trivial(self, ii.op1);
                opnext = ii.next;
            }
        }
        var next_ptr: *u16 = &n.putphi_list;
        while (next_ptr.* != NoRef) {
            var keep = false; // delet all putvars, as a bonus
            const i = self.iref(next_ptr.*) orelse return error.FLIRError;
            if (i.tag == .putphi) {
                const ref = self.iref(i.op2) orelse return error.FLIRError;
                // target not marked for deletion
                if (ref.op2 == NoRef) {
                    keep = true;
                    // but check if we read any trivial phis..
                    i.op1 = check_trivial(self, i.op1);
                }
            } else if (i.tag == .retval) {
                i.op1 = check_trivial(self, i.op1);
                keep = true; // pls..
            }
            if (keep) {
                next_ptr = &i.next;
            } else {
                const delenda = next_ptr.*;
                next_ptr.* = i.next;
                self.delete_raw(delenda);
            }
        }
    }

    // Pass 2: delete trivial phi:s, now that no one refers to them
    for (self.n.items) |*n| {
        var next_ptr: *u16 = &n.phi_list;
        while (next_ptr.* != NoRef) {
            const i = self.iref(next_ptr.*) orelse return error.FLIRError;
            if (self.iref(i.op1)) |ivar| {
                if (ivar.tag != .variable) return error.FLIRError;
                // only for debugging: refer to origin variable by index
                // (NOT A PROMISE TO KEEP CSSA, optimization will break that)
                // i_ref.op1 = self.iref(vref).?.op1;
                i.op1 = ivar.op1;
            }
            if (i.op2 != NoRef) {
                const delenda = next_ptr.*;
                next_ptr.* = i.next;
                self.delete_raw(delenda);
            } else {
                next_ptr = &i.next;
            }
        }
    }

    // variables? haha no such thing as variables
    while (self.var_list != NoRef) {
        const ref = self.var_list;
        self.var_list = self.i.items[ref].next;
        self.delete_raw(ref);
    }
}
