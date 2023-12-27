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
        for (self.n.items, 0..) |*n, ni| {
            did_phi = did_phi or try self.resolve_node(@intCast(ni), n, predbuf);
        }
        if (!did_phi) break;
    }

    try self.delete_vars();
    try self.cleanup_trivial_phi();
}

pub fn read_ref(self: *FLIR, node: u16, ref: u16) !u16 {
    const i = self.iref(ref) orelse return ref;
    if (i.tag == .variable) {
        // return try self.read_var(node, ref, i.op1);
        return try self.read_var(node, i.op1, i.spec);
    } else {
        // already on SSA-form, nothing to do
        return ref;
    }
}

pub fn read_var(self: *FLIR, node: u16, vidx: u16, vspec: u8) !u16 {
    // It matters where you are
    const n = self.n.items[node];

    // HOW CAN VARIABLES BE REAL IF PHI:s AREN'T REAL?
    // first check if the value was assigned a value in this block.
    // Then check if it already has a phi-definition
    var put_iter = n.putphi_list;
    while (put_iter != NoRef) {
        const p = &self.i.items[put_iter];
        if (p.tag == .putvar and p.op2 == vidx) {
            return self.check_trivial(p.op1);
        }
        put_iter = p.next;
    }

    var phi_iter = n.phi_list;
    while (phi_iter != NoRef) {
        const p = &self.i.items[phi_iter];
        if (p.op1 == vidx) {
            return phi_iter;
        }
        phi_iter = p.next;
    }

    const def: u16 = thedef: {
        if (self.unsealed or n.npred > 1) {
            // as an optimization, we could check if all predecessors
            // are filled (pred[i].dfnum < n.dfnum), and in that case
            // fill the phi node already;
            break :thedef try FLIR.prePhi(self, node, vidx, vspec);
        } else if (n.npred == 1) {
            const pred = self.refs.items[n.predref];
            // assert recursion eventually terminates
            assert(self.n.items[pred].dfnum < n.dfnum);
            // self.read_var() doesn't work (no recursive mixin methods :P)
            break :thedef try read_var(self, pred, vidx, vspec);
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

pub fn resolve_node(self: *FLIR, ni: u16, n: *FLIR.Node, pred_buf: []u16) !bool {
    var any = false;
    var it = n.phi_list;
    while (it != NoRef) {
        const i = &self.i.items[it];
        const next = i.next;
        if (i.tag != .phi) @panic("GRAAAK");
        any = any or try self.resolve_phi(ni, i, it, pred_buf);
        it = next;
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
        const vidx = i.op1; // TRICKY: i pointer might get invalidated by read_var()
        const vspec = i.spec;
        for (self.preds(n), 0..) |v, vi| {
            const ref = try self.read_var(v, vidx, vspec);
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
        } else {
            for (self.preds(n), 0..) |v, vi| {
                _ = try self.binop(v, .putphi, pred_buf[vi], iref);
            }
            const i_ref = self.iref(iref).?;
            i_ref.f.kill_op1 = false; // flag for "resolved"
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
