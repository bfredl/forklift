const FLIR = @import("./FLIR.zig");
const Inst = @import("./Inst.zig");
const std = @import("std");
const defs = @import("./defs.zig");
const X86Asm = @import("./X86Asm.zig");
const print = std.debug.print;
const uv = FLIR.uv;
const NoRef = FLIR.NoRef;

// we consider there to be 3 kinds of intervals:
// vreg intervalls:
//   1. born at definition point
//   2a. at the beginning of each node: activated or deactivated as per live_in
//   2b. deactivated at a kill flag
// temporary intervalls:
//   1. born at definition (a flag indicates that the result is ever used
//   2. deactivated at a kill flag in the same block (must exist if-and-only-if born flag was set)
// fixed intervalls: ABI and instruction constraints mandating specific register
//   -- TODO: not implemented in first iteration
pub fn scan_alloc(self: *FLIR, comptime ABI: type, opts: defs.CFOOptions) !void {
    // as allocation is greedy (currently) we will only use the latter when the 8 first are all filled
    const reg_first_save = 9;
    var highest_used: u8 = 0;

    print("refett \n", .{});

    for (0.., self.n.items) |nir, *n| {
        const ni = uv(nir);
        // registers currently free.
        var free_regs_ip: [FLIR.n_ipreg]bool = @splat(true);
        var free_regs_avx: [16]bool = @splat(true);

        // NOTE: registers not in ABI.reg_order will never be used, make this more explicit?

        if (opts.dbg_regalloc) {
            print("welcome to: {}\n", .{nir});
        }

        // any vreg which is "live in" should already be allocated. mark these as non-free
        for (self.vregs.items, 0..) |vref, vi| {
            const vr = self.iref(vref.ref).?;
            const flag = defs.vreg_flag(@intCast(vi));
            if ((flag & n.live_in) != 0) {
                if (vr.mckind == .ipreg) free_regs_ip[vr.mcidx] = false;
                if (opts.dbg_regalloc and vr.mckind == .ipreg) {
                    // const reg: X86Asm.IPReg = @enumFromInt(vr.mcidx);
                    // print("oops! %{} live as vr {} and allocated {}\n", .{ vref.ref, vi, reg });
                }
                if (vr.mckind == .vfreg) free_regs_avx[vr.mcidx] = false;
            }
        }

        var it = self.ins_iterator(n.firstblk);

        // TODO: this is tricky, we need to do something like mark all register args
        // as "used" (and thus blocked for all other args than itself)
        // alternatively run through all args twice, and at first round only allow
        // trivial allocations (keep register arg as it is)
        var phi = n.phi_list;
        var lastphi: u16 = NoRef;
        while (phi != NoRef) {
            const next = self.i.items[phi].next;
            const newnum = try alloc_inst(self, ABI, ni, phi, &free_regs_ip, &free_regs_avx, &highest_used, &it);
            if (newnum) |num| {
                if (lastphi != NoRef) {
                    self.i.items[lastphi].next = num;
                } else {
                    n.phi_list = num;
                }
            }

            lastphi = phi;
            phi = next;
        }
        while (it.next()) |item| {
            const i = item.i;
            // const ref = item.ref;

            const is_avx = (i.res_type() == .avxval);

            // call args/ret handled below, but op1 can be a function pointer
            if (i.f.kill_op1) {
                if (self.iref(i.op1)) |op| {
                    if (op.mckind == .ipreg) free_regs_ip[op.mcidx] = true;
                    if (op.mckind == .vfreg) free_regs_avx[op.mcidx] = true;
                    if (i.mckind == .unallocated_raw and op.mckind == .ipreg and !is_avx) {
                        i.mckind = .unallocated_ipreghint;
                        i.mcidx = op.mcidx;
                    }
                }
            }

            // note: the risk here out of convenience is that "call" tag becomes
            // a standin for "anything which uses lists for inputs and outputs".
            // For the scale of this silly project, that might end up as the right thing.
            if (i.tag != .call) {
                // TODO: reghint for killed op2? (if symmetric, like add usw)
                if (i.f.kill_op2) {
                    if (self.iref(i.op2)) |op| {
                        if (op.mckind == .ipreg) free_regs_ip[op.mcidx] = true;
                        if (op.mckind == .vfreg) free_regs_avx[op.mcidx] = true;
                    }
                }
                if (i.f.kill_op3) {
                    if (self.iref(i.next)) |op| {
                        if (op.mckind == .ipreg) free_regs_ip[op.mcidx] = true;
                        if (op.mckind == .vfreg) free_regs_avx[op.mcidx] = true;
                    }
                }

                const num = try alloc_inst(self, ABI, ni, item.ref, &free_regs_ip, &free_regs_avx, &highest_used, &it);
                if (num) |_| return error.FLIRError; // not possible yet
            } else {
                var opnext = i.next_as_op();
                while (opnext != NoRef) {
                    const ii = self.iref(opnext) orelse return error.FLIRError;
                    if (ii.f.kill_op1) {
                        if (self.iref(ii.op1)) |op| {
                            if (op.mckind == .ipreg) free_regs_ip[op.mcidx] = true;
                            if (op.mckind == .vfreg) free_regs_avx[op.mcidx] = true;
                        }
                    }
                    opnext = ii.next;
                }

                var rnext = i.op2;
                while (rnext != NoRef) {
                    const ii = self.iref(rnext) orelse return error.FLIRError;
                    // Even if `.callret` instructions end up "fixed", somewhere
                    // around here the cereal spiller would need to consider to
                    // split it anyway, to the same effect
                    const num = try alloc_inst(self, ABI, ni, rnext, &free_regs_ip, &free_regs_avx, &highest_used, null);
                    if (num) |_| return error.FLIRError; // not done yet
                    rnext = ii.next;
                }
            }
        }

        var put_iter = n.putphi_list;
        while (put_iter != NoRef) {
            const i = &self.i.items[put_iter];
            if (i.tag == .putphi) {
                // NB: we don't need to worry about kill i.op1 as we are at the end of the node
                const from = if (self.iref(i.op1)) |f| f.ipreg() else null;
                if (from) |reg| {
                    const to = self.iref(i.op2).?;
                    if (to.mckind == .unallocated_raw) {
                        to.mckind = .unallocated_ipreghint;
                        to.mcidx = reg.id();
                    }
                }
            }
            put_iter = i.next;
        }
    }

    if (opts.dbg_vregs) {
        print("used {} general purpose regs\n", .{highest_used + 1});
    }

    if (highest_used >= reg_first_save) {
        self.nsave = highest_used - reg_first_save + 1;
    }
}

pub fn alloc_inst(self: *FLIR, comptime ABI: type, node: u16, iid: u16, free_regs_ip: *[FLIR.n_ipreg]bool, free_regs_avx: *[16]bool, highest_used: *u8, iter_for_ins: ?*FLIR.InsIterator) !?u16 {
    // also return value from functions, division result on intel machines, etc etc
    var i = self.iref(iid) orelse unreachable;
    const fixed = i.tag == .arg;
    if (!(i.has_res() and (fixed or i.mckind.unallocated()))) {
        // TODO: handle vregs with a pre-allocated register
        return null;
    }

    const is_avx = (i.res_type() == .avxval);
    var usable_regs: [16]bool = undefined;
    var free_regs = if (is_avx) free_regs_avx else free_regs_ip;
    const reg_kind: defs.MCKind = if (is_avx) .vfreg else .ipreg;

    @memcpy(&usable_regs, free_regs);
    var conflicts: u16 = 0;
    if (i.f.conflicts) {
        conflicts |= i.vreg_scratch;
    }

    if (i.vreg()) |vreg| {
        const v = self.vregs.items[vreg];
        const imask = v.live_in;
        conflicts |= v.conflicts; // fixed reg conflicts
        // This O(n_intervals^2) check exists in Wimmer, Mössenböck (2005)
        // but is DELETED in Wimmer, Franz (2010) which is what we are doing
        // Now, This ONLY happens for vreg and ivreg being two args, although
        // multiple register returns could also trigger it??
        // reformulate this accordingly!!! like these fixed intervals are already "live"
        // but we are still considering them because we might split them eagerly?? or no??
        for (0.., self.vregs.items) |ivreg, vref| {
            if (vreg == ivreg) continue;
            const vr = self.iref(vref.ref).?;
            if (vr.mckind != reg_kind) continue;
            if ((imask & vref.live_in) != 0) {
                // mckind checked above
                if (usable_regs[vr.mcidx]) {
                    if (reg_kind == .ipreg) {
                        print("=== ACHTENG in {}? {} {}: {s}\n", .{ node, vref.ref, iid, @tagName(@as(X86Asm.IPReg, @fromBackingInt(@intCast(vr.mcidx)))) });
                    }
                }
                usable_regs[vr.mcidx] = false;
                break;
            }
        }
    }

    if (conflicts != 0) {
        for (0..FLIR.n_ipreg) |r| {
            if ((conflicts & defs.ipreg_flag(@intCast(r))) != 0) {
                usable_regs[r] = false;
            }
        }
    }

    // TODO: this would be handled by materializing the constant 1 in "VAL2 = 1 << VAL1"
    // as a register, in a proper target dependent isel-pass, IF WE HAD ONE
    if (i.tag == .ibinop and i.ibinop().asShift() != null and FLIR.constidx(i.op1) != null) {
        if (self.iref(i.op2)) |amt| {
            if (amt.mckind == .ipreg) {
                usable_regs[amt.mcidx] = false;
            }
        }
    }
    // TODO: paniked fix, merge with the above :D
    if (i.tag == .ibinop) {
        if (self.iref(i.op2)) |rhs| {
            if (rhs.mckind == .ipreg) {
                const ok = is_ok: {
                    if (self.iref(i.op1)) |lhs| {
                        if (lhs.mckind == .ipreg) {
                            break :is_ok lhs.mcidx == rhs.mcidx;
                        }
                    }
                    break :is_ok false;
                };
                if (!ok) usable_regs[rhs.mcidx] = false;
            }
        }
    }

    var chosen_reg: ?u8 = null;

    // NB: currently there is no praxis for back-propagating reghints
    // like these should obviosly work across a (%1 = phi; ret %1) and so on and so on
    if (i.mckind == .unallocated_ipreghint or i.mckind == .ipreg) {
        if (i.res_type() != .intptr) unreachable;
        if (usable_regs[i.mcidx]) {
            chosen_reg = i.mcidx;
        }
    } else if (i.mckind == .unallocated_vfreghint or i.mckind == .vfreg) {
        if (i.res_type() != .avxval) unreachable;
        if (usable_regs[i.mcidx]) {
            chosen_reg = i.mcidx;
        }
    } else {
        // either unallocated, or perhaps a memoryval
    }

    if (chosen_reg == null) {
        // TODO: in the og wimmer paper they do sorting of the "longest" free interval. how do we
        // do this if without reorder_inst?
        if (is_avx) {
            for (usable_regs, 0..) |usable, reg| {
                if (usable) {
                    chosen_reg = @as(u8, @intCast(reg)); // TODO remove @as
                    break;
                }
            }
        } else {
            for (ABI.reg_order, 0..) |reg_try, reg_i| {
                if (reg_i >= self.max_ipreg_use) {
                    break; // artificial pressure to test spilling
                }
                if (usable_regs[reg_try.id()]) {
                    chosen_reg = reg_try.id();
                    if (reg_i > highest_used.*) {
                        highest_used.* = @intCast(reg_i);
                    }
                    break;
                }
            }
        }
    }

    if (chosen_reg == null) {
        // AllocateBlockedReg
        // var next_use_pos: [16]u16 = undefined;

        print("I am %{}\n", .{iid});
    }

    const chosen = chosen_reg orelse return error.NotImplemented; // "implement interval splitting"

    free_regs[chosen] = false;
    if (fixed) {
        if ((i.mckind == .ipreg or i.mckind == .vfreg) and i.mcidx == chosen) {
            // exact match, we're done
            // TODO: it is weird that there is no marker on the ins going from
            // unallocated to allocated here!
            return null;
        }
        // tricky: we make the copy use the number of i;
        const copyspec = i.mem_type().into();
        const newarg = try self.addRawInst(i.*);
        const copy: Inst = .{ .tag = .copy, .op1 = newarg, .op2 = NoRef, .mckind = reg_kind, .mcidx = chosen, .spec = copyspec };
        self.i.items[iid] = copy;
        try put_for_movelist(self, node, iter_for_ins, iid);

        return newarg;
    }
    i.mckind = reg_kind;
    i.mcidx = chosen;
    return null;
}

pub fn put_for_movelist(self: *FLIR, node: u16, iter: ?*FLIR.InsIterator, inst: u16) !void {
    // TODO: unify insertion code?
    if (iter) |it| {
        try it.insert_before(inst);
    } else {
        // insert at end of node
        try self.addInstRef(node, inst);
    }
}

// Resolve a list of parallel moves. Currently supported:
// callarg instruction: source in i.op1, destination i itself
// putphi instruction: source in i.op1, destination in i.op2 (the phi instruction)
pub fn resolve_movelist(self: *FLIR, node: u16, list: u16, iter: ?*FLIR.InsIterator) !void {
    // phase one, kill trivial puts, emit non-conflicting puts
    while (true) {
        var phi_1 = list;
        var any_ready = false;
        while (phi_1 != NoRef) {
            var p1 = self.iref(phi_1) orelse return error.FLIRError;
            if (!p1.f.move_processed) {
                // fast path: a lot of putphis are going to be trivial "RAX := RAX" stuff
                if (try self.trivial_moveinst(p1)) {
                    p1.f.move_processed = true;
                }
            }
            if (p1.f.move_processed) {
                phi_1 = p1.next;
                continue;
            }
            const ready = is_ready: {
                var phi_2 = list; // unordered, we cannot start after phi_1!
                while (phi_2 != NoRef) {
                    const p2 = self.iref(phi_2) orelse return error.FLIRError;
                    // pointer comparison: skip diagonal element
                    if (!(p2.f.move_processed or p1 == p2)) {
                        if (try conflict(self, p1, p2)) {
                            break :is_ready false;
                        }
                    }
                    phi_2 = p2.next;
                }
                break :is_ready true;
            };
            if (ready) {
                p1.f.move_processed = true;
                try put_for_movelist(self, node, iter, phi_1);

                any_ready = true; // we advanced
            }
            phi_1 = p1.next;
        }
        if (!any_ready) {
            break;
        }
    }

    var phi = list;
    while (phi != NoRef) {
        const p = self.iref(phi) orelse return error.FLIRError;
        if (p.f.move_processed) {
            phi = p.next;
            continue;
        }

        const group_last_write = try self.movins_read(p) orelse @panic("trivial move in cycle group?");
        var next_read = try self.movins_dest(p);
        p.f.move_processed = true;
        p.f.do_swap = true;
        try put_for_movelist(self, node, iter, phi);
        var phi_1 = list;
        while (phi_1 != NoRef) {
            const p1 = self.iref(phi_1) orelse return error.FLIRError;
            if (p1.f.move_processed) {
                phi_1 = p1.next;
                continue;
            }
            const p1_read = (try self.movins_read(p1)) orelse @panic("trivial mode in cycle group?");
            if (FLIR.mc_equal(p1_read, next_read)) {
                // we cannot run out of pos as it is a slower (or always equal) iterator
                const p1_dest = try self.movins_dest(p1);
                try put_for_movelist(self, node, iter, phi_1);
                p1.f.move_processed = true;
                if (FLIR.mc_equal(p1_dest, group_last_write)) {
                    p1.f.swap_done = true;
                    break;
                } else {
                    p1.f.do_swap = true;
                    next_read = p1_dest;
                    phi_1 = list;
                }
            } else {
                phi_1 = p1.next;
            }
        }

        phi = p.next;
    }
}

pub fn resolve_moves(self: *FLIR) !void {
    for (self.n.items, 0..) |*n, ni| {
        try resolve_movelist(self, uv(ni), n.putphi_list, null);

        var iter = self.ins_iterator(n.firstblk);
        while (iter.peek()) |it| {
            if (it.i.tag == .call) {
                try resolve_movelist(self, uv(ni), it.i.next, &iter);
                const any = iter.next() != null; // retvals is after the instruction,
                try resolve_movelist(self, uv(ni), it.i.op2, if (any) &iter else null); // TODO: this encoding of position is crazy af
            } else {
                _ = iter.next();
            }
        }
    }
}

pub fn conflict(self: *FLIR, before: *Inst, after: *Inst) !bool {
    // note if only "after", check for a read conflict before.
    if (before.mckind == .call_memarg) {
        if (before.tag != .callarg) return error.FLIRError;
        if (after.mckind == .call_memarg) {
            if (after.tag != .callarg) return error.FLIRError;
            // do highest indicies first
            return (before.mcidx < after.mcidx);
        }
        return false;
    }
    const written = try self.movins_dest(before);
    const read = try self.movins_read(after) orelse return false;
    return FLIR.mc_equal(written, read);
}
