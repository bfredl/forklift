const std = @import("std");
const defs = @import("./defs.zig");
const FLIR = @import("./FLIR.zig");
const Inst = @import("./Inst.zig");
const abi = @import("./abi.zig");
const X86Asm = @import("./X86Asm.zig");

const VRegFlag = defs.VRegFlag;
const NodeFlag = defs.NodeFlag;

const uv = FLIR.uv;
const NoRef = FLIR.NoRef;

const vreg_flag = defs.vreg_flag;
const node_flag = defs.node_flag;

// TODO: this is liveness for the sake of regalloc.
// want simpler use-def/def-use for optimizations, especially if we
// go sea-of-nodeish with unscheduled opts
//
// not idempotent! does not reset n_use=0 first.
pub fn calc_live(self: *FLIR) !void {
    // TODO: NOT LIKE THIS
    try self.vregs.ensureTotalCapacity(self.gpa, @typeInfo(VRegFlag).int.bits);

    // step1: allocate vregs for results that leak from the block
    for (self.n.items, 0..) |*n, ni| {
        var it = self.ins_iterator(n.firstblk);
        while (it.next()) |item| {
            const i = item.i;
            for (i.ops(false)) |*op| {
                try adduse(self, uv(ni), item.ref, op.*);
            }
            var opnext = i.next_as_op();
            while (opnext != NoRef) {
                const ii = self.iref(opnext) orelse return error.FLIRError;
                try adduse(self, uv(ni), opnext, ii.op1);
                opnext = ii.next;
            }
        }

        var item = n.putphi_list;
        while (item != NoRef) {
            const i = self.iref(item) orelse return error.FLIRError;
            try adduse(self, uv(ni), item, i.op1);
            item = i.next;
        }
    }

    // step 2: process the dominator tree backwards (flattened works fine) to
    // progagate uses to predecessors
    {
        var ni: u16 = uv(self.n.items.len - 1);
        // TODO: at this point the number of vregs is known. so a bitset for
        // node X vreg can be allocated here.

        if (self.nvreg > @typeInfo(VRegFlag).int.bits or self.n.items.len > @typeInfo(NodeFlag).int.bits) {
            std.debug.print("HAHAHAHA {}, you really thought HAHAHHAHAHA {}:\n", .{ self.nvreg, self.n.items.len });
            return error.NotImplemented;
        }

        while (true) : (ni -= 1) {
            const n = &self.n.items[ni];
            var live_vregs: VRegFlag = 0;
            var used_vregs: VRegFlag = 0;
            for (n.s) |s| {
                if (s != 0) {
                    live_vregs |= self.n.items[s].live_in;
                }
            }
            // print("LIVEUT {}: {x} (dvs {})\n", .{ ni, live_vregs, @popCount(live_vregs) });

            var neg_counter: u16 = 0;
            var last_clobber: [FLIR.n_ipreg]u16 = @splat(0);

            var node_has_clobber: bool = false;

            {
                var listit = n.putphi_list;
                while (listit != NoRef) {
                    // putphi:s arent ordered, so they all have neg_counter == 0
                    const i = self.iref(listit) orelse return error.FLIRError;
                    mark_ops_as_used(self, i, &live_vregs, &used_vregs, neg_counter);
                    // TODO: flogger
                    if (i.tag == .retval) {
                        if (self.abi_tag == .X86) {
                            if (self.iref(i.op1)) |ref| {
                                if (ref.mckind == .unallocated_raw and i.res_type() == .intptr) {
                                    ref.mckind = .unallocated_ipreghint;
                                    ref.mcidx = X86Asm.IPReg.rax.id();
                                }
                            }
                        }
                    }
                    listit = i.next;
                }
            }

            var it = self.ins_iterator_rev(n.lastblk);
            while (it.next_rev()) |item| {
                neg_counter += 1;
                const i = item.i;

                if (i.tag == .call) {
                    // currently the only inst with projections
                    var r = i.op2;
                    while (r != NoRef) {
                        const rret = self.iref(r) orelse return error.FLIRError;
                        check_live_or_conflicts(self, rret, &live_vregs, node_has_clobber, &last_clobber);
                        r = rret.next;
                    }
                } else {
                    check_live_or_conflicts(self, i, &live_vregs, node_has_clobber, &last_clobber);
                }

                // TODO: currently this forbids clobbers as inputs, which is wrong when killing the op.
                // just need to check for mismatches
                // consider x86 IDIV: as op1 is fixed to be EAX, op2 conflicts with EAX even when killed!
                // TODO: could just pass used_vergs and then live_in = (live_out | used) & ~killed in the end
                mark_ops_as_used(self, i, &live_vregs, &used_vregs, neg_counter);
                var opnext = i.next_as_op();
                while (opnext != NoRef) {
                    const ii = self.iref(opnext) orelse return error.FLIRError;
                    mark_ops_as_used(self, ii, &live_vregs, &used_vregs, neg_counter);
                    opnext = ii.next;
                }

                // registers clobbered by this instruction
                // TOOO: or having it as a fixed input, which is not ALWAYS a clobber
                // (like two shr intructions in a row sharing the same ecx input)
                const clobber_mask: u16 = try abi.get_clobbers(self, i);

                if (clobber_mask != 0) {
                    node_has_clobber = true; // quick skipahead
                    for (self.vregs.items, 0..) |*v, vi| {
                        if (live_vregs & vreg_flag(@intCast(vi)) != 0) {
                            v.conflicts |= clobber_mask;
                        }
                    }
                    for (0..FLIR.n_ipreg) |r| {
                        if ((clobber_mask & defs.ipreg_flag(@intCast(r))) != 0) {
                            last_clobber[r] = neg_counter;
                        }
                    }
                }
            }

            {
                var phi = n.phi_list;
                while (phi != NoRef) {
                    const i = self.iref(phi) orelse return error.FLIRError;
                    check_live_or_conflicts(self, i, &live_vregs, node_has_clobber, &last_clobber);
                    phi = i.next;
                }
            }

            n.live_in = live_vregs;
            n.used_vregs = used_vregs;
            // print("LIVEIN {}: {x} (dvs {})\n", .{ ni, n.live_in, @popCount(n.live_in) });

            if (n.is_header) {
                // TODO: make me a loop membership bitset globally
                // TODO: if there was a call anywhere in the loop
                // propagate call clobbers onto the loop invariant vregs
                var loop_set: NodeFlag = node_flag(@intCast(ni));
                for (ni + 1..self.n.items.len) |ch_i| {
                    const ch_n = &self.n.items[ch_i];
                    const ch_loop = node_flag(@intCast(ch_i));
                    if ((node_flag(@intCast(ch_n.loop)) & loop_set) != 0) {
                        if (ch_n.is_header) {
                            loop_set |= ch_loop;
                        }
                        // NB: used_vregs remains fully local. maybe header should have "loop used" if we need it
                        ch_n.live_in |= n.live_in;
                    } else {
                        break; // assuming the contigous property by now
                    }
                }
            }
            if (ni == 0) break;
        }
    }

    // step 3: mark the instructions which kill a live range, which will be used by regalloc
    for (self.n.items, 0..) |*n, ni| {
        // transpose the node.live_in[vreg] bitfield into vreg.live_in[node] bitfield
        for (self.vregs.items, 0..) |*v, vi| {
            if ((n.live_in & vreg_flag(@intCast(vi))) != 0) {
                v.live_in |= node_flag(@intCast(ni));
            }
            if ((n.used_vregs & vreg_flag(@intCast(vi))) != 0) {
                v.used |= node_flag(@intCast(ni));
            }
        }

        var live_out: VRegFlag = 0;
        for (n.s) |s| {
            live_out |= self.n.items[s].live_in;
        }
        var killed: VRegFlag = n.live_in & ~live_out;

        {
            var listit = n.putphi_list;
            while (listit != NoRef) {
                const i = self.iref(listit) orelse return error.FLIRError;
                mark_ops_who_kill(self, i, &killed);
                listit = i.next;
            }
        }

        var it = self.ins_iterator_rev(n.lastblk);
        while (it.next_rev()) |item| {
            mark_ops_who_kill(self, item.i, &killed);
            var opnext = item.i.next_as_op();
            while (opnext != NoRef) {
                const ii = self.iref(opnext) orelse return error.FLIRError;
                mark_ops_who_kill(self, ii, &killed);
                opnext = ii.next;
            }
        }
    }
}

// ni = node id of user
fn adduse(self: *FLIR, ni: u16, user: u16, used: u16) !void {
    _ = user;
    const i = self.iref(used) orelse return;

    const def_node = try self.defNode(i);

    //ref.i.n_use += 1;
    // it leaks to another block: give it a virtual register number
    if (def_node != ni) {
        if (!i.f.is_vreg) {
            i.f.is_vreg = true;
            i.vreg_scratch = self.nvreg;
            self.nvreg += 1;
            try self.vregs.append(self.gpa, .{ .ref = used, .def_node = def_node });
        }
    }
}

fn check_live_or_conflicts(self: *FLIR, i: *Inst, live_vregs: *VRegFlag, node_has_clobber: bool, last_clobber: *const [FLIR.n_ipreg]u16) void {
    _ = self;

    // TODO: need to consider phi:s even when they are unscheduled
    if (i.vreg()) |vreg| {
        live_vregs.* &= ~vreg_flag(@intCast(vreg));
    } else {
        if (i.vreg_scratch != NoRef) {
            if (node_has_clobber) { // quick skipahead when no clobbers
                var conflicts: u16 = 0;
                for (0..FLIR.n_ipreg) |r| {
                    // negative counter: is the last_clobber _before_ the kill position
                    if (i.vreg_scratch < last_clobber[r]) {
                        conflicts |= defs.ipreg_flag(@intCast(r));
                    }
                }
                if (conflicts != 0) {
                    i.f.conflicts = true;
                }
                i.vreg_scratch = conflicts;
            } else {
                i.vreg_scratch = 0;
            }
        }
    }
}

fn mark_ops_as_used(self: *FLIR, i: *Inst, live: *VRegFlag, used: *VRegFlag, neg_counter: u16) void {
    for (i.ops(false)) |op| {
        if (self.iref(op)) |ref| {
            if (ref.vreg()) |vreg| {
                live.* |= vreg_flag(@intCast(vreg));
                used.* |= vreg_flag(@intCast(vreg)); // a little silly but see TODO at callsite
            } else {
                if (ref.vreg_scratch == NoRef) {
                    ref.vreg_scratch = neg_counter;
                }
            }
        }
    }
}

fn mark_ops_who_kill(self: *FLIR, i: *Inst, killed: *VRegFlag) void {
    for (i.ops(false), 0..) |op, i_op| {
        var kill: bool = false;
        if (self.iref(op)) |ref| {
            if (ref.vreg()) |vreg| {
                const bit = vreg_flag(@intCast(vreg));
                if ((killed.* & bit) != 0) {
                    killed.* &= ~bit;
                    kill = true;
                }
            } else {
                if (!ref.f.killed) {
                    ref.f.killed = true;
                    kill = true;
                }
            }
        }
        if (kill) {
            if (i_op == 2) {
                i.f.kill_op3 = true;
            } else if (i_op == 1) {
                i.f.kill_op2 = true;
            } else {
                i.f.kill_op1 = true;
            }
        }
    }
}
