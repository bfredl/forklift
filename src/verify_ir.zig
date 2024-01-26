const FLIR = @import("./FLIR.zig");
const Tag = FLIR.Tag;
const NoRef = FLIR.NoRef;
const uv = FLIR.uv;

const X86Asm = @import("./X86Asm.zig");

const std = @import("std");
const mem = std.mem;
const ArrayList = std.ArrayList;
const print = std.debug.print;

const common = @import("./common.zig");
const options = common.debug_options;

pub fn check_phi(self: *FLIR, worklist: *ArrayList(u16), pred: u16, succ: u16) !void {
    const pn = self.n.items[pred];

    var item = pn.putphi_list;
    while (item != NoRef) {
        const i = self.iref(item) orelse return error.FLIRError;
        if (i.tag == .putphi) {
            // it.op1 == NoRef is techy allowed ( %thephi := undefined )
            if (i.op2 == NoRef) return error.InvalidCFG;
            try worklist.append(i.op2); // TODO: check duplicate
        } else if (i.tag != .putvar) { // only putvar and putphi allowed here
            return error.InvalidCFG;
        }
        item = i.next;
    }

    var left: u16 = uv(worklist.items.len);

    var phi = self.n.items[succ].phi_list;
    while (phi != NoRef) {
        const i = self.iref(phi) orelse return error.FLIRError;
        if (i.tag != .phi) return error.FLIRError;
        if (mem.indexOfScalar(u16, worklist.items, phi)) |idx| {
            left -= 1;
            worklist.items[idx] = NoRef;
        } else {
            if (!self.unsealed) {
                return error.InvalidCFG;
            }
        }
        phi = i.next;
    }
    if (left > 0) return error.InvalidCFG;
    worklist.items.len = 0; // ALL RESET
    // RETURN
}

pub fn get_jmp_or_last(self: *FLIR, n: *FLIR.Node) !?Tag {
    var last_inst: ?Tag = null;
    var iter = self.ins_iterator(n.firstblk);
    while (iter.next()) |it| {
        if (last_inst) |l| if (l == .icmp or l == .fcmp or l == .ret) return error.InvalidCFG;
        last_inst = it.i.tag;
    }
    return last_inst;
}

pub fn check_inst(self: *FLIR, i: *FLIR.Inst) !void {
    for (i.ops(false)) |op| {
        if (self.iref(op)) |ref| {
            if (ref.tag == .freelist) {
                return error.FLIRError;
            }
        }
    }
}

/// does not use or verify node.npred
pub fn check_ir_valid(self: *FLIR) !void {
    if (comptime FLIR.minimal) {
        return;
    }
    const reached = try self.a.alloc(bool, self.n.items.len);
    defer self.a.free(reached);
    @memset(reached, false);

    var worklist = ArrayList(u16).init(self.a);
    defer worklist.deinit();
    for (self.n.items, 0..) |*n, ni| {
        for (n.s) |s| {
            if (s > self.n.items.len) return error.InvalidCFG;
            reached[s] = true;
        }
        // TODO: explicit condition for this
        if (self.refs.items.len > 0) {
            for (self.preds(@intCast(ni))) |pred| {
                const pn = self.n.items[pred];
                if (pn.s[0] != ni and pn.s[1] != ni) {
                    return error.InvalidCFG;
                }
                try self.check_phi(&worklist, pred, uv(ni));
            }
        }

        if (n.firstblk == NoRef) return error.InvalidCFG;
        var blk = n.firstblk;
        var prev_blk: u16 = NoRef;
        while (blk != NoRef) {
            const b = &self.b.items[blk];
            if (b.pred != prev_blk) return error.InvalidCFG;
            for (b.i) |i| {
                if (i != NoRef) {
                    if (i >= self.i.items.len) return error.FLIRError;
                    try self.check_inst(self.iref(i).?);
                }
            }
            prev_blk = blk;
            blk = b.succ;
        }
        if (n.lastblk != prev_blk) return error.InvalidCFG;
    }
    for (self.n.items, 0..) |*n, ni| {
        const last = try get_jmp_or_last(self, n);
        if ((last == .icmp or last == .fcmp) != (n.s[1] != 0)) return error.InvalidCFG;
        if (last == Tag.ret and n.s[0] != 0) return error.InvalidCFG;
        if (n.s[0] == 0 and (last != Tag.ret and reached[ni])) return error.InvalidCFG;
        // TODO: also !reached and n.s[0] != 0 (not verified by remove_empty)
        if (!reached[ni] and (last != null)) return error.InvalidCFG;
    }
}

pub fn check_vregs(self: *FLIR) !void {
    if (self.n.items[0].live_in != 0) return error.HOPPSANSA;
    var err = false;
    for (self.n.items, 0..) |*n, ni| {
        var live_out: u64 = 0;
        // hack: if n.s[i] == 0 then no bits will be added anyway
        live_out |= self.n.items[n.s[0]].live_in;
        live_out |= self.n.items[n.s[1]].live_in;

        const born = live_out & ~n.live_in;
        if (born != 0) {
            // print("BIRTH {}: {x} which is {}\n", .{ ni, born, @popCount(born) });
            var ireg: u16 = 0;
            while (ireg < self.nvreg) : (ireg += 1) {
                if ((born & (@as(usize, 1) << @as(u6, @intCast(ireg)))) != 0) {
                    const i = self.vregs.items[ireg].ref;
                    // print(" %{}", .{i});
                    if (self.iref(i).?.node_delete_this != ni) {
                        // print("!! {} vs {}\n", .{ self.iref(i).?.node_delete_this, ni });
                        err = true;
                    }
                }
            }
            print("\n", .{});
        }
    }
    if (err) return error.DoYouEvenLoopAnalysis;
}

// debug print functions

fn fake_ref(self: *FLIR, ref: u16) u16 {
    if (ref >= FLIR.ConstOff) {
        return ref;
    }
    _ = self;
    return ref;
}

pub fn debug_print(self: *FLIR) void {
    if (comptime FLIR.minimal) {
        return;
    }

    var fake_idx: u16 = 0;
    // TODO: something like %nodeid.ref_in_node might be preferrable
    for (self.n.items) |*n| {
        var cur_blk: ?u16 = n.firstblk;
        while (cur_blk) |blk| {
            const b = &self.b.items[blk];
            b.fakenum_ = fake_idx;
            fake_idx += 1;

            cur_blk = b.next();
        }
    }

    print("\n", .{});
    for (self.n.items, 0..) |*n, i| {
        print("node {} (npred {}, loop {}):", .{ i, n.npred, n.loop });
        if (n.is_header) print(" HEADER", .{});
        if (n.live_in != 0) {
            print(" LIVEIN", .{});
            var ireg: u16 = 0;
            while (ireg < self.nvreg) : (ireg += 1) {
                const live = (n.live_in & (@as(usize, 1) << @as(u6, @intCast(ireg)))) != 0;
                if (live) {
                    print(" %{}", .{fake_ref(self, self.vregs.items[ireg].ref)});
                }
            }
        }

        print("\n", .{});

        self.print_node(n);

        // only print liveout if we have more than one sucessor, otherwise it is BOOORING
        if (n.s[1] != 0) {
            var live_out: u64 = 0;
            live_out |= self.n.items[n.s[0]].live_in;
            live_out |= self.n.items[n.s[1]].live_in;
            if (live_out != 0) {
                print("LIVE_OUT:", .{});
                var ireg: u16 = 0;
                while (ireg < self.nvreg) : (ireg += 1) {
                    const live = (live_out & (@as(usize, 1) << @as(u6, @intCast(ireg)))) != 0;
                    if (live) {
                        print(" %{}", .{fake_ref(self, self.vregs.items[ireg].ref)});
                    }
                }
            }
            print("\n", .{});
        }

        if (n.s[1] == 0) {
            if (n.s[0] == 0) {
                print("  diverge\n", .{});
            } else if (n.s[0] != i + 1) {
                print("  jump {}\n", .{n.s[0]});
            }
        } else {
            print("  split: {any}\n", .{n.s});
        }
    }
}

pub fn print_inst(self: *FLIR, ref: u16, i: *FLIR.Inst) void {
    if (i.tag == .putphi) {
        const targ = if (self.iref(i.op2)) |iref| iref.* else empty_inst;
        const src = if (self.iref(i.op1)) |iref| iref.* else empty_inst;
        if (options.dbg_exclude_trivial_put) {
            if (src.tag == .phi and src.op1 == targ.op1 and targ.op1 != NoRef and src.mckind == targ.mckind and src.mcidx == targ.mcidx) {
                return;
            }
        }
    }

    const chr: u8 = if (i.has_res()) '=' else ' ';
    var name = @tagName(i.tag);
    if (i.tag == .ibinop) name = "i";
    print("  %{} {c} {s}", .{ fake_ref(self, ref), chr, name });

    if (i.tag == .variable) {
        print(" {s}", .{@tagName(i.mem_type())});
    }

    if (i.tag == .vmath) {
        print(".{s}", .{@tagName(i.vmathop())});
    } else if (i.tag == .vcmpf) {
        print(".{s}", .{@tagName(i.vcmpop())});
    } else if (i.tag == .ibinop) {
        print(".{s}", .{@tagName(@as(FLIR.IntBinOp, @enumFromInt(i.spec)))});
    } else if (i.tag == .icmp) {
        print(".{s}", .{@tagName(@as(FLIR.IntCond, @enumFromInt(i.spec)))});
    } else if (i.tag == .phi) {
        if (self.get_varname(i.op1)) |nam| {
            print(" {s}", .{nam});
        } else if (self.unsealed) {
            print(" ${}", .{i.op1});
        }
    } else if (i.tag == .putphi) {
        print(" %{} <-", .{fake_ref(self, i.op2)});
    }
    const nop = i.n_op(false);
    if (nop > 0) {
        print_op(self, " ", i.f.kill_op1, i.op1);
        if (nop > 1) {
            print_op(self, ", ", i.f.kill_op2, i.op2);
        }
    }
    print_mcval(i);
    var v_conflict = false;
    if (i.vreg()) |v| {
        if (self.vregs.items[v].conflicts != 0) {
            v_conflict = true;
        }
        print(" *", .{});
    }
    if (i.f.conflicts or v_conflict) {
        print(" !", .{});
    }
    if (i.tag == .putphi) {
        const targ = if (self.iref(i.op2)) |iref| iref.* else empty_inst;
        const src = if (self.iref(i.op1)) |iref| iref.* else empty_inst;
        const targvar = if (targ.tag == .phi) targ.op1 else NoRef;
        const srcvar = if (src.tag == .phi) src.op1 else NoRef;
        const targnam = self.get_varname(targvar);
        if (srcvar == targvar) {
            if (targnam) |nam| {
                print(" ({s})", .{nam});
            }
        } else {
            const srcnam = self.get_varname(srcvar);
            if (srcnam != null or targnam != null) {
                print(" ({s} <- {s})", .{ targnam orelse "*", srcnam orelse "*" });
            }
        }

        if (self.ipreg(i.op2)) |reg| {
            const regsrc = self.ipreg(i.op1);
            const tag = @tagName(X86Asm.IPReg.from(reg));
            if (regsrc == null or regsrc == reg) {
                print(" [{s}]", .{tag});
            } else {
                print(" [{s} <- {s}]", .{ tag, @tagName(X86Asm.IPReg.from(regsrc.?)) });
            }
        }
    }
    print("\n", .{});
}

// TODO: bull, but here we just use it as "anything unallocated"
const empty_inst = FLIR.Inst{ .tag = .freelist, .op1 = 0, .op2 = 0 };
pub fn print_node(self: *FLIR, n: *FLIR.Node) void {
    var phi = n.phi_list;
    while (phi != NoRef) {
        const i = self.iref(phi) orelse @panic("näää");
        self.print_inst(phi, i);
        phi = i.next;
    }
    var it = self.ins_iterator(n.firstblk);
    while (it.next()) |item| {
        self.print_inst(item.ref, item.i);
    }

    var put_iter = n.putphi_list;
    while (put_iter != NoRef) {
        const i = &self.i.items[put_iter];
        if (i.tag == .putvar) {
            print("  VAR ", .{});
            if (self.get_varname(i.op2)) |nam| {
                print("{s}", .{nam});
            } else {
                print("${}", .{i.op2});
            }
            print_op(self, " := ", i.f.kill_op1, i.op1);
            print("\n", .{});
        } else if (i.tag == .putphi) {
            if (!i.f.killed) self.print_inst(put_iter, i);
        } else {
            print("MÖG: ", .{});
            self.print_inst(put_iter, i);
        }
        put_iter = i.next;
    }
}

fn print_op(self: *FLIR, pre: []const u8, kill: bool, ref: u16) void {
    print("{s}", .{pre});
    if (ref == NoRef) {
        print("%NoRef", .{});
    } else if (self.constval(ref)) |c| {
        print("const {}", .{c});
    } else {
        const k = if (kill) "!" else "";
        print("{s}%{}", .{ k, fake_ref(self, ref) });
    }
}

fn print_mcval(i: *FLIR.Inst) void {
    if (i.tag != .phi and i.tag != .arg and !i.mckind.unallocated() and i.mckind != .fused) {
        print(" =>", .{});
    }
    switch (i.mckind) {
        .frameslot => print(" [rbp-8*{}]", .{i.mcidx}),
        .ipreg => print(" ${s}", .{@tagName(@as(X86Asm.IPReg, @enumFromInt(i.mcidx)))}),
        .vfreg => print(" $ymm{}", .{i.mcidx}),
        else => {
            if (i.tag == .load or i.tag == .phi or i.tag == .arg) {
                if (i.res_type()) |t| {
                    print(" {s}", .{@tagName(t)});
                }
            }
        },
    }
}

pub fn uses(i: *FLIR.Inst, r: u16) bool {
    const n_op = i.n_op(false);
    if (n_op >= 1 and i.op1 == r) return true;
    if (n_op >= 2 and i.op2 == r) return true;
    return false;
}

pub fn print_interval(self: *FLIR, ref: u16) void {
    if (comptime FLIR.minimal) {
        return;
    }
    const i = self.iref(ref).?;
    const vreg_flag = if (i.vreg()) |vreg| @as(u64, 1) << @as(u6, @intCast(vreg)) else null;
    print("\x1b[4m", .{});
    for (self.n.items, 0..) |n, ni| {
        var live: bool = if (vreg_flag) |f| (f & n.live_in) != 0 else false;
        if (i.tag == .phi and i.node_delete_this == ni) {
            live = true;
        }
        var it = self.ins_iterator(n.firstblk);
        while (it.next()) |item| {
            const iu = item.i;
            if (item.ref == ref) {
                if (iu.tag == .phi) {
                    print("ϕ", .{});
                } else {
                    print("S", .{});
                }
                live = true;
            } else if ((iu.f.kill_op1 and iu.op1 == ref) or (iu.f.kill_op2 and iu.op2 == ref)) {
                live = false;
                print("K", .{});
            } else if (uses(iu, ref)) {
                print("U", .{});
            } else if (iu.tag == .putphi and iu.op2 == ref) {
                print("p", .{});
                // TODO: really live at the first putphi in the block but anyway
                live = true;
            } else if (live) {
                print("-", .{});
            } else {
                print(" ", .{});
            }
        }
        print("│", .{});
    }
    print("\x1b[0m\n", .{});
}

pub fn print_loop(self: *FLIR, head: u16) void {
    var enda: u16 = 0;
    print("\x1b[4m", .{});
    for (self.n.items, 0..) |n, ni| {
        var it = self.ins_iterator(n.firstblk);
        if (ni == head) {
            enda = n.loop_end;
        }
        while (it.next()) |_| {
            if (ni == head) {
                print("H", .{});
            } else if (ni < enda) {
                print("-", .{});
            } else {
                print(" ", .{});
            }
        }
        if (ni + 1 < enda) {
            print("-", .{}); // ┼
        } else {
            print("│", .{});
        }
    }
    print("\x1b[0m\n", .{});
}

pub fn print_intervals(self: *FLIR) void {
    for (self.n.items, 0..) |n, ni| {
        if (n.is_header) {
            print("           ", .{});
            print_loop(self, uv(ni));
        }
        var it = self.ins_iterator(n.firstblk);
        while (it.next()) |item| {
            const i = item.i;
            const vreg: bool = i.vreg() != null;
            const is_phi = i.tag == .phi;
            const show_temp = i.f.killed and true;
            if (vreg or show_temp) {
                if (!vreg and !is_phi) print("\x1b[38;5;244m", .{});
                print("{s}: %{:3} ", .{ if (vreg) "VREG" else "TEMP", fake_ref(self, item.ref) });
                self.print_interval(item.ref);
            }
        }
    }
}

pub fn print_debug_map(self: *FLIR, ni: u16, target: u32) void {
    print("node {} at {x}:\n", .{ ni, target });
    const n = self.n.items[ni];
    var it = self.ins_iterator(n.firstblk);
    while (it.next()) |item| {
        const i = item.i;
        if (i.tag != .phi) break;
        if (self.get_varname(i.op1)) |nam| {
            print("{s}: ", .{nam});
            if (i.ipreg()) |reg| {
                const tag = @tagName(X86Asm.IPReg.from(reg));
                print("{s} \n", .{tag});
            } else {
                print("??\n", .{});
            }
        }
    }
    print("\n", .{});
}
