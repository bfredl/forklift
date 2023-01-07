const FLIR = @import("./FLIR.zig");
const Tag = FLIR.Tag;
const NoRef = FLIR.NoRef;
const uv = FLIR.uv;

const CFO = @import("./CFO.zig");

const std = @import("std");
const mem = std.mem;
const ArrayList = std.ArrayList;
const print = std.debug.print;

pub fn check_phi(self: *FLIR, worklist: *ArrayList(u16), pred: u16, succ: u16) !void {
    const pn = self.n.items[pred];
    var iter = self.ins_iterator(pn.firstblk);
    var saw_putphi = false;

    while (iter.next()) |it| {
        if (it.i.tag == .putphi) {
            saw_putphi = true;
            // it.op1 == NoRef is techy allowed ( %thephi := undefined )
            if (it.i.op2 == NoRef) return error.InvalidCFG;
            try worklist.append(it.i.op2); // TODO: check duplicate
        } else {
            // error: regular instruction after putphi
            if (saw_putphi) return error.InvalidCFG;
        }
    }

    var left: u16 = uv(worklist.items.len);

    var siter = self.ins_iterator(self.n.items[succ].firstblk);
    while (siter.next()) |it| {
        if (it.i.tag == .phi) {
            const idx = mem.indexOfScalar(u16, worklist.items, it.ref) orelse return error.InvalidCFG;
            left -= 1;
            worklist.items[idx] = NoRef;
        } else {
            // TODO: check for phi in the wild if we memoize this before going thu preds
            break;
        }
    }
    if (left > 0) return error.InvalidCFG;
    worklist.items.len = 0; // ALL RESET
    // RETURN
}

pub fn get_jmp_or_last(self: *FLIR, n: *FLIR.Node) !?Tag {
    var last_inst: ?Tag = null;
    var iter = self.ins_iterator(n.firstblk);
    while (iter.next()) |it| {
        if (last_inst) |l| if (l == .icmp or l == .ret) return error.InvalidCFG;
        last_inst = it.i.tag;
    }
    return last_inst;
}

/// does not use or verify node.npred
pub fn check_ir_valid(self: *FLIR) !void {
    const reached = try self.a.alloc(bool, self.n.items.len);
    defer self.a.free(reached);
    mem.set(bool, reached, false);

    var worklist = ArrayList(u16).init(self.a);
    defer worklist.deinit();
    for (self.n.items) |*n, ni| {
        for (n.s) |s| {
            if (s > self.n.items.len) return error.InvalidCFG;
            reached[s] = true;
        }
        // TODO: explicit condition for this
        if (self.refs.items.len > 0) {
            for (self.preds(@intCast(u16, ni))) |pred| {
                const pn = self.n.items[pred];
                if (pn.s[0] != ni and pn.s[1] != ni) {
                    return error.InvalidCFG;
                }
                try self.check_phi(&worklist, pred, uv(ni));
            }
        }
    }
    for (self.n.items) |*n, ni| {
        const last = try get_jmp_or_last(self, n);
        if ((last == Tag.icmp) != (n.s[1] != 0)) return error.InvalidCFG;
        if (last == Tag.ret and n.s[0] != 0) return error.InvalidCFG;
        if (n.s[0] == 0 and (last != Tag.ret and reached[ni])) return error.InvalidCFG;
        // TODO: also !reached and n.s[0] != 0 (not verified by remove_empty)
        if (!reached[ni] and (last != null)) return error.InvalidCFG;
    }
}

pub fn check_vregs(self: *FLIR) !void {
    if (self.n.items[0].live_in != 0) return error.HOPPSANSA;
    var err = false;
    for (self.n.items) |*n, ni| {
        var live_out: u64 = 0;
        // hack: if n.s[i] == 0 then no bits will be added anyway
        live_out |= self.n.items[n.s[0]].live_in;
        live_out |= self.n.items[n.s[1]].live_in;

        const born = live_out & ~n.live_in;
        if (born != 0) {
            // print("BIRTH {}: {x} which is {}\n", .{ ni, born, @popCount(born) });
            var ireg: u16 = 0;
            while (ireg < self.nvreg) : (ireg += 1) {
                if ((born & (@as(usize, 1) << @intCast(u6, ireg))) != 0) {
                    const i = self.vregs.items[ireg];
                    // print(" %{}", .{i});
                    if (self.biref(i).?.n != ni) {
                        // print("!!", .{});
                        err = true;
                    }
                }
            }
            // print("\n", .{});
        }
    }
    if (err) return error.DoYouEvenLoopAnalysis;
}

// debug print functions

pub fn debug_print(self: *FLIR) void {
    print("\n", .{});
    for (self.n.items) |*n, i| {
        print("node {} (npred {}, loop {}):", .{ i, n.npred, n.loop });
        if (n.is_header) print(" HEADER", .{});
        if (n.live_in != 0) {
            print(" LIVEIN", .{});
            var ireg: u16 = 0;
            while (ireg < self.nvreg) : (ireg += 1) {
                const live = (n.live_in & (@as(usize, 1) << @intCast(u6, ireg))) != 0;
                if (live) {
                    print(" %{}", .{self.vregs.items[ireg]});
                }
            }
        }

        if (n.firstblk == NoRef) {
            print(" VERY DEAD\n", .{});
            continue;
        }

        print("\n", .{});

        self.print_blk(n.firstblk);

        // only print liveout if we have more than one sucessor, otherwise it is BOOORING
        if (n.s[1] != 0) {
            var live_out: u64 = 0;
            live_out |= self.n.items[n.s[0]].live_in;
            live_out |= self.n.items[n.s[1]].live_in;
            if (live_out != 0) {
                print("LIVE_OUT:", .{});
                var ireg: u16 = 0;
                while (ireg < self.nvreg) : (ireg += 1) {
                    const live = (live_out & (@as(usize, 1) << @intCast(u6, ireg))) != 0;
                    if (live) {
                        print(" %{}", .{self.vregs.items[ireg]});
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

pub fn print_blk(self: *FLIR, firstblk: u16) void {
    var it = self.ins_iterator(firstblk);
    while (it.next()) |item| {
        const i = item.i.*;
        const chr: u8 = if (i.has_res()) '=' else ' ';
        print("  %{} {c} {s}", .{ item.ref, chr, @tagName(i.tag) });

        if (i.tag == .variable) {
            print(" {s}", .{@tagName(i.spec_type())});
        }

        if (i.tag == .vmath) {
            print(".{s}", .{@tagName(i.vop())});
        } else if (i.tag == .iop) {
            print(".{s}", .{@tagName(@intToEnum(CFO.AOp, i.spec))});
        } else if (i.tag == .icmp) {
            print(".{s}", .{@tagName(@intToEnum(CFO.Cond, i.spec))});
        } else if (i.tag == .constant) {
            print(" c[{}]", .{i.op1});
        } else if (i.tag == .putphi) {
            print(" %{} <-", .{i.op2});
        }
        const nop = i.n_op(false);
        if (nop > 0) {
            const k1: u8 = if (i.f.kill_op1) '!' else ' ';
            print(" {c}%{}", .{ k1, i.op1 });
            if (nop > 1) {
                const k2: u8 = if (i.f.kill_op2) '!' else ' ';
                print(",{c}%{}", .{ k2, i.op2 });
            }
        }
        print_mcval(i);
        if (i.vreg != NoRef) {
            print(" *", .{});
        }
        if (i.tag == .putphi) {
            if (self.iref(i.op2).?.ipreg()) |reg| {
                const regsrc = self.iref(i.op1).?.ipreg();
                print(" [{s} <- {s}] ", .{ @tagName(reg), if (regsrc) |r| @tagName(r) else "XX" });
            }
        }
        print("\n", .{});
    }
}

fn print_mcval(i: FLIR.Inst) void {
    if (i.tag != .phi and i.tag != .arg and !i.mckind.unallocated() and i.mckind != .fused) {
        print(" =>", .{});
    }
    switch (i.mckind) {
        .frameslot => print(" [rbp-8*{}]", .{i.mcidx}),
        .ipreg => print(" ${s}", .{@tagName(@intToEnum(CFO.IPReg, i.mcidx))}),
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

pub fn print_interval(self: *FLIR, ref: u16) void {
    const b = self.biref(ref).?;
    const vreg = b.i.vreg;
    const vreg_flag = if (vreg != NoRef) @as(u64, 1) << @intCast(u6, vreg) else null;
    for (self.n.items) |n| {
        var live: bool = if (vreg_flag) |f| (f & n.live_in) != 0 else false;
        var it = self.ins_iterator(n.firstblk);
        while (it.next()) |item| {
            const iu = item.i;
            if (item.ref == ref) {
                print("D", .{});
                live = true;
            } else if ((iu.f.kill_op1 and iu.op1 == ref) or (iu.f.kill_op2 and iu.op2 == ref)) {
                live = false;
                print("K", .{});
            } else if (live) {
                print("x", .{});
            } else {
                print("-", .{});
            }
        }
    }
    print("\n", .{});
}
