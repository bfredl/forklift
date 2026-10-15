const std = @import("std");
const defs = @import("./defs.zig");
const FLIR = @import("./FLIR.zig");
const Inst = @import("./Inst.zig");
const X86Asm = @import("./X86Asm.zig");
const BPF = std.os.linux.BPF;

const ipreg_flag = defs.ipreg_flag;
const NoRef = FLIR.NoRef;

// TODO: messy. first ABI was just a comptime struct, now it is also a tag. reorganize!
pub const ABITag = enum {
    X86,
    BPF,
};

pub const X86ABI = struct {
    const tag: ABITag = .X86;
    const Reg = X86Asm.IPReg;
    // Args: used used both for incoming args and nested calls (including syscalls)
    pub const argregs = erase([6]Reg{ .rdi, .rsi, .rdx, .rcx, .r8, .r9 });
    pub const ret_regs = erase([_]Reg{ .rax, .rdx });

    // canonical order of callee saved registers. nsave>0 means
    // the first nsave items needs to be saved and restored
    pub const callee_saved = erase([5]Reg{ .rbx, .r12, .r13, .r14, .r15 });
    pub const call_unsaved = erase([9]Reg{ .rax, .rcx, .rdx, .rsi, .rdi, .r8, .r9, .r10, .r11 });

    // excludes: stack reg, frame reg (just say NO to -fomiting the framepointer!)
    pub const reg_order: [14]defs.IPReg = call_unsaved ++ callee_saved;
};

pub const BPF_ABI = struct {
    const tag: ABITag = .BPF;
    const Reg = BPF.Insn.Reg;
    // Args: used used both for incoming args and nested calls (including syscalls)
    pub const argregs = erase([5]Reg{ .r1, .r2, .r3, .r4, .r5 });
    pub const ret_regs = erase([1]Reg{.r0});

    // canonical order of callee saved registers. nsave>0 means
    // the first nsave items needs to be saved and restored
    pub const callee_saved = erase([4]Reg{ .r6, .r7, .r8, .r9 });
    pub const call_unsaved = erase([6]Reg{ .r0, .r1, .r2, .r3, .r4, .r5 });

    // excludes: r10 (immutable frame pointer)
    pub const reg_order: [10]defs.IPReg = call_unsaved ++ callee_saved;
};

pub fn get_clobbers(self: *FLIR, i: *Inst) !u16 {
    // other platforms: why can't you just be normal. x86:
    const is_x86 = self.abi_tag == .X86;

    if (is_x86 and i.tag == .ibinop) {
        const Reg = X86Asm.IPReg;
        const op = i.ibinop();
        if (op == .sdiv or op == .udiv or op == .srem or op == .urem) {
            if (i.mckind == .unallocated_raw) {
                // FULING, but sure,  why now now?
                i.mckind = .unallocated_ipreghint;
                i.mcidx = Reg.rax.id();
            }
            return ipreg_flag(Reg.rax.id()) | ipreg_flag(Reg.rdx.id());
        } else if (op == .rotl or op == .rotr) {
            // TODO: rather input constraint only
            // TODO: but only if non-const!!
            return ipreg_flag(Reg.rcx.id());
        }
    }

    if (i.tag != .call) {
        return 0;
    }

    // call: result not considered live.
    // arguments are handled in callarg and are also not conflicting
    const kind: defs.CallKind = @fromBackingInt(i.spec);
    if (kind == .memory_intrinsic and is_x86) {
        const Reg = X86Asm.IPReg;
        const idx = self.constval(i.op1) orelse return error.FLIRError;
        const intrinsic: defs.MemoryIntrinsic = @fromBackingInt(@intCast(idx));
        const base: u16 = ipreg_flag(Reg.rdi.id()) | ipreg_flag(Reg.rcx.id());
        if (intrinsic == .memset) {
            return base + ipreg_flag(Reg.rax.id());
        } else {
            return error.FLIRError;
        }
    }
    return self.call_clobber_mask;
}

// TODO: maybe unify with mckind/mcidx repr for an instruction??
const ABIType = union(enum) { ipreg: defs.IPReg, avxreg: u16 };

// TODO: clobbers should be in here :P
const ABICallInfo = struct {
    args: []const defs.IPReg, // ABIType!!!!!
    rets: []const defs.IPReg,
};

pub fn abi_call_info(self: *FLIR, comptime ABI: type, i: *Inst) !ABICallInfo {
    const kind: defs.CallKind = @fromBackingInt(i.spec);
    if (kind == .memory_intrinsic and ABI.tag == .X86) {
        const Reg = X86Asm.IPReg;
        const idx = self.constval(i.op1) orelse return error.FLIRError;
        const intrinsic: defs.MemoryIntrinsic = @fromBackingInt(@intCast(idx));
        if (intrinsic == .memset) { // REP STOS
            const args = comptime erase([_]Reg{ .rdi, .rax, .rcx }); // (dest, val, len)
            // choice of ret is a bit arbitrary, but rdi is at least dest+len :P
            return .{ .args = &args, .rets = &comptime erase([_]Reg{.rdi}) };
        } else {
            return error.FLIRError;
        }
    }
    return .{ .args = &ABI.argregs, .rets = &ABI.ret_regs };
}

pub fn set_abi(self: *FLIR, comptime ABI: type) !void {
    if (self.n.items.len == 0) return error.FLIRError;
    {
        // SO GOOFY: args are kinda the phi nodes for the first block
        var n_ipval: u8 = 0;
        var n_avx: u8 = 0;
        var n_memory: u8 = 2; // zero and one is caller's RBP and the return address, respectively
        var a = self.n.items[0].phi_list;
        while (a != NoRef) {
            const i = self.iref(a) orelse return error.FLIRError;
            // TODO: allow custom callconvs by mckind being prefilled
            if (i.mem_type() == .intptr) {
                if (n_ipval < ABI.argregs.len) {
                    i.mckind = .ipreg;
                    i.mcidx = ABI.argregs[n_ipval].id();
                    n_ipval += 1;
                } else {
                    i.mckind = .memarg;
                    i.mcidx = n_memory;
                    n_memory += 1;
                }
            } else {
                if (n_avx >= 16) return error.NotImplemented;
                i.mckind = .vfreg;
                i.mcidx = n_avx;
                n_avx += 1;
            }
            a = i.next;
        }
    }

    for (self.n.items) |*n| {
        // TODO: if args existed nested in i_call.next we wouldn't need this :relieved:
        var it = self.ins_iterator(n.firstblk);
        var last_call_info: ABICallInfo = undefined;
        while (it.next()) |item| {
            const i: *Inst = item.i;
            // TODO: this is broken!!!
            // non-standard callconvs can be annotated by pre-setting mckind/mcindx
            // if (!i.mckind.unallocated()) continue;
            switch (i.tag) {
                .call => {
                    // currently, as a compromise, `call_info` lists how integers should behave.
                    // then we just count avxvar:s separately. this is gonna break if we pass
                    // structs and stack slots and so on.
                    const call_info = try abi_call_info(self, ABI, i);
                    last_call_info = call_info;
                    self.codegen_has_call = true;
                    {
                        var a = i.next;
                        var iregnum: u8 = 0;
                        var avxnum: u8 = 0;
                        var memnum: u8 = 0;
                        while (a != NoRef) {
                            const iarg = self.iref(a) orelse return error.FLIRError;
                            switch (iarg.mem_type()) {
                                .intptr => {
                                    if (iregnum < call_info.args.len) {
                                        iarg.mckind = .ipreg;
                                        iarg.mcidx = @backingInt(call_info.args[iregnum]);
                                        iregnum += 1;
                                    } else {
                                        iarg.mckind = .call_memarg;
                                        iarg.mcidx = memnum;
                                        memnum += 1;
                                    }
                                },
                                .avxval => {
                                    if (avxnum >= 16) return error.NotImplemented;
                                    iarg.mckind = .vfreg;
                                    iarg.mcidx = avxnum;
                                    avxnum += 1;
                                },
                            }
                            a = iarg.next;
                        }
                    }
                    {
                        var r = i.op2;
                        var inum: u8 = 0;
                        var avxnum: u8 = 0;
                        while (r != NoRef) {
                            const rret = self.iref(r) orelse return error.FLIRError;
                            switch (rret.mem_type()) {
                                .intptr => {
                                    if (inum >= call_info.rets.len) return error.NotImplemented;
                                    rret.op2 = @backingInt(call_info.rets[inum]);
                                    // if the spiller is good enough, this should just be a fixed
                                    // allocation, no op2 shenanigans, and emit extra copy instructions as needed
                                    rret.mckind = .unallocated_ipreghint;
                                    rret.mcidx = @backingInt(call_info.rets[inum]);
                                    inum += 1;
                                },
                                .avxval => {
                                    if (avxnum >= 2) return error.NotImplemented;
                                    rret.op2 = avxnum;
                                    rret.mckind = .unallocated_vfreghint;
                                    rret.mcidx = avxnum;
                                    avxnum += 1;
                                },
                            }
                            r = rret.next;
                        }
                    }
                },
                else => {},
            }
        }

        if (n.is_return) {
            var next = n.putphi_list;
            var n_ipval: u8 = 0;
            var n_avx: u8 = 0;
            // TODO: use ABICallInfo also for the function signature!!!!!
            const ret_regs: [2]X86Asm.IPReg = .{ .rax, .rdx };
            while (next != NoRef) {
                const i = self.iref(next) orelse return error.FLIRError;
                if (i.tag == .retval) {
                    switch (i.mem_type()) {
                        .intptr => {
                            if (n_ipval >= ret_regs.len) return error.NotImplemented;
                            i.mckind = .ipreg;
                            i.mcidx = ret_regs[n_ipval].id();
                            n_ipval += 1;
                        },
                        .avxval => {
                            if (n_avx >= 2) return error.NotImplemented;
                            i.mckind = .vfreg;
                            i.mcidx = n_avx;
                            n_avx += 1;
                        },
                    }
                }
                next = i.next;
            }
        }
    }

    var mask: u16 = 0;
    for (ABI.call_unsaved) |r| {
        mask |= ipreg_flag(r.id());
    }
    self.call_clobber_mask = mask;

    // this is a bit of a placeholder, either we support the X86 memory ops or nothing :P
    self.abi_tag = ABI.tag;
}

// erase(xregs: [N]X86Asm.IPReg) [N]defs.IPReg
pub fn erase(xregs: anytype) [xregs.len]defs.IPReg {
    var r: [xregs.len]defs.IPReg = undefined;
    for (xregs, 0..) |x, i| {
        r[i] = @fromBackingInt(@backingInt(x));
    }
    return r;
}
