const Module = @import("./Module.zig");
const Function = @import("./Function.zig");
const defs = @import("./defs.zig");
const dbg = std.debug.print;
const std = @import("std");
const Reader = @import("./Reader.zig");

const forklift = @import("forklift");
const X86Asm = forklift.X86Asm;
const IPReg = X86Asm.IPReg;

// "one pass" compiler, although we might cheat and allow the Function.ensure_parsed()
// pass to pre-collect some required data.
//
// TODO: eleminate stack_base as this is should be a compile time known offset from frame_base

pub const BlockFunc = *const fn (frame_base: [*]defs.StackValue, stack_base: [*]defs.StackValue, mem_start: [*]u8, mem_len: usize) callconv(.c) void;

pub const LightningTrace = struct {
    func: BlockFunc,
};

// c calling convention:
const frame_base: IPReg = .rdi; // arg1: pointer to first local (then stack)
const stack_base: IPReg = .rsi; // arg2: pointer to first stack slot to use (TODO: fold into arg1)
const mem_start: IPReg = .rdx;
// arg4: rcx = memory_len

const bo = X86Asm.bo;
pub fn local_slot(idx: u32) X86Asm.EAddr {
    return bo(frame_base, @intCast(8 * idx));
}
pub fn stack_slot(idx: u32) X86Asm.EAddr {
    return bo(stack_base, @intCast(8 * idx));
}

const ThunderLightning = @This();
cfo: X86Asm,

// keep track of up to two stack levels. the states are essentially
// 0. all of stack is in stack_base[]
// 1. top of stack is in virt_state[0]
// 2. top of stack is in virt_state[1], next in virst_state[0]
// in state 2: virt_state[0] cannot be a memory address

num_tracked: u8 = 0,
virt_state: [2]ValueState = undefined, // only used with rax_state=2
virt_wide: [2]bool = undefined, // messy, maybe a struct togheter with ValueState

val_stack_level: u16 = 0, // wasm stack level, regardless if actually stored or not

const ValueState = union(enum) {
    // only a blessed set, currently rax and r10
    reg: IPReg,
    imm: defs.StackValue,
    local: u32,
    on_stack: void, // a bit redudant but useful is some edge cases
};

// get_foo_bar() implies "make it so"! these might emit an instruction to
// ensure the proper state of the top

// if we are in state 2, transition to state 1.
// return if rax is free to use already

fn wide(typ: defs.ValType) !bool {
    return switch (typ) {
        .i32 => false,
        .i64 => true,
        else => error.NotImplemented,
    };
}

pub fn push_value_prepare(self: *ThunderLightning) !u8 {
    self.val_stack_level += 1;
    if (self.num_tracked == 2) {
        try switch (self.virt_state[0]) {
            .on_stack => {},
            .reg => |reg| self.cfo.movmr(self.virt_wide[0], stack_slot(self.val_stack_level - 3), reg),
            .imm => |val| self.cfo.movmi(false, stack_slot(self.val_stack_level - 3), val.i32), // TODO: i64_const
            .local => @panic("should not happen:p"),
        };
        self.virt_state[0] = self.virt_state[1];
        self.virt_wide[0] = self.virt_wide[1];
        self.num_tracked = 1; // FALLTHROUGH;
    }
    if (self.num_tracked == 1) {
        switch (self.virt_state[0]) {
            .local => |idx| {
                try self.cfo.movrm(self.virt_wide[0], .rax, local_slot(idx));
                self.virt_state[0] = .{ .reg = .rax };
            },
            .reg, .imm, .on_stack => {},
        }
        self.num_tracked = 2;
        return 1;
    }
    self.num_tracked = 1;
    return 0;
}

// TODO: "commutative" flag
pub fn pop_as_reg_virt(self: *ThunderLightning, w_reg: bool) !struct { IPReg, ValueState } {
    self.val_stack_level -= 1;
    self.virt_wide[0] = w_reg;
    if (self.num_tracked == 0 or (self.num_tracked == 1 and self.virt_state[0] == .on_stack)) {
        try self.cfo.movrm(w_reg, .rax, stack_slot(self.val_stack_level)); // note: pre-adjusted
        self.num_tracked = 1;
        self.virt_state[0] = .{ .reg = .rax };
        return .{ .rax, .{ .on_stack = {} } };
    } else if (self.num_tracked == 1) {
        const alt = self.virt_state[0];
        const freereg: IPReg = if (switch (alt) {
            .reg => |reg| reg != .rax,
            else => true,
        }) .rax else .r10;
        try self.cfo.movrm(w_reg, freereg, stack_slot(self.val_stack_level - 1));
        self.virt_state[0] = .{ .reg = freereg };
        return .{ freereg, alt };
    } else {
        self.num_tracked = 1;
        const alt = self.virt_state[1];
        const freereg: IPReg = if (switch (alt) {
            .reg => |reg| reg != .rax,
            else => true,
        }) .rax else .r10;
        try switch (self.virt_state[0]) {
            .on_stack => self.cfo.movrm(w_reg, freereg, stack_slot(self.val_stack_level - 1)),
            .reg => |reg| return .{ reg, alt },
            .imm => |val| self.cfo.movri(w_reg, freereg, val.i32),
            .local => @panic("should not happen:p"),
        };
        self.virt_state[0] = .{ .reg = freereg };
        return .{ freereg, alt };
    }
}

// pre_w is the pre-existing value, caller should change it afterwards on type-change
pub fn top_as_reg(self: *ThunderLightning, pre_w: bool) !IPReg {
    if (self.num_tracked == 0 or (self.num_tracked == 1 and self.virt_state[0] == .on_stack)) {
        try self.cfo.movrm(pre_w, .rax, stack_slot(self.val_stack_level - 1));
        self.num_tracked = 1;
        self.virt_state[0] = .{ .reg = .rax };
        self.virt_wide[0] = pre_w;
        return .rax;
    } else if (self.num_tracked == 1) {
        try switch (self.virt_state[0]) {
            .on_stack => unreachable, // handled above
            .reg => |reg| return reg,
            .imm => |val| self.cfo.movri(pre_w, .rax, val.i32),
            .local => |idx| self.cfo.movrm(pre_w, .rax, local_slot(idx)),
        };
        self.virt_state[0] = .{ .reg = .rax };
        return .rax;
    } else { // num_tracked == 2
        const alt = self.virt_state[0];
        const freereg: IPReg = if (switch (alt) {
            .reg => |reg| reg != .rax,
            else => true,
        }) .rax else .r10;

        try switch (self.virt_state[1]) {
            .on_stack => self.cfo.movrm(pre_w, freereg, stack_slot(self.val_stack_level - 1)),
            .reg => |reg| return reg,
            .imm => |val| self.cfo.movri(pre_w, freereg, val.i32),
            .local => |idx| self.cfo.movrm(pre_w, freereg, local_slot(idx)),
        };
        self.virt_state[1] = .{ .reg = freereg };
        return freereg;
    }
}

pub fn pop(self: *ThunderLightning) ValueState {
    self.val_stack_level -= 1;
    if (self.num_tracked > 0) {
        self.num_tracked -= 1;
        return self.virt_state[self.num_tracked]; // pre-decremented
    }
    return .{ .on_stack = {} };
}

pub fn pop2_as_reg_regimm(self: *ThunderLightning, w1: bool, w2: bool) !struct { IPReg, ValueState } {
    const reg, const virt = try self.pop_as_reg_virt(w1);
    if (self.num_tracked != 1) @panic("do not");
    self.num_tracked = 0;
    self.val_stack_level -= 1;
    const freereg: IPReg = if (reg == .rax) .r10 else .rax;
    try switch (virt) {
        .reg, .imm => return .{ reg, virt },
        .local => |idx| self.cfo.movrm(w2, freereg, local_slot(idx)),
        .on_stack => self.cfo.movrm(w2, freereg, stack_slot(self.val_stack_level + 1)), // luring, load what is above the stack
    };
    return .{ reg, .{ .reg = freereg } };
}

// blk_idx is into the control array of function
pub fn compile_block(mod: *Module, func: *Function, blk_idx: u32) !LightningTrace {
    const c = try func.ensure_parsed(mod);
    const blk_off = c[blk_idx].off;

    var code = try forklift.CodeBuffer.init(mod.allocator);
    var self: ThunderLightning = .{ .cfo = X86Asm{ .code = &code, .long_jump_mode = true } };
    const cfo = &self.cfo;

    var r = mod.reader_at(blk_off);

    if ((try r.readByte()) != @intFromEnum(defs.OpCode.loop)) return error.NotImplemented;
    const top_args, const top_ret = try (try r.blocktype()).arity(mod);
    if (top_args != 0 or top_ret != 0) return error.NotImplemented;
    const loop_header = code.get_target();

    // TODO: control should know the start level
    var level: u16 = 1;

    _ = stack_base;

    errdefer cfo.dbg_nasm(mod.allocator) catch unreachable;

    var next_cmp: ?X86Asm.Cond = null;

    while (true) {
        const pos = r.pos;
        _ = pos;
        const inst = try r.readOpCode();
        if (inst == .end or inst == .else_) level -= 1;

        const cur_cmp = next_cmp;
        next_cmp = null;

        // dbg("{x:04} => {x:04}:", .{ pos, code.get_target() });
        for (0..level) |_| dbg("  ", .{});
        dbg("[{} {}] {s}\n", .{ self.val_stack_level, self.num_tracked, @tagName(inst) });
        switch (inst) {
            .local_get => {
                const idx = try r.readu();
                const slot = try self.push_value_prepare();
                self.virt_state[slot] = .{ .local = idx };
                self.virt_wide[slot] = try wide(func.local_types[idx]);
            },
            .i32_const => {
                const val = try r.readLeb(i32);
                const slot = try self.push_value_prepare();
                self.virt_state[slot] = .{ .imm = .{ .i32 = val } };
            },
            .local_set, .local_tee => {
                const idx = try r.readu();
                // TODO: top_as_regimm??
                const w = try wide(func.local_types[idx]);
                const src = try self.top_as_reg(w);
                try cfo.movmr(w, local_slot(idx), src);
                if (inst == .local_set) _ = self.pop();
            },
            .i32_add => {
                const dst, const src = try self.pop_as_reg_virt(false);
                try switch (src) {
                    .imm => |val| cfo.aritri(.add, false, dst, val.i32),
                    .reg => |reg| cfo.arit(.add, false, dst, reg),
                    .local => |idx| cfo.aritrm(.add, false, dst, local_slot(idx)),
                    .on_stack => cfo.aritrm(.add, false, dst, stack_slot(self.val_stack_level)),
                };
            },
            .i32_mul => {
                const dst, const src = try self.pop_as_reg_virt(false);
                try switch (src) {
                    .imm => |val| cfo.imulrri(false, dst, dst, val.i32),
                    .reg => |reg| cfo.imulrr(false, dst, reg),
                    .local => |idx| cfo.imulrm(false, dst, local_slot(idx)),
                    .on_stack => cfo.imulrm(false, dst, stack_slot(self.val_stack_level)),
                };
            },
            // TODO: need to cleanup ForkLift before we can structurize this.
            .i64_load => {
                const dst = try self.top_as_reg(false);
                const alignas = try r.readu();
                _ = alignas; // "The alignment in load and store instructions does not affect the semantics."
                const offset = try r.readu();
                // TODO: baaaunds checking
                try cfo.movrm(true, dst, X86Asm.bi(mem_start, dst).o(@intCast(offset)));
                self.virt_wide[self.num_tracked - 1] = true;
            },
            .i32_load8_u => {
                const dst = try self.top_as_reg(false);
                const alignas = try r.readu();
                _ = alignas; // "The alignment in load and store instructions does not affect the semantics."
                const offset = try r.readu();
                // TODO: baaaunds checking
                try cfo.movrm_byte(false, dst, X86Asm.bi(mem_start, dst).o(@intCast(offset)));
                self.virt_wide[self.num_tracked - 1] = true;
            },
            .i64_store => {
                const dst, const src = try self.pop2_as_reg_regimm(false, true);
                const alignas = try r.readu();
                _ = alignas; // "The alignment in load and store instructions does not affect the semantics."
                const offset = try r.readu();
                // TODO: refactor forklift to use .decl constructors for EAddr!
                const dstaddr = X86Asm.bi(mem_start, dst).o(@intCast(offset));
                try switch (src) {
                    .imm => |val| cfo.movmi(true, dstaddr, @intCast(val.i64)), // TODO: fails for BIG intermediates
                    .reg => |reg| cfo.movmr(true, dstaddr, reg),
                    .local, .on_stack => unreachable, // TODO: separate type for reg/imm only?
                };
            },
            .i32_store8 => {
                const dst, const src = try self.pop2_as_reg_regimm(false, false);
                const alignas = try r.readu();
                _ = alignas; // "The alignment in load and store instructions does not affect the semantics."
                const offset = try r.readu();
                // TODO: refactor forklift to use .decl constructors for EAddr!
                const dstaddr = X86Asm.bi(mem_start, dst).o(@intCast(offset));
                try switch (src) {
                    .imm => |val| cfo.movmi_byte(dstaddr, @truncate(val.u32())),
                    .reg => |reg| cfo.movmr_byte(dstaddr, reg),
                    .local, .on_stack => unreachable, // TODO: separate type for reg/imm only?
                };
            },
            .i32_ne => {
                const dst, const src = try self.pop_as_reg_virt(false);
                try switch (src) {
                    .imm => |val| cfo.aritri(.cmp, false, dst, val.i32),
                    .reg => |reg| cfo.arit(.cmp, false, dst, reg),
                    .local => |idx| cfo.aritrm(.cmp, false, dst, local_slot(idx)),
                    .on_stack => cfo.aritrm(.cmp, false, dst, stack_slot(self.val_stack_level)),
                };
                const peekinst: defs.OpCode = @enumFromInt(r.peekByte());
                if (peekinst != .br_if) return error.NotImplemented;
                next_cmp = .ne;
                _ = self.pop();
            },
            .br_if => {
                const label = try r.readu();
                if (label != 0) return error.NotImplemented;
                if (cur_cmp) |cmp| {
                    try cfo.jbck(cmp, loop_header);
                } else {
                    const src = self.pop();
                    try switch (src) {
                        .imm => |_| @panic("unbranch this"),
                        .reg => |reg| cfo.aritri(.cmp, false, reg, 0),
                        .local => |idx| cfo.aritmi(.cmp, false, local_slot(idx), 0),
                        .on_stack => cfo.aritmi(.cmp, false, stack_slot(self.val_stack_level), 0),
                    };
                    try cfo.jbck(.ne, loop_header);
                }
            },
            .end => break,
            else => {
                return error.NotImplemented;
            },
        }
    }
    try cfo.ret();
    // cfo.dbg_nasm(mod.allocator) catch unreachable;
    try code.finalize();
    return .{ .func = code.get_ptr(0, BlockFunc) };
}
