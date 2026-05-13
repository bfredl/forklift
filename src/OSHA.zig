const std = @import("std");
const CodeBuffer = @import("./CodeBuffer.zig");
const debug = std.debug;
const os = std.os;
const posix = std.posix;
const io = std.io;

var the_cfo: ?*CodeBuffer = null;

pub const context_order: [16]u8 = .{
    REG.RAX,
    REG.RCX,
    REG.RDX,
    REG.RBX,
    REG.RSP,
    REG.RBP,
    REG.RSI,
    REG.RDI,
    REG.R8,
    REG.R9,
    REG.R10,
    REG.R11,
    REG.R12,
    REG.R13,
    REG.R14,
    REG.R15,
};

const greg_t = usize;
const gregset_t = [23]greg_t;
pub const ucontext = extern struct {
    _flags: usize,
    _link: ?*ucontext,
    _stack: std.os.linux.stack_t,
    mcontext: extern struct {
        gregs: gregset_t,
    },
};

pub const REG = struct {
    pub const R8 = 0;
    pub const R9 = 1;
    pub const R10 = 2;
    pub const R11 = 3;
    pub const R12 = 4;
    pub const R13 = 5;
    pub const R14 = 6;
    pub const R15 = 7;
    pub const RDI = 8;
    pub const RSI = 9;
    pub const RBP = 10;
    pub const RBX = 11;
    pub const RDX = 12;
    pub const RAX = 13;
    pub const RCX = 14;
    pub const RSP = 15;
    pub const RIP = 16;
    pub const EFL = 17;
    pub const CSGSFS = 18;
    pub const ERR = 19;
    pub const TRAPNO = 20;
    pub const OLDMASK = 21;
    pub const CR2 = 22;
};

fn sigHandler(sig: posix.SIG, info: *const posix.siginfo_t, ctx_ptr: ?*const anyopaque) callconv(.c) void {
    // TODO: bull, just use a big enough buffer and always do a single syscall write in the
    // end, no locks needed
    var buffer: [64]u8 = undefined;
    const slock = std.debug.lockStderr(&buffer);
    defer std.debug.unlockStderr();
    const s = &slock.file_writer.interface;

    const addr = @intFromPtr(info.fields.sigfault.addr);

    // TODO: this reimplements quite a bit of os.debug
    // ideally we should be able to do some pre-processing
    // and then "chain" the default handler
    const desc = switch (sig) {
        .SEGV => "Segmentation fault",
        .ILL => "Illegal instruction",
        .BUS => "Bus error",
        .TRAP => "Trap",
        else => "???",
    };

    const ctx: *const ucontext = @ptrCast(@alignCast(ctx_ptr));
    var ip: usize = @intCast(ctx.mcontext.gregs[REG.RIP]);
    const bp: usize = @intCast(ctx.mcontext.gregs[REG.RBP]);
    s.print("\nDo not indulge in stunt driving or horseplay!\n", .{}) catch unreachable;

    if (sig != posix.SIG.TRAP) {
        s.print("\n{s} at address 0x{x}\n\n", .{ desc, addr }) catch unreachable;
        s.print("RAX {x}\n", .{ctx.mcontext.gregs[REG.RAX]}) catch unreachable;
        s.print("RCX {x}\n\n", .{ctx.mcontext.gregs[REG.RCX]}) catch unreachable;
    }

    if (the_cfo) |c| {
        if (sig == posix.SIG.TRAP) {
            // if we trapped, ip will refer to the next instruction after the trap
            ip -= 1;
        }
        const base_addr: usize = @intFromPtr(c.buf.items.ptr);
        if (ip >= base_addr and ip < base_addr + c.buf.items.len) {
            const local_addr = ip - base_addr;
            s.print("ADRR {x}\n", .{local_addr}) catch unreachable;

            for (c.value_map.items) |it| {
                if (it.pos == local_addr) {
                    const val = ctx.mcontext.gregs[context_order[@intFromEnum(it.reg)]];
                    s.print("VAR {s} = {} ({x})\n", .{ it.name, @as(isize, @bitCast(val)), val }) catch unreachable;
                }
            }
        }

        if (false) {
            // WTF is the CFO doing?
            ip = c.lookup(ip);

            debug.dumpStackTraceFromBase(bp, ip);
        }
    }
}

pub fn install(cfo: *CodeBuffer) !void {
    the_cfo = cfo;

    var act = posix.Sigaction{
        .handler = .{ .sigaction = sigHandler },
        .mask = posix.sigemptyset(),
        .flags = (posix.SA.SIGINFO | posix.SA.RESTART),
    };

    posix.sigaction(posix.SIG.TRAP, &act, null);
    act.flags |= posix.SA.RESETHAND;
    posix.sigaction(posix.SIG.SEGV, &act, null);
    posix.sigaction(posix.SIG.ILL, &act, null);
    posix.sigaction(posix.SIG.BUS, &act, null);
}

pub fn clear() void {
    the_cfo = null;
}
