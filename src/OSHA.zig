const std = @import("std");
const CodeBuffer = @import("./CodeBuffer.zig");
const debug = std.debug;
const os = std.os;
const posix = std.posix;
const io = std.io;

var the_cfo: ?*CodeBuffer = null;

const REG = std.os.linux.REG;
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

fn sigHandler(sig: i32, info: *const posix.siginfo_t, ctx_ptr: ?*const anyopaque) callconv(.c) void {
    const s = io.getStdErr().writer();

    const addr = @intFromPtr(info.fields.sigfault.addr);

    // TODO: this reimplements quite a bit of os.debug
    // ideally we should be able to do some pre-processing
    // and then "chain" the default handler
    const desc = switch (sig) {
        posix.SIG.SEGV => "Segmentation fault",
        posix.SIG.ILL => "Illegal instruction",
        posix.SIG.BUS => "Bus error",
        posix.SIG.TRAP => "Trap",
        else => "???",
    };

    const ctx: *const posix.ucontext_t = @ptrCast(@alignCast(ctx_ptr));
    var ip: usize = @intCast(ctx.mcontext.gregs[posix.REG.RIP]);
    const bp: usize = @intCast(ctx.mcontext.gregs[posix.REG.RBP]);
    s.print("\nDo not indulge in stunt driving or horseplay!\n", .{}) catch unreachable;

    if (sig != posix.SIG.TRAP) {
        s.print("\n{s} at address 0x{x}\n\n", .{ desc, addr }) catch unreachable;
        s.print("RAX {x}\n", .{ctx.mcontext.gregs[posix.REG.RAX]}) catch unreachable;
        s.print("RCX {x}\n\n", .{ctx.mcontext.gregs[posix.REG.RCX]}) catch unreachable;
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
        .mask = posix.empty_sigset,
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
