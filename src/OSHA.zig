const std = @import("std");
const CFO = @import("./CFO.zig");
const debug = std.debug;
const os = std.os;
const io = std.io;

var the_cfo: ?*CFO = null;
pub fn addr_lookup(addr: usize) usize {
    return if (the_cfo) |c| c.lookup(addr) else addr;
}

fn sigHandler(sig: i32, info: *const os.siginfo_t, ctx_ptr: ?*const anyopaque) callconv(.C) void {
    const s = io.getStdErr().writer();
    s.print("Do not indulge in stunt driving or horseplay. {}\n", .{sig}) catch unreachable;
    _ = info;

    const ctx = @ptrCast(*const os.ucontext_t, @alignCast(@alignOf(os.ucontext_t), ctx_ptr));
    var ip = @intCast(usize, ctx.mcontext.gregs[os.REG.RIP]);
    const bp = @intCast(usize, ctx.mcontext.gregs[os.REG.RBP]);

    if (the_cfo) |c| {
        if (sig == os.SIG.TRAP) {
            // if we trapped, ip will refer to the next instruction after the trap
            ip -= 1;
        }
        // WTF is the CFO doing?
        ip = c.lookup(ip);
    }

    debug.dumpStackTraceFromBase(bp, ip);
}

pub fn install(cfo: *CFO) void {
    the_cfo = cfo;

    var act = os.Sigaction{
        .handler = .{ .sigaction = sigHandler },
        .mask = os.empty_sigset,
        .flags = (os.SA.SIGINFO | os.SA.RESTART),
    };

    os.sigaction(os.SIG.TRAP, &act, null);
}

pub fn clear() void {
    the_cfo = null;
}
