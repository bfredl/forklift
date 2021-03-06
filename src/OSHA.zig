const std = @import("std");
const CFO = @import("./CFO.zig");
const debug = std.debug;
const os = std.os;
const io = std.io;

var the_cfo: ?*CFO = null;

fn sigHandler(sig: i32, info: *const os.siginfo_t, ctx_ptr: ?*const anyopaque) callconv(.C) void {
    const s = io.getStdErr().writer();

    const addr = @ptrToInt(info.fields.sigfault.addr);

    // TODO: this reimplements quite a bit of os.debug
    // ideally we should be able to do some pre-processing
    // and then "chain" the default handler
    const desc = switch (sig) {
        os.SIG.SEGV => "Segmentation fault",
        os.SIG.ILL => "Illegal instruction",
        os.SIG.BUS => "Bus error",
        os.SIG.TRAP => "Trap",
        else => "???",
    };
    s.print("\n{s} at address 0x{x}\n\n", .{ desc, addr }) catch unreachable;
    s.print("Do not indulge in stunt driving or horseplay!\n", .{}) catch unreachable;

    const ctx = @ptrCast(*const os.ucontext_t, @alignCast(@alignOf(os.ucontext_t), ctx_ptr));
    var ip = @intCast(usize, ctx.mcontext.gregs[os.REG.RIP]);
    const bp = @intCast(usize, ctx.mcontext.gregs[os.REG.RBP]);

    s.print("RAX {x}\n", .{ctx.mcontext.gregs[os.REG.RAX]}) catch unreachable;
    s.print("RCX {x}\n\n", .{ctx.mcontext.gregs[os.REG.RCX]}) catch unreachable;

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
    act.flags |= os.SA.RESETHAND;
    os.sigaction(os.SIG.SEGV, &act, null);
    os.sigaction(os.SIG.ILL, &act, null);
    os.sigaction(os.SIG.BUS, &act, null);
}

pub fn clear() void {
    the_cfo = null;
}
