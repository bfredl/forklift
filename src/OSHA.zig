const std = @import("std");
const debug = std.debug;
const os = std.os;

fn sigHandler(sig: i32, info: *const os.siginfo_t, ctx_ptr: ?*const anyopaque) callconv(.C) void {
    debug.print("Do not indulge in stunt driving or horseplay.\n", .{});
    debug.print("{} {}\n", .{ sig, info.* });
    _ = ctx_ptr;
}

pub fn install() void {
    var act = os.Sigaction{
        .handler = .{ .sigaction = sigHandler },
        .mask = os.empty_sigset,
        .flags = (os.SA.SIGINFO | os.SA.RESTART),
    };

    os.sigaction(os.SIG.TRAP, &act, null);
}
