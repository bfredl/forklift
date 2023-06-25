const std = @import("std");
const linux = std.os.linux;
const BPF = linux.BPF;
const mem = std.mem;
const fd_t = linux.fd_t;

pub fn prog_load_test(prog_type: BPF.ProgType, c: []BPF.Insn, license: []const u8, flags: u32) !fd_t {
    var log_buf = [1]u8{0} ** 1024;
    var log = BPF.Log{ .level = 4, .buf = &log_buf };
    return BPF.prog_load(prog_type, c, &log, license, 0, flags) catch |err| {
        std.debug.print("failed load: {s}\n", .{mem.sliceTo(&log_buf, 0)});
        return err;
    };
}

pub fn prog_test_run(
    prog: fd_t,
    ctx_in: ?[]const u8,
) !u32 {
    var attr = BPF.Attr{
        .test_run = mem.zeroes(BPF.TestRunAttr),
    };

    attr.test_run.prog_fd = prog;
    if (ctx_in) |ctx| {
        attr.test_run.ctx_in = @intFromPtr(ctx.ptr);
        attr.test_run.ctx_size_in = @intCast(ctx.len);
    }

    const rc = linux.bpf(.prog_test_run, &attr, @sizeOf(BPF.TestRunAttr));
    const err = linux.getErrno(rc);
    if (err != .SUCCESS) {
        std.debug.print("\nprog_test_run: E{s}\n", .{@tagName(err)});
    }
    return switch (err) {
        .SUCCESS => attr.test_run.retval,
        .ACCES => error.UnsafeProgram,
        .FAULT => error.BPFProgramFault,
        .INVAL => error.InvalidArgument,
        .PERM => error.AccessDenied,
        else => std.os.unexpectedErrno(err),
    };
}
