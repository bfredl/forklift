const std = @import("std");
const linux = std.os.linux;
const BPF = linux.BPF;
const mem = std.mem;
const fd_t = linux.fd_t;

pub fn prog_load_test(prog_type: BPF.ProgType, c: []BPF.Insn, license: []const u8) !fd_t {
    var log_buf = [1]u8{0} ** 1024;
    var log = BPF.Log{ .level = 4, .buf = &log_buf };
    return BPF.prog_load(prog_type, c, &log, license, 0) catch |err| {
        std.debug.print("failed load: {s}\n", .{mem.sliceTo(&log_buf, 0)});
        return err;
    };
}

pub fn prog_test_run(
    prog: fd_t,
    data_in: []const u8,
) !u32 {
    var attr = BPF.Attr{
        .test_run = mem.zeroes(BPF.TestRunAttr),
    };

    attr.test_run.prog_fd = prog;

    attr.test_run.data_in = @ptrToInt(data_in.ptr);
    attr.test_run.data_size_in = @intCast(u32, data_in.len);

    attr.test_run.ctx_in = @ptrToInt(&([1]u8{0} ** 192));
    attr.test_run.ctx_size_in = 192;

    const rc = linux.bpf(.prog_test_run, &attr, @sizeOf(BPF.TestRunAttr));
    // TODO: check the docs for actually expected errors
    std.debug.print("the errno {}\n", .{linux.getErrno(rc)});
    return switch (linux.getErrno(rc)) {
        .SUCCESS => attr.test_run.retval,
        .ACCES => error.UnsafeProgram,
        .FAULT => error.BPFProgramFault,
        .INVAL => error.InvalidArgument,
        .PERM => error.AccessDenied,
        else => |err| std.os.unexpectedErrno(err),
    };
}
