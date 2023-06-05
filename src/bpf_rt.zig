const std = @import("std");
const linux = std.os.linux;
const BPF = linux.BPF;
const mem = std.mem;
const fd_t = linux.fd_t;

pub fn prog_load_test(prog_type: BPF.ProgType, c: []BPF.Insn, license: []const u8, flags: u32) !fd_t {
    var log_buf = [1]u8{0} ** 1024;
    var log = BPF.Log{ .level = 4, .buf = &log_buf };
    return prog_load(prog_type, c, &log, license, 0, flags) catch |err| {
        std.debug.print("failed load: {s}\n", .{mem.sliceTo(&log_buf, 0)});
        return err;
    };
}

// copy with added flags
pub fn prog_load(
    prog_type: BPF.ProgType,
    insns: []const BPF.Insn,
    log: ?*BPF.Log,
    license: []const u8,
    kern_version: u32,
    flags: u32,
) !fd_t {
    var attr = BPF.Attr{
        .prog_load = std.mem.zeroes(BPF.ProgLoadAttr),
    };

    attr.prog_load.prog_type = @enumToInt(prog_type);
    attr.prog_load.insns = @ptrToInt(insns.ptr);
    attr.prog_load.insn_cnt = @intCast(u32, insns.len);
    attr.prog_load.license = @ptrToInt(license.ptr);
    attr.prog_load.kern_version = kern_version;
    attr.prog_load.prog_flags = flags;

    if (log) |l| {
        attr.prog_load.log_buf = @ptrToInt(l.buf.ptr);
        attr.prog_load.log_size = @intCast(u32, l.buf.len);
        attr.prog_load.log_level = l.level;
    }

    const rc = linux.bpf(.prog_load, &attr, @sizeOf(BPF.ProgLoadAttr));
    return switch (linux.getErrno(rc)) {
        .SUCCESS => @intCast(fd_t, rc),
        .ACCES => error.UnsafeProgram,
        .FAULT => unreachable,
        .INVAL => error.InvalidProgram,
        .PERM => error.AccessDenied,
        else => |err| std.os.unexpectedErrno(err),
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
        attr.test_run.ctx_in = @ptrToInt(ctx.ptr);
        attr.test_run.ctx_size_in = @intCast(u32, ctx.len);
    }

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
