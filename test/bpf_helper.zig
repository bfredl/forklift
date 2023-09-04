const std = @import("std");
pub fn main() !void {
    const argv = std.os.argv;
    var allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const gpa = allocator.allocator();
    std.log.info("the yarg: {s}", .{argv[1]});

    const absolute_path = try std.fs.realpathAlloc(gpa, std.mem.span(argv[1]));
    const init_str = try std.fmt.allocPrintZ(gpa, "init={s}", .{absolute_path});

    const args = [_][]const u8{ "vmlinux", "root=/dev/root", "rootfstype=hostfs", init_str, "con0=fd:0,fd:1", "con=pts", "ssl=pts", "console=tty2" };

    var child = std.ChildProcess.init(&args, gpa);
    try child.spawn();

    _ = try child.wait();
}
