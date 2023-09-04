const std = @import("std");
pub fn main() !void {
    const argv = std.os.argv;
    var allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const gpa = allocator.allocator();
    // std.log.info("the yarg: {s}", .{argv[1]});
    const absolute_path = try std.fs.realpathAlloc(gpa, std.mem.span(argv[1]));

    const init_name = "Xinit.sh";
    const file = try std.fs.cwd().createFile(init_name, .{ .mode = 0o666 });
    const writer = file.writer();
    try writer.writeAll("#!/bin/bash\n");
    try writer.print("{s} --listen=- < /dev/tty0 > /dev/tty0\n", .{absolute_path});
    file.close();

    const absolute_path_init = try std.fs.realpathAlloc(gpa, init_name);
    const init_str = try std.fmt.allocPrintZ(gpa, "init={s}", .{absolute_path_init});

    const args = [_][]const u8{ "setsid", "vmlinux", "root=/dev/root", "rootfstype=hostfs", init_str, "con0=fd:0,fd:1", "con=pts", "ssl=pts", "console=tty2" };

    var child = std.ChildProcess.init(&args, gpa);
    //child.stderr_behavior = .Ignore;
    try child.spawn();

    _ = try child.wait();
}
