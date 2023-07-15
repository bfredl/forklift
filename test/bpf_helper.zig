const std = @import("std");
pub fn main() void {
    const argv = std.os.argv;
    std.log.info("the yarg: {s}", .{argv[1]});
}
