const std = @import("std");
pub fn readall(io: std.Io, gpa: std.mem.Allocator, filename: []const u8) ![]u8 {
    const fil = try std.Io.Dir.cwd().openFile(io, filename, .{});
    const stat = try fil.stat(io);
    const size = std.math.cast(usize, stat.size) orelse return error.FileTooBig;
    const buf = try gpa.alloc(u8, size);
    if (try fil.readStreaming(io, &.{buf}) < size) {
        return error.IOError;
    }
    return buf;
}
