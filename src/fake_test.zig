const std = @import("std");
const FakeList = @import("./fake_list.zig").FakeList;
const ArrayList = std.ArrayList;
const page_allocator = std.heap.page_allocator;
const Allocator = std.mem.Allocator;

const fake = false;
const List = if (fake) FakeList else ArrayList;

fn fake_list(T: type, size: usize, allocator: Allocator) List(T) {
    if (fake) {
        return FakeList(u8).initCapacity(allocator, size);
    } else {
        return ArrayList(u8).initCapacity(allocator, size) catch unreachable;
    }
}

pub fn main2() !void {
    const size = 1024 * 4;
    var list = fake_list(u8, size, page_allocator);
    _ = list;
    const ptr = try list.addOne();
    ptr.* = 4;
}

pub fn main() void {
    main2() catch unreachable;
}
