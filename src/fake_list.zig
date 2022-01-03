const std = @import("std");
const Allocator = std.mem.Allocator;

const builtin = @import("builtin");
const s2 = builtin.zig_is_stage2;
pub const ArrayList = if (s2) FakeList else std.ArrayList;

pub fn FakeList(comptime T: type) type {
    return struct {
        const Self = @This();
        items: Slice,
        capacity: usize,
        pub const Slice = []T;

        pub fn initCapacity(allocator: Allocator, num: usize) Self {
            const mem = allocator.alloc(T, num) catch unreachable;
            var items = mem;
            items.len = 0;
            return Self{ .items = items, .capacity = mem.len };
        }

        pub fn deinit(self: *Self) void {
            _ = self.items;
        }

        pub fn addOne(self: *Self) !*T {
            if (!(self.items.len < self.capacity)) return error.OutOfMemory;

            self.items.len += 1;
            return &self.items[self.items.len - 1];
        }

        pub fn append(self: *Self, item: T) !void {
            const new_item_ptr = try self.addOne();
            new_item_ptr.* = item;
        }

        pub fn addManyAsArray(self: *Self, comptime n: usize) !*[n]T {
            if (!(self.items.len + n < self.capacity)) return error.OutOfMemory;
            const prev_len = self.items.len;
            self.items.len += n;
            return self.items[prev_len..][0..n];
        }
    };
}
