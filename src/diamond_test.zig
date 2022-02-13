const std = @import("std");
const FLIR = @import("./FLIR.zig");
const CFO = @import("./CFO.zig");
const codegen = @import("./codegen.zig").codegen;

const test_allocator = std.testing.allocator;

pub fn main() !void {
    var self = try FLIR.init(8, test_allocator);
    defer self.deinit();

    const start = try self.addNode();
    const arg1 = try self.arg();
    const arg2 = try self.arg();
    const v = try self.variable();

    // const const_0 = try self.const_int(start, 0);
    const const_42 = try self.const_int(start, 42);
    try self.putvar(start, v, const_42);
    _ = try self.binop(start, .ilessthan, arg1, v);

    const left = try self.addNode();
    self.n.items[start].s[0] = left;
    const addl = try self.iop(left, .add, v, arg2);
    try self.putvar(left, v, addl);

    const right = try self.addNode();
    self.n.items[start].s[1] = right;
    const addr = try self.iop(right, .add, v, arg1);
    try self.putvar(right, v, addr);

    const end = try self.addNode();
    self.n.items[left].s[0] = end;
    self.n.items[right].s[0] = end;

    const const_77 = try self.const_int(end, 77);
    const adde = try self.iop(end, .add, v, const_77);
    try self.putvar(end, v, adde);

    try self.ret(end, v);

    try self.test_analysis();

    if (false) {
        var phii = self.iref(adde).?.op1;
        var thephi = self.iref(phii).?;

        thephi.mckind = .ipreg;
        thephi.mcidx = 8; // r8
    }

    self.debug_print();

    var cfo = try CFO.init(test_allocator);
    defer cfo.deinit();

    _ = try @import("./codegen.zig").codegen(&self, &cfo);
    try cfo.dbg_nasm(test_allocator);
}
