const std = @import("std");
const FLIR = @import("./FLIR.zig");
const CFO = @import("./CFO.zig");

const test_allocator = std.testing.allocator;

pub fn main() !void {
    var self = try FLIR.init(8, test_allocator);
    defer self.deinit();

    const start = try self.addNode();

    const arg1 = try self.arg();
    const arg2 = try self.arg();
    const arg3 = try self.arg();

    const var_i = try self.variable();
    const const_0 = try self.const_int(start, 0);

    try self.putvar(start, var_i, const_0);

    const loop = try self.addNode();
    // NB: assumes count > 0, always do first iteration
    self.n.items[start].s[0] = loop;

    const valx = try self.vbinop(loop, .load, arg1, var_i);
    const valy = try self.vbinop(loop, .load, arg2, var_i);
    const newval = try self.vmath(loop, .add, valx, valy);
    _ = try self.store(loop, arg1, var_i, newval);

    const const_1 = try self.const_int(loop, 1);
    // TODO: analysis should of course do this:
    self.iref(const_1).?.mckind = .fused;
    const iadd = try self.binop(loop, .iadd, var_i, const_1);
    try self.putvar(loop, var_i, iadd);
    _ = try self.binop(loop, .ilessthan, var_i, arg3);

    // if true
    self.n.items[loop].s[0] = loop;
    const end = try self.addNode();
    self.n.items[loop].s[1] = end;

    try self.ret(end, const_0);

    try self.test_analysis();

    self.debug_print();

    var cfo = try CFO.init(test_allocator);
    defer cfo.deinit();

    _ = try self.codegen(&cfo);
    try cfo.dbg_nasm(test_allocator);
}