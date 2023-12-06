const testutil = @import("./flir.zig");
const parse_test = testutil.parse_test;
const expect = testutil.expect;

const AFunc = *const fn (arg1: usize) callconv(.C) usize;

test "cfoscript basic" {
    var cfo = try parse_test(
        \\func scripter {
        \\  args foo;
        \\  vars i sum;
        \\  i := 0;
        \\  sum := 0;
        \\  loop {
        \\    if (i >= foo) break;
        \\    sum := sum + i;
        \\    i := i + 1;
        \\  }
        \\  return sum;
        \\}
    );
    defer cfo.deinit();

    const fun = cfo.get_ptr(0, AFunc);
    try expect(usize, 3, fun(3));
    try expect(usize, 15, fun(6));
    try expect(usize, 45, fun(10));
}
