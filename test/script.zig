const testutil = @import("./flir.zig");
const parse_test = testutil.parse_test;
const expect = testutil.expect;

const AFunc = *const fn (arg1: usize) callconv(.C) usize;
const PFunc = *const fn (arg1: [*]const u8) callconv(.C) usize;

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

test "break in else" {
    var cfo = try parse_test(
        \\func scripter {
        \\  args data;
        \\  vars i sum;
        \\  i := 0;
        \\  sum := 0;
        \\  loop {
        \\    let item = data[i];
        \\    if (item <= 10) {
        \\      sum := sum+item;
        \\    } else {
        \\      sum := sum+1000*item;
        \\      break;
        \\    }
        \\    i := i + 1;
        \\  }
        \\  return sum;
        \\}
    );
    defer cfo.deinit();

    const fun = cfo.get_ptr(0, PFunc);
    try expect(usize, 20003, fun(&[_]u8{ 1, 2, 20 }));
    try expect(usize, 234021, fun(&[_]u8{ 8, 7, 6, 234 }));
}
