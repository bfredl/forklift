const testutil = @import("./flir.zig");
const parse_test = testutil.parse_test;
const expect = testutil.expect;

const AFunc = *const fn (arg1: usize) callconv(.C) usize;
const PFunc = *const fn (arg1: [*]const u8) callconv(.C) usize;
const SFunc = *const fn (arg1: [*]const u8, arg2: usize) callconv(.C) usize;

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

test "break at end" {
    var cfo = try parse_test(
        \\func main {
        \\  args data len;
        \\  vars i imatch result;
        \\  i := 0;
        \\  result := 0;
        \\  loop {
        \\    result := data[i];
        \\    i := i + 1;
        \\    if (i >= len) break;
        \\  }
        \\  return result;
        \\}
    );
    defer cfo.deinit();
    const func = cfo.get_ptr(0, SFunc);
    const data1 = [_]u8{ 23, 28, 2, 30 };
    try expect(usize, 30, func(&data1, 4));
    try expect(usize, 2, func(&data1, 3));
    try expect(usize, 23, func(&data1, 1));
    try expect(usize, 23, func(&data1, 0));
}

test "complex control flow" {
    var cfo = try parse_test(
        \\func main {
        \\  args data len;
        \\  vars i imatch result;
        \\  i := 0;
        \\  result := 0;
        \\  loop {
        \\    let val = data[i];
        \\    i := i + 1;
        \\    if (val < 3) break;
        \\    if (val > 20) {
        \\      result := val;
        \\    }
        \\    if (val == 25) break;
        \\    if (i >= len) break;
        \\  }
        \\  return result;
        \\}
    );
    defer cfo.deinit();
    const func = cfo.get_ptr(0, SFunc);
    const data1 = [_]u8{ 23, 15, 28, 2, 30 };
    try expect(usize, 28, func(&data1, 5));
    try expect(usize, 28, func(&data1, 4));
    try expect(usize, 28, func(&data1, 3));
    try expect(usize, 23, func(&data1, 2));
    try expect(usize, 23, func(&data1, 1));
    const data2 = [_]u8{ 25, 28, 2, 30 };
    try expect(usize, 25, func(&data2, 4));
}

test "store loop" {
    var cfo = try parse_test(
        \\func scripter {
        \\  args res len;
        \\  vars i sum;
        \\  i := 0;
        \\  sum := 0;
        \\  loop {
        \\    if (i >= len) break;
        \\    sum := sum + i;
        \\    if (i & 1 == 0) {
        \\      res[i] = sum;
        \\    }
        \\    i := i + 1;
        \\  }
        \\  return sum;
        \\}
    );
    defer cfo.deinit();
    const func = cfo.get_ptr(0, *const fn (arg1: [*]u8, arg2: usize) callconv(.C) usize);
    var data1 = [_]u8{ 23, 15, 28, 2, 30, 7 };
    try expect(usize, 15, func(&data1, data1.len));
    try expect([6]u8, .{ 0, 15, 3, 2, 10, 7 }, data1);
}
