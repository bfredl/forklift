const testutil = @import("./flir.zig");
const parse_test = testutil.parse_test;
const parse_multi = testutil.parse_multi;
const expect = testutil.expect;
const std = @import("std");
const os = std.os;

const UFunc = *const fn () callconv(.C) usize;
const AFunc = *const fn (arg1: usize) callconv(.C) usize;
const AIFunc = *const fn (arg1: isize) callconv(.C) isize;
const BFunc = *const fn (arg1: usize, arg2: usize) callconv(.C) usize;
const PFunc = *const fn (arg1: [*]const u8) callconv(.C) usize;
const SFunc = *const fn (arg1: [*]const u8, arg2: usize) callconv(.C) usize;

test "returner" {
    var cfo = try parse_test(
        \\func returner() {
        \\  return 7;
        \\}
    );
    defer cfo.deinit();

    try expect(usize, 7, cfo.get_ptr(0, UFunc)());
}

test "return u64" {
    var cfo = try parse_test(
        \\func returner() {
        \\  return 5743024064959639452;
        \\}
    );
    defer cfo.deinit();

    try expect(usize, 5743024064959639452, cfo.get_ptr(0, UFunc)());
}

test "comment" {
    var cfo = try parse_test(
        \\func returner() {
        \\  return 7; // this is a comment
        \\}
    );
    defer cfo.deinit();

    try expect(usize, 7, cfo.get_ptr(0, UFunc)());
}

test "var returner" {
    var cfo = try parse_test(
        \\func returner() {
        \\  vars myvar;
        \\  myvar := 57;
        \\
        \\
        \\  return myvar;
        \\}
    );
    defer cfo.deinit();

    try expect(usize, 57, cfo.get_ptr(0, UFunc)());
}

test "diamond returner" {
    var cfo = try parse_test(
        \\func returner(y) {
        \\  vars foo;
        \\  if (y < 17) {
        \\    foo := 20;
        \\  } else {
        \\    foo := 98;
        \\  }
        \\  return foo;
        \\}
    );
    defer cfo.deinit();

    const fun = cfo.get_ptr(0, AFunc);
    try expect(usize, 20, fun(10));
    try expect(usize, 20, fun(16));
    try expect(usize, 98, fun(17));
    try expect(usize, 98, fun(35));
}

test "equality" {
    var cfo = try parse_test(
        \\func returner(x, y) {
        \\  vars res;
        \\  if (x != y) {
        \\    res := 0;
        \\  } else {
        \\    res := 1;
        \\  }
        \\  return res;
        \\}
    );
    defer cfo.deinit();

    const fun = cfo.get_ptr(0, BFunc);
    try expect(usize, 0, fun(2, 4));
    try expect(usize, 1, fun(3, 3));
    try expect(usize, 0, fun(4, 2));
    try expect(usize, 1, fun(3, 3));
}
test "bander" {
    var cfo = try parse_test(
        \\func returner(x, y) {
        \\  return x & y;
        \\}
    );
    defer cfo.deinit();

    const fun = cfo.get_ptr(0, BFunc);
    try expect(usize, 4, fun(5, 6));
    try expect(usize, 2, fun(10, 34));
}

test "loop adder" {
    var cfo = try parse_test(
        \\func scripter(foo) {
        \\  vars i, sum;
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
    try expect(usize, 0, fun(0));
    try expect(usize, 0, fun(1));
    try expect(usize, 1, fun(2));
    try expect(usize, 3, fun(3));
    try expect(usize, 10, fun(5));
    try expect(usize, 15, fun(6));
    try expect(usize, 45, fun(10));
}

test "break in else" {
    var cfo = try parse_test(
        \\func scripter(data) {
        \\  vars i, sum;
        \\  i := 0;
        \\  sum := 0;
        \\  loop {
        \\    let item = @data[i];
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
        \\func main(data, len) {
        \\  vars i, imatch, result;
        \\  i := 0;
        \\  result := 0;
        \\  loop {
        \\    result := @data[i];
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
        \\func main(data, len) {
        \\  vars i, imatch, result;
        \\  i := 0;
        \\  result := 0;
        \\  loop {
        \\    let val = @data[i];
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

test "shift hl" {
    var cfo = try parse_test(
        \\func shifter(x) {
        \\  return x << 2;
        \\}
    );
    defer cfo.deinit();

    const fun = cfo.get_ptr(0, AIFunc);
    try expect(isize, 24, fun(6));
    try expect(isize, -12, fun(-3));
}

test "shift ar" {
    var cfo = try parse_test(
        \\func shifter(x) {
        \\  return x >> 3;
        \\}
    );
    defer cfo.deinit();

    const fun = cfo.get_ptr(0, AIFunc);
    try expect(isize, 9, fun(75));
    try expect(isize, -1, fun(-3));
}

test "shift hr" {
    var cfo = try parse_test(
        \\func shifter(x) {
        \\  return x |>> 3;
        \\}
    );
    defer cfo.deinit();

    const fun = cfo.get_ptr(0, AIFunc);
    try expect(isize, 9, fun(75));
    try expect(isize, 0x1fffffffffffffff, fun(-3));
}

test "shift variable" {
    var cfo = try parse_test(
        \\func shifter(x, y) {
        \\  return x << y;
        \\}
    );
    defer cfo.deinit();

    const fun = cfo.get_ptr(0, BFunc);
    try expect(usize, 6, fun(6, 0));
    try expect(usize, 10, fun(5, 1));
    try expect(usize, 24, fun(6, 2));
}

test "multi function" {
    var res = try parse_multi(
        \\func adder(x, y) {
        \\  return y + (x << 1);
        \\}
        \\
        \\func multiplier(x, y) {
        \\  return x*y;
        \\}
    );
    defer res.deinit_mem();

    const fun1 = try res.get_func_ptr("adder", BFunc);
    const fun2 = try res.get_func_ptr("multiplier", BFunc);
    try expect(usize, 210, fun1(100, 10));
    try expect(usize, 1000, fun2(100, 10));
}

test "call near" {
    var res = try parse_multi(
        \\func kuben(x) {
        \\  let prod = x*x;
        \\  return prod*x;
        \\}
        \\
        \\func twokube(x, y) {
        \\  let xx = %near kuben(x);
        \\  let yy = %near kuben(y);
        \\  return xx+yy;
        \\}
    );
    defer res.deinit_mem();

    const fun1 = try res.get_func_ptr("kuben", AFunc);
    try expect(usize, 1000000, fun1(100));

    const fun2 = try res.get_func_ptr("twokube", BFunc);
    try expect(usize, 1008, fun2(2, 10));
}

test "swap simple" {
    // FLIR.noisy = true;
    // defer FLIR.noisy = false;
    var res = try parse_multi(
        \\func diff(x, y) {
        \\  return x - y;
        \\}
        \\
        \\func antidiff(x, y) {
        \\  let res = %near diff(y, x);
        \\  return res;
        \\}
    );
    defer res.deinit_mem();

    const fun1 = try res.get_func_ptr("antidiff", BFunc);
    try expect(usize, 30, fun1(70, 100));

    const fun2 = try res.get_func_ptr("diff", BFunc);
    try expect(usize, 40, fun2(50, 10));
}

test "store byte" {
    var cfo = try parse_test(
        \\func storer(x, y, z) {
        \\  x[y] = z;
        \\  return 0;
        \\}
    );
    defer cfo.deinit();

    const FFunc = *const fn (arg1: [*]u8, arg2: usize, val: usize) callconv(.C) usize;
    const fun = cfo.get_ptr(0, FFunc);
    var bytes: [4]u8 = .{ 17, 43, 6, 19 };
    try expect(usize, 0, fun(&bytes, 1, 4));
    try expect([4]u8, .{ 17, 4, 6, 19 }, bytes);
}
test "store loop" {
    var cfo = try parse_test(
        \\func scripter(res, len) {
        \\  vars i, sum;
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

test "vopper" {
    // z arg lite fånigt but what is tested
    var cfo = try parse_test(
        \\func returner(x, y, z) {
        \\  let xa 1d= @x[z];
        \\  let ya 1d= @y[z];
        \\  x[z] 1d= xa + ya;
        \\  return 0;
        \\}
    );
    defer cfo.deinit();
    var x: f64 = 28.0;
    var y: f64 = 2.75;

    const FFunc = *const fn (arg1: *f64, arg2: *f64, yark: usize) callconv(.C) usize;
    const fun = cfo.get_ptr(0, FFunc);
    try expect(usize, 0, fun(&x, &y, 0));
    try expect(f64, 30.75, x);
    try expect(usize, 0, fun(&x, &y, 0));
    try expect(f64, 33.5, x);
}

test "float square array" {
    var cfo = try parse_test(
        \\func scripter(data, len) {
        \\  vars i;
        \\  i := 0;
        \\  loop {
        \\    if (i >= len) break;
        \\    let val 1d= @data[i ,8]; // TODO addr[idx*8] is a good candidate of a first opt pass...
        \\    let square 1d= val * val;
        \\    data[i, 8] 1d= square;
        \\    i := i + 1;
        \\  }
        \\  return 0;
        \\}
    );
    defer cfo.deinit();

    const func = cfo.get_ptr(0, *const fn (arg1: [*]f64, arg2: usize) callconv(.C) usize);
    var data = [_]f64{ 1.0, 3.0, 0.5, 10.0 };
    try expect(usize, 0, func(&data, data.len));
    try expect([4]f64, .{ 1.0, 9.0, 0.25, 100.0 }, data);
}

test "int2float" {
    var cfo = try parse_test(
        \\func returner(x, y, z) {
        \\  let val 1d= ~x / ~y;
        \\  z[0] 1d= val;
        \\  return 0;
        \\}
    );
    defer cfo.deinit();

    var z: f64 = undefined;
    const FFunc = *const fn (x: usize, y: usize, z: *f64) callconv(.C) usize;
    const fun = cfo.get_ptr(0, FFunc);
    try expect(usize, 0, fun(3, 4, &z));
    try expect(f64, 0.75, z);

    try expect(usize, 0, fun(9, 2, &z));
    try expect(f64, 4.5, z);
}

test "float2int" {
    var cfo = try parse_test(
        \\func returner(x) {
        \\  let xa 1d= @x[0];
        \\  let res 1d= xa*xa;
        \\  return #res;
        \\}
    );
    defer cfo.deinit();

    const FFunc = *const fn (z: *const f64) callconv(.C) usize;
    const fun = cfo.get_ptr(0, FFunc);
    try expect(usize, 23, fun(&4.8));
}

test "float variable" {
    if (true) return error.SkipZigTest;
    var cfo = try parse_test(
        \\func scripter(lim) {
        \\  vars i, sum: 1d;
        \\  i := 0;
        \\  sum :1d= 0;
        \\  let flim 1d= $lim;
        \\  loop {
        \\    i := i + 1;
        \\    let incr 1d= 1 / ~i;
        \\    sum :1d= sum + incr;
        \\    if 1d(sum > flim) break;
        \\  }
        \\  return i;
        \\}
    );
    defer cfo.deinit();

    const func = cfo.get_ptr(0, *const fn (arg: usize) callconv(.C) usize);
    try expect(usize, 4, func(2));
}

test "syscall" {
    var cfo = try parse_test(
        \\func returner(x) {
        \\  let y = $exit(x);
        \\  return y;
        \\}
    );

    defer cfo.deinit();
    const fun = cfo.get_ptr(0, AFunc);
    const pid = try os.fork();
    if (pid > 0) {
        const status = os.waitpid(pid, 0);
        try expect(usize, 11 * 256, status.status);
    } else {
        _ = fun(11);
        @panic("exit syscall failed");
    }
}
