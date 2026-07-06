const testutil = @import("./flir.zig");
const parse_multi = testutil.parse_multi;
const expect = testutil.expect;
const std = @import("std");

const TFunc = *const fn (arg1: [*]const u8, arg2: usize, arg3: [*]const u8, arg4: usize) callconv(.c) usize;
fn t_call(func: TFunc, str: []const u8, tab: []const u8) usize {
    return func(str.ptr, str.len, tab.ptr, tab.len);
}

const table =
    \\zero
    \\one
    \\two
    \\three
    \\four
    \\five
    \\six
    \\seven
    \\eight
    \\nine
    \\
;
test "register pressure (aoc 2023 1a)" {
    var res = try parse_multi(
        \\func main(data, len, table, tablelen) {
        \\  vars x, tpos, i, imatch, scan;
        \\  vars first, current;
        \\  x = 0;
        \\  first = 200;
        \\  current = 0;
        \\  loop {
        \\    if (x >= len) break;
        \\    let byteval = @data[x];
        \\    let trydigit = byteval - '0';
        \\    if (trydigit <|= 9) {
        \\      current = trydigit;
        \\      if (first >= 10) {
        \\        first = trydigit;
        \\      }
        \\    } else {
        \\      tpos = 0;
        \\      i = 0;
        \\      loop {
        \\        scan = x;
        \\        imatch = 1;
        \\        loop {
        \\          let tval = @table[tpos];
        \\          tpos = tpos + 1;
        \\          if (tval == 10) break;
        \\          if (scan < len) {
        \\            if (tval != @data[scan]) {
        \\              imatch = 0;
        \\            }
        \\          } else {
        \\              imatch = 0;
        \\          }
        \\          scan = scan + 1;
        \\        }
        \\        if (imatch == 1) {
        \\          current = i;
        \\          if (first >= 10) {
        \\            first = i;
        \\          }
        \\          break;
        \\        }
        \\        if (tpos >= tablelen) break;
        \\        i = i + 1;
        \\      }
        \\    }
        \\    x = x + 1;
        \\  }
        \\  let item = 10*first+current;
        \\  return item;
        \\}
    , .{ .max_ipreg_use = 12 });
    defer res.deinit_mem();
    const func = try res.get_func_ptr("main", TFunc);
    try expect(usize, 30, t_call(func, "three0", table));
    try expect(usize, 73, t_call(func, "ba7fu3xze", table));
    try expect(usize, 1, t_call(func, "zerone", table));
    try expect(usize, 43, t_call(func, "refourcetwog3re", table));
    try expect(usize, 2000, t_call(func, "zer", table));
    try expect(usize, 2000, t_call(func, "", table));
}
