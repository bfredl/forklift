const std = @import("std");
const forklift = @import("forklift");

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

const ir =
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
;

pub fn main(init: std.process.Init) !void {
    var mod = try forklift.CFOModule.init(init.gpa);
    defer mod.deinit_mem();
    try forklift.parse_mod(&mod, init.gpa, ir, .{
        .max_ipreg_use = 12,
        .dbg_regalloc = true,
    });
    try mod.code.finalize();
    const func = try mod.get_func_ptr("main", TFunc);
    _ = func;
}
