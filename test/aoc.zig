const testutil = @import("./flir.zig");
const parse_test = testutil.parse_test;
const expect = testutil.expect;
const std = @import("std");

const SFunc = *const fn (arg1: [*]const u8, arg2: usize) callconv(.C) usize;
const TFunc = *const fn (arg1: [*]const u8, arg2: usize, arg3: [*]const u8, arg4: usize) callconv(.C) usize;

fn s_call(func: SFunc, str: []const u8) usize {
    return func(str.ptr, str.len);
}

fn t_call(func: TFunc, str: []const u8, tab: []const u8) usize {
    return func(str.ptr, str.len, tab.ptr, tab.len);
}
// aoc 2022 1

test "parse digit" {
    var cfo = try parse_test(
        \\func parser(x) {
        \\  vars num;
        \\  num := 127;
        \\  let byte = @x[0];
        \\  let token = byte - 48;
        \\  if (token <| 9) {
        \\    num := token;
        \\  }
        \\  return num;
        \\}
    );
    defer cfo.deinit();

    const func = cfo.get_ptr(0, SFunc);

    try expect(usize, 5, s_call(func, "56 428"));
    try expect(usize, 127, s_call(func, "x4127"));
}

test "parse int" {
    var cfo = try parse_test(
        \\func parser(x, len) {
        \\  vars num, ipos;
        \\  ipos := 0;
        \\  num := 0;
        \\  loop {
        \\    let byte = @x[ipos];
        \\    let token = byte - 48;
        \\    if (token |> 9) break;
        \\    num := token + (num*10);
        \\    ipos := ipos + 1;
        \\  }
        \\  return num;
        \\}
    );
    defer cfo.deinit();

    const func = cfo.get_ptr(0, SFunc);

    try expect(usize, 56, s_call(func, "56 428"));
    try expect(usize, 4127, s_call(func, "4127"));
}

test "summer 1" {
    // %ipos >= $len check is useless, but harmless
    var cfo = try parse_test(
        \\func returner(x, len) {
        \\  vars sum, item, ipos;
        \\  ipos := 0;
        \\  sum := 0;
        \\  loop {
        \\    if (ipos > len) break;
        \\    item := 0;
        \\    loop {
        \\      let byte = @x[ipos];
        \\      let token = byte - 48;
        \\      if (token |> 9) break;
        \\      item := token + (item*10);
        \\      ipos := ipos + 1;
        \\    }
        \\    sum := sum + item;
        \\    ipos := ipos + 1;
        \\  }
        \\  return sum;
        \\}
    );
    defer cfo.deinit();

    const func = cfo.get_ptr(0, SFunc);
    try expect(usize, 2480, s_call(func, "56 428  1996"));
    try expect(usize, 4127, s_call(func, "4127"));
    // does not yet work
    if (false) try expect(usize, 412, s_call(func, "4127"[0..3]));
}

test "summer 2" {
    // %ipos >= $len check is useless, but harmless
    var cfo = try parse_test(
        \\func returner(x, len) {
        \\  vars sum, item, ipos;
        \\  ipos := 0;
        \\  sum := 0;
        \\  loop {
        \\    item := 0;
        \\    if (ipos >= len) break;
        \\    let pretoken = @ x[ipos] - 48;
        \\    if (pretoken |> 9) break;
        \\    loop {
        \\      let token = @ x[ipos] - 48;
        \\      if (token |> 9) break;
        \\      item := (item*10) + token;
        \\      ipos := ipos + 1;
        \\    }
        \\    sum := sum + item;
        \\    ipos := ipos + 1;
        \\  }
        \\  return sum;
        \\}
    );
    defer cfo.deinit();

    const func = cfo.get_ptr(0, SFunc);
    try expect(usize, 484, s_call(func, "56 428  1996"));
    try expect(usize, 4127, s_call(func, "4127"));
    try expect(usize, 56, s_call(func, "56 428"[0..2]));
    try expect(usize, 56, s_call(func, "56 428"[0..3]));
    // does not yet work
    if (false) try expect(usize, 60, s_call(func, "56 428"[0..4]));
    if (false) try expect(usize, 412, s_call(func, "4127"[0..3]));
}

test "summer/maxxer 3" {
    var cfo = try parse_test(
        \\func returner(x, len) {
        \\  vars sum, item, ipos, max;
        \\  ipos := 0;
        \\  max := 0;
        \\  loop {
        \\    sum := 0;
        \\    if (ipos >= len) break;
        \\    loop {
        \\      item := 0;
        \\      if (ipos >= len) break;
        \\      let pretoken = @ x[ipos] - 48;
        \\      if (pretoken |> 9) break;
        \\      loop {
        \\        let token = @ x[ipos] - 48;
        \\        if (token |> 9) break;
        \\        item := (item*10) + token;
        \\        ipos := ipos + 1;
        \\        if (ipos >= len) break;
        \\      }
        \\      sum := sum + item;
        \\      ipos := ipos + 1;
        \\    }
        \\    ipos := ipos + 1;
        \\    if (sum > max) {
        \\      max := sum+0;
        \\    }
        \\  }
        \\  return max;
        \\}
    );
    defer cfo.deinit();

    const func = cfo.get_ptr(0, SFunc);
    try expect(usize, 1996, s_call(func, "56 428  1996"));
    try expect(usize, 4127, s_call(func, "4127"));
    try expect(usize, 56, s_call(func, "56 428"[0..2]));
    try expect(usize, 56, s_call(func, "56 428"[0..3]));
    try expect(usize, 60, s_call(func, "56 428"[0..4]));
    try expect(usize, 412, s_call(func, "4127"[0..3]));
    try expect(usize, 150, s_call(func, "12 13 20\n\n40 50 60\n\n20 7 8"));
}

// aoc 2022 2
test "scorer" {
    var cfo = try parse_test(
        \\func scorer(data, len) {
        \\  vars ipos, summa;
        \\  let table = %alloc 2;
        \\  table[0] = 4;
        \\  table[1] = 8;
        \\  table[2] = 3;
        \\  table[3] = 1;
        \\  table[4] = 5;
        \\  table[5] = 9;
        \\  table[6] = 7;
        \\  table[7] = 2;
        \\  table[8] = 6;
        \\  ipos := 0;
        \\  summa := 0;
        \\  loop {
        \\    if (ipos >= len) break;
        \\    let abyte = @data[ipos];
        \\    let apos = abyte - 65;
        \\    ipos := ipos + 2;
        \\    let xbyte = @data[ipos];
        \\    let xpos = xbyte - 88;
        \\    ipos := ipos + 2;
        \\    let index = (apos * 3) + xpos;
        \\    summa := summa + @table[index];
        \\  }
        \\  return summa;
        \\}
    );
    defer cfo.deinit();
    const func = cfo.get_ptr(0, SFunc);
    try expect(usize, 15, s_call(func, "A Y\nB X\nC Z\n"));
    try expect(usize, 20, s_call(func, "B Y,A Z,A Z,B Y,A Z,B X,"));
    try expect(usize, 45, s_call(func, "A X,A Y,A Z,B X,B Y,B Z,C X,C Y,C Z,"));
}

// aoc 2022 3
test "scorer 3" {
    var cfo = try parse_test(
        \\func finder(x, len) {
        \\  vars rowlen, ipos;
        \\  ipos := 0;
        \\  rowlen := 0;
        \\  loop {
        \\    let char = @ x[ipos];
        \\    if (char == 10) break;
        \\    ipos := ipos + 1;
        \\    rowlen := rowlen + 1;
        \\  }
        \\  let halflen = rowlen >> 1;
        \\  return halflen;
        \\}
    );
    defer cfo.deinit();
    const func = cfo.get_ptr(0, SFunc);
    try expect(usize, 12, s_call(func, "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRN"));
}

test "score pairs" {
    var cfo = try parse_test(
        \\func finder(data, len) {
        \\  vars rowlen, ipos, xpos, ypos, retval;
        \\  ipos := 0;
        \\  rowlen := 0;
        \\  loop {
        \\    let char = @ data[rowlen];
        \\    if (char == 10) break;
        \\    rowlen := rowlen + 1;
        \\  }
        \\  let halflen = rowlen >> 1;
        \\  xpos := 0;
        \\  loop {
        \\    if (xpos >= halflen) {
        \\      retval := 5000;
        \\      break;
        \\    }
        \\    let xchar = @ data[xpos];
        \\    ypos := halflen;
        \\    loop {
        \\     let ychar = @ data[ypos];
        \\     if (ychar == xchar) {
        \\       retval := xchar;
        \\       break 2;
        \\     }
        \\     ypos := ypos + 1;
        \\     if (ypos >= rowlen) break;
        \\    }
        \\    xpos := xpos + 1;
        \\  }
        \\  return retval;
        \\}
    );
    defer cfo.deinit();
    const func = cfo.get_ptr(0, SFunc);
    try expect(usize, 'p', s_call(func, "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRN"));
    try expect(usize, 5000, s_call(func, "vJrwxWtwJgWrhcsFMMfFFhFp\njqHRN"));
}

test "aoc 2023 day 1 part one" {
    var cfo = try parse_test(
        \\func main(data, len) {
        \\  vars x, first, current, sum;
        \\  x := 0;
        \\  sum := 0;
        \\  loop {
        \\    if (x >= len) break;
        \\    first := 10;
        \\    current := 0; // dummy init
        \\    loop {
        \\      if (x >= len) break;
        \\      let byteval = @data[x];
        \\      let trydigit = byteval - '0';
        \\      if (trydigit <|= 9) {
        \\        current := trydigit;
        \\        if (first >= 10) {
        \\          first := trydigit;
        \\        }
        \\      }
        \\      x := x + 1;
        \\      if (byteval == 10) break;
        \\    }
        \\    let item = 10*first+current;
        \\    sum := sum + item;
        \\  }
        \\  return sum;
        \\}
    );
    defer cfo.deinit();
    const func = cfo.get_ptr(0, SFunc);
    try expect(usize, 142, s_call(func, "1abc2\n pqr3stu8vwx\n a1b2c3d4e5f\ntreb7uchet\n"));
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

test "numberfinderer borked" {
    // case where the OOB check is slightly wrong so that "ze" maps to zero and so on
    var cfo = try parse_test(
        \\func main(data, len, table, tablelen) {
        \\  vars x, tpos, i, imatch;
        \\  vars scan, result;
        \\  tpos := 0;
        \\  i := 0;
        \\  result := 200;
        \\  loop {
        \\    scan := 0;
        \\    imatch := 1;
        \\    loop {
        \\      let tval = @table[tpos];
        \\      tpos := tpos + 1;
        \\      if (tval == 10) break;
        \\      if (scan < len) {
        \\        if (tval != @data[scan]) {
        \\          imatch := 0;
        \\        }
        \\      }
        \\      scan := scan + 1;
        \\    }
        \\    if (imatch == 1) {
        \\      result := i;
        \\    }
        \\    if (tpos >= tablelen) break;
        \\    i := i + 1;
        \\  }
        \\  return result;
        \\}
    );
    defer cfo.deinit();
    const func = cfo.get_ptr(0, TFunc);
    try expect(usize, 0, t_call(func, "zero\n", table));
    try expect(usize, 3, t_call(func, "three\n", table));
    try expect(usize, 0, t_call(func, "zero", table));
    try expect(usize, 3, t_call(func, "three", table));
    try expect(usize, 0, t_call(func, "ze", table));
    try expect(usize, 3, t_call(func, "thr", table));
    try expect(usize, 200, t_call(func, "q", table));
}

test "numberfinderer" {
    // case where the OOB check is slightly wrong so that "ze" maps to zero and so on
    var cfo = try parse_test(
        \\func main(data, len, table, tablelen) {
        \\  vars x, tpos, i, imatch;
        \\  vars scan, result;
        \\  tpos := 0;
        \\  i := 0;
        \\  result := 200;
        \\  loop {
        \\    scan := 0;
        \\    imatch := 1;
        \\    loop {
        \\      let tval = @table[tpos];
        \\      tpos := tpos + 1;
        \\      if (tval == 10) break;
        \\      if (scan < len) {
        \\        if (tval != @data[scan]) {
        \\          imatch := 0;
        \\        }
        \\      } else {
        \\          imatch := 0;
        \\      }
        \\      scan := scan + 1;
        \\    }
        \\    if (imatch == 1) {
        \\      result := i;
        \\      break;
        \\    }
        \\    if (tpos >= tablelen) break;
        \\    i := i + 1;
        \\  }
        \\  return result;
        \\}
    );
    defer cfo.deinit();
    const func = cfo.get_ptr(0, TFunc);
    try expect(usize, 0, t_call(func, "zero\n", table));
    try expect(usize, 3, t_call(func, "three\n", table));
    try expect(usize, 0, t_call(func, "zero", table));
    try expect(usize, 3, t_call(func, "three", table));
    try expect(usize, 200, t_call(func, "ze", table));
    try expect(usize, 200, t_call(func, "thr", table));
    try expect(usize, 200, t_call(func, "q", table));
}

test "aoc 2023 1a" {
    var cfo = try parse_test(
        \\func main(data, len, table, tablelen) {
        \\  vars x, tpos, i, imatch, scan;
        \\  vars first, current;
        \\  x := 0;
        \\  first := 200;
        \\  current := 0;
        \\  loop {
        \\    if (x >= len) break;
        \\    let byteval = @data[x];
        \\    let trydigit = byteval - '0';
        \\    if (trydigit <|= 9) {
        \\      current := trydigit;
        \\      if (first >= 10) {
        \\        first := trydigit;
        \\      }
        \\    } else {
        \\      tpos := 0;
        \\      i := 0;
        \\      loop {
        \\        scan := x;
        \\        imatch := 1;
        \\        loop {
        \\          let tval = @table[tpos];
        \\          tpos := tpos + 1;
        \\          if (tval == 10) break;
        \\          if (scan < len) {
        \\            if (tval != @data[scan]) {
        \\              imatch := 0;
        \\            }
        \\          } else {
        \\              imatch := 0;
        \\          }
        \\          scan := scan + 1;
        \\        }
        \\        if (imatch == 1) {
        \\          current := i;
        \\          if (first >= 10) {
        \\            first := i;
        \\          }
        \\          break;
        \\        }
        \\        if (tpos >= tablelen) break;
        \\        i := i + 1;
        \\      }
        \\    }
        \\    x := x + 1;
        \\  }
        \\  let item = 10*first+current;
        \\  return item;
        \\}
    );
    defer cfo.deinit();
    const func = cfo.get_ptr(0, TFunc);
    try expect(usize, 30, t_call(func, "three0", table));
    try expect(usize, 73, t_call(func, "ba7fu3xze", table));
    try expect(usize, 1, t_call(func, "zerone", table));
    try expect(usize, 43, t_call(func, "refourcetwog3re", table));
    try expect(usize, 2000, t_call(func, "zer", table));
    try expect(usize, 2000, t_call(func, "", table));
}

// TODO: not complete, handles only values up to 63!
test "aoc 2023 4a: one row" {
    var cfo = try parse_test(
        \\func main(data, len) {
        \\  vars x, setlow, sethigh;
        \\  vars char, value, count;
        \\  x := 0;
        \\  setlow := 0;
        \\  loop {
        \\    let kolla = @data[x];
        \\    x := x + 1;
        \\    if (kolla == ':') break;
        \\  }
        \\
        \\  loop {
        \\    loop {
        \\      char := @data[x];
        \\      if (char != ' ') break;
        \\      x := x + 1;
        \\    }
        \\
        \\    if (char == '|') break;
        \\    let digit = char - '0';
        \\    value := digit;
        \\    x := x + 1;
        \\    let maybedigit = @data[x] - '0';
        \\    if (maybedigit <|= 9) {
        \\      value := 10*value + maybedigit;
        \\      x := x + 1;
        \\    }
        \\
        \\    let increment = 1 << value;
        \\    setlow := setlow | increment;
        \\  }
        \\
        \\  count := 0;
        \\  loop {
        \\    loop {
        \\      char := @data[x];
        \\      if (char != ' ') break;
        \\      x := x + 1;
        \\    }
        \\
        \\    if (char == '
        \\') break;
        \\    let digit = char - '0';
        \\    value := digit;
        \\    x := x + 1;
        \\    let maybedigit = @data[x] - '0';
        \\    if (maybedigit <|= 9) {
        \\      value := 10*value + maybedigit;
        \\      x := x + 1;
        \\    }
        \\
        \\    let check = 1 << value;
        \\    if (setlow & check != 0) {
        \\      count := count + 1;
        \\    }
        \\  }
        \\
        \\  return count;
        \\}
    );
    defer cfo.deinit();
    const func = cfo.get_ptr(0, SFunc);
    try expect(usize, 2, s_call(func, "Card 1: 1 3 8 | 3 2 8\n"));
    try expect(usize, 4, s_call(func, "Card 1: 41 48 19 22 17 | 19 22  6 31 17  9 48 53\n"));
    try expect(usize, 5, s_call(func, "Card 1: 41 48 19 22 17 | 19 22  6 31 17 19 48 53\n"));
}
