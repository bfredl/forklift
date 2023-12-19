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
        \\func parser
        \\  var %num
        \\  %x = arg
        \\  %num := 127
        \\  %byte = load byte [%x 0]
        \\  %token = sub %byte 48
        \\  ja %token 9 :enda
        \\:
        \\  %num := %token
        \\:enda
        \\  ret %num
        \\end
    );
    defer cfo.deinit();

    const func = cfo.get_ptr(0, SFunc);

    try expect(usize, 5, s_call(func, "56 428"));
    try expect(usize, 127, s_call(func, "x4127"));
}

test "parse int" {
    var cfo = try parse_test(
        \\func parser
        \\  var %num
        \\  var %ipos
        \\  %x = arg
        \\  %len = arg
        \\  %ipos := 0
        \\  %num := 0
        \\:loop
        \\  %byte = load byte [%x %ipos]
        \\  %token = sub %byte 48
        \\  ja %token 9 :enda
        \\:doit
        \\  %base = 10
        \\  %adjust = mul %num %base
        \\  %num := add %adjust %token
        \\  %ipos := add %ipos 1
        \\  jmp :loop
        \\:enda
        \\  ret %num
        \\end
    );
    defer cfo.deinit();

    const func = cfo.get_ptr(0, SFunc);

    try expect(usize, 56, s_call(func, "56 428"));
    try expect(usize, 4127, s_call(func, "4127"));
}

test "summer 1" {
    // %ipos >= $len check is useless, but harmless
    var cfo = try parse_test(
        \\func returner
        \\  var %sum
        \\  var %item
        \\  var %ipos
        \\  %x = arg
        \\  %len = arg
        \\  %ipos := 0
        \\  %sum := 0
        \\:sumloop
        \\  %item := 0
        \\  jge %ipos %len :retvrn
        \\:parseloop
        \\  %byte = load byte [%x %ipos]
        \\  %token = sub %byte 48
        \\  ja %token 9 :enda
        \\:doit
        \\  %base = 10
        \\  %adjust = mul %item %base
        \\  %item := add %adjust %token
        \\  %ipos := add %ipos 1
        \\  jmp :parseloop
        \\:enda
        \\  %sum := add %sum %item
        \\  %ipos := add %ipos 1
        \\  jmp :sumloop
        \\:retvrn
        \\  ret %sum
        \\end
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
        \\func returner
        \\  var %sum
        \\  var %item
        \\  var %ipos
        \\  %x = arg
        \\  %len = arg
        \\  %ipos := 0
        \\  %sum := 0
        \\:sumloop
        \\  %item := 0
        \\  jge %ipos %len :retvrn
        \\:check
        \\  %prebyte = load byte [%x %ipos]
        \\  %pretoken = sub %prebyte 48
        \\  ja %pretoken 9 :retvrn
        \\:parseloop
        \\  %byte = load byte [%x %ipos]
        \\  %token = sub %byte 48
        \\  ja %token 9 :enda
        \\:doit
        \\  %base = 10
        \\  %adjust = mul %item %base
        \\  %item := add %adjust %token
        \\  %ipos := add %ipos 1
        \\  jmp :parseloop
        \\:enda
        \\  %sum := add %sum %item
        \\  %ipos := add %ipos 1
        \\  jmp :sumloop
        \\:retvrn
        \\  ret %sum
        \\end
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
    // %ipos >= $len check is useless, but harmless
    var cfo = try parse_test(
        \\func returner
        \\  var %sum
        \\  var %item
        \\  var %ipos
        \\  var %max
        \\  %x = arg
        \\  %len = arg
        \\  %ipos := 0
        \\  %max := 0
        \\:maxloop
        \\  %sum := 0
        \\  jge %ipos %len :retvrn
        \\:sumloop
        \\  %item := 0
        \\  jge %ipos %len :enda_sum
        \\:check
        \\  %prebyte = load byte [%x %ipos]
        \\  %pretoken = sub %prebyte 48
        \\  ja %pretoken 9 :enda_sum
        \\:parseloop
        \\  %byte = load byte [%x %ipos]
        \\  %token = sub %byte 48
        \\  ja %token 9 :enda
        \\:doit
        \\  %base = 10
        \\  %adjust = mul %item %base
        \\  %item := add %adjust %token
        \\  %ipos := add %ipos 1
        \\  jl %ipos %len :parseloop
        \\:enda
        \\  %sum := add %sum %item
        \\  %ipos := add %ipos 1
        \\  jmp :sumloop
        \\:enda_sum
        \\  %ipos := add %ipos 1
        \\  jge %max %sum :maxloop
        \\:
        \\  %max := add %sum 0
        \\  jmp :maxloop
        \\:retvrn
        \\  ret %max
        \\end
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
        \\func scorer
        \\  var %ipos
        \\  var %summa
        \\  %data = arg
        \\  %len = arg
        \\  %table = alloc 2
        \\  store byte [%table 0] 4
        \\  store byte [%table 1] 8
        \\  store byte [%table 2] 3
        \\  store byte [%table 3] 1
        \\  store byte [%table 4] 5
        \\  store byte [%table 5] 9
        \\  store byte [%table 6] 7
        \\  store byte [%table 7] 2
        \\  store byte [%table 8] 6
        \\  %ipos := 0
        \\  %summa := 0
        \\:loop
        \\  jge %ipos %len :enda
        \\:doit
        \\  %abyte = load byte [%data %ipos]
        \\  %apos = sub %abyte 65
        \\  %ipos := add %ipos 2
        \\  %xbyte = load byte [%data %ipos]
        \\  %xpos = sub %xbyte 88
        \\  %ipos := add %ipos 2
        \\  %scaled = mul %apos 3
        \\  %index = add %scaled %xpos
        \\  %tabval = load byte [%table %index]
        \\  %summa := add %summa %tabval
        \\  jmp :loop
        \\:enda
        \\  ret %summa
        \\end
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
        \\func finder
        \\  var %rowlen
        \\  var %ipos
        \\  %x = arg
        \\  %len = arg
        \\  %ipos := 0
        \\  %rowlen := 0
        \\:loopen
        \\  %char = load byte [%x %ipos]
        \\  je %char 10 :foundlen
        \\:
        \\  %ipos := add %ipos 1
        \\  %rowlen := add %rowlen 1
        \\  jmp :loopen
        \\:foundlen
        \\  %halflen = shr %rowlen 1
        \\  ret %halflen
        \\end
    );
    defer cfo.deinit();
    const func = cfo.get_ptr(0, SFunc);
    try expect(usize, 12, s_call(func, "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRN"));
}

test "score pairs" {
    var cfo = try parse_test(
        \\func finder
        \\  var %rowlen
        \\  var %ipos
        \\  var %xpos
        \\  var %ypos
        \\  var %retval
        \\  %data = arg
        \\  %len = arg
        \\  %ipos := 0
        \\  %rowlen := 0
        \\:loopen
        \\  %char = load byte [%data %rowlen]
        \\  je %char 10 :foundlen
        \\:
        \\  %rowlen := add %rowlen 1
        \\  jmp :loopen
        \\:foundlen
        \\  %halflen = shr %rowlen 1
        \\  %xpos := 0
        \\:xloop
        \\  jge %xpos %halflen :feeel
        \\:
        \\  %xchar = load byte [%data %xpos]
        \\  %ypos := %halflen
        \\:yloop
        \\  %ychar = load byte [%data %ypos]
        \\  je %xchar %ychar :good
        \\:
        \\  %ypos := add %ypos 1
        \\  jl %ypos %rowlen :yloop
        \\:
        \\  %xpos := add %xpos 1
        \\  jmp :xloop
        \\:feeel
        \\  %retval := 5000
        \\  jmp :enda
        \\:good
        \\  %retval := %xchar
        \\:enda
        \\  ret %retval
        \\end
    );
    defer cfo.deinit();
    const func = cfo.get_ptr(0, SFunc);
    try expect(usize, 'p', s_call(func, "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRN"));
    try expect(usize, 5000, s_call(func, "vJrwxWtwJgWrhcsFMMfFFhFp\njqHRN"));
}

test "aoc 2023 day 1 part one" {
    var cfo = try parse_test(
        \\func main {
        \\  args data len;
        \\  vars x first current sum;
        \\  x := 0;
        \\  sum := 0;
        \\  loop {
        \\    if (x >= len) break;
        \\    first := 10;
        \\    current := 0; // dummy init
        \\    loop {
        \\      if (x >= len) break;
        \\      let byteval = data[x];
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
        \\func main {
        \\  args data len table tablelen;
        \\  vars x tpos i imatch scan result;
        \\  tpos := 0;
        \\  i := 0;
        \\  result := 200;
        \\  loop {
        \\    scan := 0;
        \\    imatch := 1;
        \\    loop {
        \\      let tval = table[tpos];
        \\      tpos := tpos + 1;
        \\      if (tval == 10) break;
        \\      if (scan < len) {
        \\        if (tval != data[scan]) {
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
        \\func main {
        \\  args data len table tablelen;
        \\  vars x tpos i imatch scan result;
        \\  tpos := 0;
        \\  i := 0;
        \\  result := 200;
        \\  loop {
        \\    scan := 0;
        \\    imatch := 1;
        \\    loop {
        \\      let tval = table[tpos];
        \\      tpos := tpos + 1;
        \\      if (tval == 10) break;
        \\      if (scan < len) {
        \\        if (tval != data[scan]) {
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
        \\func main {
        \\  args data len table tablelen;
        \\  vars x tpos i imatch scan first current;
        \\  x := 0;
        \\  first := 200;
        \\  current := 0;
        \\  loop {
        \\    if (x >= len) break;
        \\    let byteval = data[x];
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
        \\          let tval = table[tpos];
        \\          tpos := tpos + 1;
        \\          if (tval == 10) break;
        \\          if (scan < len) {
        \\            if (tval != data[scan]) {
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
        \\func main {
        \\  args data len;
        \\  vars x setlow sethigh char value count;
        \\  x := 0;
        \\  setlow := 0;
        \\  loop {
        \\    let kolla = data[x];
        \\    x := x + 1;
        \\    if (kolla == ':') break;
        \\  }
        \\
        \\  loop {
        \\    loop {
        \\      char := data[x];
        \\      if (char != ' ') break;
        \\      x := x + 1;
        \\    }
        \\
        \\    if (char == '|') break;
        \\    let digit = char - '0';
        \\    value := digit;
        \\    x := x + 1;
        \\    let maybedigit = data[x] - '0';
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
        \\      char := data[x];
        \\      if (char != ' ') break;
        \\      x := x + 1;
        \\    }
        \\
        \\    if (char == '
        \\') break;
        \\    let digit = char - '0';
        \\    value := digit;
        \\    x := x + 1;
        \\    let maybedigit = data[x] - '0';
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
