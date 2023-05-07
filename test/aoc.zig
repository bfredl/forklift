const testutil = @import("./flir.zig");
const parse_test = testutil.parse_test;
const expect = testutil.expect;

const SFunc = *const fn (arg1: [*]const u8, arg2: usize) callconv(.C) usize;

fn s_call(func: SFunc, str: []const u8) usize {
    return func(str.ptr, str.len);
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
test "scorer" {
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
