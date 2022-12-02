const testutil = @import("./TestFLIR.zig");
const parse_test = testutil.parse_test;
const expect = testutil.expect;

const SFunc = *const fn (arg1: [*]const u8, arg2: usize) callconv(.C) usize;

fn s_call(func: SFunc, str: []const u8) usize {
    return func(str.ptr, str.len);
}

test "parse digit" {
    var cfo = try parse_test(
        \\func parser
        \\  var %num
        \\  %x = arg
        \\  %num := 127
        \\  %byte = load byte [%x 0]
        \\  %token = sub %byte 48
        \\  ja %token 9 :enda
        \\:doit
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
