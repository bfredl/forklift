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
