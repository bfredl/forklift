const std = @import("std");
const dbg = std.debug.print;
const util = @import("./util.zig");

const wasm_shelf = @import("wasm_shelf");
const StackValue = wasm_shelf.StackValue;
const clap = @import("clap");

pub var options: @import("wasm_shelf").forklift.DebugOptions = .{};

const params_def = clap.parseParamsComptime(
    \\-h, --help             Display this help and exit.
    \\-f, --specname <str>   name
    \\-e, --errors <str>     expected number of errors
    \\-m, --heavy            Compile entire module using HeavyMachineTool
    \\--stdin <str>          override wasi stdin
    \\<str>
    \\
);
const ConstKind = enum { @"i32.const", @"i64.const", @"f32.const", @"f64.const", @"ref.null", @"ref.extern" };
pub fn main() !u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var diag = clap.Diagnostic{};
    const p = clap.parse(clap.Help, &params_def, clap.parsers.default, .{
        .diagnostic = &diag,
        .allocator = gpa.allocator(),
    }) catch |err| {
        // Report useful error and exit.
        try diag.reportToFile(.stderr(), err);
        return err;
    };

    const filearg = p.positionals[0] orelse @panic("usage");
    const buf = try util.readall(allocator, filearg);
    defer allocator.free(buf);

    var maxerr: u32 = 0;
    const specname = p.args.specname;

    const machine_tool: bool = p.args.heavy > 0;

    var interpreter: wasm_shelf.Interpreter = .init(allocator);
    defer interpreter.deinit();

    var tool: wasm_shelf.HeavyMachineTool = try .init(allocator);

    const engine: wasm_shelf.Engine = if (machine_tool) .{ .heavy = &tool } else .{ .interpreter = &interpreter };

    if (p.args.errors) |errarg| {
        maxerr = try std.fmt.parseInt(@TypeOf(maxerr), errarg, 10);
    }

    var t: Tokenizer = .{ .str = buf };
    errdefer t.fail_pos();

    var did_mod = false;
    // mod is "almost always" available
    var mod: wasm_shelf.Module = undefined;
    var in: wasm_shelf.Instance = undefined;

    var imports: wasm_shelf.ImportTable = .init(allocator);
    defer imports.deinit();

    var i32val: StackValue = .{ .i32 = 666 };
    var i64val: StackValue = .{ .i64 = 666 };
    try imports.add_global("global_i32", &i32val, .i32);
    try imports.add_global("global_i64", &i64val, .i64);

    try imports.add_func("modadder", .{ .cb = &modadder, .data = undefined, .n_args = 2, .n_res = 1 });

    defer if (did_mod) {
        in.deinit();
        mod.deinit();
    };

    var cases: u32 = 0;
    var failures: u32 = 0;
    var unapplicable: u32 = 0;

    var params: std.ArrayList(StackValue) = .empty;

    const Toplevel = enum { module, invoke, assert_return, assert_trap, assert_invalid, assert_malformed, assert_exhaustion };

    if (specname) |nam| dbg("time to {s}: \n", .{nam});

    while (t.nonws()) |_| {
        dbg("\rtest at {}:", .{t.lnum + 1});

        const start_pos = t.pos;
        _ = try t.expect(.LeftParen);
        const kind = try t.expectAtomChoice(Toplevel);
        if (kind == .module) {
            if (did_mod) {
                in.deinit();
                mod.deinit();
            }
            try t.skip(1);

            const mod_source = buf[start_pos..t.pos];
            const mod_code = try wat2wasm(mod_source, allocator);

            mod = try .parse(mod_code, allocator);
            in = try .init(&mod, &imports);

            if (machine_tool) {
                if (did_mod) return error.NotImplemented; // need to reinit HMT for new module.
                try tool.compileInstance(&in);
            }

            did_mod = true;
            continue;
        } else {
            if (!did_mod) return error.NotImplemented;
        }

        if (kind == .assert_invalid or kind == .assert_malformed) {
            // invalid already as text formats. we need separate tests for invalid binary formats..
            unapplicable += 1;
            try t.skip(1);
            continue;
        }
        if (kind != .invoke) {
            _ = try t.expect(.LeftParen);
            try t.expectAtom("invoke");
        }
        const name_tok = try t.expect(.String);
        const name = try t.simple_string(name_tok);

        params.items.len = 0;

        var parse_fail = false;

        while (try t.expect_maybe(.LeftParen)) |_| {
            const typ = try t.expectAtomChoice(ConstKind);
            const param = try t.expect(.Atom);
            const value: StackValue = t.as_res(typ, param) catch parm: {
                dbg("{s} ", .{t.rawtext(param)});
                parse_fail = true;
                break :parm undefined;
            };
            _ = try t.expect(.RightParen);
            try params.append(allocator, value);
        }
        _ = try t.expect(.RightParen);

        const sym = try mod.lookup_export(name) orelse {
            if (kind == .invoke and std.mem.eql(u8, name, "meta_compile")) {
                if (params.items.len != 2) return error.InvalidMeta;
                const func = params.items[0].u32();
                const blk = params.items[1].u32();
                try mod.dbg_compile(func, blk);
                continue; // NB: only syntax safe with invoke!
            }
            return error.NotFound;
        };

        if (sym.kind != .func) return error.Wattaf;

        cases += 1;

        const max_res = 4;
        var expected_trap = false;
        var expected_n_res: u32 = 0;
        var expected_res: [max_res]StackValue = undefined;
        var expected_type: [max_res]ConstKind = undefined;

        switch (kind) {
            .assert_return => {
                while (try t.expect_maybe(.LeftParen)) |_| {
                    const typ = try t.expectAtomChoice(ConstKind);
                    const ret = try t.expect(.Atom);
                    _ = try t.expect(.RightParen);

                    if (expected_n_res == max_res) @panic("increase 'max_res' mayhaps");
                    expected_type[expected_n_res] = typ;
                    expected_res[expected_n_res] = t.as_res(typ, ret) catch parm: {
                        dbg("{s} ", .{t.rawtext(ret)});
                        parse_fail = true;
                        break :parm undefined;
                    };
                    expected_n_res = expected_n_res + 1;
                }
            },
            .assert_trap, .assert_exhaustion => {
                _ = try t.expect(.String);
                expected_trap = true;
            },
            .invoke => {},
            .assert_invalid, .assert_malformed, .module => unreachable,
        }
        if (kind != .invoke) {
            _ = try t.expect(.RightParen);
        }

        if (parse_fail) {
            // aha!
            failures += 1;
            dbg("parse??\n", .{});
            continue;
        }
        if (kind == .assert_exhaustion) {
            failures += 1;
            dbg("TODO: exhaust\n", .{});
            continue;
        }

        var res: [max_res]StackValue = undefined;
        try interpreter.assert_clean();
        var err_ret: ?[]const u8 = null;
        const maybe_n_res = in.execute_either(engine, sym.idx, params.items, &res, false, &err_ret) catch |err| fail: {
            switch (err) {
                error.NotImplemented => {
                    if (err_ret) |msg| {
                        std.debug.print("NYI: {s}\n", .{msg});
                    } else {
                        dbg("NYI\n", .{});
                        if (@errorReturnTrace()) |trace| std.debug.dumpStackTrace(trace.*);
                    }
                    failures += 1;
                    continue;
                },
                error.WASMTrap => break :fail null,
                else => |e| return e,
            }
        };

        if (kind == .invoke) {
            if (maybe_n_res == null) {
                // I think this should be a failure as well?
                dbg("{s}(...): TRAP in invoke\n", .{name});
                failures += 1;
            }
        } else if (!expected_trap) {
            if (maybe_n_res) |n_res| {
                for (expected_type[0..n_res], expected_res[0..n_res], 0..) |typ, val, i| {
                    switch (typ) {
                        inline else => |ctyp| {
                            const expected = @field(val, @tagName(ctyp)[0..3]);

                            const actual = @field(res[i], @tagName(ctyp)[0..3]);
                            // TODO: if "expected" is canonical then "actual" should be as well
                            if (@typeInfo(@TypeOf(expected)) == .float and std.math.isNan(expected) and std.math.isNan(actual)) {
                                // ok
                            } else if (actual != expected) {
                                dbg("{s}(...): actual: {}, expected: {}\n", .{ name, actual, expected });
                                failures += 1;
                            }
                        },
                    }
                }
            } else {
                dbg("{s}(...): TRAP, expected ok\n", .{name});
                failures += 1;
            }
        } else {
            if (maybe_n_res) |_| {
                dbg("{s}(...): expected trap but got success\n", .{name});
                failures += 1;
            }
        }
    }

    dbg("\r", .{});
    if (specname) |nam| dbg("{s}: ", .{nam});
    dbg("{} tests, {} ok, {} fail ({} unapplicable, {} max)\n", .{ cases, cases - failures, failures, unapplicable, maxerr });
    return if (failures > maxerr) 1 else 0;
}

fn modadder(args_ret: []StackValue, in: *wasm_shelf.Instance, data: *anyopaque) !void {
    _ = data;
    _ = in;
    const x = &args_ret[0].i32;
    const y = args_ret[1].i32;
    if (y == 0) {
        x.* = 10 * x.*;
    } else {
        x.* = @rem(x.*, y) + y;
    }
}

const Tokenizer = struct {
    str: []const u8,
    pos: usize = 0,
    lnum: u32 = 0,
    lpos: usize = 0,
    // if non-null, pos is already at the end of `peeked_tok`
    peeked_tok: ?Token = null,

    const ParseError = error{ParseError};

    pub fn err_pos(self: *Tokenizer) struct { u32, u32 } {
        return .{ self.lnum + 1, @intCast(self.pos - self.lpos) };
    }

    pub fn fail_pos(self: *Tokenizer) void {
        const line, const col = self.err_pos();
        dbg("fail at {}:{}\n", .{ line + 1, col });
    }

    fn block_comment(self: *Tokenizer) void {
        var lvl: u32 = 1;
        self.pos += 2;
        var c1: u8 = 0;
        while (self.pos < self.str.len) : (self.pos += 1) {
            const c2 = self.str[self.pos];
            if (c1 == '(' and c2 == ';') {
                lvl += 1;
                c1 = 0;
            } else if (c1 == ';' and c2 == ')') {
                lvl -= 1;
                if (lvl == 0) {
                    self.pos += 1;
                    return;
                }
                c1 = 0;
            } else {
                c1 = c2;
            }
        }
    }

    pub fn nonws(self: *Tokenizer) ?u8 {
        while (self.pos < self.str.len) : (self.pos += 1) {
            switch (self.str[self.pos]) {
                ' ', '\t', '\r' => continue,
                '\n' => {
                    self.lnum += 1;
                    self.lpos = self.pos;
                    continue;
                },
                ';' => {
                    if (self.pos + 1 < self.str.len and self.str[self.pos + 1] == ';') {
                        while (self.pos < self.str.len and self.str[self.pos] != '\n') {
                            self.pos += 1;
                        }
                        if (self.pos < self.str.len) self.pos -= 1; // use \n above
                        continue;
                    }
                    return ';';
                },
                '(' => {
                    if (self.pos + 1 < self.str.len and self.str[self.pos + 1] == ';') {
                        self.block_comment();
                        continue;
                    }
                    return '(';
                },
                else => |c| return c,
            }
        }
        return null;
    }

    pub fn idlike(self: *Tokenizer) bool {
        if (self.pos >= self.str.len) return false;
        const char = self.str[self.pos];
        if (char <= 32) return false;
        if (char == '"' or char == ';' or char == '(' or char == ')') return false;
        return true;
    }

    const TokenKind = enum {
        LeftParen,
        RightParen,
        Atom,
        String,
    };
    const Token = struct {
        kind: TokenKind,
        pos: usize,
        len: usize,
    };

    fn next_inner(self: *Tokenizer) !?Token {
        const t = self.nonws() orelse return null;
        const start = self.pos;

        const kind: TokenKind = valid: {
            switch (t) {
                '(' => {
                    self.pos += 1;
                    break :valid .LeftParen;
                },
                ')' => {
                    self.pos += 1;
                    break :valid .RightParen;
                },
                '"' => {
                    try self.string();
                    break :valid .String;
                },
                // nonws should already skipped valid comments
                ';' => return error.ParseError,
                else => {
                    if (!self.idlike()) return error.ParseError;
                    self.pos += 1;
                    while (self.idlike()) self.pos += 1;
                    break :valid .Atom;
                },
            }
        };
        return .{ .kind = kind, .pos = start, .len = self.pos - start };
    }

    fn next(self: *Tokenizer) !?Token {
        if (self.peeked_tok) |tok| {
            self.peeked_tok = null;
            return tok;
        }
        return self.next_inner();
    }

    fn peek(self: *Tokenizer) !?Token {
        if (self.peeked_tok) |tok| {
            return tok;
        }
        self.peeked_tok = try self.next_inner();
        return self.peeked_tok;
    }

    pub fn skip(self: *Tokenizer, levels: u32) !void {
        var level: u32 = levels;

        while (try self.next()) |tok| {
            // dbg("{},{}: {s} {}\n", .{ self.lnum + 1, tok.pos - self.lpos, @tagName(tok.kind), tok.len });
            switch (tok.kind) {
                .LeftParen => level += 1,
                .RightParen => level -= 1,
                else => continue,
            }
            if (level == 0) break;
        }
    }

    fn string(self: *Tokenizer) !void {
        self.pos += 1; // first "
        while (self.pos < self.str.len) : (self.pos += 1) {
            switch (self.str[self.pos]) {
                '\\' => self.pos += 1,
                '"' => {
                    self.pos += 1;
                    return;
                },
                else => continue,
            }
        }
        return error.ParseError;
    }

    pub fn expect(self: *Tokenizer, t: TokenKind) !Token {
        const tok = try self.next() orelse return error.ParseError;
        if (tok.kind != t) return error.ParseError;
        return tok;
    }

    pub fn expect_maybe(self: *Tokenizer, t: TokenKind) !?Token {
        const tok = try self.peek() orelse return error.ParseError;
        if (tok.kind != t) return null;
        self.peeked_tok = null;
        return tok;
    }

    pub fn expectAtom(self: *Tokenizer, atom: []const u8) !void {
        const tok = try self.expect(.Atom);
        if (!std.mem.eql(u8, self.rawtext(tok), atom)) {
            return error.ParseError;
        }
    }

    pub fn expectAtomChoice(self: *Tokenizer, comptime Choices: type) !Choices {
        const tok = try self.expect(.Atom);
        return std.meta.stringToEnum(Choices, self.rawtext(tok)) orelse error.ParseError;
    }

    fn rawtext(self: *Tokenizer, t: Token) []const u8 {
        return self.str[t.pos..][0..t.len];
    }

    // dummy hack, give us any string value which can be read without allocation
    fn simple_string(self: *Tokenizer, t: Token) ![]const u8 {
        const text = self.rawtext(t);
        if (text.len < 2 or text[0] != '"' or text[text.len - 1] != '"') return error.FormatError;
        if (std.mem.indexOfScalar(u8, text, '\\')) |_| return error.NotImplemented;
        return text[1 .. text.len - 1];
    }

    fn int(self: *Tokenizer, ityp: type, t: Token) !ityp {
        const utyp = if (ityp == i32) u32 else if (ityp == i64) u64 else unreachable;
        const text = self.rawtext(t);
        if (text.len >= 2 and text[0] == '0' and (text[1] == 'x' or text[1] == 'X')) {
            return @bitCast(try std.fmt.parseInt(utyp, text[2..], 16));
        } else if (text.len >= 3 and text[0] == '-' and text[1] == '0' and (text[2] == 'x' or text[2] == 'X')) {
            return -@as(ityp, @bitCast(try std.fmt.parseInt(utyp, text[3..], 16)));
        }

        if (text[0] == '-') {
            return std.fmt.parseInt(ityp, text, 10);
        } else {
            return @bitCast(try std.fmt.parseInt(utyp, text, 10));
        }
    }

    fn nandesc(ftyp: type, desc: []const u8) !if (ftyp == f64) u64 else u32 {
        if (std.mem.eql(u8, desc, "canonical")) {
            return if (ftyp == f64) 0x7ff8000000000000 else 0x7fc00000;
        } else if (std.mem.eql(u8, desc, "arithmetic")) {
            // really any quiet NaN including canonical but whatever
            return if (ftyp == f64) 0x7fffffffffffffff else 0x7fffffff;
        } else if (desc.len > 2 and std.mem.eql(u8, desc[0..2], "0x")) {
            const mag = if (ftyp == f64) 0x7ff0000000000000 else 0x7f800000;
            return mag + try std.fmt.parseInt(if (ftyp == f64) u64 else u32, desc[2..], 16);
        }
        return error.NotImplemented;
    }

    fn float(self: *Tokenizer, ftyp: type, t: Token) !ftyp {
        const text = self.rawtext(t);
        if (text.len >= 4 and std.mem.eql(u8, text[0..4], "nan:")) {
            return @bitCast(try nandesc(ftyp, text[4..]));
        } else if (text.len >= 5 and std.mem.eql(u8, text[0..5], "-nan:")) {
            const ival = try nandesc(ftyp, text[5..]);
            const sval = if (ftyp == f64) 0x8000000000000000 else 0x80000000;
            return @bitCast(ival + sval);
        }
        return try std.fmt.parseFloat(ftyp, text);
    }

    fn as_res(self: *Tokenizer, typ: ConstKind, param: Token) !StackValue {
        return switch (typ) {
            .@"i32.const" => .{ .i32 = try self.int(i32, param) },
            .@"i64.const" => .{ .i64 = try self.int(i64, param) },
            .@"f32.const" => .{ .f32 = try self.float(f32, param) },
            .@"f64.const" => .{ .f64 = try self.float(f64, param) },
            .@"ref.null" => .{ .ref = if (std.mem.eql(u8, self.rawtext(param), "extern")) 0 else return error.NotImplemented },
            .@"ref.extern" => .{ .ref = @intCast(try self.int(i32, param)) }, // BLUFF
        };
    }
};

fn wat2wasm(source: []const u8, allocator: std.mem.Allocator) ![]u8 {
    const argv = &[_][]const u8{ "wat2wasm", "-", "--output=-" };

    const Child = std.process.Child;
    var child: Child = .init(argv, allocator);

    child.stdout_behavior = Child.StdIo.Pipe;
    child.stdin_behavior = Child.StdIo.Pipe;
    child.stderr_behavior = Child.StdIo.Inherit;
    try child.spawn();
    try child.stdin.?.writeAll(source);
    child.stdin.?.close();
    child.stdin = null; // dumma
    const out = child.stdout.?.readToEndAlloc(allocator, 10 * 1024 * 1024);
    switch (try child.wait()) {
        .Exited => |res| {
            if (res == 0) return out;
        },
        else => {},
    }
    return error.ProcessError;
}
