const std = @import("std");
const linux = std.os.linux;
const BPF = linux.BPF;
const mem = std.mem;
const Insn = BPF.Insn;

pub fn dump_bpf(w: anytype, code: []const Insn) !void {
    for (code, 0..) |*i, ni| {
        try dump_ins(w, i.*, ni);
    }
}

pub fn dump_ins(w: anytype, i: BPF.Insn, ni: usize) !void {
    try w.print("{:03}: {x:0>2} ", .{ ni, i.code });
    try w.print("{x} {x} {x:3} {x:4} ", .{ i.dst, i.src, i.off, i.imm });
    const grp = switch (@as(u3, @intCast(i.code & 0x07))) {
        BPF.LD => "LD",
        BPF.LDX => "LDX",
        BPF.ST => "ST",
        BPF.STX => "STX",
        BPF.ALU => "ALU",
        BPF.JMP => "JMP",
        BPF.RET => "RET",
        BPF.MISC => "A8M", // ALU64 or MISC
    };

    const h = i.code & 0xf0;
    const aluspec = switch (h) {
        BPF.ADD => "ADD",
        BPF.SUB => "SUB",
        BPF.MUL => "MUL",
        BPF.DIV => "DIV",
        BPF.OR => "OR",
        BPF.AND => "AND",
        BPF.LSH => "LSH",
        BPF.RSH => "RSH",
        BPF.NEG => "NEG",
        BPF.MOD => "MOD",
        BPF.XOR => "XOR",
        BPF.MOV => "MOV",
        BPF.ARSH => "ARSH",
        else => "???",
    };
    const siz = i.code & 0x18;
    const mspec = i.code & 0xe0;
    switch (@as(u3, @intCast(i.code & 0x07))) {
        BPF.ALU, BPF.ALU64 => {
            try w.print("{s}", .{aluspec});
            if (i.code & 0x07 == BPF.ALU64) try w.print("64", .{});
            try w.print(" r{}, ", .{i.dst});
            if (i.code & BPF.X == BPF.X) try w.print("r{}", .{i.src}) else try w.print("{}", .{i.imm});
        },
        BPF.ST, BPF.LD => {
            if (i.code == 0 and i.imm == 0) {
                // TODO: not like this!
                try w.print("\n", .{});
                return;
            }
            _ = siz;
            try w.print("{s} ", .{grp});
            if (mspec == BPF.MEM) {
                try w.print("[r{}{:02}], ", .{ i.dst, i.off });
            } else if (mspec == BPF.IMM and i.src == BPF.PSEUDO_MAP_FD) {
                try w.print("r{}, map_fd ", .{i.dst});
            } else if (mspec == BPF.IMM and i.src == BPF.PSEUDO_MAP_VALUE) {
                try w.print("r{}, map_value ", .{i.dst});
            } else {
                try w.print("?? ", .{});
            }
            try w.print("{}", .{i.imm});
        },
        BPF.STX => {
            _ = siz;
            if (mspec == BPF.MEM) {
                try w.print("STX [r{}{:02}], ", .{ i.dst, i.off });
            } else if (i.code == 0xdb) {
                try w.print("XADD [r{}{:02}], ", .{ i.dst, i.off });
            } else {
                try w.print("STX.?? ", .{});
            }
            try w.print("r{}", .{i.src});
        },
        BPF.LDX => {
            _ = siz;
            try w.print("LDX r{}, ", .{i.dst});
            if (mspec == BPF.MEM) {
                try w.print("[r{}{:02}]", .{ i.src, i.off });
            } else {
                try w.print("???", .{});
            }
        },
        BPF.JMP => {
            const jmpspec = switch (h) {
                BPF.JA => "JA",
                BPF.JEQ => "JEQ",
                BPF.JGT => "JGT",
                BPF.JGE => "JGE",
                BPF.JSET => "JSET",
                BPF.JNE => "JNE",
                BPF.JLT => "JLT",
                BPF.JLE => "JLE",
                BPF.JSGT => "JSGT",
                BPF.JSLT => "JSLT",
                BPF.JSLE => "JSLE",
                else => "J??",
            };

            if (h == BPF.EXIT) {
                try w.print("EXIT", .{});
            } else if (h == BPF.CALL) {
                try w.print("CALL ${s}", .{@tagName(@as(BPF.Helper, @enumFromInt(i.imm)))});
            } else {
                try w.print("{s} r{}, ", .{ jmpspec, i.dst });
                if (i.code & BPF.X == BPF.X) try w.print("r{}", .{i.src}) else try w.print("{}", .{i.imm});
                try w.print(" => {}", .{@as(i32, @intCast(ni)) + i.off + 1});
            }
        },
        else => try w.print("{s}.???", .{grp}),
    }
    try w.print("\n", .{});
}

test "write" {
    const insn = [_]Insn{
        Insn.mov(.r0, 0),
        Insn.exit(),
    };

    var data = std.ArrayList(u8).init(std.testing.allocator);
    defer data.deinit();
    try dump_bpf(data.writer(), insn[0..]);
    // std.debug.print("\n{s}\n", .{data.items});
    try std.testing.expectEqualSlices(u8,
        \\  0: b7 0 0  +0   +0 MOV64 r0, 0
        \\  1: 95 0 0  +0   +0 EXIT
        \\
    , data.items);
}
