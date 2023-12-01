str: []const u8,
pos: usize = 0,
lnum: u32 = 0,
lpos: usize = 0,

const Self = @This();
const ParseError = error{ParseError};

const print = std.debug.print;
const std = @import("std");

pub fn err_pos(self: *Self) struct { u32, u32 } {
    return .{ self.lnum + 1, @intCast(self.pos - self.lpos) };
}

pub fn fail_pos(self: *Self) void {
    const line, const col = self.err_pos();
    print("fail at {}:{}\n", .{ line, col });
}

pub fn nonws(self: *Self) ?u8 {
    while (self.pos < self.str.len) : (self.pos += 1) {
        if (self.str[self.pos] != ' ') {
            return self.str[self.pos];
        }
    }
    return null;
}

pub fn lbrk(self: *Self) ParseError!void {
    var val = self.nonws() orelse return;
    while (true) {
        if (val == '/' and self.pos < self.str.len - 1 and self.str[self.pos + 1] == '/') {
            self.pos += 2;
            while (self.str[self.pos] != '\n') : (self.pos += 1) {
                if (self.pos >= self.str.len - 1) return;
            }
        } else {
            if (val != '\n') return error.ParseError;
        }
        self.pos += 1;
        self.lnum += 1;
        self.lpos = self.pos;

        val = self.nonws() orelse return;
        if (val != '\n' and val != '/') return;
    }
}

pub fn idlike(c: u8) bool {
    return ('a' <= c and c <= 'z') or ('A' <= c and c <= 'Z') or ('0' < c and c < '9') or c == '_';
}

pub fn oplike(c: u8) bool {
    return (c == '+' or c == '-' or c == '/' or c == '*' or c == '=' or c == '<' or c == '>' or c == '!' or c == '|');
}

pub const Chunk = []const u8;
pub fn keyword(self: *Self) ?Chunk {
    const chunk = self.peek_keyword() orelse return null;
    self.pos += chunk.len;
    return chunk;
}

pub fn peek_keyword(self: *Self) ?Chunk {
    const c = self.nonws() orelse return null;
    if (!('a' <= c and c <= 'z') and !('A' <= c and c <= 'Z')) return null;
    const start = self.pos;
    var pos = self.pos;
    while (pos < self.str.len) : (pos += 1) {
        const next = self.str[pos];
        if (!idlike(next)) {
            break;
        }
    }
    return self.str[start..pos];
}

pub fn prefixed(self: *Self, sigil: u8) ParseError!?Chunk {
    if (self.nonws() != sigil) return null;
    self.pos += 1;
    return try self.identifier();
}

pub fn expect_char(self: *Self, char: u8) ParseError!void {
    if (self.nonws() == char) {
        self.pos += 1;
    } else {
        print("expected '{c}'\n", .{char});
        return error.ParseError;
    }
}

pub fn identifier(self: *Self) ParseError!Chunk {
    const start = self.pos;
    while (self.pos < self.str.len) : (self.pos += 1) {
        const next = self.str[self.pos];
        if (!idlike(next)) {
            break;
        }
    }
    if (self.pos == start) return error.ParseError;
    return self.str[start..self.pos];
}

pub fn num(self: *Self) ?u64 {
    const first = self.nonws() orelse return null;
    if (!('0' <= first and first <= '9')) return null;
    var val: u64 = 0;
    while (self.pos < self.str.len) : (self.pos += 1) {
        const next = self.str[self.pos];
        if ('0' <= next and next <= '9') {
            val = val * 10 + (next - '0');
        } else {
            break;
        }
    }
    return val;
}

pub fn operator(self: *Self) ?Chunk {
    const chunk = self.peek_operator() orelse return null;
    self.pos += chunk.len;
    return chunk;
}
pub fn peek_operator(self: *Self) ?Chunk {
    const first = self.nonws() orelse return null;
    if (!oplike(first)) return null;
    const start = self.pos;
    var pos = self.pos;
    while (pos < self.str.len) : (pos += 1) {
        const next = self.str[pos];
        if (!oplike(next)) {
            break;
        }
    }
    return self.str[start..pos];
}
