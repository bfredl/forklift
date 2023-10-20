const std = @import("std");
const linux = std.os.linux;
const BPF = linux.BPF;
const mem = std.mem;
const fd_t = linux.fd_t;
const bpf = @import("./bpf.zig");
const CodeBuffer = @import("./CodeBuffer.zig");

pub const BPFCode = std.ArrayList(BPF.Insn);

// TODO: turn on/off BPF support as a compile-time feature
pub const RTObject = union(enum) {
    func: struct {
        // entry point in "code"
        code_start: u32,
    },
    bpf_map: struct {
        fd: fd_t,
        key_size: u32,
        val_size: u32,
        n_entries: u32,
        kind: BPF.MapType,
    },
    bpf_prog: struct {
        fd: fd_t,
        // slice into bpf_code
        code_start: u32,
        code_len: u32,
    },
    // elf: struct {
    //     fname: []const u8,
    //     syms: ElfSymbols,
    //     sdts: ArrayList(ElfSymbols.Stapsdt),
    // },
};

const CFOModule = @This();

const ObjsMap = std.StringArrayHashMap(RTObject);
const Relocations = std.ArrayList(struct { pos: u32, obj_idx: u32 });

bpf_code: BPFCode,
code: CodeBuffer,
objs: ObjsMap,
// bpf_code.items[id] needs to point at fd of object specified by obj_idx
relocations: Relocations,

pub fn init(allocator: std.mem.Allocator) !CFOModule {
    return .{
        .code = try CodeBuffer.init(allocator),
        .bpf_code = BPFCode.init(allocator),
        .objs = ObjsMap.init(allocator),
        .relocations = Relocations.init(allocator),
    };
}

/// Frees all memory from `allocator`, but does not close any fd:s.
pub fn deinit_mem(self: *CFOModule) void {
    self.bpf_code.deinit();
    self.code.deinit();
    self.objs.deinit();
    self.relocations.deinit();
}

pub fn load(self: *CFOModule) !void {
    for (0.., self.objs.values()) |i, *v| {
        switch (v.*) {
            .bpf_prog => |*p| {
                const code = self.bpf_code.items[p.code_start..][0..p.code_len];
                if (false) {
                    try bpf.dump_bpf(std.io.getStdErr().writer(), code);
                }
                p.fd = try bpf.prog_load_test(.syscall, code, "MIT", BPF.F_SLEEPABLE);
            },
            .bpf_map => |*m| {
                m.fd = try BPF.map_create(m.kind, m.key_size, m.val_size, m.n_entries);
                // O(N^2) but who the fuck cares
                for (self.relocations.items) |r| {
                    if (r.obj_idx == i) {
                        // note: technically [r.pos+1] contains the upper 32 bits of fd :zany_face:
                        self.bpf_code.items[r.pos].imm = @intCast(m.fd);
                    }
                }
            },
            .func => {},
        }
    }
}

pub fn get_func_off(self: *CFOModule, name: []const u8) ?u32 {
    const val = self.objs.get(name) orelse return null;
    return switch (val) {
        .func => |f| f.code_start,
        else => null,
    };
}

pub fn get_func_ptr(self: *CFOModule, name: []const u8, comptime T: type) !T {
    const off = self.get_func_off(name) orelse return error.FAILURE;
    return self.code.get_ptr(off, T);
}

pub fn bpf_get_fd(self: *CFOModule, name: []const u8) ?fd_t {
    const val = self.objs.get(name) orelse return null;
    return switch (val) {
        .bpf_prog => |p| p.fd,
        .bpf_map => |m| m.fd,
        else => null,
    };
}

pub fn bpf_test_run(
    self: *CFOModule,
    name: []const u8,
    ctx_in: ?[]const u8,
) !u32 {
    const val = self.objs.get(name) orelse return error.NotAProgram;
    const fd = switch (val) {
        .bpf_prog => |p| p.fd,
        else => return error.NotAProgram,
    };
    return bpf.prog_test_run(fd, ctx_in);
}
