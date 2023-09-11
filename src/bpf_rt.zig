const std = @import("std");
const linux = std.os.linux;
const BPF = linux.BPF;
const mem = std.mem;
const fd_t = linux.fd_t;

pub const Code = std.ArrayList(BPF.Insn);

pub const BPFObject = union(enum) {
    map: struct {
        fd: fd_t,
        key_size: u32,
        val_size: u32,
        n_entries: u32,
        kind: BPF.MapType,
    },
    prog: struct {
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

pub const BPFModule = struct {
    bpf_code: Code,
    objs: std.StringArrayHashMap(BPFObject),
    // bpf_code.items[id] needs to point at fd of object specified by obj_idx
    relocations: std.ArrayList(struct { pos: u32, obj_idx: u32 }),

    pub fn init(allocator: std.mem.Allocator) BPFModule {
        return .{
            .bpf_code = Code.init(allocator),
            .objs = @TypeOf(init(allocator).objs).init(allocator),
            .relocations = @TypeOf(init(allocator).relocations).init(allocator),
        };
    }

    /// Frees all memory from `allocator`, but does not close any fd:s.
    pub fn deinit_mem(self: *BPFModule) void {
        self.bpf_code.deinit();
        self.objs.deinit();
        self.relocations.deinit();
    }

    pub fn load(self: *BPFModule) !void {
        if (self.relocations.items.len > 0) {
            unreachable;
        }

        for (self.objs.values()) |*v| {
            switch (v.*) {
                .prog => |*p| {
                    const code = self.bpf_code.items[p.code_start..][0..p.code_len];
                    p.fd = try prog_load_test(.syscall, code, "MIT", BPF.F_SLEEPABLE);
                },
                .map => |*m| {
                    m.fd = try BPF.map_create(m.kind, m.key_size, m.val_size, m.n_entries);
                },
            }
        }
    }

    pub fn get_fd(self: *BPFModule, name: []const u8) ?fd_t {
        const val = self.objs.get(name) orelse return null;
        return switch (val) {
            .prog => |p| p.fd,
            .map => |m| m.fd,
        };
    }
};

pub fn prog_load_test(prog_type: BPF.ProgType, c: []BPF.Insn, license: []const u8, flags: u32) !fd_t {
    var log_buf = [1]u8{0} ** 1024;
    var log = BPF.Log{ .level = 4, .buf = &log_buf };
    return BPF.prog_load(prog_type, c, &log, license, 0, flags) catch |err| {
        std.debug.print("failed load: {s}\n", .{mem.sliceTo(&log_buf, 0)});
        return err;
    };
}

pub fn prog_test_run(
    prog: fd_t,
    ctx_in: ?[]const u8,
) !u32 {
    var attr = BPF.Attr{
        .test_run = mem.zeroes(BPF.TestRunAttr),
    };

    attr.test_run.prog_fd = prog;
    if (ctx_in) |ctx| {
        attr.test_run.ctx_in = @intFromPtr(ctx.ptr);
        attr.test_run.ctx_size_in = @intCast(ctx.len);
    }

    const rc = linux.bpf(.prog_test_run, &attr, @sizeOf(BPF.TestRunAttr));
    const err = linux.getErrno(rc);
    if (err != .SUCCESS) {
        std.debug.print("\nprog_test_run: E{s}\n", .{@tagName(err)});
    }
    return switch (err) {
        .SUCCESS => attr.test_run.retval,
        .ACCES => error.UnsafeProgram,
        .FAULT => error.BPFProgramFault,
        .INVAL => error.InvalidArgument,
        .PERM => error.AccessDenied,
        else => std.os.unexpectedErrno(err),
    };
}
