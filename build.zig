const std = @import("std");
pub fn build(b: *std.Build) void {
    const opt = b.standardOptimizeOption(.{});

    const ir = b.step("ir", "run some ir");
    var exe_ir = b.addExecutable(.{
        .name = "run",
        .root_source_file = .{ .path = "src/run_ir.zig" },
        .optimize = opt,
    });
    b.installArtifact(exe_ir);
    ir.dependOn(&exe_ir.step);

    const forklift = b.createModule(.{
        .source_file = .{ .path = "src/forklift.zig" },
    });

    const cfo_test = b.addTest(.{
        .root_source_file = .{ .path = "test/all.zig" },
        .optimize = opt,
    });

    cfo_test.addModule("forklift", forklift);

    const run = b.addRunArtifact(cfo_test);
    const test_step = b.step("test", "Check it!");
    test_step.dependOn(&run.step);
}
