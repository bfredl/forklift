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

    const bpf_helper = b.addExecutable(.{
        .name = "bpf_helper",
        .root_source_file = .{ .path = "test/bpf_helper.zig" },
        .optimize = opt,
    });
    b.installArtifact(bpf_helper); // debug the test

    const test_user = b.addTest(.{
        .root_source_file = .{ .path = "test/all_user.zig" },
        .optimize = opt,
    });
    test_user.addModule("forklift", forklift);
    const run_test_user = b.addRunArtifact(test_user);

    // requires root or virtualization, so separate
    const test_bpf = b.addTest(.{
        .root_source_file = .{ .path = "test/bpf.zig" },
        .optimize = opt,
    });
    test_bpf.addModule("forklift", forklift);

    const run_test_bpf = b.addRunArtifact(bpf_helper);
    run_test_bpf.addArtifactArg(test_bpf);
    // run_test_bpf.stdio = .zig_test; // BAD BINOCULARS!

    const test_step = b.step("test", "Check it!");
    test_step.dependOn(&run_test_user.step);
    test_step.dependOn(&run_test_bpf.step);

    const test_step_user = b.step("test_user", "Check it!");
    test_step_user.dependOn(&run_test_user.step);

    const test_step_bpf = b.step("test_bpf", "Check it!");
    test_step_bpf.dependOn(&run_test_bpf.step);
}
