const std = @import("std");
pub fn build(b: *std.Build) void {
    const t = b.standardTargetOptions(.{});
    const opt = b.standardOptimizeOption(.{});

    const ir = b.step("ir", "run some ir");
    const exe_ir = b.addExecutable(.{
        .name = "run",
        .root_source_file = b.path("src/run_ir.zig"),
        .optimize = opt,
        .target = t,
    });
    b.installArtifact(exe_ir);
    ir.dependOn(&exe_ir.step);

    const forklift = b.addModule("forklift", .{
        .root_source_file = b.path("src/forklift.zig"),
    });

    if (false) {
        const exe_bpf = b.addExecutable(.{
            .name = "run_bpf",
            .root_source_file = b.path("src/main_bpf.zig"),
            .optimize = opt,
            .target = t,
        });
        b.installArtifact(exe_bpf);
    }

    const bpf_helper = b.addExecutable(.{
        .name = "bpf_helper",
        .root_source_file = b.path("test/bpf_helper.zig"),
        .optimize = opt,
        .target = t,
    });
    // b.installArtifact(bpf_helper); // debug the test

    const test_user = b.addTest(.{
        .root_source_file = b.path("test/all_user.zig"),
        .optimize = opt,
    });
    test_user.root_module.addImport("forklift", forklift);
    const run_test_user = b.addRunArtifact(test_user);

    // requires root or virtualization, so separate
    const test_bpf = b.addTest(.{
        .root_source_file = b.path("test/bpf.zig"),
        .optimize = opt,
    });
    test_bpf.root_module.addImport("forklift", forklift);

    // b.installArtifact(test_bpf);
    const run_test_bpf_sudo = b.addRunArtifact(test_bpf);

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

    const test_step_bpf_sudo = b.step("test_bpf_sudo", "Check it!");
    test_step_bpf_sudo.dependOn(&run_test_bpf_sudo.step);
}
