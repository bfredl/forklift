const std = @import("std");
pub fn build(b: *std.Build) void {
    const opt = b.standardOptimizeOption(.{});
    var exe = b.addExecutable(.{
        .name = "operator",
        .root_source_file = .{ .path = "src/main.zig" },
        .optimize = opt,
    });
    exe.install();

    const ir = b.step("ir", "run some ir");
    var exe_ir = b.addExecutable(.{
        .name = "run",
        .root_source_file = .{ .path = "src/run_ir.zig" },
        .optimize = opt,
    });
    exe_ir.install();
    const install_ir = b.addInstallArtifact(exe_ir);
    ir.dependOn(&install_ir.step);

    if (false) {
        var exe_parse = b.addExecutable("parse", "src/parse.zig");
        exe_parse.install();

        const operate = b.step("operate", "operate the forklift");
        const run = exe.run();
        run.step.dependOn(b.getInstallStep());
        operate.dependOn(&run.step);

        var exe_looptest = b.addExecutable("loop_test", "src/loop_test.zig");
        exe_looptest.install();

        const loop_test = b.step("loop_test", "loop_test the forklift");
        const run_looptest = exe_looptest.run();
        run_looptest.step.dependOn(b.getInstallStep());
        loop_test.dependOn(&run_looptest.step);
    }
}
