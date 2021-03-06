const Builder = @import("std").build.Builder;
pub fn build(b: *Builder) void {
    const mode = b.standardReleaseOptions();
    var exe = b.addExecutable("operator", "src/main.zig");
    exe.setBuildMode(mode);
    exe.install();

    var exe_parse = b.addExecutable("parse", "src/parse.zig");
    exe_parse.setBuildMode(mode);
    exe_parse.install();

    const operate = b.step("operate", "operate the forklift");
    const run = exe.run();
    run.step.dependOn(b.getInstallStep());
    operate.dependOn(&run.step);

    var exe_looptest = b.addExecutable("loop_test", "src/loop_test.zig");
    exe_looptest.setBuildMode(mode);
    exe_looptest.install();

    const loop_test = b.step("loop_test", "loop_test the forklift");
    const run_looptest = exe_looptest.run();
    run_looptest.step.dependOn(b.getInstallStep());
    loop_test.dependOn(&run_looptest.step);
}
