const Builder = @import("std").build.Builder;
pub fn build(b: *Builder) void {
    const mode = b.standardReleaseOptions();
    var exe = b.addExecutable("operator", "src/main.zig");
    exe.setBuildMode(mode);
    exe.install();

    const operate = b.step("operate", "operate the forklift");
    const run = exe.run();
    run.step.dependOn(b.getInstallStep());
    operate.dependOn(&run.step);
}
