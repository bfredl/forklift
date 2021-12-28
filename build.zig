const Builder = @import("std").build.Builder;
pub fn build(b: *Builder) void {
    const mode = b.standardReleaseOptions();
    var exe = b.addExecutable("operator", "src/main.zig");
    exe.setBuildMode(mode);
    exe.install();

    var exe2 = b.addExecutable("rpn", "src/FLIR.zig");
    exe2.setBuildMode(mode);
    exe2.install();

    const operate = b.step("operate", "operate the forklift");
    const run = exe.run();
    run.step.dependOn(b.getInstallStep());
    operate.dependOn(&run.step);

    const dorpn = b.step("rpn", "compile rpn expression");
    const runrpc = exe2.run();
    runrpc.step.dependOn(b.getInstallStep());
    dorpn.dependOn(&runrpc.step);
}
