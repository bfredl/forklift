const Builder = @import("std").build.Builder;
pub fn build(b: *Builder) void {
    const mode = b.standardReleaseOptions();
    var exe = b.addExecutable("operator", "src/main.zig");
    exe.setBuildMode(mode);
    exe.install();

    var exe_rpn = b.addExecutable("rpn", "src/rpn.zig");
    exe_rpn.setBuildMode(mode);
    exe_rpn.install();

    var exe_parse = b.addExecutable("parse", "src/parse.zig");
    exe_parse.setBuildMode(mode);
    exe_parse.install();

    const operate = b.step("operate", "operate the forklift");
    const run = exe.run();
    run.step.dependOn(b.getInstallStep());
    operate.dependOn(&run.step);

    const do_rpn = b.step("rpn", "compile rpn expression");
    const run_rpc = exe_rpn.run();
    run_rpc.step.dependOn(b.getInstallStep());
    do_rpn.dependOn(&run_rpc.step);
}
