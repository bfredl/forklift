const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const llvm = b.option(bool, "llvm", "use llvm");

    const wasm_shelf = b.addModule("wasm_shelf", .{
        .root_source_file = b.path("src/wasm_shelf.zig"),
    });

    // TODO: this is a ugly hack. need a standard pattern for developing
    // two packages togheter. or maybe this is a usecase where
    // one repo should just GLOM both packages?
    const forklift = b.addModule("forklift", .{
        .root_source_file = b.path("../src/forklift.zig"),
    });
    wasm_shelf.addImport("forklift", forklift);

    const exe = b.addExecutable(.{
        .name = "wasm_run",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    if (llvm) |val| exe.use_llvm = val;

    exe.root_module.addImport("wasm_shelf", wasm_shelf);

    const clap = b.dependency("clap", .{});
    exe.root_module.addImport("clap", clap.module("clap"));

    const inst = b.addInstallArtifact(exe, .{});
    b.getInstallStep().dependOn(&inst.step);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(&inst.step);
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const ts_exe = b.addExecutable(.{
        .name = "ts_run",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/ts_runner.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    if (llvm) |val| ts_exe.use_llvm = val;

    ts_exe.root_module.addImport("wasm_shelf", wasm_shelf);
    ts_exe.root_module.addIncludePath(b.path("src/"));
    b.installArtifact(ts_exe);

    const run_cmd_ts = b.addRunArtifact(ts_exe);
    run_cmd_ts.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd_ts.addArgs(args);
    }
    const run_step_ts = b.step("ts", "tree-sitter thing");
    run_step_ts.dependOn(&run_cmd_ts.step);

    const wast_exe = b.addExecutable(.{
        .name = "run_wast_tests",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/wast_runner.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    if (llvm) |val| wast_exe.use_llvm = val;

    wast_exe.root_module.addImport("wasm_shelf", wasm_shelf);
    wast_exe.root_module.addImport("clap", clap.module("clap"));
    b.installArtifact(wast_exe);

    const wast_run_cmd = b.addRunArtifact(wast_exe);
    wast_run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        wast_run_cmd.addArgs(args);
    }
    const wast_run_step = b.step("wast", "Run the app");
    wast_run_step.dependOn(&wast_run_cmd.step);

    // Creates a step for unit testing. This only builds the test executable
    // but does not run it.
    const lib_unit_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/wasm_shelf.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);

    const use_upstream = true; // can we lazyDependency only for some b.step() ?
    const maybe_spec_dep = if (use_upstream) b.lazyDependency("spec", .{}) else null;
    const run_spec_tests = b.step("spectest", "Run spec tests");

    if (maybe_spec_dep) |spec_dep| {
        const upstream_specs = [_]struct { []const u8, u32, ?u32 }{
            .{ "i32", 0, 1 },
            .{ "i64", 0, 1 },
            .{ "f32", 2, null },
            .{ "f64", 2, null },
            .{ "f32_cmp", 0, null },
            .{ "f64_cmp", 0, null },
            .{ "labels", 0, null },
            .{ "local_get", 0, 4 },
            .{ "local_set", 0, null },
            .{ "local_tee", 0, null },
            .{ "br_if", 0, 39 },
            // .{ "loop", 0, null },
            .{ "conversions", 6, null },
            .{ "memory", 0, null },
            .{ "memory_copy", 0, null },
            .{ "memory_fill", 0, null },
            .{ "load", 0, null },
            .{ "store", 0, null },
            .{ "call", 2, null },
            .{ "global", 0, null },
        };
        for (upstream_specs) |item| {
            const name, const fail, const heavy = item;
            add_spectest(b, run_spec_tests, wast_exe, spec_dep.path(b.fmt("test/core/{s}.wast", .{name})), name, fail, false);
            if (heavy) |heavy_fail| { // if heavy also run non-heavy
                add_spectest(b, run_spec_tests, wast_exe, spec_dep.path(b.fmt("test/core/{s}.wast", .{name})), name, heavy_fail, true);
            }
        }
    }

    add_spectest(b, run_spec_tests, wast_exe, b.path("test/misc.wast"), "misc", 0, false);
    add_spectest(b, run_spec_tests, wast_exe, b.path("test/loop.wast"), "loop", 0, false);

    // Similar to creating the run step earlier, this exposes a `test` step to
    // the `zig build --help` menu, providing a way for the user to request
    // running the unit tests.
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_unit_tests.step);
}

fn add_spectest(b: *std.Build, test_step: *std.Build.Step, wast_exe: *std.Build.Step.Compile, file: std.Build.LazyPath, name: []const u8, fail: u32, heavy: bool) void {
    const spec_step = b.addRunArtifact(wast_exe);
    spec_step.addFileArg(file);
    spec_step.addArgs(&.{ "--specname", name });
    if (fail > 0) {
        spec_step.addArgs(&.{ "--errors", b.fmt("{}", .{fail}) });
    }
    if (heavy) spec_step.addArg("--heavy");
    test_step.dependOn(&spec_step.step);
}
