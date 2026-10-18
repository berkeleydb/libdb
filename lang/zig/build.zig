// SPDX-License-Identifier: Sleepycat
//! Build for the libdb Zig binding (RFC 0016). Pinned to Zig 0.16.
//!
//! Steps:
//!   zig build test  --summary all   -- run the binding tests (set -Doptimize)
//!   zig build drift                 -- the drift gate (C #error vs db.h)
//!   zig build example-kv            -- key/value CLI example
//!   zig build example-counter       -- transactional counter with retry
//!   zig build example-address       -- secondary-index address book
//!   zig build examples              -- build+run all three
const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const prefix = b.option([]const u8, "libdb-prefix", "libdb install prefix") orelse
        "/usr/local/BerkeleyDB.2026.0";
    const inc = b.pathJoin(&.{ prefix, "include" });
    const libdir = b.pathJoin(&.{ prefix, "lib" });

    // The frozen-constant module, shared by Layer 2 and the drift-gate generator.
    const consts_mod = b.createModule(.{
        .root_source_file = b.path("src/consts.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Layer 2 public module. The flat method shim (Layer 1 seam) + the C library
    // are attached here so any importer links them transitively.
    const mod = b.addModule("libdb", .{
        .root_source_file = b.path("src/libdb.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    mod.addImport("consts.zig", consts_mod);
    mod.addIncludePath(.{ .cwd_relative = inc });
    mod.addIncludePath(b.path(".")); // for shim.h
    mod.addCSourceFile(.{ .file = b.path("shim.c"), .flags = &.{"-O2"} });
    mod.addLibraryPath(.{ .cwd_relative = libdir });
    mod.linkSystemLibrary("db-2026.0", .{});

    // ---- tests ----
    const test_mod = b.createModule(.{
        .root_source_file = b.path("src/test.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    test_mod.addImport("libdb", mod);
    test_mod.addIncludePath(.{ .cwd_relative = inc });
    test_mod.addIncludePath(b.path("."));
    const tests = b.addTest(.{ .root_module = test_mod });
    const run_tests = b.addRunArtifact(tests);
    b.step("test", "Run the binding tests").dependOn(&run_tests.step);

    // ---- drift gate ----
    // Build the generator (a host tool) that reads consts.zig and emits a C
    // program; then compile+run that C program against the installed db.h. The
    // C compiler #errors if any baked constant disagrees with the header.
    const gen_mod = b.createModule(.{
        .root_source_file = b.path("gen-drift-gate.zig"),
        .target = b.graph.host,
        .optimize = .Debug,
        .link_libc = true,
    });
    gen_mod.addImport("consts.zig", consts_mod);
    const gen = b.addExecutable(.{ .name = "gen-drift-gate", .root_module = gen_mod });

    const gen_run = b.addRunArtifact(gen);
    const gen_c = gen_run.addOutputFileArg("drift_gate.c");

    // Compile the generated C against db.h and run it.
    const cc = b.addSystemCommand(&.{ "clang", "-I", inc });
    cc.addFileArg(gen_c);
    cc.addArg("-o");
    const gate_exe = cc.addOutputFileArg("drift_gate");
    const run_gate = std.Build.Step.Run.create(b, "run drift gate");
    run_gate.addFileArg(gate_exe);
    b.step("drift", "Run the drift gate (C #error vs db.h)").dependOn(&run_gate.step);

    // ---- examples ----
    const examples_step = b.step("examples", "Build and run all examples");
    const names = [_][]const u8{ "kv", "counter", "address" };
    inline for (names) |name| {
        const ex_mod = b.createModule(.{
            .root_source_file = b.path("examples/" ++ name ++ ".zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        });
        ex_mod.addImport("libdb", mod);
        const ex = b.addExecutable(.{ .name = "example-" ++ name, .root_module = ex_mod });
        b.installArtifact(ex);
        const run_ex = b.addRunArtifact(ex);
        if (b.args) |args| run_ex.addArgs(args);
        const step = b.step("example-" ++ name, "Run the " ++ name ++ " example");
        step.dependOn(&run_ex.step);
        examples_step.dependOn(&run_ex.step);
    }
}
