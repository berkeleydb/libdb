// SPDX-License-Identifier: Sleepycat
//! Build for the libdb Zig binding (RFC 0016). Pinned to Zig 0.16.
//!
//!   zig build test --summary all   -- run the binding tests (set -Doptimize)
const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const prefix = b.option([]const u8, "libdb-prefix", "libdb install prefix") orelse
        "/usr/local/BerkeleyDB.2026.0";
    const inc = b.pathJoin(&.{ prefix, "include" });
    const libdir = b.pathJoin(&.{ prefix, "lib" });

    // The frozen-constant module (Layer 1), shared by Layer 2 and later the
    // drift-gate generator.
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
}
