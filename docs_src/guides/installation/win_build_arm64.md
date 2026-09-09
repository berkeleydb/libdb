---
title: "Building Berkeley DB for Windows on ARM64"
api-name: "Building Berkeley DB for Windows on ARM64"
---
## Building Berkeley DB for Windows on ARM64

The bundled solution `build_windows\Berkeley_DB_vs2010.sln` provides `ARM64`
configurations (`Debug|ARM64`, `Release|ARM64`, `Static Debug|ARM64`,
`Static Release|ARM64`) for the library and for the C command-line utilities.
Output goes to `build_windows\ARM64\<Configuration>\`, mirroring the `x64`
layout described in
<a href="win_build64.md" class="xref" title="Building Berkeley DB for 64-bit Windows">Building Berkeley DB for 64-bit Windows</a>.

### Requirements

Visual Studio 2022 (or the standalone Build Tools) with:

- the **Desktop development with C++** workload,
- **MSVC v143 - VS 2022 C++ ARM64 build tools**
  (`Microsoft.VisualStudio.Component.VC.Tools.ARM64`),
- a **Windows 10 or 11 SDK**.

### Building

The solution is a VS2010-era project and declares no `<PlatformToolset>`, so the
toolset and SDK version must be supplied on the command line; otherwise msbuild
defaults to the VS2010 toolset and fails with `MSB8020`. From a shell where the
ARM64 cross-tools are active (`vcvarsall.bat x64_arm64`, or the
"ARM64 Native/Cross Tools Command Prompt"):

```
cd build_windows
msbuild Berkeley_DB_vs2010.sln /m /t:db ^
    /p:Configuration=Release /p:Platform=ARM64 ^
    /p:PlatformToolset=v143 /p:WindowsTargetPlatformVersion=10.0
```

Substitute a utility name for `db` to build a single tool, or omit `/t:` to build
everything the `ARM64` solution configuration covers. Building from the IDE works
too: select `ARM64` in the Platform dropdown, and retarget the solution when
prompted.

Because ARM64 build tools are available on x64 hosts, this cross-compiles from
an ordinary x64 machine as well as building natively on Windows-on-ARM.

Verify what you produced with `dumpbin`; an ARM64 image reports `AA64`:

```
dumpbin /headers build_windows\ARM64\Release\libdb53.dll | findstr machine
            AA64 machine (ARM64)
```

### Scope and status

The `ARM64` configurations cover the library (`db`) and these utilities:
`db_archive`, `db_checkpoint`, `db_deadlock`, `db_dump`, `db_hotbackup`,
`db_load`, `db_log_verify`, `db_printlog`, `db_recover`, `db_replicate`,
`db_stat`, `db_tuner`, `db_upgrade`, `db_verify`.

The Java, Tcl, SQL, PHP, C++ STL and example projects remain x64-only; they
depend on external SDKs that are not part of a cross-build.

> **Note**
>
> The ARM64 configurations are verified to **build**, and the resulting binaries
> are confirmed to be genuine ARM64 images (`AA64`), but they have **not been
> exercised at runtime** and no test suite has been run against them. Treat
> Windows ARM64 as untested: validate it in your own environment before relying
> on it. In particular, Berkeley DB's mutex and memory-ordering code has not been
> tested on a weakly-ordered Windows machine.

### Maintaining the configurations

`dist/win_arm64_configs.py` generates the ARM64 configurations by duplicating
each `x64` configuration in the solution, the affected `.vcxproj` files and
`VS10\library.props` / `VS10\application.props`, substituting
`/machine:arm64` for `/machine:x64`. After adding a project that should build for
ARM64, add its name to `ARM64_PROJECTS` in that script and re-run it:

```
python3 dist/win_arm64_configs.py
```

`python3 dist/win_arm64_configs.py --check` reports whether any file is missing
its ARM64 configurations without modifying anything.
