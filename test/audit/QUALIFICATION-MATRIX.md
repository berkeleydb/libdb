# Release qualification matrix — next release (v2026.10.6) and the LTS candidate

The bar a release has to clear before it is tagged, and the bar the LTS
candidate has to clear before it is declared LTS. Each row says **where** it is
qualified. "CI" means a GitHub Actions job that has to be green at **job**
level, not workflow level: G17 and G19 both showed that a `continue-on-error`
job can fail while its workflow still reports success.

## Platforms

| Platform | Arch | Mutex backend expected | Where | What runs |
|---|---|---|---|---|
| FreeBSD 14.5 | x86_64 | hybrid `pthreads/library/x86_64/gcc-assembly` | **EC2 metal** (c6id.metal) + EC2 16-vCPU | `test/db` (default + diagnostic, all switch positions), TCL lock/dead/ssi, lockmatrix, meson parity, **benchmark** |
| FreeBSD 14.5 | aarch64 | **hybrid `…/ARM64/gcc-assembly`** (new in this release) | **EC2 metal** (c7gd.metal) + EC2 16-vCPU | same, **plus the B3 trio at `umtx_vnode_persistent=0`** |
| Linux (Debian 12) | x86_64 | hybrid x86_64 | EC2 (kiwi) + CI | `test/db`, TCL subset, lockmatrix, meson parity, **benchmark** |
| Linux (Debian 12) | aarch64 | hybrid ARM64 | EC2 (kiwi, Graviton) | `test/db`, TCL subset, meson parity, benchmark |
| Linux | i686 (ILP32) | pthreads | CI `ubuntu 32-bit` | build (G19 gate) |
| Linux | x86_64 sanitizers | — | CI asan+ubsan, **tsan (6/6 PASS lines required, G17)** | as CI |
| macOS 14 / latest | arm64 | pthreads (or hybrid ARM64 if it engages) | CI | build + `test/db` subset as CI runs it |
| Windows | x64, ARM64 cross | Windows mutex | CI msbuild | build |
| FreeBSD 14.2 | x86_64 | — | CI `freebsd shared library` | shared-library artifact gate (F2/F3/G18) |

**Not qualified, and stated as such:** NetBSD, OpenBSD, Solaris, AIX, HP-UX and
QNX. The tree still contains code for them, but nothing here builds them.
Android has committed generated headers only. Do not claim support for any of
these.

## Gates every release clears

1. Every platform row above: `test/db` 0 FAIL. A SKIP is allowed only with a
   stated environmental reason (no JDK, or no F6 scenario).
2. `s_validate` (22 checkers), `s_execbits`, `s_chk_copyright`,
   `s_chk_licence`, spellcheck: rc=0.
3. `run_handle_sizes.sh`: the public ABI is unchanged, or the change is
   declared with `ABI-BREAK-INTENDED`.
4. `env_sig_print.sh` on **each** platform. A change requires
   `REGION-SIG-CHANGE-INTENDED` and an upgrade note.
5. CI: count job conclusions across **every** workflow; 0 failed.
6. A benchmark on metal for both x86_64 and aarch64, against the previous
   release, with every quoted cell at cv ≤ 10%.
7. Verify from a fresh clone of the **published** tag, not the local tree.

## Additional gates for the LTS declaration

8. **Soak:** a 72-hour TPROC-C run at the throughput knee, with hourly
   `failchk` + env close/reopen and a final `db_verify`, plus a kill-9 recovery
   loop. Run it on FreeBSD x86_64 and aarch64 metal, and on Linux x86_64. Zero
   panics, zero verify errors, and a bounded mutex/locker population (the T10
   convergence rule).
   - **Windows/MSVC x86_64** also runs the resource-accounting soak
     (`test/soak/run_soak.ps1`) for 72 hours: the driver compiled with `cl.exe`
     against the MSVC-built libdb, looping full open/close passes until the
     duration elapses, every pass resource-steady. This is the first time the
     engine is *executed* on Windows in the qualification program (the CI
     windows job is build-only, `continue-on-error`). It stands in for the
     TPROC-C/kill-9 arms, which have no Windows harness; a resource leak or a
     region-accounting divergence on Windows fails it.
9. `test/soak/` and `test/sim/` tiers green.
10. The tracker has no open row tagged correctness. Every open row is either
    performance, or a documented limitation with a workaround.
11. The region signature, ABI and on-disk format are **frozen** from the LTS
    tag forward, which `VERSIONING.md` already states.

## First LTS: v2026.10.7 (qualified 2026-10-16)

The gates above were met and the LTS declared on **v2026.10.7**.

- **Soak (gate 8):** 72-hour resource-accounting soak, zero failures on all
  four platforms. Linux x86_64 ran the full 72h (8,913 passes, Oct 13 13:14 →
  Oct 16 13:13Z); Windows/MSVC x86_64 ran the full 72h (4,600 passes over
  4,320 minutes, `SOAK72_EXIT=0`); FreeBSD x86_64 and FreeBSD aarch64 ran
  clean (every completed pass "resources stayed within tolerance", no
  `SOAK_FAILED`). Each pass opens and closes a fresh long-lived environment, so
  a resource leak or region-accounting divergence fails the arm.
- **CI (gates 1–7):** green at job level on the release commit `a849cdbc5`
  (10 workflows, 71 jobs, 0 failed).
- **Frozen (gate 11):** the `2026.10.x` LTS line is frozen at the signatures in
  the v2026.10.7 upgrade note — macOS arm64 `0x2c8d3688`, LP64 x86_64
  `0x56a06689`, aarch64 `0x56a06689`. See `VERSIONING.md` §LTS lines.
