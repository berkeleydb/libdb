# Meson on Windows: why it does not build, and what it would take

Status: **not supported.** `meson setup` configures successfully on Windows with
MSVC, but the build fails at compile time. Autoconf (`dist/configure`) and the
bundled MSVC solution (`build_windows/Berkeley_DB_vs2010.sln`) are the supported
Windows build paths; see `docs_src/guides/installation/build_win.md`.

This file records the exact blockers, in the order the compiler hits them, so the
work can be scoped without rediscovering them. Measured on Windows Server 2022
x64, VS 2022 Build Tools 17.14 (MSVC 19.44.35228), Meson 1.12.0, Ninja 1.12.1.
Each blocker was found by stubbing the previous one out of the *generated*
headers under `<builddir>/dist/` and recompiling.

`meson setup` itself is fine: it detects `cl`, resolves 269 sources and 11
targets, and writes `db_config.h`, `db.h`, `db_int.h`, `db_int_def.h` and
`clib_port.h`. Only compilation fails.

## Blockers, in order

1. **`src/dbinc/db_int.in:49` — `#include <sys/uio.h>`, unconditional.**
   The first failure (`fatal error C1083`). Unlike its neighbours it has *no*
   autoconf probe — there is no `HAVE_SYS_UIO_H` in `dist/config.hin` — so it
   cannot be made conditional without adding one.

   What needs it: only `struct iovec`, for the `readv`/`writev` calls in
   `src/repmgr/repmgr_posix.c:582,605`. Windows already has the equivalent and
   does not need the header at all: `src/dbinc/repmgr.h:110` types `db_iovec_t`
   as `WSABUF`, and `src/repmgr/repmgr_windows.c:595,612` implement
   `__repmgr_writev`/`__repmgr_readv` over `WSASend`/`WSARecv`. Making this
   include conditional is correct and safe — but it is not sufficient, and on its
   own it buys nothing.

2. **`src/dbinc/db_int.in:55-57` — `<netinet/in.h>`, `<netdb.h>`,
   `<arpa/inet.h>`** (inside the `HAVE_REPLICATION_THREADS` block).
   Windows equivalents (`<winsock2.h>`, `<ws2tcpip.h>`) are already included by
   `src/dbinc/win_db.h`, which the `DB_WIN32` branch at `db_int.in:79` pulls in.

3. **`src/dbinc/db_int.in:73` — `<unistd.h>`.**

4. **`src/dbinc/db.in:29-30` — `@unistd_h_decl@` and `@thread_h_decl@`.**
   `dist/meson/db_subs.json` fills these with `#include <unistd.h>` and
   `#include <pthread.h>`. These are template *substitutions*, not source
   `#ifdef`s, so no amount of conditional compilation in the sources fixes them —
   Windows needs its own substitution set.

5. **~102 distinct syntax errors in the generated `db.h`** once the missing
   headers are stubbed out: `db_ssize_t`, `db_threadid_t`, `db_pgno_t`,
   `db_indx_t`, `db_recno_t`, `db_timeout_t`, `u_int32_t` and every struct member
   declared with them (`C2059`/`C2061`).

   Root cause, and the reason this is the expensive blocker:
   `dist/meson/db_subs.json` encodes *POSIX* substitutions. `db_threadid_t_decl`
   is `typedef pthread_t db_threadid_t;`, and the BSD type slots
   (`u_int8_decl`, `u_int16_decl`, `u_int32_decl`, `u_int64_decl`, `ssize_t_decl`,
   `off_t_decl`, `uintptr_t_decl`, `uintmax_t_decl`, `pid_t_decl`, ...) are all
   **empty**, because glibc already declares those types. MSVC declares none of
   them. `dist/s_windows` fills exactly these slots with MSVC-specific text
   (`typedef unsigned int u_int32_t;`, `typedef __int64 int64_t;`, the `_WIN64`
   `ssize_t`/`uintptr_t` branches, the `off_t` -> `__db_off_t` override, the
   `_MSC_VER` warning pragmas). A Windows Meson build needs a `db_subs`
   equivalent of that.

## Further gaps, visible without compiling

`dist/meson.build` is POSIX-shaped beyond the headers:

- Sets `HAVE_SYSTEM_INCLUDE_FILES=1` (line 28) and never defines `DB_WIN32`.
  Both must change together: `db_int.in` selects the Windows include path via
  `#ifdef DB_WIN32` (line 79) only after the
  `#ifdef HAVE_SYSTEM_INCLUDE_FILES` block (line 15) is skipped. Defining
  `DB_WIN32` alone leaves the POSIX includes active — confirmed by probe.
- Hardcodes `dependency('threads')`, `HAVE_MUTEX_PTHREADS` and
  `HAVE_PTHREAD_SELF` (lines 33-35) and compiles `src/mutex/mut_pthread.c`.
  Windows needs `HAVE_MUTEX_WIN32` and `src/mutex/mut_win32.c` (which exists).
- Compiles the POSIX OS layer. **22 files** it lists have `src/os_windows/`
  twins that must be swapped in: `os_abs`, `os_clock`, `os_config`, `os_cpu`,
  `os_csprng`, `os_dir`, `os_errno`, `os_fid`, `os_flock`, `os_fsync`,
  `os_getenv`, `os_handle`, `os_map`, `os_mkdir`, `os_open`, `os_rename`,
  `os_rw`, `os_seek`, `os_stat`, `os_truncate`, `os_unlink`, `os_yield`.
  **18 files** have no Windows twin and must be dropped or otherwise provided:
  `os_abort`, `os_addrinfo`, `os_aio`, `os_aio_iocp`, `os_aio_kqueue`,
  `os_aio_pool`, `os_aio_posix`, `os_aio_uring`, `os_alloc`, `os_atomic`,
  `os_ctime`, `os_path`, `os_pid`, `os_root`, `os_rpath`, `os_stack`,
  `os_tmpdir`, `os_uid`. (The MSVC projects do include `src/os/os_alloc.c`,
  `os_atomic.c` and the IOCP AIO files, so several of these are "keep, they are
  portable" rather than "drop" — the point is that the list needs deciding
  file by file, not that all 18 are unusable.)
- Compiles `src/repmgr/repmgr_posix.c`; Windows needs
  `src/repmgr/repmgr_windows.c`.

## Assessment

**No source portability bug was found.** The Windows plumbing all exists:
`src/dbinc/win_db.h`, `src/os_windows/` (23 files), `src/mutex/mut_win32.c`,
`src/repmgr/repmgr_windows.c`, and the `DB_WIN32` guards in `db_int.in`. Meson
simply does not wire any of it up — `dist/meson.build`'s own header comment
scopes it to "POSIX (Linux/macOS/\*BSD)".

Making it work is a new `host_machine.system() == 'windows'` branch spanning the
config data, a second `db_subs` substitution set mirroring `dist/s_windows`, the
mutex selection, and ~40 source-list decisions — i.e. re-deriving in Meson what
`dist/s_windows` and the `.vcxproj` files already encode. That is a feature, not
a fix, and it is deliberately not attempted. Only step 1 (making `<sys/uio.h>`
conditional, with a proper `HAVE_SYS_UIO_H` probe) is independently worthwhile,
since that include is genuinely unnecessary on any platform that has `WSABUF`.
