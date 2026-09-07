# Known-slow seeds (not gated)

Seeds here reproduce a *bounded* denial of service: libdb terminates and corrupts
nothing, but a hostile or corrupt file makes it spin far longer than any real
workload would. They live outside `crashes/` because `check-crashes.sh` is a
correctness gate — it must stay green — and these have no fix yet.

Run one by hand:

```sh
cd test/fuzz
ASAN_OPTIONS=detect_leaks=0 \
  ./build/fuzz_dbfile_standalone known-slow/<seed>
```

## `dbfile_dos_qam_extent_scan.seed`

A queue meta page with `first_recno = 4000000000`, `cur_recno = 10`,
`rec_page = 1`, `page_ext = 4`.

The **verify** path is fixed (`__qam_vrfy_walkqueue` now clamps its scan to
`vdp->last_pgno`), and `db_verify` on this file completes instantly. What this
seed still reaches is the **normal cursor read path**, which is a separate
problem:

```
__qamc_get            qam.c:910
 -> __qam_position    qam.c:58     cp->pgno = QAM_RECNO_PAGE(dbp, *recnop)
 -> __qam_fprobe      qam_files.c:265
 -> __memp_fopen      mp_fopen.c:358
 -> __db_appname      env_name.c:165   "./__dbq.fuzz.db.1000525154"
 -> __os_exists       os_stat.c:39     one stat(2) per extent file
```

A cursor walk starting at `first_recno` computes an extent file name per page and
`stat(2)`s it. With `first_recno` near `UINT32_MAX` and one record per page, that
is on the order of four billion filesystem probes, each of which correctly
returns "no such file" and moves to the next recno.

A fix has to bound the read path the way the verify path is now bounded — most
plausibly by rejecting a queue meta page whose `first_recno`/`cur_recno` pair
cannot correspond to any page in the file (or any existing extent) at
`__qam_open`/meta-validation time, rather than discovering it one `stat` at a
time. That is a change to the read path's trust model, not a verify-only clamp,
so it is deliberately not bundled with the verify fix.
