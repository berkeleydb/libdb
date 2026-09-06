# test/tiers/meson.build -- Meson wiring for the isolation / soak / lock-matrix
# test tiers (B1/B2/B3).
#
# Entered from the ROOT meson.build (not from dist/, because subdir() cannot
# climb back out of dist/ with '..').  The variables it relies on -- `inc`,
# `libdb`, `thread_dep` -- are set by dist/meson.build and stay in scope
# because subdir() shares scope.
#
# These tiers need no library-side hooks: they drive the public DB_ENV / DB API
# only, so there is no build option to enable and nothing is added to the
# library.  The executables are build_by_default: false, so a plain
# `ninja -C build` is unaffected; build them explicitly:
#
#   ninja -C build test/tiers/test_iso_anomaly \
#                  test/tiers/test_soak_resources \
#                  test/tiers/test_lock_matrix
#
# and `meson test -C build --suite tiers` runs the gating tier (B1).  The other
# two are in separate suites because on current master they legitimately fail:
#
#   meson test -C build --suite tiers        # B1, gating
#   meson test -C build --suite tiers-xfail  # B3, reproduces #140
#   meson test -C build --suite tiers-slow   # B2, minutes-long soak
#
# For the ASan build that tier B3 is designed for (the out-of-bounds write is
# inside libdb's own allocation, so libdb itself must be instrumented for a
# precise report), use test/lockmatrix/run.sh, which reuses the
# build_asan_gate/ mechanism.  Note that the corruption is severe enough that
# even an UNinstrumented build trips glibc's own heap checks ("double free or
# corruption"), so this plain-meson run also detects it -- just without the
# faulting line number.

tiers_tests = [
  # name                  source dir     source file             suite args
  ['iso_anomaly',    'isolation',  'test_iso_anomaly.c',    'tiers',       []],
  # B3 aborts on current master: that abort IS the #140 reproduction, so it
  # lives in its own suite rather than failing the gating one.  Move it to
  # 'tiers' in the same PR that fixes #140.
  ['lock_matrix',    'lockmatrix', 'test_lock_matrix.c',    'tiers-xfail', []],
  # The soak is minutes, not seconds: keep it out of the default suite and run
  # a reduced transaction count when it is asked for by name.
  ['soak_resources', 'soak',       'test_soak_resources.c', 'tiers-slow',  ['-n', '2000']],
]

foreach t : tiers_tests
  exe = executable('test_' + t[0],
    files(meson.project_source_root() / 'test' / t[1] / t[2]),
    include_directories: inc,
    link_with: libdb,
    dependencies: thread_dep,
    build_by_default: false,
    install: false)
  test(t[0], exe,
    args: t[4],
    suite: t[3],
    workdir: meson.current_build_dir(),
    timeout: t[3] == 'tiers-slow' ? 1800 : 600)
endforeach
