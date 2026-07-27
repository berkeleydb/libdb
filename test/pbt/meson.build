# test/pbt/meson.build -- Hegel property-based tests (PBT tier).
#
# Gated on the `hegel` feature option (default disabled).  This subdir is
# only entered from the top-level meson.build when -Dhegel is not
# 'disabled', so the core library build is unaffected.
#
# Two modes, chosen by whether hegel-c is actually found:
#
#   * hegel-c found  -> compile with -DPBT_HAVE_HEGEL and link -lhegel
#     -lcbor -lz; the tests draw random inputs and check the properties.
#     Requires the `hegel` server binary at run time (pip install
#     hegel-core), overridable via HEGEL_SERVER_COMMAND.
#
#   * hegel-c NOT found -> build in graceful STUB mode: the same pbt_*.c
#     files compile and LINK against libdb (proving the symbols under
#     test are reachable), then print SKIP and exit 0.
#
# hegel-c (https://github.com/gburd/hegel-c) is a CMake project.  Meson
# cannot FetchContent a CMake project the way CMake does, so we look for
# an installed/available `hegel` dependency; if absent we fall back to
# stub mode rather than failing the build.  To use the real library,
# build+install hegel-c (cmake -S . -B build && cmake --build build) and
# point PKG_CONFIG_PATH / CMAKE_PREFIX_PATH at it, or set
# -Dhegel_include_dir / -Dhegel_lib (see README.md).

pbt_common = files('pbt_common.h')

pbt_props = [
  'log_compare',   # LSN total-order: log_compare() (public db.h)
  'byteswap',      # M_*_SWAP + P_*_COPYSWAP involution (src/dbinc/db_swap.h)
  'defcmp',        # __bam_defcmp total-order over byte strings
  'put_get',       # in-memory B-tree put/get round-trip (end-to-end)
  'hash_model',    # DB_HASH vs in-test model map (stateful)
  'hash_func',     # __ham_func2/3/4/5 determinism + length-honoring + FNV oracle
  'compint',       # varint codec round-trip/order-preserving (src/common/db_compint.c)
  'compress',      # __bam_defcompress/decompress round-trip (src/btree/bt_compress.c)
  'recno',         # recno append/get + DB_RENUMBER contiguity (src/btree/bt_recno.c)
]

# Try to locate hegel-c.  cmake + pkg-config are both attempted; either
# turning up the library flips us into real-PBT mode.
hegel_dep = dependency('hegel', required: false)
if not hegel_dep.found()
  hegel_dep = cc.find_library('hegel', required: false)
endif

pbt_args = ['-D_DEFAULT_SOURCE', '-D_BSD_SOURCE']
pbt_deps = [thread_dep]
have_hegel = false

if hegel_dep.found()
  cbor_dep = dependency('libcbor', required: false)
  if not cbor_dep.found()
    cbor_dep = cc.find_library('cbor', required: false)
  endif
  zlib_dep = dependency('zlib', required: false)
  if not zlib_dep.found()
    zlib_dep = cc.find_library('z', required: false)
  endif
  if cbor_dep.found() and zlib_dep.found()
    have_hegel = true
    pbt_args += '-DPBT_HAVE_HEGEL'
    pbt_deps += [hegel_dep, cbor_dep, zlib_dep]
  endif
endif

if have_hegel
  message('PBT: hegel-c found -> real property-based tests')
else
  message('PBT: hegel-c not found -> stub mode (tests compile+link, then SKIP)')
endif

foreach p : pbt_props
  exe = executable('pbt_' + p, ['pbt_' + p + '.c', db_h, db_int_h, db_int_def_h],
    include_directories: [inc, include_directories('.')],
    link_with: libdb,
    dependencies: pbt_deps,
    c_args: pbt_args,
    build_by_default: true)
  test('pbt_' + p, exe, suite: 'pbt', timeout: 300)
endforeach
