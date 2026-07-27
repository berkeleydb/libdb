# test/sim/meson.build -- Deterministic Simulation Testing (DST) build wiring.
#
# This subdir is fully owned by the DST work.  It is NOT yet entered by the
# top-level meson.build (to avoid clobbering the concurrently-edited
# meson.build / meson_options.txt owned by the PBT effort).  To enable the
# meson DST build, a follow-up adds exactly two things:
#
#   # meson_options.txt
#   option('dst', type: 'boolean', value: false,
#     description: 'Build with Deterministic Simulation Testing hooks.')
#
#   # meson.build (near the `hegel` subdir block)
#   if get_option('dst')
#     add_project_arguments('-DHAVE_DST', language: 'c')
#     libdb_sources += files(
#       'test/sim/sim_core.c', 'test/sim/sim_os_hooks.c')
#     # test/sim must be on the include path for the __os_* hooks:
#     #   add_project_arguments('-I' + meson.project_source_root() / 'test/sim', language: 'c')
#     subdir('test/sim')
#   endif
#
# The `#ifdef HAVE_DST` guards in src/os/os_rw.c and src/os/os_fsync.c work
# unchanged under meson once -DHAVE_DST is set and test/sim is on the include
# path.  The autoconf build (dist/configure --enable-dst) is the primary,
# fully-working path today.

sim_inc = include_directories('.')

# The three pilot scenarios.  They link the shared libdb (built with the DST
# core when -Ddst is set) and the DST include dir.
foreach t : ['test_sim_rng', 'test_sim_crash_recover', 'test_sim_torn']
  exe = executable(t, t + '.c',
    include_directories : [sim_inc, libdb_inc],
    link_with : libdb_shared,
    build_by_default : false,
    install : false)
  test('dst_' + t, exe, timeout : 120)
endforeach
