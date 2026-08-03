# Thin root Meson entry point. Meson requires meson.build + meson_options.txt
# at the setup directory, so the project()/options declaration lives here; the
# actual build logic is in dist/meson.build (entered via subdir below). This
# keeps `meson setup <builddir>` and the nix flake (src = ./.) working with no
# argument change, while the build recipe itself lives under dist/ alongside
# the Autoconf build.
#
#   meson setup build        # configures (Ninja backend by default)
#   ninja -C build           # parallel build -> libdb
#   ninja -C build docs      # render docs_src/ -> docs-build/
#   ninja -C build bench     # build the test/bench microbenchmark drivers

project('libdb', 'c',
  version: '5.3.34',
  license: 'AGPL-3.0-or-later OR Sleepycat',
  meson_version: '>=0.56.0',
  default_options: ['c_std=c99', 'warning_level=1'])

cc = meson.get_compiler('c')

# Berkeley DB's public db.h uses BSD type names (u_int, u_char, ...). glibc
# only exposes those under _DEFAULT_SOURCE/_BSD_SOURCE; macOS/*BSD provide
# them unconditionally, so this is a harmless no-op there.
add_project_arguments('-D_DEFAULT_SOURCE', '-D_BSD_SOURCE', language: 'c')

# The real build: defines cc-derived config, generated headers, libdb, and the
# docs/bench run-targets. Variables it sets (inc, db_h, db_int_h, db_int_def_h,
# thread_dep, libdb) stay in scope here, since subdir() shares the scope.
subdir('dist')

# PBT tier (Hegel property-based tests) -- opt-in, off by default. Lives in
# test/pbt; entered here (not from dist/) because meson's subdir() cannot use
# '..' to climb back out of dist/.
if not get_option('hegel').disabled()
  subdir('test/pbt')
endif
