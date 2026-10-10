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
#   meson test -C build --suite tiers   # isolation + lock-matrix tiers

project('libdb', 'c',
  version: '2026.10.4',
  # Sleepycat, matching LICENSE (byte-identical to Oracle's) and README.md.
  # This field previously named a copyleft licence that this fork
  # deliberately excludes: README.md explains that Berkeley DB releases from
  # 6.0 onward carry one, which is incompatible with redistributing under
  # Sleepycat terms, and are therefore intentionally absent from the
  # historical archive.  This was the only licence metadata in the shipped
  # tree, so the wrong value here was what a packager would have read.
  license: 'Sleepycat',
  meson_version: '>=0.56.0',
  default_options: ['c_std=c99', 'warning_level=1'])

cc = meson.get_compiler('c')

# Berkeley DB's public db.h uses BSD type names (u_int, u_char, ...). glibc
# only exposes those under _DEFAULT_SOURCE/_BSD_SOURCE; macOS/*BSD provide
# them unconditionally, so this is a harmless no-op there.
#
# _GNU_SOURCE and _REENTRANT match the autoconf build's CFLAGS.  Without them,
# c_std=c99 plus _BSD_SOURCE alone leaves glibc hiding POSIX declarations the
# engine uses unconditionally -- pthread_rwlock_t is not even a known type name,
# which silently turned the HAVE_PTHREAD_RWLOCK_REINIT_OKAY probe into "DID NOT
# COMPILE" and so into a false negative. Part of U6: the two build systems must
# agree on their feature macros or they agree on nothing downstream.
add_project_arguments('-D_DEFAULT_SOURCE', '-D_BSD_SOURCE',
    '-D_GNU_SOURCE', '-D_REENTRANT', language: 'c')

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

# Isolation / soak / lock-matrix test tiers (B1/B2/B3) -- test/tiers. Always
# entered: the drivers are build_by_default:false so a plain `ninja` is
# unaffected, and they need no library-side hooks (public API only).
subdir('test/tiers')
