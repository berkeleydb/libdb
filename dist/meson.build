# Real Meson build logic for Berkeley DB (libdb) -- entered via subdir('dist')
# from the thin root meson.build (which owns project()/options; Meson requires
# meson.build + meson_options.txt at the setup root). Everything here is
# anchored at the project root through `root` ('..'), so the sources under
# src/, the templates under src/dbinc*, and dist/clib_port.in all resolve.
#
# Scope: builds the core C library (B-tree/Hash/Heap/Queue/Recno, lock, log,
# mpool, txn, replication, crypto, compression) on POSIX (Linux/macOS/*BSD).
# The Autoconf build (build_unix) remains the reference and covers the language
# bindings (C++, Java, Tcl, SQL) and non-POSIX platforms.
#
# `cc` and `project()` are set by the root shim before this file is entered.

root = '..'   # this file lives in dist/; project root is one level up.

# ---------------------------------------------------------------------------
# Platform configuration -> db_config.h (mirrors what dist/configure detects).
# ---------------------------------------------------------------------------
cdata = configuration_data()

# Feature set (full library build). Toggle with -Dfeature=false if desired.
#
# U6: HAVE_DBM and HAVE_ATOMICFILEREAD are NOT in this list, because autoconf
# does not define them by default -- dbm is opt-in (--enable-dbm, or implied by
# --enable-tcl) and atomicfileread defaults to "no" and is only turned on for
# platforms known to read and write files atomically
# (dist/aclocal/options.m4:395).  Hardcoding them here made every meson build
# differ from every autoconf build in two more capability bits.
foreach feat : [
    'HAVE_HASH', 'HAVE_HEAP', 'HAVE_QUEUE', 'HAVE_VERIFY', 'HAVE_STATISTICS',
    'HAVE_REPLICATION', 'HAVE_REPLICATION_THREADS', 'HAVE_CRYPTO',
    'HAVE_COMPRESSION', 'HAVE_PARTITION', 'HAVE_MUTEX_SUPPORT',
    'HAVE_SHARED_LATCHES', 'HAVE_64BIT_TYPES',
    'HAVE_LOG_CHECKSUM', 'HAVE_UPGRADE_SUPPORT',
    'HAVE_SIMPLE_THREAD_TYPE', 'HAVE_SYSTEM_INCLUDE_FILES', 'STDC_HEADERS']
  cdata.set(feat, 1)
endforeach

# POSIX threads provide our mutex/latch implementation.
thread_dep = dependency('threads')
cdata.set('HAVE_MUTEX_PTHREADS', 1)
cdata.set('HAVE_PTHREAD_SELF', 1)

# Headers.
foreach h : [
    'dirent.h', 'dlfcn.h', 'execinfo.h', 'inttypes.h', 'stdint.h', 'stdio.h',
    'stdlib.h', 'string.h', 'strings.h', 'sys/select.h', 'sys/socket.h',
    'sys/stat.h', 'sys/time.h', 'sys/types.h', 'time.h', 'unistd.h',
    # U6: autoconf checks sys/random.h; db_int.in includes it under
    # HAVE_GETRANDOM && HAVE_SYS_RANDOM_H, so omitting it here meant
    # getrandom(2) was declared nowhere in a meson build.
    'sys/random.h']
  if cc.has_header(h)
    cdata.set('HAVE_' + h.underscorify().to_upper(), 1)
  endif
endforeach

# Functions.
foreach fn : [
    'abort', 'atoi', 'atol', 'backtrace', 'backtrace_symbols', 'bsearch',
    'clock_gettime', 'ctime_r', 'fchmod', 'fclose', 'fcntl', 'fdatasync',
    'fgetc', 'fgets', 'fopen', 'ftruncate', 'fwrite', 'getaddrinfo', 'getcwd',
    'getenv', 'getgid', 'getopt', 'getrusage', 'gettimeofday', 'getuid',
    'hstrerror', 'isalpha', 'isdigit', 'isprint', 'isspace', 'localtime',
    'memcmp', 'memcpy', 'memmove', 'mlock', 'mmap', 'mprotect', 'munlock',
    'munmap', 'pread', 'printf', 'pwrite', 'qsort', 'raise', 'rand', 'random',
    'sched_yield', 'select', 'setgid', 'setuid', 'shmget', 'sigaction',
    'snprintf', 'stat', 'strcasecmp', 'strcat', 'strchr', 'strdup', 'strerror',
    'strftime', 'strncat', 'strncmp', 'strrchr', 'strsep', 'strtol', 'strtoul',
    'sysconf', 'time', 'vsnprintf',
    # U6: autoconf checks these two (dist/configure.ac AC_CHECK_FUNCS) and the
    # meson build did not, so os_csprng.c silently fell back to a weaker source
    # under meson while the autoconf build used getrandom(2).
    'getrandom', 'arc4random_buf']
  if cc.has_function(fn)
    cdata.set('HAVE_' + fn.to_upper(), 1)
  endif
endforeach

# Misc capability defines.

# cc.run()/cc.links() do NOT inherit add_project_arguments, so every probe that
# touches a POSIX declaration needs the project's feature macros passed
# explicitly -- otherwise the probe compiles under different rules than the
# library does, and answers a question about a dialect nothing is built in.
probe_args = ['-D_DEFAULT_SOURCE', '-D_BSD_SOURCE', '-D_GNU_SOURCE',
    '-D_REENTRANT']

# U6: feature parity with the autoconf build.  Each probe below mirrors a
# specific autoconf test, and all of them were MISSING from this file -- so
# meson produced a library with different capabilities AND a different
# __env_struct_sig() to the autoconf one, and an environment created by either
# could not be attached by the other ("BDB1539 Build signature doesn't match
# environment").  Keep these in step with dist/configure.ac and
# dist/aclocal/mutex.m4.

# Atomic operations: dist/aclocal/mutex.m4's db_cv_atomic "gcc/__atomic" tier,
# which is the link test reproduced verbatim here.
if cc.links('''#include <stdint.h>
int main(void) {
	uint32_t val32 = 1, exp32 = 1;
	intptr_t valp = 1, expp = 1;
	__atomic_add_fetch(&val32, 1, __ATOMIC_SEQ_CST);
	__atomic_compare_exchange_n(&val32, &exp32, 2, 0,
	    __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
	__atomic_compare_exchange_n(&valp, &expp, 2, 0,
	    __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
	return (0);
}''', args: probe_args, name: 'GCC __atomic builtins')
  cdata.set('HAVE_ATOMIC_SUPPORT', 1)
  cdata.set('HAVE_ATOMIC_BUILTINS', 1)
  cdata.set('HAVE_ATOMIC_GCC_BUILTIN', 1)
  if cc.sizeof('void *') >= 8
    cdata.set('HAVE_ATOMIC_64BIT', 1)
  endif
endif

# io_uring: header AND the library symbol, as AC_CHECK_LIB(uring, ...) does.
# os_aio_uring.c is in the source list unconditionally and is whole-file guarded
# on HAVE_IO_URING, so without this it compiled to nothing and the io_uring
# backend was simply absent from a meson build.
uring_dep = dependency('liburing', required: false)
if not uring_dep.found()
  uring_dep = cc.find_library('uring', required: false)
endif
if cc.has_header('liburing.h') and uring_dep.found() and \
    cc.has_function('io_uring_queue_init', dependencies: uring_dep)
  cdata.set('HAVE_IO_URING', 1)
else
  uring_dep = dependency('', required: false)
endif

# POSIX aio: aio_read may be in libc, librt or libaio.
aio_dep = dependency('', required: false)
if cc.has_header('aio.h')
  if cc.has_function('aio_read')
    cdata.set('HAVE_AIO_POSIX', 1)
  else
    foreach l : ['rt', 'aio']
      cand = cc.find_library(l, required: false)
      if cand.found() and cc.has_function('aio_read', dependencies: cand)
        cdata.set('HAVE_AIO_POSIX', 1)
        aio_dep = cand
        break
      endif
    endforeach
  endif
endif

# Thread-pool AIO offload: autoconf defines this whenever pthread.h exists.
if cc.has_header('pthread.h')
  cdata.set('HAVE_AIO_THREADPOOL', 1)
endif

# BSD kqueue+aio needs BOTH EVFILT_AIO and sigev_notify_kqueue; macOS has the
# former only, which is why autoconf tests the member as well.
if cc.has_member('struct sigevent', 'sigev_notify_kqueue',
    prefix: '#include <signal.h>\n#include <aio.h>') and \
    cc.has_header_symbol('sys/event.h', 'EVFILT_AIO')
  cdata.set('HAVE_AIO_KQUEUE', 1)
endif

# SHM_LOCK for shmctl(2).
if cc.has_header_symbol('sys/shm.h', 'SHM_LOCK')
  cdata.set('HAVE_SHMCTL_SHM_LOCK', 1)
endif

# Test-and-set mutexes in x86-64 inline assembly, and the HYBRID configuration
# that follows from having BOTH a pthreads mutex and a TAS one
# (dist/aclocal/mutex.m4: `if test "$hybrid" = pthread/tas').  Missing these
# meant a meson build used pure pthread mutexes where the autoconf build used
# the hybrid spin-then-block path -- a different mutex implementation, not just
# a different #define, and one of the layout inputs to __env_struct_sig().
if host_machine.cpu_family() == 'x86_64' and cc.links('''
int main(void) {
	unsigned char tsl = 0;
	__asm__ volatile("movl $1,%%eax; lock; xchgb %1,%%al; xorl $1,%%eax"
	    : "=&qa" (tsl) : "m" (tsl) : "memory", "cc");
	return (tsl);
}''', args: probe_args, name: 'x86_64 gcc-assembly TAS mutex')
  cdata.set('HAVE_MUTEX_X86_64_GCC_ASSEMBLY', 1)
  # pthreads is unconditional above, so pthread + tas == hybrid.
  cdata.set('HAVE_MUTEX_HYBRID', 1)
endif

# Whether a pthread cond/rwlock may be re-initialised over a previously
# initialised one (dist/aclocal/mutex.m4's AM_PTHREADS_CONDVAR_DUPINITCHK and
# AM_PTHREADS_RWLOCKVAR_DUPINITCHK).  These gate failchk recovery paths.
#
# autoconf uses AC_RUN_IFELSE, not a link test: the question is whether the
# second init SUCCEEDS at runtime, and the program exits non-zero when it does
# not.  A link test answers "does the symbol exist", which is always yes and
# therefore the wrong question -- getting this wrong the first time set the cond
# flag and left the rwlock flag unset, which is a divergence of its own.
# cc.run() ABORTS a cross build ("Can not run test applications in this cross
# environment"), so the run tests are guarded on meson.is_cross_build() and a
# cross build uses a link test instead -- which is exactly what autoconf's third
# AC_RUN_IFELSE argument does for the same reason.
cond_src = '''#include <stdlib.h>
#include <pthread.h>
int main(void) {
	pthread_cond_t cond;
	pthread_condattr_t condattr;
	exit(pthread_condattr_init(&condattr) ||
	    pthread_cond_init(&cond, &condattr) ||
	    pthread_cond_init(&cond, &condattr));
}'''
if meson.is_cross_build()
  if cc.links(cond_src, dependencies: thread_dep, args: probe_args,
      name: 'pthread_cond re-init (link only, cross)')
    cdata.set('HAVE_PTHREAD_COND_REINIT_OKAY', 1)
  endif
else
  cond_reinit = cc.run(cond_src, dependencies: thread_dep, args: probe_args,
      name: 'pthread_cond re-init')
  if cond_reinit.compiled() and (cond_reinit.returncode() == 0)
    cdata.set('HAVE_PTHREAD_COND_REINIT_OKAY', 1)
  endif
endif

rwlock_src = '''#include <stdlib.h>
#include <pthread.h>
int main(void) {
	pthread_rwlock_t rwlock;
	pthread_rwlockattr_t rwlockattr;
	exit(pthread_rwlockattr_init(&rwlockattr) ||
	    pthread_rwlock_init(&rwlock, &rwlockattr) ||
	    pthread_rwlock_init(&rwlock, &rwlockattr));
}'''
if meson.is_cross_build()
  if cc.links(rwlock_src, dependencies: thread_dep, args: probe_args,
      name: 'pthread_rwlock re-init (link only, cross)')
    cdata.set('HAVE_PTHREAD_RWLOCK_REINIT_OKAY', 1)
  endif
else
  rwlock_reinit = cc.run(rwlock_src, dependencies: thread_dep, args: probe_args,
      name: 'pthread_rwlock re-init')
  if rwlock_reinit.compiled() and (rwlock_reinit.returncode() == 0)
    cdata.set('HAVE_PTHREAD_RWLOCK_REINIT_OKAY', 1)
  endif
endif

if cc.has_header_symbol('time.h', 'CLOCK_MONOTONIC')
  cdata.set('HAVE_CLOCK_MONOTONIC', 1)
endif
if cc.has_header_symbol('stdlib.h', 'EXIT_SUCCESS')
  cdata.set('HAVE_EXIT_SUCCESS', 1)
endif
if cc.has_member('struct stat', 'st_blksize', prefix: '#include <sys/stat.h>')
  cdata.set('HAVE_STRUCT_STAT_ST_BLKSIZE', 1)
endif
if host_machine.system() == 'darwin'
  cdata.set('HAVE_GETOPT_OPTRESET', 1)   # BSD getopt has optreset
endif
cdata.set('HAVE_FCNTL_F_SETFD', 1)

# Type sizes.
cdata.set('SIZEOF_CHAR', cc.sizeof('char'))
cdata.set('SIZEOF_UNSIGNED_CHAR', cc.sizeof('unsigned char'))
cdata.set('SIZEOF_SHORT', cc.sizeof('short'))
cdata.set('SIZEOF_UNSIGNED_SHORT', cc.sizeof('unsigned short'))
cdata.set('SIZEOF_INT', cc.sizeof('int'))
cdata.set('SIZEOF_UNSIGNED_INT', cc.sizeof('unsigned int'))
cdata.set('SIZEOF_LONG', cc.sizeof('long'))
cdata.set('SIZEOF_UNSIGNED_LONG', cc.sizeof('unsigned long'))
cdata.set('SIZEOF_LONG_LONG', cc.sizeof('long long'))
cdata.set('SIZEOF_UNSIGNED_LONG_LONG', cc.sizeof('unsigned long long'))
cdata.set('SIZEOF_SIZE_T', cc.sizeof('size_t'))
cdata.set('SIZEOF_CHAR_P', cc.sizeof('void *'))

# Package strings.
cdata.set_quoted('PACKAGE_NAME', 'Berkeley DB')
cdata.set_quoted('PACKAGE_TARNAME', 'db')
cdata.set_quoted('PACKAGE_VERSION', '2026.10.1')
cdata.set_quoted('PACKAGE_STRING', 'libdb 2026.10.1')
cdata.set_quoted('PACKAGE_BUGREPORT', '')
cdata.set_quoted('PACKAGE_URL', '')
cdata.set_quoted('LT_OBJDIR', '.libs/')

configure_file(output: 'db_config.h', configuration: cdata)

# ---------------------------------------------------------------------------
# Public/internal headers generated from templates (db.in, db_int.in).
# ---------------------------------------------------------------------------
py = find_program('python3')
gen_header = files('meson/gen_header.py')
db_subs = files('meson/db_subs.json')

# db.h = db.in + api_flags.in + ext_def.in + ext_prot.in (as dist/configure does).
db_h = custom_target('db.h', output: 'db.h',
  input: [root / 'src/dbinc/db.in', root / 'src/dbinc_auto/api_flags.in',
          root / 'src/dbinc_auto/ext_def.in', root / 'src/dbinc_auto/ext_prot.in'],
  command: [py, gen_header, '@OUTPUT@', db_subs, '@INPUT@'],
  depend_files: db_subs)

db_int_def_h = custom_target('db_int_def.h', output: 'db_int_def.h',
  input: root / 'src/dbinc_auto/int_def.in',
  command: [py, gen_header, '@OUTPUT@', db_subs, '@INPUT@'], depend_files: db_subs)
db_int_h = custom_target('db_int.h', output: 'db_int.h',
  input: root / 'src/dbinc/db_int.in',
  command: [py, gen_header, '@OUTPUT@', db_subs, '@INPUT@'], depend_files: db_subs)

# Per-port C library shims (printf formats for the 64-bit seq type).
clib_port_h = custom_target('clib_port.h', output: 'clib_port.h',
  input: 'clib_port.in',
  command: [py, gen_header, '@OUTPUT@', db_subs, '@INPUT@'], depend_files: db_subs)

# Include search: current source dir (dist/) + current build dir (holds the
# generated db.h/db_int*.h/db_config.h under builddir/dist/) are added by Meson
# implicitly; add the project root and the src trees explicitly.
inc = include_directories('.', root, root / 'src', root / 'src/dbinc')

# Source files, listed relative to the project root and anchored via `root`.
libdb_srcnames = [
  'src/btree/bt_compact.c',
  'src/btree/bt_compare.c',
  'src/btree/bt_compress.c',
  'src/btree/bt_conv.c',
  'src/btree/bt_curadj.c',
  'src/btree/bt_cursor.c',
  'src/btree/bt_delete.c',
  'src/btree/bt_method.c',
  'src/btree/bt_open.c',
  'src/btree/bt_put.c',
  'src/btree/bt_rec.c',
  'src/btree/bt_reclaim.c',
  'src/btree/bt_recno.c',
  'src/btree/bt_rsearch.c',
  'src/btree/bt_search.c',
  'src/btree/bt_split.c',
  'src/btree/bt_stat.c',
  'src/btree/bt_upgrade.c',
  'src/btree/bt_verify.c',
  'src/btree/btree_auto.c',
  'src/btree/btree_autop.c',
  'src/common/clock.c',
  'src/common/db_byteorder.c',
  'src/common/db_compint.c',
  'src/common/db_err.c',
  'src/common/db_getlong.c',
  'src/common/db_idspace.c',
  'src/common/db_log2.c',
  'src/common/db_shash.c',
  'src/common/dbt.c',
  'src/common/mkpath.c',
  'src/common/openflags.c',
  'src/common/os_method.c',
  'src/common/util_cache.c',
  'src/common/util_log.c',
  'src/common/util_sig.c',
  'src/common/zerofill.c',
  'src/crypto/aes_method.c',
  'src/crypto/crypto.c',
  'src/crypto/rijndael/rijndael-alg-fst.c',
  'src/crypto/rijndael/rijndael-api-fst.c',
  'src/crypto/mersenne/mt19937db.c',
  'src/db/crdel_auto.c',
  'src/db/crdel_autop.c',
  'src/db/crdel_rec.c',
  'src/db/db_am.c',
  'src/db/db_auto.c',
  'src/db/db_autop.c',
  'src/db/db_backup.c',
  'src/db/db_cam.c',
  'src/db/db_cds.c',
  'src/db/db_compact.c',
  'src/db/db_conv.c',
  'src/db/db_copy.c',
  'src/db/db_dispatch.c',
  'src/db/db_dup.c',
  'src/db/db_iface.c',
  'src/db/db_join.c',
  'src/db/db_meta.c',
  'src/db/db_method.c',
  'src/db/db_open.c',
  'src/db/db_overflow.c',
  'src/db/db_ovfl_vrfy.c',
  'src/db/db_pr.c',
  'src/db/db_rec.c',
  'src/db/db_reclaim.c',
  'src/db/db_remove.c',
  'src/db/db_rename.c',
  'src/db/db_ret.c',
  'src/db/db_setid.c',
  'src/db/db_setlsn.c',
  'src/db/db_sort_multiple.c',
  'src/db/db_stati.c',
  'src/db/db_truncate.c',
  'src/db/db_upg_opd.c',
  'src/db/db_upg.c',
  'src/db/db_vrfy.c',
  'src/db/db_vrfyutil.c',
  'src/db/db.c',
  'src/db/partition.c',
  'src/dbreg/dbreg_auto.c',
  'src/dbreg/dbreg_autop.c',
  'src/dbreg/dbreg_rec.c',
  'src/dbreg/dbreg_stat.c',
  'src/dbreg/dbreg_util.c',
  'src/dbreg/dbreg.c',
  'src/env/env_alloc.c',
  'src/env/env_backup.c',
  'src/env/env_config.c',
  'src/env/env_failchk.c',
  'src/env/env_file.c',
  'src/env/env_globals.c',
  'src/env/env_method.c',
  'src/env/env_name.c',
  'src/env/env_open.c',
  'src/env/env_recover.c',
  'src/env/env_region.c',
  'src/env/env_register.c',
  'src/env/env_sig.c',
  'src/env/env_stat.c',
  'src/fileops/fileops_auto.c',
  'src/fileops/fileops_autop.c',
  'src/fileops/fop_basic.c',
  'src/fileops/fop_rec.c',
  'src/fileops/fop_util.c',
  'src/hash/hash_auto.c',
  'src/hash/hash_autop.c',
  'src/hash/hash_compact.c',
  'src/hash/hash_conv.c',
  'src/hash/hash_dup.c',
  'src/hash/hash_func.c',
  'src/hash/hash_meta.c',
  'src/hash/hash_method.c',
  'src/hash/hash_open.c',
  'src/hash/hash_page.c',
  'src/hash/hash_rec.c',
  'src/hash/hash_reclaim.c',
  'src/hash/hash_stat.c',
  'src/hash/hash_upgrade.c',
  'src/hash/hash_verify.c',
  'src/hash/hash.c',
  'src/heap/heap_auto.c',
  'src/heap/heap_autop.c',
  'src/heap/heap_backup.c',
  'src/heap/heap_conv.c',
  'src/heap/heap_method.c',
  'src/heap/heap_open.c',
  'src/heap/heap_rec.c',
  'src/heap/heap_reclaim.c',
  'src/heap/heap_stat.c',
  'src/heap/heap_verify.c',
  'src/heap/heap.c',
  'src/hmac/hmac.c',
  'src/hmac/sha1.c',
  'src/lock/lock_deadlock.c',
  'src/lock/lock_failchk.c',
  'src/lock/lock_id.c',
  'src/lock/lock_list.c',
  'src/lock/lock_method.c',
  'src/lock/lock_region.c',
  'src/lock/lock_stat.c',
  'src/lock/lock_timer.c',
  'src/lock/lock_util.c',
  'src/lock/lock.c',
  'src/log/log_archive.c',
  'src/log/log_compare.c',
  'src/log/log_debug.c',
  'src/log/log_get.c',
  'src/log/log_method.c',
  'src/log/log_print.c',
  'src/log/log_put.c',
  'src/log/log_stat.c',
  'src/log/log_verify_auto.c',
  'src/log/log_verify_int.c',
  'src/log/log_verify_util.c',
  'src/log/log_verify.c',
  'src/log/log.c',
  'src/mp/mp_alloc.c',
  'src/mp/mp_backup.c',
  'src/mp/mp_bh.c',
  'src/mp/mp_fget.c',
  'src/mp/mp_fmethod.c',
  'src/mp/mp_fopen.c',
  'src/mp/mp_fput.c',
  'src/mp/mp_fset.c',
  'src/mp/mp_method.c',
  'src/mp/mp_mvcc.c',
  'src/mp/mp_region.c',
  'src/mp/mp_register.c',
  'src/mp/mp_resize.c',
  'src/mp/mp_stat.c',
  'src/mp/mp_sync.c',
  'src/mp/mp_trickle.c',
  'src/mutex/mut_alloc.c',
  'src/mutex/mut_order.c',
  'src/mutex/mut_failchk.c',
  'src/mutex/mut_method.c',
  'src/mutex/mut_pthread.c',
  'src/mutex/mut_region.c',
  'src/mutex/mut_stat.c',
  'src/os/os_abort.c',
  'src/os/os_abs.c',
  'src/os/os_addrinfo.c',
  'src/os/os_aio.c',
  'src/os/os_aio_iocp.c',
  'src/os/os_aio_kqueue.c',
  'src/os/os_aio_pool.c',
  'src/os/os_aio_posix.c',
  'src/os/os_aio_uring.c',
  'src/os/os_alloc.c',
  'src/os/os_atomic.c',
  'src/os/os_clock.c',
  'src/os/os_config.c',
  'src/os/os_csprng.c',
  'src/os/os_cpu.c',
  'src/os/os_ctime.c',
  'src/os/os_dir.c',
  'src/os/os_errno.c',
  'src/os/os_fid.c',
  'src/os/os_flock.c',
  'src/os/os_fsync.c',
  'src/os/os_getenv.c',
  'src/os/os_handle.c',
  'src/os/os_map.c',
  'src/os/os_mkdir.c',
  'src/os/os_open.c',
  'src/os/os_path.c',
  'src/os/os_pid.c',
  'src/os/os_rename.c',
  'src/os/os_root.c',
  'src/os/os_rpath.c',
  'src/os/os_rw.c',
  'src/os/os_seek.c',
  'src/os/os_stack.c',
  'src/os/os_stat.c',
  'src/os/os_tmpdir.c',
  'src/os/os_truncate.c',
  'src/os/os_uid.c',
  'src/os/os_unlink.c',
  'src/os/os_yield.c',
  'src/qam/qam_auto.c',
  'src/qam/qam_autop.c',
  'src/qam/qam_conv.c',
  'src/qam/qam_files.c',
  'src/qam/qam_method.c',
  'src/qam/qam_open.c',
  'src/qam/qam_rec.c',
  'src/qam/qam_stat.c',
  'src/qam/qam_upgrade.c',
  'src/qam/qam_verify.c',
  'src/qam/qam.c',
  'src/rep/rep_automsg.c',
  'src/rep/rep_backup.c',
  'src/rep/rep_elect.c',
  'src/rep/rep_lease.c',
  'src/rep/rep_log.c',
  'src/rep/rep_method.c',
  'src/rep/rep_record.c',
  'src/rep/rep_region.c',
  'src/rep/rep_stat.c',
  'src/rep/rep_util.c',
  'src/rep/rep_verify.c',
  'src/repmgr/repmgr_auto.c',
  'src/repmgr/repmgr_automsg.c',
  'src/repmgr/repmgr_autop.c',
  'src/repmgr/repmgr_elect.c',
  'src/repmgr/repmgr_method.c',
  'src/repmgr/repmgr_msg.c',
  'src/repmgr/repmgr_net.c',
  'src/repmgr/repmgr_posix.c',
  'src/repmgr/repmgr_queue.c',
  'src/repmgr/repmgr_rec.c',
  'src/repmgr/repmgr_sel.c',
  'src/repmgr/repmgr_stat.c',
  'src/repmgr/repmgr_util.c',
  'src/sequence/seq_stat.c',
  'src/sequence/sequence.c',
  'src/txn/txn_auto.c',
  'src/txn/txn_autop.c',
  'src/txn/txn_chkpt.c',
  'src/txn/txn_failchk.c',
  'src/txn/txn_method.c',
  'src/txn/txn_rec.c',
  'src/txn/txn_recover.c',
  'src/txn/txn_region.c',
  'src/txn/txn_stat.c',
  'src/txn/txn_util.c',
  'src/txn/txn.c',
  'src/xa/xa_map.c',
  'src/xa/xa.c',
]

# mut_tas.c implements the test-and-set mutexes and has NO whole-file #ifdef --
# it is compiled only when a TAS implementation was selected, which is how
# autoconf treats it (ADDITIONAL_OBJS="mut_tas$o ..." inside the matching arm of
# dist/aclocal/mutex.m4).  Adding it unconditionally builds it on platforms where
# struct __db_mutex_t has no `tas' or `sharecount' member, which is exactly how
# macOS failed: "no member named 'sharecount' in 'struct __db_mutex_t'".
if cdata.has('HAVE_MUTEX_X86_64_GCC_ASSEMBLY')
  libdb_srcnames += 'src/mutex/mut_tas.c'
endif

libdb_sources = []
foreach s : libdb_srcnames
  libdb_sources += files(root / s)
endforeach


# clib/ holds portable fallbacks for libc functions; build each only when the
# platform lacks it (snprintf is always built -- BDB's portable formatter).
clib_map = {
  'atoi':'atoi', 'atol':'atol', 'bsearch':'bsearch', 'getcwd':'getcwd',
  'getopt':'getopt', 'isalpha':'isalpha', 'isdigit':'isdigit',
  'isprint':'isprint', 'isspace':'isspace', 'memcmp':'memcmp',
  'memmove':'memmove', 'printf':'printf', 'qsort':'qsort', 'raise':'raise',
  'rand':'rand', 'strcasecmp':'strcasecmp', 'strcat':'strcat',
  'strchr':'strchr', 'strdup':'strdup', 'strerror':'strerror',
  'strncat':'strncat', 'strncmp':'strncmp', 'strrchr':'strrchr',
  'strsep':'strsep', 'strtol':'strtol', 'strtoul':'strtoul', 'time':'time'}
clib_sources = [files(root / 'src/clib/snprintf.c')]
foreach fname, fn : clib_map
  if not cc.has_function(fn)
    clib_sources += files(root / ('src/clib/' + fname + '.c'))
  endif
endforeach

libdb = library('db', libdb_sources, clib_sources, db_h, db_int_h, db_int_def_h, clib_port_h,
  include_directories: inc,
  dependencies: [thread_dep, uring_dep, aio_dep],
  gnu_symbol_visibility: 'default',
  install: true)

summary({'backend': 'ninja (default)', 'sources': '@0@ files'.format(libdb_srcnames.length())},
  section: 'libdb')

# ---------------------------------------------------------------------------
# Documentation + microbenchmark run-targets (Task: buildable via ninja).
#   ninja docs         render HTML/man(/PDF) from docs_src/ into docs-build/
#   ninja docs-check   run the docs.yml gates locally (build+coverage+spelling)
#   ninja bench        build the test/bench microbenchmark drivers vs this lib
# docs use build.py (needs pandoc); bench links the freshly built libdb.
# ---------------------------------------------------------------------------
run_target('docs',
  command: [py, root / 'dist/meson/run_docs.py', 'build', meson.project_source_root()])
run_target('docs-check',
  command: [py, root / 'dist/meson/run_docs.py', 'check', meson.project_source_root()])
run_target('bench',
  command: [py, root / 'dist/meson/run_bench.py',
            meson.project_source_root(), meson.current_build_dir()],
  depends: libdb)

# ---------------------------------------------------------------------------
# PBT tier: entered from the ROOT shim (subdir cannot reach '../test/pbt' from
# here). The shim runs subdir('test/pbt') after this file defines inc/db_h/etc.
# ---------------------------------------------------------------------------
