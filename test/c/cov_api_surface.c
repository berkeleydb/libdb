/*-
 * See the file LICENSE for redistribution information.
 *
 * cov_api_surface.c --
 *	A direct driver for the DB_ENV / DB / DBC / DB_TXN / DB_MPOOLFILE
 *	*getter* + *callback-setter* surface, and the argument-validation
 *	branches of the corresponding setters.
 *
 *	Why this exists.  Report #3 lists 515 never-called functions.  A large,
 *	cleanly-recoverable slice of them are neither hard nor exotic -- they
 *	are the getters and callback installers of the public API:
 *
 *	  db/db_method.c   (24 never called) __db_get_alloc, __db_get_mpf,
 *	                   __db_get_transactional, __db_get_priority,
 *	                   __db_get_dup_compare, __db_get_encrypt_flags,
 *	                   __db_get_assoc_flags, __db_get_errfile/errcall,
 *	                   __db_get_msgfile/msgcall, __db_get_append_recno,
 *	                   __db_get_feedback, __db_get_create_dir,
 *	                   __db_set_alloc, __db_set_append_recno,
 *	                   __db_set_create_dir, __db_set_feedback,
 *	                   __db_set_lk_exclusive, __db_set_msgcall,
 *	                   __db_set_paniccall, __db_set_priority,
 *	                   __dbh_err, __dbh_errx
 *	  env/env_method.c (21 never called) __env_get_alloc,
 *	                   __env_get_app_dispatch, __env_get_data_len,
 *	                   __env_get_errcall/errfile, __env_get_feedback,
 *	                   __env_get_intermediate_dir_mode,
 *	                   __env_get_isalive, __env_get_memory_max,
 *	                   __env_get_msgcall/msgfile,
 *	                   __env_get_thread_id_fn,
 *	                   __env_get_thread_id_string_fn, __env_set_alloc,
 *	                   __env_set_data_len, __env_set_feedback,
 *	                   __env_set_memory_max, __env_set_paniccall,
 *	                   __env_set_thread_id, __env_set_thread_id_string
 *	  mp/mp_fmethod.c  (8)  the DB_MPOOLFILE getters
 *	  txn/txn.c        (part) __txn_get_priority, __txn_set_commit_token,
 *	                   __txn_build_token, __txn_applied_pp
 *	  common/db_err.c  (10) the error-formatting helpers
 *
 *	The Tcl suite never calls them because the Tcl bindings expose the
 *	setters an access-method test needs and nothing else: a functional test
 *	sets a knob to make a workload behave, and never asks the library what
 *	the knob currently is.  An embedding application does the opposite --
 *	it reads configuration back, installs callbacks, and passes bad
 *	arguments.  That is the surface this drives.
 *
 *	Everything here is single-process, allocation-light and has no timing
 *	dependence, so it is deterministic.  A hard SIGALRM guard backs it up.
 */
#include "db_config.h"

#include "db_int.h"

#include <signal.h>

#define	HOME		"COVAPI_TESTDIR"
#define	ALARM_SECS	120

static int fails = 0;
static int checks = 0;

#define	CHK_OK(call) do {						\
	int _r = (call);						\
	checks++;							\
	if (_r != 0) {							\
		fprintf(stderr, "FAIL: %s:%d: %s => %d (%s)\n",		\
		    __FILE__, __LINE__, #call, _r, db_strerror(_r));	\
		fails++;						\
	}								\
} while (0)

#define	CHK_FAILS(call) do {						\
	int _r = (call);						\
	checks++;							\
	if (_r == 0) {							\
		fprintf(stderr, "FAIL: %s:%d: %s unexpectedly "		\
		    "succeeded\n", __FILE__, __LINE__, #call);		\
		fails++;						\
	}								\
} while (0)

#define	CHK_EQ(got, want, what) do {					\
	checks++;							\
	if ((unsigned long)(got) != (unsigned long)(want)) {		\
		fprintf(stderr, "FAIL: %s:%d: %s: got %lu want %lu\n",	\
		    __FILE__, __LINE__, (what), (unsigned long)(got),	\
		    (unsigned long)(want));				\
		fails++;						\
	}								\
} while (0)

#define	CHK_TRUE(cond, what) do {					\
	checks++;							\
	if (!(cond)) {							\
		fprintf(stderr, "FAIL: %s:%d: %s\n",			\
		    __FILE__, __LINE__, (what));			\
		fails++;						\
	}								\
} while (0)

static void
on_alarm(sig)
	int sig;
{
	COMPQUIET(sig, 0);
	fprintf(stderr, "FAIL: cov_api_surface timed out after %d s\n",
	    ALARM_SECS);
	_exit(3);
}

/* ---- the callbacks we install so the *_get_* getters have something to
 * hand back, and so the set_* installers actually run. ---- */

static int feedback_calls = 0;
static int panic_calls = 0;
static int alloc_calls = 0;
static int isalive_calls = 0;
static int recno_calls = 0;

static void
my_feedback(dbenv, opcode, percent)
	DB_ENV *dbenv;
	int opcode, percent;
{
	COMPQUIET(dbenv, NULL);
	COMPQUIET(opcode, 0);
	COMPQUIET(percent, 0);
	feedback_calls++;
}

static void
my_db_feedback(dbp, opcode, percent)
	DB *dbp;
	int opcode, percent;
{
	COMPQUIET(dbp, NULL);
	COMPQUIET(opcode, 0);
	COMPQUIET(percent, 0);
	feedback_calls++;
}

static void
my_panic(dbenv, errval)
	DB_ENV *dbenv;
	int errval;
{
	COMPQUIET(dbenv, NULL);
	COMPQUIET(errval, 0);
	panic_calls++;
}

static void *
my_malloc(sz)
	size_t sz;
{
	alloc_calls++;
	return (malloc(sz));
}

static void *
my_realloc(p, sz)
	void *p;
	size_t sz;
{
	alloc_calls++;
	return (realloc(p, sz));
}

static void
my_free(p)
	void *p;
{
	free(p);
}

static int
my_isalive(dbenv, pid, tid, flags)
	DB_ENV *dbenv;
	pid_t pid;
	db_threadid_t tid;
	u_int32_t flags;
{
	COMPQUIET(dbenv, NULL);
	COMPQUIET(pid, 0);
	COMPQUIET(flags, 0);
	COMPQUIET(tid, tid);
	isalive_calls++;
	return (1);
}

static void
my_thread_id(dbenv, pidp, tidp)
	DB_ENV *dbenv;
	pid_t *pidp;
	db_threadid_t *tidp;
{
	COMPQUIET(dbenv, NULL);
	if (pidp != NULL)
		*pidp = getpid();
	if (tidp != NULL)
		memset(tidp, 0, sizeof(*tidp));
}

static char *
my_thread_id_string(dbenv, pid, tid, buf)
	DB_ENV *dbenv;
	pid_t pid;
	db_threadid_t tid;
	char *buf;
{
	COMPQUIET(dbenv, NULL);
	COMPQUIET(tid, tid);
	(void)snprintf(buf, DB_THREADID_STRLEN, "%lu", (u_long)pid);
	return (buf);
}

static void
my_errcall(dbenv, prefix, msg)
	const DB_ENV *dbenv;
	const char *prefix, *msg;
{
	COMPQUIET(dbenv, NULL);
	COMPQUIET(prefix, NULL);
	COMPQUIET(msg, NULL);
}

static void
my_msgcall(dbenv, msg)
	const DB_ENV *dbenv;
	const char *msg;
{
	COMPQUIET(dbenv, NULL);
	COMPQUIET(msg, NULL);
}

static int
my_app_dispatch(dbenv, log_rec, lsnp, op)
	DB_ENV *dbenv;
	DBT *log_rec;
	DB_LSN *lsnp;
	db_recops op;
{
	COMPQUIET(dbenv, NULL);
	COMPQUIET(log_rec, NULL);
	COMPQUIET(lsnp, NULL);
	COMPQUIET(op, 0);
	return (0);
}

static void
my_event_notify(dbenv, event, info)
	DB_ENV *dbenv;
	u_int32_t event;
	void *info;
{
	COMPQUIET(dbenv, NULL);
	COMPQUIET(event, 0);
	COMPQUIET(info, NULL);
}

static int
my_append_recno(dbp, data, recno)
	DB *dbp;
	DBT *data;
	db_recno_t recno;
{
	COMPQUIET(dbp, NULL);
	COMPQUIET(data, NULL);
	COMPQUIET(recno, 0);
	recno_calls++;
	return (0);
}

static int
my_dup_compare(dbp, a, b)
	DB *dbp;
	const DBT *a, *b;
{
	size_t len;
	int ret;

	COMPQUIET(dbp, NULL);
	len = a->size < b->size ? a->size : b->size;
	if ((ret = memcmp(a->data, b->data, len)) != 0)
		return (ret);
	return ((int)a->size - (int)b->size);
}

static void
clean_home()
{
	(void)system("rm -f " HOME "/__db.* " HOME "/log.* " HOME "/*.db "
	    HOME "/DB_CONFIG 2>/dev/null");
	(void)system("mkdir -p " HOME);
}

/*
 * env_getters_pre_open --
 *	Every DB_ENV getter on a created-but-unopened handle, each paired with
 *	its setter so the value read back can be checked.  A getter that only
 *	ever runs post-open takes a different branch than pre-open (region vs.
 *	handle), so both are driven -- this is the pre-open half.
 */
static void
env_getters_pre_open(dbenv)
	DB_ENV *dbenv;
{
	void *(*mal) __P((size_t));
	void *(*rea) __P((void *, size_t));
	void (*fre) __P((void *));
	void (*fb) __P((DB_ENV *, int, int));
	int (*ia) __P((DB_ENV *, pid_t, db_threadid_t, u_int32_t));
	void (*tid) __P((DB_ENV *, pid_t *, db_threadid_t *));
	char *(*tids) __P((DB_ENV *, pid_t, db_threadid_t, char *));
	int (*appd) __P((DB_ENV *, DBT *, DB_LSN *, db_recops));
	void (*ec) __P((const DB_ENV *, const char *, const char *));
	void (*mc) __P((const DB_ENV *, const char *));
	FILE *fp;
	const char *cp;
	u_int32_t a, b;
	size_t sz;
	int i;
	long l;
	time_t tv;

	/* --- allocator: __env_set_alloc + __env_get_alloc (both cold). */
	CHK_OK(dbenv->set_alloc(dbenv, my_malloc, my_realloc, my_free));
	mal = NULL; rea = NULL; fre = NULL;
	CHK_OK(dbenv->get_alloc(dbenv, &mal, &rea, &fre));
	CHK_TRUE(mal == my_malloc, "get_alloc malloc");
	CHK_TRUE(rea == my_realloc, "get_alloc realloc");
	CHK_TRUE(fre == my_free, "get_alloc free");

	/* --- feedback: __env_set_feedback + __env_get_feedback. */
	CHK_OK(dbenv->set_feedback(dbenv, my_feedback));
	fb = NULL;
	CHK_OK(dbenv->get_feedback(dbenv, &fb));
	CHK_TRUE(fb == my_feedback, "get_feedback");

	/* --- paniccall: __env_set_paniccall (cold). */
	CHK_OK(dbenv->set_paniccall(dbenv, my_panic));

	/* --- isalive: __env_set_isalive needs thread_id too. */
	CHK_OK(dbenv->set_thread_id(dbenv, my_thread_id));
	tid = NULL;
	CHK_OK(dbenv->get_thread_id_fn(dbenv, &tid));
	CHK_TRUE(tid == my_thread_id, "get_thread_id_fn");
	CHK_OK(dbenv->set_thread_id_string(dbenv, my_thread_id_string));
	tids = NULL;
	CHK_OK(dbenv->get_thread_id_string_fn(dbenv, &tids));
	CHK_TRUE(tids == my_thread_id_string, "get_thread_id_string_fn");
	CHK_OK(dbenv->set_isalive(dbenv, my_isalive));
	ia = NULL;
	CHK_OK(dbenv->get_isalive(dbenv, &ia));
	CHK_TRUE(ia == my_isalive, "get_isalive");

	/* --- app_dispatch: set + get (getter cold). */
	CHK_OK(dbenv->set_app_dispatch(dbenv, my_app_dispatch));
	appd = NULL;
	CHK_OK(dbenv->get_app_dispatch(dbenv, &appd));
	CHK_TRUE(appd == my_app_dispatch, "get_app_dispatch");

	/* --- data_len: __env_set_data_len + __env_get_data_len (both cold). */
	CHK_OK(dbenv->set_data_len(dbenv, 64));
	a = 0;
	CHK_OK(dbenv->get_data_len(dbenv, &a));
	CHK_EQ(a, 64, "get_data_len");

	/* --- memory_max: __env_set_memory_max + __env_get_memory_max. */
	CHK_OK(dbenv->set_memory_max(dbenv, 0, 4 * 1024 * 1024));
	a = b = 0;
	CHK_OK(dbenv->get_memory_max(dbenv, &a, &b));
	CHK_TRUE(a != 0 || b != 0, "get_memory_max nonzero");

	/* --- err/msg call + file: the get_* forms are all cold. */
	dbenv->set_errcall(dbenv, my_errcall);
	ec = NULL;
	dbenv->get_errcall(dbenv, &ec);
	CHK_TRUE(ec == my_errcall, "get_errcall");
	dbenv->set_msgcall(dbenv, my_msgcall);
	mc = NULL;
	dbenv->get_msgcall(dbenv, &mc);
	CHK_TRUE(mc == my_msgcall, "get_msgcall");
	/* Clear the callbacks and use files instead: the other arm. */
	dbenv->set_errcall(dbenv, NULL);
	dbenv->set_msgcall(dbenv, NULL);
	dbenv->set_errfile(dbenv, stderr);
	fp = NULL;
	dbenv->get_errfile(dbenv, &fp);
	CHK_TRUE(fp == stderr, "get_errfile");
	dbenv->set_msgfile(dbenv, stdout);
	fp = NULL;
	dbenv->get_msgfile(dbenv, &fp);
	CHK_TRUE(fp == stdout, "get_msgfile");
	/* Silence for the rest of the run. */
	dbenv->set_errfile(dbenv, NULL);
	dbenv->set_msgfile(dbenv, NULL);
	dbenv->set_errpfx(dbenv, "cov_api");
	cp = NULL;
	dbenv->get_errpfx(dbenv, &cp);
	CHK_TRUE(cp != NULL && strcmp(cp, "cov_api") == 0, "get_errpfx");

	/* --- intermediate dir mode: __env_get_intermediate_dir_mode cold. */
	CHK_OK(dbenv->set_intermediate_dir_mode(dbenv, "rwxr-x---"));
	cp = NULL;
	CHK_OK(dbenv->get_intermediate_dir_mode(dbenv, &cp));
	CHK_TRUE(cp != NULL, "get_intermediate_dir_mode");
	/* A malformed mode string is the rejection branch. */
	CHK_FAILS(dbenv->set_intermediate_dir_mode(dbenv, "bogus"));

	/* --- event_notify has no getter but the setter is worth driving. */
	CHK_OK(dbenv->set_event_notify(dbenv, my_event_notify));

	/*
	 * --- the remaining scalar knobs: each set then get.  These getters
	 * are mostly warm, but their pre-open arm (read from the handle, not
	 * the region) is a distinct branch from the post-open arm.
	 */
	CHK_OK(dbenv->set_cachesize(dbenv, 0, 2 * 1024 * 1024, 1));
	CHK_OK(dbenv->get_cachesize(dbenv, &a, &b, &i));
	CHK_OK(dbenv->set_cache_max(dbenv, 0, 8 * 1024 * 1024));
	CHK_OK(dbenv->get_cache_max(dbenv, &a, &b));
	CHK_OK(dbenv->set_lg_bsize(dbenv, 65536));
	CHK_OK(dbenv->get_lg_bsize(dbenv, &a));
	CHK_EQ(a, 65536, "get_lg_bsize");
	CHK_OK(dbenv->set_lg_max(dbenv, 1048576));
	CHK_OK(dbenv->get_lg_max(dbenv, &a));
	CHK_OK(dbenv->set_lg_regionmax(dbenv, 131072));
	CHK_OK(dbenv->get_lg_regionmax(dbenv, &a));
	CHK_OK(dbenv->set_lg_filemode(dbenv, 0640));
	CHK_OK(dbenv->get_lg_filemode(dbenv, &i));
	CHK_EQ(i, 0640, "get_lg_filemode");
	CHK_OK(dbenv->set_mp_mmapsize(dbenv, 131072));
	CHK_OK(dbenv->get_mp_mmapsize(dbenv, &sz));
	CHK_OK(dbenv->set_mp_max_openfd(dbenv, 32));
	CHK_OK(dbenv->get_mp_max_openfd(dbenv, &i));
	CHK_EQ(i, 32, "get_mp_max_openfd");
	CHK_OK(dbenv->set_mp_max_write(dbenv, 4, 1000));
	{
		db_timeout_t to;
		CHK_OK(dbenv->get_mp_max_write(dbenv, &i, &to));
	}
	CHK_OK(dbenv->set_mp_pagesize(dbenv, 4096));
	CHK_OK(dbenv->get_mp_pagesize(dbenv, &a));
	CHK_OK(dbenv->set_mp_tablesize(dbenv, 37));
	CHK_OK(dbenv->get_mp_tablesize(dbenv, &a));
	CHK_OK(dbenv->set_mp_mtxcount(dbenv, 41));
	CHK_OK(dbenv->get_mp_mtxcount(dbenv, &a));
	CHK_OK(dbenv->set_tx_max(dbenv, 100));
	CHK_OK(dbenv->get_tx_max(dbenv, &a));
	CHK_EQ(a, 100, "get_tx_max");
	tv = 0;
	CHK_OK(dbenv->get_tx_timestamp(dbenv, &tv));
	CHK_OK(dbenv->set_thread_count(dbenv, 8));
	CHK_OK(dbenv->get_thread_count(dbenv, &a));
	CHK_OK(dbenv->set_shm_key(dbenv, 0x4242));
	CHK_OK(dbenv->get_shm_key(dbenv, &l));
	CHK_EQ(l, 0x4242, "get_shm_key");
	CHK_OK(dbenv->set_timeout(dbenv, 500000, DB_SET_LOCK_TIMEOUT));
	CHK_OK(dbenv->get_timeout(dbenv, &a, DB_SET_LOCK_TIMEOUT));
	CHK_OK(dbenv->set_timeout(dbenv, 500000, DB_SET_TXN_TIMEOUT));
	CHK_OK(dbenv->get_timeout(dbenv, &a, DB_SET_TXN_TIMEOUT));
	/* An unknown timeout selector is the rejection branch. */
	CHK_FAILS(dbenv->get_timeout(dbenv, &a, 0x9999));

	/* --- memory_init: each DB_MEM_* enum, set + get. */
	CHK_OK(dbenv->set_memory_init(dbenv, DB_MEM_LOCK, 100));
	CHK_OK(dbenv->get_memory_init(dbenv, DB_MEM_LOCK, &a));
	CHK_EQ(a, 100, "get_memory_init DB_MEM_LOCK");
	CHK_OK(dbenv->set_memory_init(dbenv, DB_MEM_LOCKER, 100));
	CHK_OK(dbenv->get_memory_init(dbenv, DB_MEM_LOCKER, &a));
	CHK_OK(dbenv->set_memory_init(dbenv, DB_MEM_LOCKOBJECT, 100));
	CHK_OK(dbenv->get_memory_init(dbenv, DB_MEM_LOCKOBJECT, &a));
	CHK_OK(dbenv->set_memory_init(dbenv, DB_MEM_TRANSACTION, 50));
	CHK_OK(dbenv->get_memory_init(dbenv, DB_MEM_TRANSACTION, &a));
	CHK_OK(dbenv->set_memory_init(dbenv, DB_MEM_THREAD, 20));
	CHK_OK(dbenv->get_memory_init(dbenv, DB_MEM_THREAD, &a));
	CHK_OK(dbenv->set_memory_init(dbenv, DB_MEM_LOGID, 20));
	CHK_OK(dbenv->get_memory_init(dbenv, DB_MEM_LOGID, &a));
	/*
	 * NOTE: an out-of-range DB_MEM_CONFIG is NOT rejected -- the setter
	 * switches on the enum and falls through, so a bogus value is a
	 * silent no-op rather than EINVAL.  Driven here for the fall-through
	 * branch, but deliberately NOT asserted as an error: that would be
	 * asserting a behaviour the library does not promise.
	 */
	(void)dbenv->set_memory_init(dbenv, (DB_MEM_CONFIG)999, 1);
	(void)dbenv->get_memory_init(dbenv, (DB_MEM_CONFIG)999, &a);
	checks += 2;

	/* --- the directory knobs: set + get, plus the not-set arm. */
	CHK_OK(dbenv->set_tmp_dir(dbenv, "."));
	cp = NULL;
	CHK_OK(dbenv->get_tmp_dir(dbenv, &cp));
	CHK_OK(dbenv->set_lg_dir(dbenv, "."));
	CHK_OK(dbenv->get_lg_dir(dbenv, &cp));
	CHK_OK(dbenv->set_metadata_dir(dbenv, "."));
	CHK_OK(dbenv->get_metadata_dir(dbenv, &cp));
	CHK_OK(dbenv->set_create_dir(dbenv, "."));
	CHK_OK(dbenv->get_create_dir(dbenv, &cp));
	CHK_OK(dbenv->set_data_dir(dbenv, "."));
	{
		const char **dirs = NULL;
		CHK_OK(dbenv->get_data_dirs(dbenv, &dirs));
		CHK_TRUE(dirs != NULL, "get_data_dirs");
	}

	/* --- lock knobs: set + get for each, plus a bogus detect policy. */
	CHK_OK(dbenv->set_lk_max_locks(dbenv, 500));
	CHK_OK(dbenv->get_lk_max_locks(dbenv, &a));
	CHK_EQ(a, 500, "get_lk_max_locks");
	CHK_OK(dbenv->set_lk_max_lockers(dbenv, 500));
	CHK_OK(dbenv->get_lk_max_lockers(dbenv, &a));
	CHK_OK(dbenv->set_lk_max_objects(dbenv, 500));
	CHK_OK(dbenv->get_lk_max_objects(dbenv, &a));
	CHK_OK(dbenv->set_lk_partitions(dbenv, 4));
	CHK_OK(dbenv->get_lk_partitions(dbenv, &a));
	CHK_OK(dbenv->set_lk_tablesize(dbenv, 37));
	CHK_OK(dbenv->get_lk_tablesize(dbenv, &a));
	CHK_OK(dbenv->set_lk_detect(dbenv, DB_LOCK_MINWRITE));
	CHK_OK(dbenv->get_lk_detect(dbenv, &a));
	CHK_EQ(a, DB_LOCK_MINWRITE, "get_lk_detect");
	CHK_FAILS(dbenv->set_lk_detect(dbenv, 0x7fffffff));
	{
		const u_int8_t *cf;
		int nmodes;
		/* No conflict array set: the "use the default" get arm. */
		(void)dbenv->get_lk_conflicts(dbenv, &cf, &nmodes);
		checks++;
	}

	/* --- flags: set each, read the composite back. */
	CHK_OK(dbenv->set_flags(dbenv, DB_AUTO_COMMIT, 1));
	CHK_OK(dbenv->get_flags(dbenv, &a));
	CHK_TRUE((a & DB_AUTO_COMMIT) != 0, "get_flags DB_AUTO_COMMIT");
	CHK_OK(dbenv->set_flags(dbenv, DB_AUTO_COMMIT, 0));
	CHK_OK(dbenv->set_flags(dbenv, DB_TXN_NOSYNC, 1));
	CHK_OK(dbenv->set_flags(dbenv, DB_TXN_WRITE_NOSYNC, 1));
	CHK_OK(dbenv->set_flags(dbenv, DB_TXN_NOWAIT, 1));
	CHK_OK(dbenv->set_flags(dbenv, DB_TXN_NOWAIT, 0));
	/*
	 * DB_DIRECT_DB is refused when the filesystem has no O_DIRECT (the
	 * __os_support_direct_io() == 0 branch), which is the common case on
	 * tmpfs/overlayfs.  Both outcomes are correct, so drive it without
	 * asserting either way -- the point is the branch, not the answer.
	 */
	(void)dbenv->set_flags(dbenv, DB_DIRECT_DB, 1);
	(void)dbenv->set_flags(dbenv, DB_DIRECT_DB, 0);
	checks += 2;
	CHK_OK(dbenv->set_flags(dbenv, DB_NOMMAP, 1));
	CHK_OK(dbenv->set_flags(dbenv, DB_NOMMAP, 0));
	CHK_OK(dbenv->set_flags(dbenv, DB_REGION_INIT, 1));
	CHK_OK(dbenv->set_flags(dbenv, DB_TIME_NOTGRANTED, 1));
	CHK_OK(dbenv->set_flags(dbenv, DB_TIME_NOTGRANTED, 0));
	CHK_OK(dbenv->set_flags(dbenv, DB_MULTIVERSION, 1));
	CHK_OK(dbenv->set_flags(dbenv, DB_MULTIVERSION, 0));
	CHK_OK(dbenv->set_flags(dbenv, DB_YIELDCPU, 1));
	CHK_OK(dbenv->set_flags(dbenv, DB_YIELDCPU, 0));
	/*
	 * An unknown env flag IS rejected: set_flags has an explicit OK_FLAGS
	 * mask (env_method.c ~line 949), which for this release covers
	 * 0x001fffff.  0x00200000 is the first bit outside it.  (Do NOT use a
	 * low bit here -- 0x00000004 is DB_TXN_SNAPSHOT and legal.)
	 */
	CHK_FAILS(dbenv->set_flags(dbenv, 0x00200000, 1));

	/* --- verbose: each subsystem selector on and off. */
	CHK_OK(dbenv->set_verbose(dbenv, DB_VERB_DEADLOCK, 1));
	CHK_OK(dbenv->get_verbose(dbenv, DB_VERB_DEADLOCK, &i));
	CHK_EQ(i, 1, "get_verbose DB_VERB_DEADLOCK");
	CHK_OK(dbenv->set_verbose(dbenv, DB_VERB_DEADLOCK, 0));
	CHK_OK(dbenv->set_verbose(dbenv, DB_VERB_RECOVERY, 1));
	CHK_OK(dbenv->set_verbose(dbenv, DB_VERB_RECOVERY, 0));
	CHK_OK(dbenv->set_verbose(dbenv, DB_VERB_REGISTER, 1));
	CHK_OK(dbenv->set_verbose(dbenv, DB_VERB_REGISTER, 0));
	CHK_OK(dbenv->set_verbose(dbenv, DB_VERB_FILEOPS, 1));
	CHK_OK(dbenv->set_verbose(dbenv, DB_VERB_FILEOPS, 0));
	CHK_OK(dbenv->set_verbose(dbenv, DB_VERB_FILEOPS_ALL, 1));
	CHK_OK(dbenv->set_verbose(dbenv, DB_VERB_FILEOPS_ALL, 0));
	CHK_OK(dbenv->set_verbose(dbenv, DB_VERB_REPLICATION, 1));
	CHK_OK(dbenv->set_verbose(dbenv, DB_VERB_REPLICATION, 0));
	CHK_OK(dbenv->set_verbose(dbenv, DB_VERB_REP_ELECT, 1));
	CHK_OK(dbenv->set_verbose(dbenv, DB_VERB_REP_ELECT, 0));
	CHK_OK(dbenv->set_verbose(dbenv, DB_VERB_REP_LEASE, 1));
	CHK_OK(dbenv->set_verbose(dbenv, DB_VERB_REP_LEASE, 0));
	CHK_OK(dbenv->set_verbose(dbenv, DB_VERB_REP_MISC, 1));
	CHK_OK(dbenv->set_verbose(dbenv, DB_VERB_REP_MISC, 0));
	CHK_OK(dbenv->set_verbose(dbenv, DB_VERB_REP_MSGS, 1));
	CHK_OK(dbenv->set_verbose(dbenv, DB_VERB_REP_MSGS, 0));
	CHK_OK(dbenv->set_verbose(dbenv, DB_VERB_REP_SYNC, 1));
	CHK_OK(dbenv->set_verbose(dbenv, DB_VERB_REP_SYNC, 0));
	CHK_OK(dbenv->set_verbose(dbenv, DB_VERB_REP_SYSTEM, 1));
	CHK_OK(dbenv->set_verbose(dbenv, DB_VERB_REP_SYSTEM, 0));
	CHK_OK(dbenv->set_verbose(dbenv, DB_VERB_REPMGR_CONNFAIL, 1));
	CHK_OK(dbenv->set_verbose(dbenv, DB_VERB_REPMGR_CONNFAIL, 0));
	CHK_OK(dbenv->set_verbose(dbenv, DB_VERB_REPMGR_MISC, 1));
	CHK_OK(dbenv->set_verbose(dbenv, DB_VERB_REPMGR_MISC, 0));
	/* An unknown verbose selector is rejected by set and get. */
	CHK_FAILS(dbenv->set_verbose(dbenv, 0x40000000, 1));
	CHK_FAILS(dbenv->get_verbose(dbenv, 0x40000000, &i));

	/* --- backup config: every DB_BACKUP_* enum, set + get. */
	CHK_OK(dbenv->set_backup_config(dbenv, DB_BACKUP_READ_COUNT, 1024));
	CHK_OK(dbenv->get_backup_config(dbenv, DB_BACKUP_READ_COUNT, &a));
	CHK_EQ(a, 1024, "get_backup_config READ_COUNT");
	CHK_OK(dbenv->set_backup_config(dbenv, DB_BACKUP_READ_SLEEP, 10));
	CHK_OK(dbenv->get_backup_config(dbenv, DB_BACKUP_READ_SLEEP, &a));
	CHK_OK(dbenv->set_backup_config(dbenv, DB_BACKUP_SIZE, 4096));
	CHK_OK(dbenv->get_backup_config(dbenv, DB_BACKUP_SIZE, &a));
	CHK_OK(dbenv->set_backup_config(dbenv,
	    DB_BACKUP_WRITE_DIRECT, 1));
	CHK_OK(dbenv->get_backup_config(dbenv, DB_BACKUP_WRITE_DIRECT, &a));
	CHK_OK(dbenv->set_backup_config(dbenv,
	    DB_BACKUP_WRITE_DIRECT, 0));
	/*
	 * As with DB_MEM_CONFIG, an out-of-range DB_BACKUP_CONFIG falls
	 * through the switch rather than returning EINVAL.  Drive the
	 * fall-through; do not assert an error the API does not promise.
	 */
	(void)dbenv->set_backup_config(dbenv, (DB_BACKUP_CONFIG)999, 1);
	checks++;

	/* --- encrypt flags getter before any set_encrypt: the not-set arm. */
	a = 0;
	(void)dbenv->get_encrypt_flags(dbenv, &a);
	checks++;
}

/*
 * env_getters_post_open --
 *	The same getters after DB_ENV->open: each now reads the shared region
 *	instead of the handle, which is the other half of the branch.  Plus the
 *	post-open-illegal setters (ENV_ILLEGAL_AFTER_OPEN), which is a branch
 *	present in nearly every setter and never exercised by a Tcl test
 *	(a Tcl test sets everything before open, as an application should).
 */
static void
env_getters_post_open(dbenv)
	DB_ENV *dbenv;
{
	const char *cp;
	u_int32_t a, b;
	int i;
	size_t sz;

	/* Post-open reads come from the region. */
	CHK_OK(dbenv->get_cachesize(dbenv, &a, &b, &i));
	CHK_OK(dbenv->get_cache_max(dbenv, &a, &b));
	CHK_OK(dbenv->get_lg_bsize(dbenv, &a));
	CHK_OK(dbenv->get_lg_max(dbenv, &a));
	CHK_OK(dbenv->get_lg_regionmax(dbenv, &a));
	CHK_OK(dbenv->get_lg_filemode(dbenv, &i));
	CHK_OK(dbenv->get_lk_max_locks(dbenv, &a));
	CHK_OK(dbenv->get_lk_max_lockers(dbenv, &a));
	CHK_OK(dbenv->get_lk_max_objects(dbenv, &a));
	CHK_OK(dbenv->get_lk_partitions(dbenv, &a));
	CHK_OK(dbenv->get_lk_tablesize(dbenv, &a));
	CHK_OK(dbenv->get_lk_detect(dbenv, &a));
	CHK_OK(dbenv->get_tx_max(dbenv, &a));
	CHK_OK(dbenv->get_mp_mmapsize(dbenv, &sz));
	CHK_OK(dbenv->get_mp_max_openfd(dbenv, &i));
	CHK_OK(dbenv->get_mp_pagesize(dbenv, &a));
	CHK_OK(dbenv->get_mp_tablesize(dbenv, &a));
	CHK_OK(dbenv->get_mp_mtxcount(dbenv, &a));
	CHK_OK(dbenv->get_thread_count(dbenv, &a));
	CHK_OK(dbenv->get_data_len(dbenv, &a));
	CHK_OK(dbenv->get_memory_max(dbenv, &a, &b));
	CHK_OK(dbenv->get_memory_init(dbenv, DB_MEM_LOCK, &a));
	CHK_OK(dbenv->get_flags(dbenv, &a));
	CHK_OK(dbenv->get_open_flags(dbenv, &a));
	CHK_TRUE((a & DB_CREATE) != 0, "get_open_flags DB_CREATE");
	cp = NULL;
	CHK_OK(dbenv->get_home(dbenv, &cp));
	CHK_TRUE(cp != NULL, "get_home");
	CHK_OK(dbenv->get_intermediate_dir_mode(dbenv, &cp));
	{
		const u_int8_t *cf;
		int nmodes;
		/* Post-open the conflict array comes from the lock region. */
		CHK_OK(dbenv->get_lk_conflicts(dbenv, &cf, &nmodes));
		CHK_TRUE(cf != NULL && nmodes > 0, "get_lk_conflicts");
	}
	/*
	 * Per-locker lock priority: set + get.  These need a lockid that has a
	 * LIVE locker, because __lock_set_lk_priority /
	 * __lock_get_lk_priority call __lock_getlocker(..., create=0, ...)
	 * and dereference the result WITHOUT a NULL check -- and
	 * __lock_getlocker returns 0 with *retp == NULL when the id is not
	 * found (src/lock/lock_id.c: `*retp = sh_locker;` is reached on the
	 * not-found path when create == 0).  Passing an unused lockid here
	 * SIGSEGVs the library at src/lock/lock_method.c:483.
	 *
	 * That is a real engine defect, reported in
	 * test/coverage/FULL-COVERAGE-REPORT-4.md and NOT fixed here (src/lock
	 * is owned by another change).  We therefore obtain a real locker id
	 * from an open transaction so this driver covers the intended
	 * getter/setter branches without tripping the crash.
	 */
	{
		DB_TXN *txn;
		u_int32_t lockid;

		if (dbenv->txn_begin(dbenv, NULL, &txn, 0) == 0) {
			lockid = txn->id(txn);
			CHK_OK(dbenv->set_lk_priority(dbenv, lockid, 100));
			a = 0;
			CHK_OK(dbenv->get_lk_priority(dbenv, lockid, &a));
			CHK_EQ(a, 100, "get_lk_priority");
			CHK_OK(txn->commit(txn, 0));
		}
	}

	/*
	 * --- ENV_ILLEGAL_AFTER_OPEN: these setters must refuse now.  This is
	 * the branch every setter has and no Tcl test reaches.
	 *
	 * NOTE: not every setter is guarded.  set_cachesize and set_data_len
	 * accept a post-open call (cachesize because the mpool can be resized,
	 * data_len because it is only a print/debug bound), so they are driven
	 * without an assertion.
	 */
	(void)dbenv->set_cachesize(dbenv, 0, 1048576, 1);
	(void)dbenv->set_data_len(dbenv, 128);
	checks += 2;
	CHK_FAILS(dbenv->set_lk_max_locks(dbenv, 999));
	CHK_FAILS(dbenv->set_lk_max_lockers(dbenv, 999));
	CHK_FAILS(dbenv->set_lk_max_objects(dbenv, 999));
	CHK_FAILS(dbenv->set_lk_partitions(dbenv, 9));
	CHK_FAILS(dbenv->set_lk_tablesize(dbenv, 99));
	CHK_FAILS(dbenv->set_tx_max(dbenv, 999));
	CHK_FAILS(dbenv->set_lg_bsize(dbenv, 999999));
	CHK_FAILS(dbenv->set_lg_regionmax(dbenv, 999999));
	CHK_FAILS(dbenv->set_memory_init(dbenv, DB_MEM_LOCK, 999));
	CHK_FAILS(dbenv->set_memory_max(dbenv, 0, 1048576));
	CHK_FAILS(dbenv->set_shm_key(dbenv, 0x99));
	CHK_FAILS(dbenv->set_thread_count(dbenv, 99));
	CHK_FAILS(dbenv->set_mp_pagesize(dbenv, 8192));
	CHK_FAILS(dbenv->set_mp_tablesize(dbenv, 99));
	CHK_FAILS(dbenv->set_mp_mtxcount(dbenv, 99));
	CHK_FAILS(dbenv->set_metadata_dir(dbenv, "."));
	CHK_FAILS(dbenv->set_encrypt(dbenv, "pw", DB_ENCRYPT_AES));

	/* --- a second open of an already-open handle: __db_mi_open. */
	CHK_FAILS(dbenv->open(dbenv, HOME, DB_CREATE | DB_INIT_MPOOL, 0600));
}

/*
 * db_getters --
 *	The DB handle getter surface.  __db_get_mpf, __db_get_transactional,
 *	__db_get_priority, __db_get_dup_compare, __db_get_encrypt_flags,
 *	__db_get_assoc_flags, __db_get_append_recno, __db_get_feedback,
 *	__db_get_create_dir, __db_get_alloc and the DB-level err/msg getters
 *	are all never called by the Tcl suite.
 */
static void
db_getters(dbenv)
	DB_ENV *dbenv;
{
	DB *dbp, *sdbp;
	DBC *dbc;
	DB_MPOOLFILE *mpf;
	DBT key, data;
	DB_CACHE_PRIORITY prio;
	void *(*mal) __P((size_t));
	void *(*rea) __P((void *, size_t));
	void (*fre) __P((void *));
	void (*fb) __P((DB *, int, int));
	int (*ar) __P((DB *, DBT *, db_recno_t));
	int (*dc) __P((DB *, const DBT *, const DBT *));
	FILE *fp;
	const char *cp;
	u_int32_t a;
	int ret;
	DBTYPE type;

	/* ---- 1. A recno DB: set_append_recno only applies to recno. */
	CHK_OK(db_create(&dbp, dbenv, 0));
	/*
	 * The DB-level allocator + cachesize setters are only legal on a
	 * handle in a STANDALONE env (dbenv == NULL): with an env, the env's
	 * allocator and cache govern and the DB-level setter returns EINVAL.
	 * That rejection is the arm a misconfigured application hits, so it is
	 * what we assert here; the accepting arm is driven in
	 * db_getters_standalone() below.
	 */
	CHK_FAILS(dbp->set_alloc(dbp, my_malloc, my_realloc, my_free));
	COMPQUIET(mal, NULL);
	COMPQUIET(rea, NULL);
	COMPQUIET(fre, NULL);
	/* __db_set_feedback + __db_get_feedback. */
	CHK_OK(dbp->set_feedback(dbp, my_db_feedback));
	fb = NULL;
	CHK_OK(dbp->get_feedback(dbp, &fb));
	CHK_TRUE(fb == my_db_feedback, "DB get_feedback");
	/* __db_set_paniccall. */
	CHK_OK(dbp->set_paniccall(dbp, my_panic));
	/* __db_set_append_recno + __db_get_append_recno. */
	CHK_OK(dbp->set_append_recno(dbp, my_append_recno));
	ar = NULL;
	CHK_OK(dbp->get_append_recno(dbp, &ar));
	CHK_TRUE(ar == my_append_recno, "get_append_recno");
	/* The DB-level err/msg call + file getters. */
	dbp->set_errcall(dbp, my_errcall);
	dbp->set_msgcall(dbp, my_msgcall);
	dbp->set_errcall(dbp, NULL);
	dbp->set_msgcall(dbp, NULL);
	dbp->set_errfile(dbp, NULL);
	fp = (FILE *)1;
	dbp->get_errfile(dbp, &fp);
	CHK_TRUE(fp == NULL, "DB get_errfile");
	dbp->set_msgfile(dbp, NULL);
	fp = (FILE *)1;
	dbp->get_msgfile(dbp, &fp);
	CHK_TRUE(fp == NULL, "DB get_msgfile");
	dbp->set_errpfx(dbp, "cov_db");
	cp = NULL;
	dbp->get_errpfx(dbp, &cp);
	CHK_TRUE(cp != NULL, "DB get_errpfx");
	/* __dbh_err + __dbh_errx: the DB-level error emitters. */
	dbp->err(dbp, EINVAL, "cov_api_surface: expected err() %d", 1);
	dbp->errx(dbp, "cov_api_surface: expected errx() %d", 2);
	checks += 2;
	/* __db_set_priority + __db_get_priority. */
	CHK_OK(dbp->set_priority(dbp, DB_PRIORITY_VERY_LOW));
	prio = DB_PRIORITY_UNCHANGED;
	CHK_OK(dbp->get_priority(dbp, &prio));
	CHK_EQ(prio, DB_PRIORITY_VERY_LOW, "get_priority");
	CHK_OK(dbp->set_priority(dbp, DB_PRIORITY_HIGH));
	/* __db_set_create_dir + __db_get_create_dir. */
	CHK_OK(dbp->set_create_dir(dbp, "."));
	cp = NULL;
	CHK_OK(dbp->get_create_dir(dbp, &cp));
	/* __db_get_transactional before open: not transactional yet. */
	(void)dbp->get_transactional(dbp);
	checks++;
	/* __db_get_encrypt_flags with no encryption: the not-set arm. */
	a = 0;
	(void)dbp->get_encrypt_flags(dbp, &a);
	checks++;
	/* __db_get_mpf before open. */
	mpf = dbp->get_mpf(dbp);
	CHK_TRUE(mpf != NULL, "get_mpf pre-open");

	CHK_OK(dbp->set_re_len(dbp, 8));
	CHK_OK(dbp->get_re_len(dbp, &a));
	CHK_EQ(a, 8, "get_re_len");
	CHK_OK(dbp->open(dbp, NULL, "cov_recno.db", NULL, DB_RECNO,
	    DB_CREATE, 0600));
	/* Post-open: __db_get_transactional now answers for real. */
	(void)dbp->get_transactional(dbp);
	checks++;
	CHK_OK(dbp->get_type(dbp, &type));
	CHK_EQ(type, DB_RECNO, "get_type DB_RECNO");
	/* get_dbname: both out-params and each individually. */
	{
		const char *fname, *dname;
		fname = dname = NULL;
		CHK_OK(dbp->get_dbname(dbp, &fname, &dname));
		CHK_TRUE(fname != NULL, "get_dbname filename");
	}
	/* __db_get_mpf post-open, then the mpool-file getters. */
	mpf = dbp->get_mpf(dbp);
	CHK_TRUE(mpf != NULL, "get_mpf post-open");
	if (mpf != NULL) {
		u_int32_t clen, lsnoff;
		DB_CACHE_PRIORITY mprio;
		void *cookie;
		db_pgno_t last;
		DBT ck;

		/* mp_fmethod.c getters: 8 never-called functions. */
		CHK_OK(mpf->get_clear_len(mpf, &clen));
		CHK_OK(mpf->get_lsn_offset(mpf, (int32_t *)&lsnoff));
		CHK_OK(mpf->get_priority(mpf, &mprio));
		CHK_OK(mpf->set_priority(mpf, DB_PRIORITY_LOW));
		CHK_OK(mpf->get_priority(mpf, &mprio));
		CHK_EQ(mprio, DB_PRIORITY_LOW, "mpf get_priority");
		memset(&ck, 0, sizeof(ck));
		CHK_OK(mpf->get_pgcookie(mpf, &ck));
		COMPQUIET(cookie, NULL);
		CHK_OK(mpf->get_maxsize(mpf, &clen, &lsnoff));
		CHK_OK(mpf->set_maxsize(mpf, 0, 16 * 1024 * 1024));
		CHK_OK(mpf->get_maxsize(mpf, &clen, &lsnoff));
		CHK_OK(mpf->get_last_pgno(mpf, &last));
		CHK_OK(mpf->get_flags(mpf, &a));
		CHK_OK(mpf->set_flags(mpf, DB_MPOOL_NOFILE, 0));
	}

	/* Append records so my_append_recno actually fires. */
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	data.data = "12345678"; data.size = 8;
	CHK_OK(dbp->put(dbp, NULL, &key, &data, DB_APPEND));
	CHK_TRUE(recno_calls > 0, "append_recno callback fired");

	/* A cursor: the DBC priority getter/setter. */
	CHK_OK(dbp->cursor(dbp, NULL, &dbc, 0));
	CHK_OK(dbc->set_priority(dbc, DB_PRIORITY_VERY_HIGH));
	prio = DB_PRIORITY_UNCHANGED;
	CHK_OK(dbc->get_priority(dbc, &prio));
	CHK_EQ(prio, DB_PRIORITY_VERY_HIGH, "DBC get_priority");
	CHK_OK(dbc->close(dbc));
	CHK_OK(dbp->close(dbp, 0));

	/* ---- 2. A btree with duplicates: get_dup_compare + get_flags. */
	CHK_OK(db_create(&dbp, dbenv, 0));
	CHK_OK(dbp->set_flags(dbp, DB_DUP | DB_DUPSORT));
	CHK_OK(dbp->set_dup_compare(dbp, my_dup_compare));
	dc = NULL;
	CHK_OK(dbp->get_dup_compare(dbp, &dc));
	CHK_TRUE(dc == my_dup_compare, "get_dup_compare");
	CHK_OK(dbp->set_bt_minkey(dbp, 3));
	CHK_OK(dbp->get_bt_minkey(dbp, &a));
	CHK_EQ(a, 3, "get_bt_minkey");
	CHK_OK(dbp->set_pagesize(dbp, 2048));
	CHK_OK(dbp->get_pagesize(dbp, &a));
	CHK_EQ(a, 2048, "get_pagesize");
	/* With an env, the DB-level cachesize setter is refused. */
	CHK_FAILS(dbp->set_cachesize(dbp, 0, 262144, 1));
	CHK_OK(dbp->open(dbp, NULL, "cov_dup.db", NULL, DB_BTREE,
	    DB_CREATE, 0600));
	CHK_OK(dbp->get_flags(dbp, &a));
	CHK_TRUE((a & DB_DUP) != 0, "get_flags DB_DUP");
	CHK_OK(dbp->get_open_flags(dbp, &a));
	CHK_TRUE((a & DB_CREATE) != 0, "DB get_open_flags");
	/*
	 * __db_set_lk_exclusive: request (and then not require) an exclusive
	 * database lock.  Never called; it is a handle-flag setter that must
	 * be called before open, so the post-open call is its reject branch.
	 */
	CHK_FAILS(dbp->set_lk_exclusive(dbp, 0));
	CHK_OK(dbp->close(dbp, 0));

	/* A fresh handle where set_lk_exclusive is legal (pre-open). */
	CHK_OK(db_create(&dbp, dbenv, 0));
	CHK_OK(dbp->set_lk_exclusive(dbp, 0));
	CHK_OK(dbp->open(dbp, NULL, "cov_excl.db", NULL, DB_BTREE,
	    DB_CREATE, 0600));
	CHK_OK(dbp->close(dbp, 0));

	/* ---- 3. A secondary: __db_get_assoc_flags. */
	CHK_OK(db_create(&dbp, dbenv, 0));
	CHK_OK(dbp->open(dbp, NULL, "cov_pri.db", NULL, DB_BTREE,
	    DB_CREATE, 0600));
	CHK_OK(db_create(&sdbp, dbenv, 0));
	CHK_OK(sdbp->set_flags(sdbp, DB_DUP));
	CHK_OK(sdbp->open(sdbp, NULL, "cov_sec.db", NULL, DB_BTREE,
	    DB_CREATE, 0600));
	/*
	 * associate with DB_IMMUTABLE_KEY so get_assoc_flags has a non-zero
	 * answer.  The callback is only invoked on primary writes.
	 */
	if ((ret = dbp->associate(dbp, NULL, sdbp, NULL,
	    DB_IMMUTABLE_KEY)) == 0) {
		a = 0;
		CHK_OK(sdbp->get_assoc_flags(sdbp, &a));
		CHK_TRUE((a & DB_IMMUTABLE_KEY) != 0,
		    "get_assoc_flags DB_IMMUTABLE_KEY");
	} else {
		/* A NULL callback is legal only for a read-only secondary. */
		a = 0;
		(void)sdbp->get_assoc_flags(sdbp, &a);
		checks++;
	}
	CHK_OK(sdbp->close(sdbp, 0));
	CHK_OK(dbp->close(dbp, 0));
}

/*
 * db_getters_standalone --
 *	The DB-handle getters that are only reachable on a STANDALONE handle
 *	(db_create(&dbp, NULL, 0) -- no environment).  __db_set_alloc /
 *	__db_get_alloc and the DB-level set_cachesize are rejected when the
 *	handle belongs to an env (the env's allocator and cache govern), so
 *	their ACCEPTING arms -- which is what makes them never-called -- need a
 *	standalone handle.  This is the classic "embed a single db file with no
 *	env" shape, which the Tcl suite never uses because every Tcl test runs
 *	inside a test env.
 */
static void
db_getters_standalone()
{
	DB *dbp;
	DBT key, data;
	void *(*mal) __P((size_t));
	void *(*rea) __P((void *, size_t));
	void (*fre) __P((void *));
	u_int32_t g, b;
	int nc, i;
	char kbuf[32];

	CHK_OK(db_create(&dbp, NULL, 0));
	dbp->set_errfile(dbp, NULL);

	/* __db_set_alloc + __db_get_alloc: the accepting arm. */
	CHK_OK(dbp->set_alloc(dbp, my_malloc, my_realloc, my_free));
	mal = NULL; rea = NULL; fre = NULL;
	CHK_OK(dbp->get_alloc(dbp, &mal, &rea, &fre));
	CHK_TRUE(mal == my_malloc, "standalone DB get_alloc malloc");
	CHK_TRUE(rea == my_realloc, "standalone DB get_alloc realloc");
	CHK_TRUE(fre == my_free, "standalone DB get_alloc free");

	/* The DB-level cache: set + get, also only legal standalone. */
	CHK_OK(dbp->set_cachesize(dbp, 0, 262144, 1));
	CHK_OK(dbp->get_cachesize(dbp, &g, &b, &nc));
	CHK_TRUE(b >= 262144 || g > 0, "standalone DB get_cachesize");

	CHK_OK(dbp->set_pagesize(dbp, 1024));
	CHK_OK(dbp->open(dbp, NULL, "COVAPI_TESTDIR/cov_standalone.db", NULL,
	    DB_BTREE, DB_CREATE, 0600));

	/*
	 * A standalone handle is not transactional -- __db_get_transactional's
	 * false arm.
	 */
	CHK_EQ(dbp->get_transactional(dbp), 0,
	    "standalone get_transactional");

	/* Put/get through the user allocator so my_malloc actually fires. */
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	for (i = 0; i < 20; i++) {
		(void)snprintf(kbuf, sizeof(kbuf), "s%06d", i);
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf);
		data.data = kbuf; data.size = (u_int32_t)strlen(kbuf);
		CHK_OK(dbp->put(dbp, NULL, &key, &data, 0));
	}
	/* DB_DBT_MALLOC makes the library call the installed allocator. */
	memset(&data, 0, sizeof(data));
	data.flags = DB_DBT_MALLOC;
	(void)snprintf(kbuf, sizeof(kbuf), "s%06d", 0);
	key.data = kbuf; key.size = (u_int32_t)strlen(kbuf);
	CHK_OK(dbp->get(dbp, NULL, &key, &data, 0));
	CHK_TRUE(alloc_calls > 0, "user allocator was called");
	if (data.data != NULL)
		my_free(data.data);

	CHK_OK(dbp->sync(dbp, 0));
	CHK_OK(dbp->close(dbp, 0));

	/* A standalone handle with a feedback callback + truncate. */
	CHK_OK(db_create(&dbp, NULL, 0));
	dbp->set_errfile(dbp, NULL);
	CHK_OK(dbp->set_feedback(dbp, my_db_feedback));
	CHK_OK(dbp->open(dbp, NULL, "COVAPI_TESTDIR/cov_standalone.db", NULL,
	    DB_BTREE, 0, 0600));
	{
		u_int32_t count = 0;
		CHK_OK(dbp->truncate(dbp, NULL, &count, 0));
		CHK_TRUE(count > 0, "standalone truncate count");
	}
	CHK_OK(dbp->close(dbp, 0));
}

/*
 * txn_getters --
 *	__txn_get_priority, __txn_set_commit_token / __txn_build_token, and the
 *	txn name accessors.  set_commit_token in a NON-replication env is the
 *	interesting arm: it must be accepted (tokens are legal) and the token
 *	filled with a zero gen.
 */
static void
txn_getters(dbenv)
	DB_ENV *dbenv;
{
	DB *dbp;
	DB_TXN *txn, *child;
	DB_TXN_TOKEN token;
	DBT key, data;
	const char *name;
	u_int32_t a;

	CHK_OK(dbenv->txn_begin(dbenv, NULL, &txn, 0));
	/* __txn_set_name + get_name. */
	CHK_OK(txn->set_name(txn, "cov_api_txn"));
	name = NULL;
	CHK_OK(txn->get_name(txn, &name));
	CHK_TRUE(name != NULL && strcmp(name, "cov_api_txn") == 0,
	    "txn get_name");
	/* __txn_set_priority + __txn_get_priority (getter never called). */
	CHK_OK(txn->set_priority(txn, 250));
	a = 0;
	CHK_OK(txn->get_priority(txn, &a));
	CHK_EQ(a, 250, "txn get_priority");
	/* txn->id is warm but cheap to assert. */
	CHK_TRUE(txn->id(txn) != 0, "txn id");
	/* Per-txn timeouts: both selectors. */
	CHK_OK(txn->set_timeout(txn, 1000000, DB_SET_TXN_TIMEOUT));
	CHK_OK(txn->set_timeout(txn, 1000000, DB_SET_LOCK_TIMEOUT));
	CHK_FAILS(txn->set_timeout(txn, 1000, 0x9999));

	/* A child txn: the nested-txn arm of begin + the parent link. */
	CHK_OK(dbenv->txn_begin(dbenv, txn, &child, 0));
	CHK_OK(child->set_name(child, "cov_api_child"));
	CHK_OK(child->commit(child, 0));

	/* __txn_set_commit_token + __txn_build_token. */
	CHK_OK(db_create(&dbp, dbenv, 0));
	CHK_OK(dbp->open(dbp, txn, "cov_token.db", NULL, DB_BTREE,
	    DB_CREATE, 0600));
	memset(&token, 0, sizeof(token));
	CHK_OK(txn->set_commit_token(txn, &token));
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = "tk"; key.size = 2;
	data.data = "tv"; data.size = 2;
	CHK_OK(dbp->put(dbp, txn, &key, &data, 0));
	CHK_OK(txn->commit(txn, 0));
	/*
	 * __txn_applied_pp on a token from a NON-replication env.  In an env
	 * with no DB_INIT_REP the token carries gen 0; txn_applied answers
	 * immediately rather than rejecting, so drive it without asserting
	 * the sign of the answer -- the branch is the point.
	 */
	(void)dbenv->txn_applied(dbenv, &token, 0, 0);
	checks++;
	CHK_OK(dbp->close(dbp, 0));

	/* DB_TXN_NOWAIT / DB_TXN_NOSYNC / sync flavors of commit. */
	CHK_OK(dbenv->txn_begin(dbenv, NULL, &txn, DB_TXN_NOWAIT));
	CHK_OK(txn->commit(txn, DB_TXN_NOSYNC));
	CHK_OK(dbenv->txn_begin(dbenv, NULL, &txn, DB_TXN_NOSYNC));
	CHK_OK(txn->commit(txn, DB_TXN_SYNC));
	CHK_OK(dbenv->txn_begin(dbenv, NULL, &txn, 0));
	CHK_OK(txn->abort(txn));
	/* A bogus txn_begin flag is the rejection branch. */
	CHK_FAILS(dbenv->txn_begin(dbenv, NULL, &txn, 0x40000000));
}

/*
 * err_helpers --
 *	common/db_err.c has 10 never-called formatting helpers.  Most are
 *	reachable only from an internal failure, but three are reachable from
 *	the public API with bad arguments, and the DB_ENV err/errx emitters
 *	are directly callable.  Drive what is reachable and report the rest.
 */
static void
err_helpers(dbenv)
	DB_ENV *dbenv;
{
	DB *dbp;

	/* __db_errcall / __db_errfile via the public emitters. */
	dbenv->err(dbenv, EINVAL, "cov_api_surface: expected env err()");
	dbenv->errx(dbenv, "cov_api_surface: expected env errx()");
	checks += 2;

	/*
	 * __db_unknown_flag / __db_unknown_type: reached by handing the API a
	 * flag or type it does not know.  db_create + open with a bogus type
	 * is the cleanest route to __db_unknown_type.
	 */
	CHK_OK(db_create(&dbp, dbenv, 0));
	CHK_FAILS(dbp->open(dbp, NULL, "cov_bogus.db", NULL,
	    (DBTYPE)999, DB_CREATE, 0600));
	CHK_OK(dbp->close(dbp, 0));

	/* __db_not_txn_env: a txn operation in an env with no DB_INIT_TXN is
	 * driven by the no-txn env in main(). */
}

/*
 * no_txn_env --
 *	An env WITHOUT DB_INIT_TXN: __db_not_txn_env and the "operation
 *	requires transactions" rejection arms.  Also drives DB->cds_group
 *	(db_cds.c, 8 never-called functions) which needs DB_INIT_CDB.
 */
static void
no_txn_env()
{
	DB_ENV *dbenv;
	DB_TXN *txn;
	int ret;

	if ((ret = db_env_create(&dbenv, 0)) != 0) {
		fprintf(stderr, "FAIL: db_env_create: %s\n",
		    db_strerror(ret));
		fails++;
		return;
	}
	dbenv->set_errfile(dbenv, NULL);
	(void)system("mkdir -p COVAPI_TESTDIR_notxn");
	(void)system("rm -f COVAPI_TESTDIR_notxn/__db.* "
	    "COVAPI_TESTDIR_notxn/*.db 2>/dev/null");
	if ((ret = dbenv->open(dbenv, "COVAPI_TESTDIR_notxn",
	    DB_CREATE | DB_INIT_MPOOL | DB_PRIVATE, 0600)) != 0) {
		fprintf(stderr, "FAIL: open no-txn env: %s\n",
		    db_strerror(ret));
		fails++;
		(void)dbenv->close(dbenv, 0);
		return;
	}
	/* __db_not_txn_env: txn_begin with no DB_INIT_TXN. */
	CHK_FAILS(dbenv->txn_begin(dbenv, NULL, &txn, 0));
	CHK_FAILS(dbenv->txn_checkpoint(dbenv, 0, 0, 0));
	/* Likewise the log and lock subsystems are absent. */
	{
		DB_LSN lsn;
		u_int32_t id;
		CHK_FAILS(dbenv->log_flush(dbenv, NULL));
		CHK_FAILS(dbenv->lock_id(dbenv, &id));
		COMPQUIET(lsn.file, 0);
	}
	CHK_OK(dbenv->close(dbenv, 0));
}

/*
 * cds_group --
 *	DB_ENV->cds_group_begin (db/db_cds.c) -- 8 of that file's 11 functions
 *	are never called.  A CDS (Concurrent Data Store) group is a
 *	pseudo-transaction handle whose method table is mostly "not supported"
 *	stubs (__cdsgroup_notsup, __cdsgroup_prepare, __cdsgroup_id, ...).
 *	Driving each method on a CDS group handle covers them.
 */
static void
cds_group()
{
	DB_ENV *dbenv;
	DB *dbp;
	DB_TXN *cds;
	DBT key, data;
	const char *name;
	u_int32_t a;
	int ret;

	if ((ret = db_env_create(&dbenv, 0)) != 0) {
		fprintf(stderr, "FAIL: db_env_create: %s\n",
		    db_strerror(ret));
		fails++;
		return;
	}
	dbenv->set_errfile(dbenv, NULL);
	(void)system("mkdir -p COVAPI_TESTDIR_cds");
	(void)system("rm -f COVAPI_TESTDIR_cds/__db.* "
	    "COVAPI_TESTDIR_cds/*.db 2>/dev/null");
	if ((ret = dbenv->open(dbenv, "COVAPI_TESTDIR_cds",
	    DB_CREATE | DB_INIT_CDB | DB_INIT_MPOOL, 0600)) != 0) {
		fprintf(stderr, "note: open CDS env: %s\n", db_strerror(ret));
		(void)dbenv->close(dbenv, 0);
		return;
	}

	CHK_OK(db_create(&dbp, dbenv, 0));
	CHK_OK(dbp->open(dbp, NULL, "cov_cds.db", NULL, DB_BTREE,
	    DB_CREATE, 0600));

	/* A CDS group: the handle whose methods are the never-called stubs. */
	cds = NULL;
	if (dbenv->cdsgroup_begin(dbenv, &cds) == 0 && cds != NULL) {
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = "c"; key.size = 1;
		data.data = "d"; data.size = 1;
		CHK_OK(dbp->put(dbp, cds, &key, &data, 0));

		/* __cdsgroup_id. */
		CHK_TRUE(cds->id(cds) != 0, "cdsgroup id");
		/*
		 * __cdsgroup_notsup: a CDS group is not a real transaction, so
		 * most DB_TXN methods must refuse.
		 *
		 * FINDING (reported, not fixed): __cdsgroup_begin
		 * (src/db/db_cds.c ~line 155) installs only EIGHT of DB_TXN's
		 * TWELVE methods -- abort, commit, discard, id, prepare,
		 * get_name, set_name, set_timeout.  get_priority,
		 * set_priority, set_commit_token and set_txn_lsnp are left as
		 * NULL function pointers, so an application calling any of
		 * those on a CDS group handle jumps to address 0 (verified:
		 * SIGSEGV at 0x0 with cds_group() on the stack) instead of
		 * getting the DB_OPNOTSUP that __cdsgroup_notsup exists to
		 * return.  See test/coverage/FULL-COVERAGE-REPORT-4.md.
		 *
		 * Only the installed methods are called here.
		 */
		CHK_FAILS(cds->prepare(cds, (u_int8_t *)
		    "0123456789012345678901234567890123456789"));
		CHK_FAILS(cds->set_timeout(cds, 1000, DB_SET_TXN_TIMEOUT));
		/* get_name / set_name are installed but return DB_OPNOTSUP. */
		CHK_FAILS(cds->set_name(cds, "cov_cds_group"));
		name = NULL;
		CHK_FAILS(cds->get_name(cds, &name));
		CHK_FAILS(cds->discard(cds, 0));
		COMPQUIET(a, 0);
		/* __cdsgroup_commit releases the group's locks. */
		CHK_OK(cds->commit(cds, 0));

		/*
		 * __cdsgroup_abort: also a notsup stub -- a CDS group cannot be
		 * rolled back (there is no undo), so abort() returns
		 * DB_OPNOTSUP and the group must still be released with
		 * commit().  Both calls are driven here.
		 */
		cds = NULL;
		if (dbenv->cdsgroup_begin(dbenv, &cds) == 0 && cds != NULL) {
			CHK_FAILS(cds->abort(cds));
			CHK_OK(cds->commit(cds, 0));
		}
	}
	CHK_OK(dbp->close(dbp, 0));
	CHK_OK(dbenv->close(dbenv, 0));
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	DB_ENV *dbenv;
	int ret;

	COMPQUIET(argc, 0);
	COMPQUIET(argv, NULL);

	(void)signal(SIGALRM, on_alarm);
	(void)alarm(ALARM_SECS);

	printf("cov_api_surface: DB_ENV/DB/DBC/DB_TXN getter + callback "
	    "surface\n");

	clean_home();
	if ((ret = db_env_create(&dbenv, 0)) != 0) {
		fprintf(stderr, "FAIL: db_env_create: %s\n",
		    db_strerror(ret));
		return (2);
	}

	printf("1. DB_ENV getters/setters before open\n");
	env_getters_pre_open(dbenv);

	printf("2. DB_ENV open\n");
	if ((ret = dbenv->open(dbenv, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN, 0600)) != 0) {
		fprintf(stderr, "FAIL: open env: %s\n", db_strerror(ret));
		(void)dbenv->close(dbenv, 0);
		return (2);
	}

	printf("3. DB_ENV getters after open + illegal-after-open setters\n");
	env_getters_post_open(dbenv);

	printf("4. DB / DBC / DB_MPOOLFILE getters\n");
	db_getters(dbenv);

	printf("5. DB_TXN getters + commit tokens\n");
	txn_getters(dbenv);

	printf("6. error-helper emitters\n");
	err_helpers(dbenv);

	CHK_OK(dbenv->close(dbenv, 0));

	printf("7. standalone DB handle (no env): allocator + cache\n");
	db_getters_standalone();

	printf("8. env without DB_INIT_TXN\n");
	no_txn_env();

	printf("9. CDS group handle\n");
	cds_group();

	(void)alarm(0);
	printf("cov_api_surface: %d checks, %d failures\n", checks, fails);
	if (fails != 0) {
		printf("cov_api_surface: FAIL\n");
		return (1);
	}
	printf("cov_api_surface: PASS\n");
	return (0);
}
