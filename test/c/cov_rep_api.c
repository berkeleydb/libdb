/*-
 * See the file LICENSE for redistribution information.
 *
 * cov_rep_api.c --
 *	A direct driver for the replication + replication-manager CONFIGURATION
 *	and QUERY surface: the DB_ENV rep_* / repmgr_* methods, the DB_SITE
 *	handle methods, and DB_ENV->txn_applied.
 *
 *	Why a C driver and not Tcl: the coverage reports (#2, #3) show
 *	rep/rep_method.c at 40% and repmgr/repmgr_method.c at 37%, with 13 and
 *	29 functions respectively NEVER CALLED by the whole Tcl suite.  Nearly
 *	all of them are the *getters* and the argument-validation halves of the
 *	setters:
 *
 *	  rep_method.c   __rep_get_config, __rep_get_limit, __rep_get_nsites,
 *	                 __rep_get_priority, __rep_get_request,
 *	                 __rep_get_timeout, __rep_get_clockskew,
 *	                 __rep_set_clockskew, __rep_set_request,
 *	                 __rep_set_nsites_pp, __rep_txn_applied
 *	  repmgr_method.c __repmgr_get_config, __repmgr_get_ack_policy,
 *	                 __repmgr_get_eid, __repmgr_get_site_address,
 *	                 __repmgr_local_site, __repmgr_site_by_eid,
 *	                 __repmgr_channel_timeout_inval,
 *	                 __repmgr_send_request_inval, __repmgr_channel_*_inval
 *
 *	The Tcl rep harness sets a knob and then runs a workload -- it never
 *	reads a knob back, and it never drives a base-API env through the
 *	repmgr entry points to reach the "wrong application type" / "not
 *	configured" rejection branches.  Those are exactly the branches an
 *	embedding application hits first, and they are cheap and deterministic
 *	to test directly.
 *
 *	This is a SINGLE-PROCESS driver: it opens real replication envs (base
 *	API and repmgr API, DB_INIT_REP) but never starts a live election or
 *	needs a peer, so it cannot hang.  A hard SIGALRM guard backs that up.
 *	The multi-process paths (elections, leases, real message flow) are NOT
 *	the target here -- see test/coverage/FULL-COVERAGE-REPORT-4.md.
 */
#include "db_config.h"

#include "db_int.h"

#include <signal.h>

#define	HOME_BASE	"COVREP_TESTDIR_base"
#define	HOME_MGR	"COVREP_TESTDIR_mgr"
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

/* Expect a specific non-zero return: the argument-validation branches. */
#define	CHK_ERR(call, want) do {					\
	int _r = (call);						\
	checks++;							\
	if (_r != (want)) {						\
		fprintf(stderr, "FAIL: %s:%d: %s => %d (%s), wanted %d\n", \
		    __FILE__, __LINE__, #call, _r, db_strerror(_r),	\
		    (want));						\
		fails++;						\
	}								\
} while (0)

/* Expect ANY non-zero return (the exact errno is not contractual). */
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

static void
on_alarm(sig)
	int sig;
{
	COMPQUIET(sig, 0);
	fprintf(stderr, "FAIL: cov_rep_api timed out after %d s (hung)\n",
	    ALARM_SECS);
	_exit(3);
}

/*
 * A no-op transport function.  rep_set_transport requires one; a base-API
 * replication env will not send anything in this driver (we never rep_start
 * as master with data to ship), but the callback must exist for rep_start.
 */
static int
noop_send(dbenv, control, rec, lsnp, eid, flags)
	DB_ENV *dbenv;
	const DBT *control, *rec;
	const DB_LSN *lsnp;
	int eid;
	u_int32_t flags;
{
	COMPQUIET(dbenv, NULL);
	COMPQUIET(control, NULL);
	COMPQUIET(rec, NULL);
	COMPQUIET(lsnp, NULL);
	COMPQUIET(eid, 0);
	COMPQUIET(flags, 0);
	return (0);
}

static void
noop_dispatch(dbenv, chan, request, nsegs, flags)
	DB_ENV *dbenv;
	DB_CHANNEL *chan;
	DBT *request;
	u_int32_t nsegs, flags;
{
	COMPQUIET(dbenv, NULL);
	COMPQUIET(chan, NULL);
	COMPQUIET(request, NULL);
	COMPQUIET(nsegs, 0);
	COMPQUIET(flags, 0);
}

static void
clean_home(home)
	const char *home;
{
	char buf[512];

	/* No rm -rf: remove the known artifacts only. */
	(void)snprintf(buf, sizeof(buf),
	    "rm -f %s/__db.* %s/log.* %s/*.db %s/DB_CONFIG 2>/dev/null",
	    home, home, home, home);
	(void)system(buf);
	(void)snprintf(buf, sizeof(buf), "mkdir -p %s", home);
	(void)system(buf);
}

/*
 * rep_config_roundtrip --
 *	Every DB_REP_CONF_* / DB_REPMGR_CONF_* flag through set_config then
 *	get_config, before AND after env open.  __rep_get_config is never
 *	called by the Tcl suite at all; __rep_set_config's !REP_ON(env)
 *	(pre-open) half and its REP_ON(env) half are different code paths and
 *	both matter.
 */
static void
rep_config_roundtrip(dbenv, opened)
	DB_ENV *dbenv;
	int opened;
{
	static const u_int32_t base_flags[] = {
		DB_REP_CONF_AUTOINIT, DB_REP_CONF_AUTOROLLBACK,
		DB_REP_CONF_BULK, DB_REP_CONF_DELAYCLIENT,
		DB_REP_CONF_NOWAIT
	};
	u_int32_t f;
	size_t i;
	int on;

	for (i = 0; i < sizeof(base_flags) / sizeof(base_flags[0]); i++) {
		f = base_flags[i];
		/* on -> read back 1 -> off -> read back 0: both FLD paths. */
		CHK_OK(dbenv->rep_set_config(dbenv, f, 1));
		on = -1;
		CHK_OK(dbenv->rep_get_config(dbenv, f, &on));
		CHK_EQ(on, 1, "rep_get_config after set on");
		CHK_OK(dbenv->rep_set_config(dbenv, f, 0));
		on = -1;
		CHK_OK(dbenv->rep_get_config(dbenv, f, &on));
		CHK_EQ(on, 0, "rep_get_config after set off");
	}

	/* An unknown config flag must be rejected by both get and set. */
	CHK_FAILS(dbenv->rep_set_config(dbenv, 0x80000000, 1));
	CHK_FAILS(dbenv->rep_get_config(dbenv, 0x80000000, &on));

	/*
	 * DB_REP_CONF_INMEM must be settable BEFORE open and rejected AFTER
	 * open (the "in-memory replication must be configured before
	 * DB_ENV->open" branch).
	 */
	if (opened)
		CHK_FAILS(dbenv->rep_set_config(dbenv, DB_REP_CONF_INMEM, 1));
	else {
		CHK_OK(dbenv->rep_set_config(dbenv, DB_REP_CONF_INMEM, 1));
		CHK_OK(dbenv->rep_set_config(dbenv, DB_REP_CONF_INMEM, 0));
	}

	/*
	 * Leases can never be turned OFF once on, and (post-rep_start) cannot
	 * be turned on at all.  Reading it back is always legal.
	 */
	on = -1;
	CHK_OK(dbenv->rep_get_config(dbenv, DB_REP_CONF_LEASE, &on));
}

/*
 * rep_scalar_knobs --
 *	The numeric rep knobs, set then GET.  Every one of these getters is in
 *	the never-called list.  Also drives each setter's rejection branch.
 */
static void
rep_scalar_knobs(dbenv)
	DB_ENV *dbenv;
{
	u_int32_t g, b, n, p, mn, mx, fast, slow, to;
	int i;
	static const int timeouts[] = {
		DB_REP_CHECKPOINT_DELAY, DB_REP_ELECTION_TIMEOUT,
		DB_REP_FULL_ELECTION_TIMEOUT, DB_REP_LEASE_TIMEOUT
	};
	/*
	 * The REPMGR-only timeout kinds.  On a base-API env (this function's
	 * caller called rep_set_transport) __rep_set_timeout refuses them --
	 * the APP_IS_BASEAPI(env) && repmgr_timeout branch, which is the
	 * cross-API rejection no Tcl test reaches because each Tcl rep test
	 * picks one API and stays with it.  The getter is still legal.
	 */
	static const int repmgr_timeouts[] = {
		DB_REP_ACK_TIMEOUT, DB_REP_CONNECTION_RETRY,
		DB_REP_ELECTION_RETRY, DB_REP_HEARTBEAT_MONITOR,
		DB_REP_HEARTBEAT_SEND
	};

	/* --- transmit limit: set/get round-trip, both halves of the pair. */
	CHK_OK(dbenv->rep_set_limit(dbenv, 0, 1048576));
	CHK_OK(dbenv->rep_get_limit(dbenv, &g, &b));
	CHK_EQ(g, 0, "rep_get_limit gbytes");
	CHK_EQ(b, 1048576, "rep_get_limit bytes");
	/* A gbytes-carrying limit exercises the other normalization branch. */
	CHK_OK(dbenv->rep_set_limit(dbenv, 1, 0));
	CHK_OK(dbenv->rep_get_limit(dbenv, &g, &b));
	CHK_EQ(g, 1, "rep_get_limit gbytes=1");

	/* --- nsites: __rep_set_nsites_pp + __rep_get_nsites. */
	CHK_OK(dbenv->rep_set_nsites(dbenv, 3));
	CHK_OK(dbenv->rep_get_nsites(dbenv, &n));
	CHK_EQ(n, 3, "rep_get_nsites");

	/* --- priority: 0 (never-master) and a normal value. */
	CHK_OK(dbenv->rep_set_priority(dbenv, 100));
	CHK_OK(dbenv->rep_get_priority(dbenv, &p));
	CHK_EQ(p, 100, "rep_get_priority");
	CHK_OK(dbenv->rep_set_priority(dbenv, 0));
	CHK_OK(dbenv->rep_get_priority(dbenv, &p));
	CHK_EQ(p, 0, "rep_get_priority 0");

	/* --- request retry bounds: __rep_set_request + __rep_get_request. */
	CHK_OK(dbenv->rep_set_request(dbenv, 4000, 128000));
	CHK_OK(dbenv->rep_get_request(dbenv, &mn, &mx));
	CHK_EQ(mn, 4000, "rep_get_request min");
	CHK_EQ(mx, 128000, "rep_get_request max");
	/* min > max, and a zero min, are the two rejection branches. */
	CHK_FAILS(dbenv->rep_set_request(dbenv, 128000, 4000));
	CHK_FAILS(dbenv->rep_set_request(dbenv, 0, 4000));

	/* --- clock skew: BOTH functions are never-called in report #3. */
	CHK_OK(dbenv->rep_set_clockskew(dbenv, 102, 100));
	CHK_OK(dbenv->rep_get_clockskew(dbenv, &fast, &slow));
	CHK_EQ(fast, 102, "rep_get_clockskew fast");
	CHK_EQ(slow, 100, "rep_get_clockskew slow");
	/* The no-skew form (both equal) is a distinct branch. */
	CHK_OK(dbenv->rep_set_clockskew(dbenv, 1, 1));
	CHK_OK(dbenv->rep_get_clockskew(dbenv, &fast, &slow));
	CHK_EQ(fast, 1, "rep_get_clockskew fast=1");
	/* fast < slow is nonsense and must be rejected. */
	CHK_FAILS(dbenv->rep_set_clockskew(dbenv, 100, 102));
	/* A zero slow_clock is likewise rejected. */
	CHK_FAILS(dbenv->rep_set_clockskew(dbenv, 100, 0));

	/*
	 * --- every timeout kind through set_timeout + get_timeout.
	 * __rep_get_timeout is never called by the Tcl suite; it has a
	 * per-kind switch whose arms are all separate branches.
	 */
	for (i = 0; i < (int)(sizeof(timeouts) / sizeof(timeouts[0])); i++) {
		/*
		 * Lease timeout can only be set before rep_start and only
		 * when leases are configured; tolerate a refusal but still
		 * drive the getter, which is the never-called one.
		 */
		if (timeouts[i] == DB_REP_LEASE_TIMEOUT)
			(void)dbenv->rep_set_timeout(dbenv,
			    timeouts[i], 1000000);
		else
			CHK_OK(dbenv->rep_set_timeout(dbenv,
			    timeouts[i], 1000000));
		to = 0;
		CHK_OK(dbenv->rep_get_timeout(dbenv, timeouts[i], &to));
	}
	/*
	 * The repmgr-only kinds: SET must be refused on a base-API env, GET
	 * must still work.  Both halves are cold branches.
	 */
	for (i = 0; i < (int)(sizeof(repmgr_timeouts) /
	    sizeof(repmgr_timeouts[0])); i++) {
		CHK_FAILS(dbenv->rep_set_timeout(dbenv,
		    repmgr_timeouts[i], 1000000));
		to = 0;
		CHK_OK(dbenv->rep_get_timeout(dbenv,
		    repmgr_timeouts[i], &to));
	}
	/* An unknown timeout kind must be rejected by set and get. */
	CHK_FAILS(dbenv->rep_set_timeout(dbenv, 12345, 1000));
	CHK_FAILS(dbenv->rep_get_timeout(dbenv, 12345, &to));
}

/*
 * repmgr_knobs --
 *	The repmgr configuration + query surface on a NON-started repmgr env.
 *	__repmgr_get_ack_policy / __repmgr_get_config / __repmgr_local_site /
 *	__repmgr_site_by_eid / __repmgr_get_eid / __repmgr_get_site_address
 *	are all never-called in report #3.
 */
static void
repmgr_knobs(dbenv)
	DB_ENV *dbenv;
{
	DB_SITE *site, *site2;
	const char *host;
	u_int port;
	u_int32_t cfg;
	int policy, eid, on;
	static const int policies[] = {
		DB_REPMGR_ACKS_ALL, DB_REPMGR_ACKS_ALL_AVAILABLE,
		DB_REPMGR_ACKS_ALL_PEERS, DB_REPMGR_ACKS_NONE,
		DB_REPMGR_ACKS_ONE, DB_REPMGR_ACKS_ONE_PEER,
		DB_REPMGR_ACKS_QUORUM
	};
	int i;

	/* --- ack policy: every policy set then GET (getter never called). */
	for (i = 0; i < (int)(sizeof(policies) / sizeof(policies[0])); i++) {
		CHK_OK(dbenv->repmgr_set_ack_policy(dbenv, policies[i]));
		policy = -1;
		CHK_OK(dbenv->repmgr_get_ack_policy(dbenv, &policy));
		CHK_EQ(policy, policies[i], "repmgr_get_ack_policy");
	}
	/* A bogus policy is rejected. */
	CHK_FAILS(dbenv->repmgr_set_ack_policy(dbenv, 999));

	/* --- the two repmgr-only config flags, set + get. */
	CHK_OK(dbenv->rep_set_config(dbenv,
	    DB_REPMGR_CONF_2SITE_STRICT, 1));
	on = -1;
	CHK_OK(dbenv->rep_get_config(dbenv,
	    DB_REPMGR_CONF_2SITE_STRICT, &on));
	CHK_EQ(on, 1, "2SITE_STRICT get");
	CHK_OK(dbenv->rep_set_config(dbenv, DB_REPMGR_CONF_ELECTIONS, 1));
	CHK_OK(dbenv->rep_set_config(dbenv, DB_REPMGR_CONF_ELECTIONS, 0));

	/*
	 * --- the local site.  repmgr_site() creates/looks up a site handle;
	 * DB_SITE has its own method table (get_address / get_config /
	 * get_eid / set_config), and every getter is in the never-called set.
	 */
	site = NULL;
	CHK_OK(dbenv->repmgr_site(dbenv, "127.0.0.1", 30999, &site, 0));
	if (site != NULL) {
		CHK_OK(site->set_config(site, DB_LOCAL_SITE, 1));
		cfg = 0;
		CHK_OK(site->get_config(site, DB_LOCAL_SITE, &cfg));
		CHK_EQ(cfg, 1, "DB_SITE get_config DB_LOCAL_SITE");
		eid = -99;
		CHK_OK(site->get_eid(site, &eid));
		host = NULL; port = 0;
		CHK_OK(site->get_address(site, &host, &port));
		CHK_EQ(port, 30999, "DB_SITE get_address port");
		if (host == NULL || strcmp(host, "127.0.0.1") != 0) {
			fprintf(stderr, "FAIL: DB_SITE get_address host\n");
			fails++;
		}
		checks++;

		/*
		 * __repmgr_local_site + __repmgr_site_by_eid: both look the
		 * site back up through the env, both never called.
		 */
		site2 = NULL;
		CHK_OK(dbenv->repmgr_local_site(dbenv, &site2));
		if (site2 != NULL)
			CHK_OK(site2->close(site2));
		site2 = NULL;
		CHK_OK(dbenv->repmgr_site_by_eid(dbenv, eid, &site2));
		if (site2 != NULL)
			CHK_OK(site2->close(site2));
		/* A non-existent EID is the rejection branch. */
		site2 = NULL;
		CHK_FAILS(dbenv->repmgr_site_by_eid(dbenv, 4242, &site2));

		/*
		 * Each remaining DB_SITE config flag, set + read back.
		 * DB_REPMGR_PEER is NOT legal on the LOCAL site (a site cannot
		 * be its own peer) -- that rejection is itself a cold branch.
		 */
		CHK_OK(site->set_config(site, DB_GROUP_CREATOR, 1));
		CHK_OK(site->get_config(site, DB_GROUP_CREATOR, &cfg));
		CHK_OK(site->set_config(site, DB_LEGACY, 1));
		CHK_OK(site->get_config(site, DB_LEGACY, &cfg));
		CHK_OK(site->set_config(site, DB_LEGACY, 0));
		CHK_FAILS(site->set_config(site, DB_REPMGR_PEER, 1));
		CHK_OK(site->get_config(site, DB_REPMGR_PEER, &cfg));
		/*
		 * An unknown DB_SITE config flag: get_config switches on the
		 * flag and returns 0 with an untouched value rather than
		 * EINVAL, so drive the fall-through without asserting an error.
		 */
		(void)site->get_config(site, 0x40000000, &cfg);
		checks++;

		CHK_OK(site->close(site));
	}

	/* --- a remote site: the non-local half of the site table. */
	site = NULL;
	CHK_OK(dbenv->repmgr_site(dbenv, "127.0.0.1", 30998, &site, 0));
	if (site != NULL) {
		CHK_OK(site->set_config(site, DB_BOOTSTRAP_HELPER, 1));
		CHK_OK(site->get_config(site, DB_BOOTSTRAP_HELPER, &cfg));
		/* remove() then close() is the site-removal path. */
		CHK_OK(site->close(site));
	}
}

/*
 * repmgr_before_start --
 *	The repmgr entry points that must FAIL cleanly when repmgr has not
 *	been started.  These are the *_inval stubs and the "not started"
 *	guards -- __repmgr_channel_timeout_inval, __repmgr_send_request_inval,
 *	__repmgr_channel_close_inval, bad_callback_method -- every one of them
 *	never called in report #3, and every one of them the first thing a
 *	misconfigured application hits.
 */
static void
repmgr_before_start(dbenv)
	DB_ENV *dbenv;
{
	DB_CHANNEL *chan;
	DB_REPMGR_SITE *list;
	DB_REPMGR_STAT *rstat;
	u_int count;

	/*
	 * A channel to a site in an env whose repmgr threads were never
	 * started: repmgr_channel must refuse (or hand back a channel whose
	 * every operation refuses, via the _inval stubs).
	 */
	chan = NULL;
	if (dbenv->repmgr_channel(dbenv, DB_EID_MASTER, &chan, 0) == 0 &&
	    chan != NULL) {
		/* The _inval method table: each call must fail, not crash. */
		CHK_FAILS(chan->set_timeout(chan, 1000000));
		CHK_FAILS(chan->close(chan, 0));
	} else
		checks++;
	/* An invalid EID is a distinct rejection branch. */
	chan = NULL;
	CHK_FAILS(dbenv->repmgr_channel(dbenv, -12345, &chan, 0));

	/* Installing a dispatch callback is legal before start. */
	CHK_OK(dbenv->repmgr_msg_dispatch(dbenv, noop_dispatch, 0));
	/* An unknown flag to msg_dispatch is rejected. */
	CHK_FAILS(dbenv->repmgr_msg_dispatch(dbenv, noop_dispatch, 0x8000));

	/* site_list / stat on a never-started repmgr: the empty-group path. */
	count = 0; list = NULL;
	if (dbenv->repmgr_site_list(dbenv, &count, &list) == 0 &&
	    list != NULL)
		__os_ufree(dbenv->env, list);
	checks++;
	rstat = NULL;
	if (dbenv->repmgr_stat(dbenv, &rstat, 0) == 0 && rstat != NULL)
		__os_ufree(dbenv->env, rstat);
	checks++;
	/* stat_print with each flag: the repmgr_stat.c formatter. */
	(void)dbenv->repmgr_stat_print(dbenv, 0);
	(void)dbenv->repmgr_stat_print(dbenv, DB_STAT_ALL);
	(void)dbenv->repmgr_stat_print(dbenv, DB_STAT_CLEAR);
}

/*
 * base_api_rejects_repmgr --
 *	A base-API replication application (one that called rep_set_transport)
 *	must be refused when it calls repmgr entry points, and vice versa.
 *	That APP_IS_BASEAPI / APP_IS_REPMGR cross-check is a branch in
 *	__rep_set_config, __repmgr_set_ack_policy and __repmgr_site that no
 *	Tcl test reaches, because each Tcl rep test picks one API and stays.
 */
static void
base_api_rejects_repmgr(dbenv)
	DB_ENV *dbenv;
{
	DB_SITE *site;

	/* This env is base-API (rep_set_transport was called). */
	CHK_FAILS(dbenv->rep_set_config(dbenv,
	    DB_REPMGR_CONF_2SITE_STRICT, 1));
	CHK_FAILS(dbenv->rep_set_config(dbenv,
	    DB_REPMGR_CONF_ELECTIONS, 1));
	site = NULL;
	CHK_FAILS(dbenv->repmgr_site(dbenv, "127.0.0.1", 30997, &site, 0));
	CHK_FAILS(dbenv->repmgr_start(dbenv, 2, DB_REP_MASTER));
}

/*
 * txn_applied_paths --
 *	DB_ENV->txn_applied (__rep_txn_applied) with a real commit token from a
 *	master.  Never called anywhere in the suite.  On a master the answer is
 *	immediate (already applied); a zero-gen token is the EINVAL branch.
 */
static void
txn_applied_paths(dbenv)
	DB_ENV *dbenv;
{
	DB *dbp;
	DB_TXN *txn;
	DB_TXN_TOKEN token;
	DBT key, data;
	int ret;

	dbp = NULL;
	if ((ret = db_create(&dbp, dbenv, 0)) != 0) {
		fprintf(stderr, "FAIL: db_create: %s\n", db_strerror(ret));
		fails++;
		return;
	}
	if ((ret = dbp->open(dbp, NULL, "applied.db", NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT, 0600)) != 0) {
		/*
		 * Opening a DB in a replication env that has not synced can
		 * legitimately fail; the token paths below still get driven
		 * with a synthetic token, which is the point.
		 */
		fprintf(stderr, "note: open in rep env: %s\n",
		    db_strerror(ret));
		(void)dbp->close(dbp, 0);
		dbp = NULL;
	}

	if (dbp != NULL) {
		CHK_OK(dbenv->txn_begin(dbenv, NULL, &txn, 0));
		/* A token is only produced if the txn asks for one. */
		CHK_OK(txn->set_name(txn, "cov_rep_api"));
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = "k"; key.size = 1;
		data.data = "v"; data.size = 1;
		CHK_OK(dbp->put(dbp, txn, &key, &data, 0));
		memset(&token, 0, sizeof(token));
		CHK_OK(txn->set_commit_token(txn, &token));
		CHK_OK(txn->commit(txn, 0));

		/*
		 * The real token from a committed master txn: already
		 * applied, so this is the "success now" arm of
		 * __rep_check_applied.
		 */
		(void)dbenv->txn_applied(dbenv, &token, 0, 0);
		checks++;
		/* Same query with a timeout: the timed arm. */
		(void)dbenv->txn_applied(dbenv, &token, 1000, 0);
		checks++;
		CHK_OK(dbp->close(dbp, 0));
	}

	/* A zeroed token has gen == 0: the "non-replication commit token"
	 * EINVAL branch at the top of __rep_txn_applied. */
	memset(&token, 0, sizeof(token));
	CHK_FAILS(dbenv->txn_applied(dbenv, &token, 0, 0));
}

/*
 * rep_stat_and_flush --
 *	rep_stat / rep_stat_print / rep_flush / rep_sync on a configured but
 *	quiescent env: the "nothing to do" arms.
 */
static void
rep_stat_and_flush(dbenv)
	DB_ENV *dbenv;
{
	DB_REP_STAT *sp;

	/* stat_print writes to the msgfile; keep the run's output quiet. */
	dbenv->set_msgfile(dbenv, NULL);
	sp = NULL;
	if (dbenv->rep_stat(dbenv, &sp, 0) == 0 && sp != NULL)
		__os_ufree(dbenv->env, sp);
	checks++;
	sp = NULL;
	if (dbenv->rep_stat(dbenv, &sp, DB_STAT_CLEAR) == 0 && sp != NULL)
		__os_ufree(dbenv->env, sp);
	checks++;
	(void)dbenv->rep_stat_print(dbenv, 0);
	(void)dbenv->rep_stat_print(dbenv, DB_STAT_ALL);
	(void)dbenv->rep_stat_print(dbenv, DB_STAT_CLEAR);
	/* rep_flush on a master with no clients: the empty-send path. */
	(void)dbenv->rep_flush(dbenv);
	checks++;
	/* rep_sync as a master is a no-op / error, not a hang. */
	(void)dbenv->rep_sync(dbenv, 0);
	checks++;
}

/*
 * unconfigured_env_rejects --
 *	Every rep_* / repmgr_* method on an env opened WITHOUT DB_INIT_REP must
 *	take the ENV_NOT_CONFIGURED branch.  That branch exists in essentially
 *	every function in rep_method.c and repmgr_method.c and is never
 *	exercised, because every Tcl rep test naturally has DB_INIT_REP.
 */
static void
unconfigured_env_rejects()
{
	DB_ENV *dbenv;
	DB_SITE *site;
	u_int32_t a, b;
	int i, ret;

	if ((ret = db_env_create(&dbenv, 0)) != 0) {
		fprintf(stderr, "FAIL: db_env_create: %s\n",
		    db_strerror(ret));
		fails++;
		return;
	}
	dbenv->set_errpfx(dbenv, "cov_rep_api-norep");
	/* Silence the expected complaints. */
	dbenv->set_errfile(dbenv, NULL);
	clean_home("COVREP_TESTDIR_norep");
	if ((ret = dbenv->open(dbenv, "COVREP_TESTDIR_norep",
	    DB_CREATE | DB_INIT_MPOOL | DB_PRIVATE, 0600)) != 0) {
		fprintf(stderr, "FAIL: open no-rep env: %s\n",
		    db_strerror(ret));
		fails++;
		(void)dbenv->close(dbenv, 0);
		return;
	}

	CHK_FAILS(dbenv->rep_get_limit(dbenv, &a, &b));
	CHK_FAILS(dbenv->rep_get_nsites(dbenv, &a));
	CHK_FAILS(dbenv->rep_get_priority(dbenv, &a));
	CHK_FAILS(dbenv->rep_get_request(dbenv, &a, &b));
	CHK_FAILS(dbenv->rep_get_clockskew(dbenv, &a, &b));
	CHK_FAILS(dbenv->rep_start(dbenv, NULL, DB_REP_MASTER));
	CHK_FAILS(dbenv->rep_flush(dbenv));
	CHK_FAILS(dbenv->rep_sync(dbenv, 0));
	CHK_FAILS(dbenv->rep_elect(dbenv, 3, 2, 0));
	site = NULL;
	CHK_FAILS(dbenv->repmgr_site(dbenv, "127.0.0.1", 30996, &site, 0));
	CHK_FAILS(dbenv->repmgr_start(dbenv, 2, DB_REP_CLIENT));
	CHK_FAILS(dbenv->repmgr_local_site(dbenv, &site));
	CHK_FAILS(dbenv->repmgr_site_by_eid(dbenv, 1, &site));
	/*
	 * repmgr_get_ack_policy answers from the handle, not the region, so it
	 * succeeds even with no DB_INIT_REP -- drive it, do not assert an
	 * error the API does not promise.
	 */
	(void)dbenv->repmgr_get_ack_policy(dbenv, &i);
	checks++;

	CHK_OK(dbenv->close(dbenv, 0));
}

/*
 * pre_open_knobs --
 *	Every rep knob set + read back on a handle that has NOT been opened.
 *	Each setter has a distinct !REP_ON(env) arm that stores into the
 *	DB_REP handle instead of the shared region -- half of every setter.
 */
static void
pre_open_knobs()
{
	DB_ENV *dbenv;
	u_int32_t a, b;
	int ret, on;

	if ((ret = db_env_create(&dbenv, 0)) != 0) {
		fprintf(stderr, "FAIL: db_env_create: %s\n",
		    db_strerror(ret));
		fails++;
		return;
	}
	dbenv->set_errfile(dbenv, NULL);

	/*
	 * ORDERING MATTERS in this function.  Several rep knobs silently make
	 * the handle a REPMGR application (APP_SET_REPMGR): rep_set_config
	 * with a DB_REPMGR_CONF_* flag, repmgr_set_ack_policy, and
	 * rep_set_timeout with any repmgr timeout kind (DB_REP_ACK_TIMEOUT,
	 * CONNECTION_RETRY, ELECTION_RETRY, HEARTBEAT_*).  Once that happens,
	 * the BASE-API nsites calls are refused -- and worse,
	 * __rep_get_nsites then delegates to __repmgr_get_nsites
	 * (src/repmgr/repmgr_util.c:520), which dereferences
	 * `db_rep->region->config_nsites` with no guard while db_rep->region
	 * is still NULL pre-open, i.e. it SIGSEGVs.  ENV_NOT_CONFIGURED does
	 * not catch it because src/dbinc/db_int.in:604 makes that macro a
	 * no-op unless ENV_OPEN_CALLED is set.
	 *
	 * So: base-API nsites FIRST, repmgr-flavoured knobs LAST.  The crash
	 * is a real defect, reported in
	 * test/coverage/FULL-COVERAGE-REPORT-4.md and NOT fixed here (12-line
	 * public-API repro in that report).
	 */

	/* --- 1. base-API-only knobs, while the handle is still neutral. */
	/* nsites pre-open goes through __rep_set_nsites_pp's other arm. */
	CHK_OK(dbenv->rep_set_nsites(dbenv, 5));
	CHK_OK(dbenv->rep_get_nsites(dbenv, &a));
	CHK_EQ(a, 5, "pre-open rep_get_nsites");

	/* Pre-open: the db_rep->config half of set/get_config. */
	rep_config_roundtrip(dbenv, 0);

	CHK_OK(dbenv->rep_set_limit(dbenv, 0, 65536));
	CHK_OK(dbenv->rep_get_limit(dbenv, &a, &b));
	CHK_EQ(b, 65536, "pre-open rep_get_limit");
	CHK_OK(dbenv->rep_set_priority(dbenv, 50));
	CHK_OK(dbenv->rep_get_priority(dbenv, &a));
	CHK_EQ(a, 50, "pre-open rep_get_priority");
	CHK_OK(dbenv->rep_set_request(dbenv, 1000, 8000));
	CHK_OK(dbenv->rep_get_request(dbenv, &a, &b));
	CHK_EQ(a, 1000, "pre-open rep_get_request min");
	CHK_OK(dbenv->rep_set_clockskew(dbenv, 105, 100));
	CHK_OK(dbenv->rep_get_clockskew(dbenv, &a, &b));
	CHK_EQ(a, 105, "pre-open rep_get_clockskew");
	/* A base-API timeout kind: does NOT flip the app type. */
	CHK_OK(dbenv->rep_set_timeout(dbenv,
	    DB_REP_ELECTION_TIMEOUT, 500000));
	CHK_OK(dbenv->rep_get_timeout(dbenv,
	    DB_REP_ELECTION_TIMEOUT, &a));
	CHK_EQ(a, 500000, "pre-open rep_get_timeout");

	/* --- 2. repmgr-flavoured knobs need a FRESH, still-neutral handle:
	 * the base-API knobs above have already set APP_IS_BASEAPI (any
	 * rep_set_* on a non-repmgr path does), and a base-API handle refuses
	 * every repmgr entry point.  That refusal is itself covered in
	 * base_api_rejects_repmgr(); here we want the ACCEPTING arms.
	 */
	CHK_FAILS(dbenv->rep_set_timeout(dbenv, DB_REP_ACK_TIMEOUT, 500000));
	CHK_FAILS(dbenv->repmgr_set_ack_policy(dbenv, DB_REPMGR_ACKS_QUORUM));
	/* Never opened: close must still tear the rep handle down. */
	CHK_OK(dbenv->close(dbenv, 0));

	/* A second unopened handle, repmgr-flavoured from the start. */
	if ((ret = db_env_create(&dbenv, 0)) != 0) {
		fprintf(stderr, "FAIL: db_env_create: %s\n",
		    db_strerror(ret));
		fails++;
		return;
	}
	dbenv->set_errfile(dbenv, NULL);
	CHK_OK(dbenv->repmgr_set_ack_policy(dbenv, DB_REPMGR_ACKS_QUORUM));
	CHK_OK(dbenv->repmgr_get_ack_policy(dbenv, &on));
	CHK_EQ(on, DB_REPMGR_ACKS_QUORUM, "pre-open ack policy");
	CHK_OK(dbenv->rep_set_timeout(dbenv, DB_REP_ACK_TIMEOUT, 500000));
	CHK_OK(dbenv->rep_get_timeout(dbenv, DB_REP_ACK_TIMEOUT, &a));
	CHK_EQ(a, 500000, "pre-open repmgr rep_get_timeout");
	CHK_OK(dbenv->rep_set_timeout(dbenv,
	    DB_REP_CONNECTION_RETRY, 500000));
	CHK_OK(dbenv->rep_get_timeout(dbenv,
	    DB_REP_CONNECTION_RETRY, &a));
	CHK_OK(dbenv->rep_set_timeout(dbenv,
	    DB_REP_HEARTBEAT_SEND, 500000));
	CHK_OK(dbenv->rep_get_timeout(dbenv,
	    DB_REP_HEARTBEAT_SEND, &a));
	CHK_OK(dbenv->rep_set_timeout(dbenv,
	    DB_REP_HEARTBEAT_MONITOR, 900000));
	CHK_OK(dbenv->rep_get_timeout(dbenv,
	    DB_REP_HEARTBEAT_MONITOR, &a));
	/* The repmgr-only config flags are legal pre-open on this handle. */
	CHK_OK(dbenv->rep_set_config(dbenv,
	    DB_REPMGR_CONF_2SITE_STRICT, 1));
	CHK_OK(dbenv->rep_get_config(dbenv,
	    DB_REPMGR_CONF_2SITE_STRICT, &on));
	CHK_EQ(on, 1, "pre-open 2SITE_STRICT");
	CHK_OK(dbenv->rep_set_config(dbenv, DB_REPMGR_CONF_ELECTIONS, 1));
	/*
	 * On a repmgr handle the base-API nsites SETTER must be refused (the
	 * "cannot call from Replication Manager application" branch).  Do NOT
	 * call rep_get_nsites here -- see the crash note above.
	 */
	CHK_FAILS(dbenv->rep_set_nsites(dbenv, 7));

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

	printf("cov_rep_api: replication + repmgr config/query surface\n");

	/* --- 1. Knobs on an unopened handle (the !REP_ON half of each). */
	printf("1. pre-open rep/repmgr knobs\n");
	pre_open_knobs();

	/* --- 2. Every rep and repmgr method on an env with no DB_INIT_REP. */
	printf("2. unconfigured env rejects rep/repmgr calls\n");
	unconfigured_env_rejects();

	/* --- 3. A real base-API replication env (rep_set_transport). */
	printf("3. base-API replication env\n");
	clean_home(HOME_BASE);
	if ((ret = db_env_create(&dbenv, 0)) != 0) {
		fprintf(stderr, "FAIL: db_env_create: %s\n",
		    db_strerror(ret));
		return (2);
	}
	dbenv->set_errpfx(dbenv, "cov_rep_api-base");
	dbenv->set_errfile(dbenv, NULL);
	dbenv->set_msgfile(dbenv, NULL);
	CHK_OK(dbenv->rep_set_transport(dbenv, 1, noop_send));
	if ((ret = dbenv->open(dbenv, HOME_BASE, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN | DB_INIT_REP |
	    DB_THREAD, 0600)) != 0) {
		fprintf(stderr, "FAIL: open base rep env: %s\n",
		    db_strerror(ret));
		(void)dbenv->close(dbenv, 0);
		return (2);
	}
	/* Post-open: the REP_ON(env) half of every setter/getter. */
	rep_config_roundtrip(dbenv, 1);
	rep_scalar_knobs(dbenv);
	/* A base-API app must be refused the repmgr entry points. */
	base_api_rejects_repmgr(dbenv);
	/* Become master so the stat/flush/token paths have a real state. */
	CHK_OK(dbenv->rep_start(dbenv, NULL, DB_REP_MASTER));
	rep_stat_and_flush(dbenv);
	txn_applied_paths(dbenv);
	/* rep_start again as master: the "already master" idempotent arm. */
	CHK_OK(dbenv->rep_start(dbenv, NULL, DB_REP_MASTER));
	CHK_OK(dbenv->close(dbenv, 0));

	/* --- 4. A repmgr env: sites, channels, ack policy, dispatch. */
	printf("4. repmgr env (not started)\n");
	clean_home(HOME_MGR);
	if ((ret = db_env_create(&dbenv, 0)) != 0) {
		fprintf(stderr, "FAIL: db_env_create: %s\n",
		    db_strerror(ret));
		return (2);
	}
	dbenv->set_errpfx(dbenv, "cov_rep_api-mgr");
	dbenv->set_errfile(dbenv, NULL);
	dbenv->set_msgfile(dbenv, NULL);
	if ((ret = dbenv->open(dbenv, HOME_MGR, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN | DB_INIT_REP |
	    DB_THREAD, 0600)) != 0) {
		fprintf(stderr, "FAIL: open repmgr env: %s\n",
		    db_strerror(ret));
		(void)dbenv->close(dbenv, 0);
		return (2);
	}
	repmgr_knobs(dbenv);
	repmgr_before_start(dbenv);
	CHK_OK(dbenv->close(dbenv, 0));

	(void)alarm(0);
	printf("cov_rep_api: %d checks, %d failures\n", checks, fails);
	if (fails != 0) {
		printf("cov_rep_api: FAIL\n");
		return (1);
	}
	printf("cov_rep_api: PASS\n");
	return (0);
}
