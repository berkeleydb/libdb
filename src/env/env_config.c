/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996, 2011 Oracle and/or its affiliates.  All rights reserved.
 *
 * $Id$
 */

#include "db_config.h"

#include "db_int.h"
#include "dbinc/lock.h"
#include "dbinc/log.h"
#include "dbinc/mp.h"
#include "dbinc/txn.h"

static int __config_parse __P((ENV *, char *, int));

/*
 * __env_read_db_config --
 *	Read the DB_CONFIG file.
 *
 * PUBLIC: int __env_read_db_config __P((ENV *));
 */
int
__env_read_db_config(env)
	ENV *env;
{
	FILE *fp;
	int lc, ret;
	char *p, buf[256];

	/* Parse the config file. */
	p = NULL;
	if ((ret = __db_appname(env,
	    DB_APP_NONE, "DB_CONFIG", NULL, &p)) != 0)
		return (ret);
	if (p == NULL)
		fp = NULL;
	else {
		fp = fopen(p, "r");
		__os_free(env, p);
	}

	if (fp == NULL)
		return (0);

	for (lc = 1; fgets(buf, sizeof(buf), fp) != NULL; ++lc) {
		if ((p = strchr(buf, '\n')) == NULL)
			p = buf + strlen(buf);
		if (p > buf && p[-1] == '\r')
			--p;
		*p = '\0';
		for (p = buf; *p != '\0' && isspace((int)*p); ++p)
			;
		if (*p == '\0' || *p == '#')
			continue;

		if ((ret = __config_parse(env, p, lc)) != 0)
			break;
	}
	(void)fclose(fp);

	return (ret);
}

#undef	CONFIG_GET_INT
#define	CONFIG_GET_INT(s, vp) do {					\
	int __ret;							\
	if ((__ret =							\
	    __db_getlong(env->dbenv, NULL, s, 0, INT_MAX, vp)) != 0)	\
		return (__ret);						\
} while (0)
#undef	CONFIG_GET_LONG
#define	CONFIG_GET_LONG(s, vp) do {					\
	int __ret;							\
	if ((__ret =							\
	    __db_getlong(env->dbenv, NULL, s, 0, LONG_MAX, vp)) != 0)	\
		return (__ret);						\
} while (0)
#undef	CONFIG_GET_UINT
#define	CONFIG_GET_UINT(s, vp) do {					\
	int __ret;							\
	if ((__ret =							\
	    __db_getulong(env->dbenv, NULL, s, 0, UINT_MAX, vp)) != 0)	\
		return (__ret);						\
} while (0)
#undef	CONFIG_INT
#define	CONFIG_INT(s, f) do {						\
	if (strcasecmp(s, argv[0]) == 0) {				\
		long __v;						\
		if (nf != 2)						\
			goto format;					\
		CONFIG_GET_INT(argv[1], &__v);				\
		return (f(env->dbenv, (int)__v));			\
	}								\
} while (0)
#undef	CONFIG_GET_UINT32
#define	CONFIG_GET_UINT32(s, vp) do {					\
	if (__db_getulong(env->dbenv, NULL, s, 0, UINT32_MAX, vp) != 0)	\
		return (EINVAL);					\
} while (0)
#undef	CONFIG_UINT32
#define	CONFIG_UINT32(s, f) do {					\
	if (strcasecmp(s, argv[0]) == 0) {				\
		u_long __v;						\
		if (nf != 2)						\
			goto format;					\
		CONFIG_GET_UINT32(argv[1], &__v);			\
		return (f(env->dbenv, (u_int32_t)__v));			\
	}								\
} while (0)

#undef	CONFIG_SLOTS
#define	CONFIG_SLOTS	10

/*
 * __config_parse --
 *	Parse a single NAME VALUE pair.
 */
static int
__config_parse(env, s, lc)
	ENV *env;
	char *s;
	int lc;
{
	DB_ENV *dbenv;
	DB_SITE *site;
	u_long uv1, uv2;
	u_int32_t flags;
	long lv1, lv2;
	u_int port;
	int i, nf, onoff, bad, ret, t_ret;
	char *argv[CONFIG_SLOTS];
	DB_MEM_CONFIG mem_conf;

	bad = 0;
	dbenv = env->dbenv;
					/* Split the line by white-space. */
	if ((nf = __config_split(s, argv)) < 2) {
format:		__db_errx(env, DB_STR_A("1584",
		    "line %d: %s: incorrect name-value pair", "%d %s"),
			    lc, argv[0]);
		return (EINVAL);
	}

	if (strcasecmp(argv[0], "set_memory_max") == 0) {
		if (nf != 3)
			goto format;
		CONFIG_GET_UINT32(argv[1], &uv1);
		CONFIG_GET_UINT32(argv[2], &uv2);
		return (__env_set_memory_max(
		    dbenv, (u_int32_t)uv1, (u_int32_t)uv2));
	}
	if (strcasecmp(argv[0], "set_memory_init") == 0) {
		if (nf != 3)
			goto format;
		if (strcasecmp(argv[1], "DB_MEM_LOCK") == 0)
			mem_conf = DB_MEM_LOCK;
		else if (strcasecmp(argv[1], "DB_MEM_LOCKER") == 0)
			mem_conf = DB_MEM_LOCKER;
		else if (strcasecmp(argv[1], "DB_MEM_LOCKOBJECT") == 0)
			mem_conf = DB_MEM_LOCKOBJECT;
		else if (strcasecmp(argv[1], "DB_MEM_TRANSACTION") == 0)
			mem_conf = DB_MEM_TRANSACTION;
		else if (strcasecmp(argv[1], "DB_MEM_THREAD") == 0)
			mem_conf = DB_MEM_THREAD;
		else if (strcasecmp(argv[1], "DB_MEM_LOGID") == 0)
			mem_conf = DB_MEM_LOGID;
		else
			goto format;
		CONFIG_GET_UINT32(argv[2], &uv2);
		return (__env_set_memory_init(dbenv, mem_conf, (u_int32_t)uv2));
	}

	CONFIG_UINT32("mutex_set_align", __mutex_set_align);
	CONFIG_UINT32("mutex_set_increment", __mutex_set_increment);
	CONFIG_UINT32("mutex_set_init", __mutex_set_init);
	CONFIG_UINT32("mutex_set_max", __mutex_set_max);
	CONFIG_UINT32("mutex_set_tas_spins", __mutex_set_tas_spins);

	if (strcasecmp(argv[0], "rep_set_clockskew") == 0) {
		if (nf != 3)
			goto format;
		CONFIG_GET_UINT32(argv[1], &uv1);
		CONFIG_GET_UINT32(argv[2], &uv2);
		return (__rep_set_clockskew(
		    dbenv, (u_int32_t)uv1, (u_int32_t)uv2));
	}

	if (strcasecmp(argv[0], "rep_set_config") == 0) {
		if (nf != 2 && nf != 3)
			goto format;
		onoff = 1;
		if (nf == 3) {
			if (strcasecmp(argv[2], "off") == 0)
				onoff = 0;
			else if (strcasecmp(argv[2], "on") != 0)
				goto format;
		}
		if (strcasecmp(argv[1], "db_rep_conf_autoinit") == 0)
			return (__rep_set_config(dbenv,
			    DB_REP_CONF_AUTOINIT, onoff));
		if (strcasecmp(argv[1], "db_rep_conf_autorollback") == 0)
			return (__rep_set_config(dbenv,
			    DB_REP_CONF_AUTOROLLBACK, onoff));
		if (strcasecmp(argv[1], "db_rep_conf_bulk") == 0)
			return (__rep_set_config(dbenv,
			    DB_REP_CONF_BULK, onoff));
		if (strcasecmp(argv[1], "db_rep_conf_delayclient") == 0)
			return (__rep_set_config(dbenv,
			    DB_REP_CONF_DELAYCLIENT, onoff));
		if (strcasecmp(argv[1], "db_rep_conf_inmem") == 0)
			return (__rep_set_config(dbenv,
			    DB_REP_CONF_INMEM, onoff));
		if (strcasecmp(argv[1], "db_rep_conf_lease") == 0)
			return (__rep_set_config(dbenv,
			    DB_REP_CONF_LEASE, onoff));
		if (strcasecmp(argv[1], "db_rep_conf_nowait") == 0)
			return (__rep_set_config(dbenv,
			    DB_REP_CONF_NOWAIT, onoff));
		if (strcasecmp(argv[1], "db_repmgr_conf_2site_strict") == 0)
			return (__rep_set_config(dbenv,
			    DB_REPMGR_CONF_2SITE_STRICT, onoff));
		if (strcasecmp(argv[1], "db_repmgr_conf_elections") == 0)
			return (__rep_set_config(dbenv,
			    DB_REPMGR_CONF_ELECTIONS, onoff));
		goto format;
	}

	if (strcasecmp(argv[0], "rep_set_limit") == 0) {
		if (nf != 3)
			goto format;
		CONFIG_GET_UINT32(argv[1], &uv1);
		CONFIG_GET_UINT32(argv[2], &uv2);
		return (__rep_set_limit(
		    dbenv, (u_int32_t)uv1, (u_int32_t)uv2));
	}

	if (strcasecmp(argv[0], "rep_set_nsites") == 0) {
		if (nf != 2)
			goto format;
		CONFIG_GET_UINT32(argv[1], &uv1);
		return (__rep_set_nsites_pp(
		    dbenv, (u_int32_t)uv1));
	}

	if (strcasecmp(argv[0], "rep_set_priority") == 0) {
		if (nf != 2)
			goto format;
		CONFIG_GET_UINT32(argv[1], &uv1);
		return (__rep_set_priority(
		    dbenv, (u_int32_t)uv1));
	}

	if (strcasecmp(argv[0], "rep_set_request") == 0) {
		if (nf != 3)
			goto format;
		CONFIG_GET_UINT32(argv[1], &uv1);
		CONFIG_GET_UINT32(argv[2], &uv2);
		return (__rep_set_request(
		    dbenv, (u_int32_t)uv1, (u_int32_t)uv2));
	}

	if (strcasecmp(argv[0], "rep_set_timeout") == 0) {
		if (nf != 3)
			goto format;
		CONFIG_GET_UINT32(argv[2], &uv2);
		if (strcasecmp(argv[1], "db_rep_ack_timeout") == 0)
			return (__rep_set_timeout(
			    dbenv, DB_REP_ACK_TIMEOUT, (u_int32_t)uv2));
		if (strcasecmp(argv[1], "db_rep_checkpoint_delay") == 0)
			return (__rep_set_timeout(
			    dbenv, DB_REP_CHECKPOINT_DELAY, (u_int32_t)uv2));
		if (strcasecmp(argv[1], "db_rep_connection_retry") == 0)
			return (__rep_set_timeout(
			    dbenv, DB_REP_CONNECTION_RETRY, (u_int32_t)uv2));
		if (strcasecmp(argv[1], "db_rep_election_timeout") == 0)
			return (__rep_set_timeout(
			    dbenv, DB_REP_ELECTION_TIMEOUT, (u_int32_t)uv2));
		if (strcasecmp(argv[1], "db_rep_election_retry") == 0)
			return (__rep_set_timeout(
			    dbenv, DB_REP_ELECTION_RETRY, (u_int32_t)uv2));
		if (strcasecmp(argv[1], "db_rep_full_election_timeout") == 0)
			return (__rep_set_timeout(dbenv,
			    DB_REP_FULL_ELECTION_TIMEOUT, (u_int32_t)uv2));
		if (strcasecmp(argv[1], "db_rep_heartbeat_monitor") == 0)
			return (__rep_set_timeout(
			    dbenv, DB_REP_HEARTBEAT_MONITOR, (u_int32_t)uv2));
		if (strcasecmp(argv[1], "db_rep_heartbeat_send") == 0)
			return (__rep_set_timeout(
			    dbenv, DB_REP_HEARTBEAT_SEND, (u_int32_t)uv2));
		if (strcasecmp(argv[1], "db_rep_lease_timeout") == 0)
			return (__rep_set_timeout(
			    dbenv, DB_REP_LEASE_TIMEOUT, (u_int32_t)uv2));
		goto format;
	}

	if (strcasecmp(argv[0], "repmgr_set_ack_policy") == 0) {
		if (nf != 2)
			goto format;
		if (strcasecmp(argv[1], "db_repmgr_acks_all") == 0)
			return (__repmgr_set_ack_policy(
			    dbenv, DB_REPMGR_ACKS_ALL));
		if (strcasecmp(argv[1], "db_repmgr_acks_all_available") == 0)
			return (__repmgr_set_ack_policy(
			    dbenv, DB_REPMGR_ACKS_ALL_AVAILABLE));
		if (strcasecmp(argv[1], "db_repmgr_acks_all_peers") == 0)
			return (__repmgr_set_ack_policy(
			    dbenv, DB_REPMGR_ACKS_ALL_PEERS));
		if (strcasecmp(argv[1], "db_repmgr_acks_none") == 0)
			return (__repmgr_set_ack_policy(
			    dbenv, DB_REPMGR_ACKS_NONE));
		if (strcasecmp(argv[1], "db_repmgr_acks_one") == 0)
			return (__repmgr_set_ack_policy(
			    dbenv, DB_REPMGR_ACKS_ONE));
		if (strcasecmp(argv[1], "db_repmgr_acks_one_peer") == 0)
			return (__repmgr_set_ack_policy(
			    dbenv, DB_REPMGR_ACKS_ONE_PEER));
		if (strcasecmp(argv[1], "db_repmgr_acks_quorum") == 0)
			return (__repmgr_set_ack_policy(
			    dbenv, DB_REPMGR_ACKS_QUORUM));
		goto format;
	}

	/*
	 * Configure name/value pairs of config information for a site (local or
	 * remote).
	 *
	 * repmgr_site host port [which value] ...
	 */
	if (strcasecmp(argv[0], "repmgr_site") == 0) {
		if (nf < 3 || (nf % 2) == 0)
			goto format;
		CONFIG_GET_UINT(argv[2], &uv2);
		port = (u_int)uv2;

		if ((ret = __repmgr_site(dbenv, argv[1], port, &site, 0)) != 0)
			return (ret);
#ifdef HAVE_REPLICATION_THREADS
		for (i = 3; i < nf; i += 2) {
			if (strcasecmp(argv[i], "db_bootstrap_helper") == 0)
				uv1 = DB_BOOTSTRAP_HELPER;
			else if (strcasecmp(argv[i], "db_group_creator") == 0)
				uv1 = DB_GROUP_CREATOR;
			else if (strcasecmp(argv[i], "db_legacy") == 0)
				uv1 = DB_LEGACY;
			else if (strcasecmp(argv[i], "db_local_site") == 0)
				uv1 = DB_LOCAL_SITE;
			else if (strcasecmp(argv[i], "db_repmgr_peer") == 0)
				uv1 = DB_REPMGR_PEER;
			else {
				bad = 1;
				break;
			}

			if (strcasecmp(argv[i + 1], "on") == 0)
				uv2 = 1;
			else if (strcasecmp(argv[i + 1], "off") == 0)
				uv2 = 0;
			else
				CONFIG_GET_UINT32(argv[i + 1], &uv2);
			if ((ret = __repmgr_site_config(site,
			    (u_int32_t)uv1, (u_int32_t)uv2)) != 0)
				break;
		}
		if ((t_ret = __repmgr_site_close(site)) != 0 && ret == 0)
			ret = t_ret;
		if (bad)
			goto format;
#else
		/* If repmgr not built, __repmgr_site() returns DB_OPNOTSUP. */
		COMPQUIET(i, 0);
		COMPQUIET(t_ret, 0);
		DB_ASSERT(env, 0);
#endif
		return (ret);
	}

	if (strcasecmp(argv[0], "set_cachesize") == 0) {
		if (nf != 4)
			goto format;
		CONFIG_GET_UINT32(argv[1], &uv1);
		CONFIG_GET_UINT32(argv[2], &uv2);
		CONFIG_GET_INT(argv[3], &lv1);
		return (__memp_set_cachesize(
		    dbenv, (u_int32_t)uv1, (u_int32_t)uv2, (int)lv1));
	}

	if (strcasecmp(argv[0], "set_cache_max") == 0) {
		if (nf != 3)
			goto format;
		CONFIG_GET_UINT32(argv[1], &uv1);
		CONFIG_GET_UINT32(argv[2], &uv2);
		return (__memp_set_cache_max(
		    dbenv, (u_int32_t)uv1, (u_int32_t)uv2));
	}

	if (strcasecmp(argv[0], "set_data_dir") == 0 ||
	    strcasecmp(argv[0], "db_data_dir") == 0) {	/* Compatibility. */
		if (nf != 2)
			goto format;
		return (__env_set_data_dir(dbenv, argv[1]));
	}

	if (strcasecmp(argv[0], "add_data_dir") == 0) {
		if (nf != 2)
			goto format;
		return (__env_add_data_dir(dbenv, argv[1]));
	}
	if (strcasecmp(argv[0], "set_create_dir") == 0) {
		if (nf != 2)
			goto format;
		return (__env_set_create_dir(dbenv, argv[1]));
	}

							/* Compatibility */
	if (strcasecmp(argv[0], "set_intermediate_dir") == 0) {
		if (nf != 2)
			goto format;
		CONFIG_GET_INT(argv[1], &lv1);
		if (lv1 <= 0)
			goto format;
		env->dir_mode = (int)lv1;
		return (0);
	}
	if (strcasecmp(argv[0], "set_intermediate_dir_mode") == 0) {
		if (nf != 2)
			goto format;
		return (__env_set_intermediate_dir_mode(dbenv, argv[1]));
	}

	if (strcasecmp(argv[0], "set_flags") == 0) {
		if (nf != 2 && nf != 3)
			goto format;
		onoff = 1;
		if (nf == 3) {
			if (strcasecmp(argv[2], "off") == 0)
				onoff = 0;
			else if (strcasecmp(argv[2], "on") != 0)
				goto format;
		}
		if (strcasecmp(argv[1], "db_auto_commit") == 0)
			return (__env_set_flags(dbenv, DB_AUTO_COMMIT, onoff));
		if (strcasecmp(argv[1], "db_cdb_alldb") == 0)
			return (__env_set_flags(dbenv, DB_CDB_ALLDB, onoff));
		if (strcasecmp(argv[1], "db_direct_db") == 0)
			return (__env_set_flags(dbenv, DB_DIRECT_DB, onoff));
		if (strcasecmp(argv[1], "db_dsync_db") == 0)
			return (__env_set_flags(dbenv, DB_DSYNC_DB, onoff));
		if (strcasecmp(argv[1], "db_multiversion") == 0)
			return (__env_set_flags(dbenv, DB_MULTIVERSION, onoff));
		if (strcasecmp(argv[1], "db_nolocking") == 0)
			return (__env_set_flags(dbenv, DB_NOLOCKING, onoff));
		if (strcasecmp(argv[1], "db_nommap") == 0)
			return (__env_set_flags(dbenv, DB_NOMMAP, onoff));
		if (strcasecmp(argv[1], "db_nopanic") == 0)
			return (__env_set_flags(dbenv, DB_NOPANIC, onoff));
		if (strcasecmp(argv[1], "db_overwrite") == 0)
			return (__env_set_flags(dbenv, DB_OVERWRITE, onoff));
		if (strcasecmp(argv[1], "db_region_init") == 0)
			return (__env_set_flags(dbenv, DB_REGION_INIT, onoff));
		if (strcasecmp(argv[1], "db_time_notgranted") == 0)
			return (
			    __env_set_flags(dbenv, DB_TIME_NOTGRANTED, onoff));
		if (strcasecmp(argv[1], "db_txn_nosync") == 0)
			return (__env_set_flags(dbenv, DB_TXN_NOSYNC, onoff));
		if (strcasecmp(argv[1], "db_txn_nowait") == 0)
			return (__env_set_flags(dbenv, DB_TXN_NOWAIT, onoff));
		if (strcasecmp(argv[1], "db_txn_snapshot") == 0)
			return (__env_set_flags(dbenv, DB_TXN_SNAPSHOT, onoff));
		if (strcasecmp(argv[1], "db_txn_write_nosync") == 0)
			return (
			    __env_set_flags(dbenv, DB_TXN_WRITE_NOSYNC, onoff));
		if (strcasecmp(argv[1], "db_yieldcpu") == 0)
			return (__env_set_flags(dbenv, DB_YIELDCPU, onoff));
		if (strcasecmp(argv[1], "db_log_inmemory") == 0)
			return (
			    __log_set_config(dbenv, DB_LOG_IN_MEMORY, onoff));
		if (strcasecmp(argv[1], "db_direct_log") == 0)
			return (__log_set_config(dbenv, DB_LOG_DIRECT, onoff));
		if (strcasecmp(argv[1], "db_dsync_log") == 0)
			return (__log_set_config(dbenv, DB_LOG_DSYNC, onoff));
		if (strcasecmp(argv[1], "db_log_autoremove") == 0)
			return (
			    __log_set_config(dbenv, DB_LOG_AUTO_REMOVE, onoff));
		goto format;
	}
	if (strcasecmp(argv[0], "log_set_config") == 0) {
		if (nf != 2 && nf != 3)
			goto format;
		onoff = 1;
		if (nf == 3) {
			if (strcasecmp(argv[2], "off") == 0)
				onoff = 0;
			else if (strcasecmp(argv[2], "on") != 0)
				goto format;
		}
		if (strcasecmp(argv[1], "db_log_auto_remove") == 0)
			return (
			    __log_set_config(dbenv, DB_LOG_AUTO_REMOVE, onoff));
		if (strcasecmp(argv[1], "db_log_direct") == 0)
			return (__log_set_config(dbenv, DB_LOG_DIRECT, onoff));
		if (strcasecmp(argv[1], "db_log_dsync") == 0)
			return (__log_set_config(dbenv, DB_LOG_DSYNC, onoff));
		if (strcasecmp(argv[1], "db_log_in_memory") == 0)
			return (
			    __log_set_config(dbenv, DB_LOG_IN_MEMORY, onoff));
		if (strcasecmp(argv[1], "db_log_zero") == 0)
			return (__log_set_config(dbenv, DB_LOG_ZERO, onoff));
		goto format;
	}

	CONFIG_UINT32("set_data_len", __env_set_data_len);
	CONFIG_UINT32("set_lg_bsize", __log_set_lg_bsize);
	CONFIG_INT("set_lg_filemode", __log_set_lg_filemode);
	CONFIG_UINT32("set_lg_max", __log_set_lg_max);
	CONFIG_UINT32("set_lg_regionmax", __log_set_lg_regionmax);

	if (strcasecmp(argv[0], "set_lg_dir") == 0 ||
	    strcasecmp(argv[0], "db_log_dir") == 0) {	/* Compatibility. */
		if (nf != 2)
			goto format;
		return (__log_set_lg_dir(dbenv, argv[1]));
	}

	if (strcasecmp(argv[0], "set_lk_detect") == 0) {
		if (nf != 2)
			goto format;
		if (strcasecmp(argv[1], "db_lock_default") == 0)
			flags = DB_LOCK_DEFAULT;
		else if (strcasecmp(argv[1], "db_lock_expire") == 0)
			flags = DB_LOCK_EXPIRE;
		else if (strcasecmp(argv[1], "db_lock_maxlocks") == 0)
			flags = DB_LOCK_MAXLOCKS;
		else if (strcasecmp(argv[1], "db_lock_maxwrite") == 0)
			flags = DB_LOCK_MAXWRITE;
		else if (strcasecmp(argv[1], "db_lock_minlocks") == 0)
			flags = DB_LOCK_MINLOCKS;
		else if (strcasecmp(argv[1], "db_lock_minwrite") == 0)
			flags = DB_LOCK_MINWRITE;
		else if (strcasecmp(argv[1], "db_lock_oldest") == 0)
			flags = DB_LOCK_OLDEST;
		else if (strcasecmp(argv[1], "db_lock_random") == 0)
			flags = DB_LOCK_RANDOM;
		else if (strcasecmp(argv[1], "db_lock_youngest") == 0)
			flags = DB_LOCK_YOUNGEST;
		else
			goto format;
		return (__lock_set_lk_detect(dbenv, flags));
	}

	CONFIG_UINT32("set_lk_max_locks", __lock_set_lk_max_locks);
	CONFIG_UINT32("set_lk_max_lockers", __lock_set_lk_max_lockers);
	CONFIG_UINT32("set_lk_max_objects", __lock_set_lk_max_objects);
	CONFIG_UINT32("set_lk_partitions", __lock_set_lk_partitions);
	CONFIG_UINT32("set_lk_tablesize", __lock_set_lk_tablesize);

	if (strcasecmp(argv[0], "set_lock_timeout") == 0) {
		if (nf != 2)
			goto format;
		CONFIG_GET_UINT32(argv[1], &uv1);
		return (__lock_set_env_timeout(
		    dbenv, (u_int32_t)uv1, DB_SET_LOCK_TIMEOUT));
	}

	CONFIG_INT("set_mp_max_openfd", __memp_set_mp_max_openfd);
	CONFIG_UINT32("set_mp_mtxcount", __memp_set_mp_mtxcount);
	CONFIG_UINT32("set_mp_pagesize", __memp_set_mp_pagesize);

	if (strcasecmp(argv[0], "set_mp_max_write") == 0) {
		if (nf != 3)
			goto format;
		CONFIG_GET_INT(argv[1], &lv1);
		CONFIG_GET_INT(argv[2], &lv2);
		return (__memp_set_mp_max_write(
		    dbenv, (int)lv1, (db_timeout_t)lv2));
	}

	CONFIG_UINT32("set_mp_mmapsize", __memp_set_mp_mmapsize);

	if (strcasecmp(argv[0], "set_open_flags") == 0) {
		if (nf != 2 && nf != 3)
			goto format;
		onoff = 1;
		if (nf == 3) {
			if (strcasecmp(argv[2], "off") == 0)
				onoff = 0;
			else if (strcasecmp(argv[2], "on") != 0)
				goto format;
		}
		if (strcasecmp(argv[1], "db_init_rep") == 0) {
			if (onoff == 1)
				FLD_SET(env->open_flags, DB_INIT_REP);
			else
				FLD_CLR(env->open_flags, DB_INIT_REP);
			return (0);
		} else if (strcasecmp(argv[1], "db_private") == 0) {
			if (onoff == 1)
				FLD_SET(env->open_flags, DB_PRIVATE);
			else
				FLD_CLR(env->open_flags, DB_PRIVATE);
			return (0);
		} else if (strcasecmp(argv[1], "db_register") == 0) {
			if (onoff == 1)
				FLD_SET(env->open_flags, DB_REGISTER);
			else
				FLD_CLR(env->open_flags, DB_REGISTER);
			return (0);
		} else if (strcasecmp(argv[1], "db_thread") == 0) {
			if (onoff == 1)
				FLD_SET(env->open_flags, DB_THREAD);
			else
				FLD_CLR(env->open_flags, DB_THREAD);
			return (0);
		} else
			goto format;
	}

	if (strcasecmp(argv[0], "set_region_init") == 0) {
		if (nf != 2)
			goto format;
		CONFIG_GET_INT(argv[1], &lv1);
		if (lv1 != 0 && lv1 != 1)
			goto format;
		return (__env_set_flags(
		    dbenv, DB_REGION_INIT, lv1 == 0 ? 0 : 1));
	}

	if (strcasecmp(argv[0], "set_reg_timeout") == 0) {
		if (nf != 2)
			goto format;
		CONFIG_GET_UINT32(argv[1], &uv1);
		return (__env_set_timeout(
		    dbenv, (u_int32_t)uv1, DB_SET_REG_TIMEOUT));
	}

	if (strcasecmp(argv[0], "set_shm_key") == 0) {
		if (nf != 2)
			goto format;
		CONFIG_GET_LONG(argv[1], &lv1);
		return (__env_set_shm_key(dbenv, lv1));
	}

	/*
	 * The set_tas_spins method has been replaced by mutex_set_tas_spins.
	 * The set_tas_spins argv[0] remains for DB_CONFIG compatibility.
	 */
	CONFIG_UINT32("set_tas_spins", __mutex_set_tas_spins);

	if (strcasecmp(argv[0], "set_tmp_dir") == 0 ||
	    strcasecmp(argv[0], "db_tmp_dir") == 0) {	/* Compatibility.*/
		if (nf != 2)
			goto format;
		return (__env_set_tmp_dir(dbenv, argv[1]));
	}

	CONFIG_UINT32("set_thread_count", __env_set_thread_count);
	CONFIG_UINT32("set_tx_max", __txn_set_tx_max);

	if (strcasecmp(argv[0], "set_txn_timeout") == 0) {
		if (nf != 2)
			goto format;
		CONFIG_GET_UINT32(argv[1], &uv1);
		return (__lock_set_env_timeout(
		    dbenv, (u_int32_t)uv1, DB_SET_TXN_TIMEOUT));
	}

	if (strcasecmp(argv[0], "set_verbose") == 0) {
		if (nf != 2 && nf != 3)
			goto format;
		onoff = 1;
		if (nf == 3) {
			if (strcasecmp(argv[2], "off") == 0)
				onoff = 0;
			else if (strcasecmp(argv[2], "on") != 0)
				goto format;
		}
		if (strcasecmp(argv[1], "db_verb_deadlock") == 0)
			flags = DB_VERB_DEADLOCK;
		else if (strcasecmp(argv[1], "db_verb_fileops") == 0)
			flags = DB_VERB_FILEOPS;
		else if (strcasecmp(argv[1], "db_verb_fileops_all") == 0)
			flags = DB_VERB_FILEOPS_ALL;
		else if (strcasecmp(argv[1], "db_verb_recovery") == 0)
			flags = DB_VERB_RECOVERY;
		else if (strcasecmp(argv[1], "db_verb_register") == 0)
			flags = DB_VERB_REGISTER;
		else if (strcasecmp(argv[1], "db_verb_replication") == 0)
			flags = DB_VERB_REPLICATION;
		else if (strcasecmp(argv[1], "db_verb_rep_elect") == 0)
			flags = DB_VERB_REP_ELECT;
		else if (strcasecmp(argv[1], "db_verb_rep_lease") == 0)
			flags = DB_VERB_REP_LEASE;
		else if (strcasecmp(argv[1], "db_verb_rep_misc") == 0)
			flags = DB_VERB_REP_MISC;
		else if (strcasecmp(argv[1], "db_verb_rep_msgs") == 0)
			flags = DB_VERB_REP_MSGS;
		else if (strcasecmp(argv[1], "db_verb_rep_sync") == 0)
			flags = DB_VERB_REP_SYNC;
		else if (strcasecmp(argv[1], "db_verb_rep_system") == 0)
			flags = DB_VERB_REP_SYSTEM;
		else if (strcasecmp(argv[1], "db_verb_rep_test") == 0)
			flags = DB_VERB_REP_TEST;
		else if (strcasecmp(argv[1], "db_verb_repmgr_connfail") == 0)
			flags = DB_VERB_REPMGR_CONNFAIL;
		else if (strcasecmp(argv[1], "db_verb_repmgr_misc") == 0)
			flags = DB_VERB_REPMGR_MISC;
		else if (strcasecmp(argv[1], "db_verb_waitsfor") == 0)
			flags = DB_VERB_WAITSFOR;
		else
			goto format;
		return (__env_set_verbose(dbenv, flags, onoff));
	}

	__db_errx(env, DB_STR_A("1585",
	    "unrecognized name-value pair: %s", "%s"), s);
	return (EINVAL);
}

/*
 * __config_split --
 *	Split lines into white-space separated fields, returning the count of
 *	fields.
 *
 * PUBLIC: int __config_split __P((char *, char *[]));
 */
int
__config_split(input, argv)
	char *input, *argv[CONFIG_SLOTS];
{
	int count;
	char **ap;

	for (count = 0, ap = argv; (*ap = strsep(&input, " \t\n")) != NULL;)
		if (**ap != '\0') {
			++count;
			if (++ap == &argv[CONFIG_SLOTS - 1]) {
				*ap = NULL;
				break;
			}
		}
	return (count);
}
