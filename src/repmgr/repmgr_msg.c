/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2005, 2011 Oracle and/or its affiliates.  All rights reserved.
 *
 * $Id$
 */

#include "db_config.h"

#include "db_int.h"

static int message_loop __P((ENV *, REPMGR_RUNNABLE *));
static int process_message __P((ENV*, DBT*, DBT*, int));
static int handle_newsite __P((ENV *, const DBT *));
static int send_permlsn __P((ENV *, u_int32_t, int, DB_LSN *));
static int send_permlsn_conn __P((ENV *,
		REPMGR_CONNECTION *, u_int32_t, DB_LSN *));

/*
 * PUBLIC: void *__repmgr_msg_thread __P((void *));
 */
void *
__repmgr_msg_thread(argsp)
	void *argsp;
{
	REPMGR_RUNNABLE *th;
	ENV *env;
	int ret;

	th = argsp;
	env = th->env;

	if ((ret = message_loop(env, th)) != 0) {
		__db_err(env, ret, "message thread failed");
		__repmgr_thread_failure(env, ret);
	}
	return (NULL);
}

static int
message_loop(env, th)
	ENV *env;
	REPMGR_RUNNABLE *th;
{
	REPMGR_MESSAGE *msg;
	int ret;

	while ((ret = __repmgr_queue_get(env, &msg, th)) == 0) {
		while ((ret = process_message(env, &msg->control, &msg->rec,
		    msg->originating_eid)) == DB_LOCK_DEADLOCK)
			RPRINT(env, (env, DB_VERB_REPMGR_MISC,
			    "repmgr deadlock retry"));

		__os_free(env, msg);
		if (ret != 0)
			return (ret);
	}

	return (ret == DB_REP_UNAVAIL ? 0 : ret);
}

static int
process_message(env, control, rec, eid)
	ENV *env;
	DBT *control, *rec;
	int eid;
{
	DB_LSN permlsn;
	DB_REP *db_rep;
	REP *rep;
	int bcast, ret;
	u_int32_t generation;

	db_rep = env->rep_handle;
	rep = db_rep->region;

	/*
	 * Save initial generation number, in case it changes in a close race
	 * with a NEWMASTER.
	 */
	generation = rep->gen;

	switch (ret =
	    __rep_process_message_int(env, control, rec, eid, &permlsn)) {
	case 0:
		if (db_rep->takeover_pending) {
			db_rep->takeover_pending = FALSE;
			return (__repmgr_repstart(env, DB_REP_MASTER));
		}
		break;

	case DB_REP_NEWSITE:
		return (handle_newsite(env, rec));

	case DB_REP_HOLDELECTION:
		LOCK_MUTEX(db_rep->mutex);
		ret = __repmgr_init_election(env,
		    ELECT_F_IMMED | ELECT_F_INVITEE);
		UNLOCK_MUTEX(db_rep->mutex);
		if (ret != 0)
			return (ret);
		break;

	case DB_REP_DUPMASTER:
		if ((ret = __repmgr_repstart(env, DB_REP_CLIENT)) != 0)
			return (ret);
		/*
		 * Initiate an election if we're configured to be using
		 * elections, but only if we're *NOT* using leases.  When using
		 * leases, there is never any uncertainty over which site is the
		 * rightful master, and only the loser gets the DUPMASTER return
		 * code.
		 */
		if (FLD_ISSET(rep->config, REP_C_LEASE | REP_C_ELECTIONS)
		    == REP_C_ELECTIONS) {
			LOCK_MUTEX(db_rep->mutex);
			ret = __repmgr_init_election(env, ELECT_F_IMMED);
			UNLOCK_MUTEX(db_rep->mutex);
		}
		DB_EVENT(env, DB_EVENT_REP_DUPMASTER, NULL);
		if (ret != 0)
			return (ret);
		break;

	case DB_REP_ISPERM:
		/*
		 * Don't bother sending ack if master doesn't care about it.
		 * Archiving cares on all sites, if the file number changes.
		 */
		bcast = FALSE;
		if (db_rep->perm_lsn.file == permlsn.file &&
		    (rep->perm_policy == DB_REPMGR_ACKS_NONE ||
		    (IS_PEER_POLICY(rep->perm_policy) &&
		    rep->priority == 0)))
			break;

#ifdef	CONFIG_TEST
		if (env->test_abort == DB_TEST_REPMGR_PERM)
			VPRINT(env, (env, DB_VERB_REPMGR_MISC,
			"ISPERM: Test hook.  Skip ACK for permlsn [%lu][%lu]",
			(u_long)permlsn.file, (u_long)permlsn.offset));
#endif
		DB_TEST_SET(env->test_abort, DB_TEST_REPMGR_PERM);
		if (db_rep->perm_lsn.file != permlsn.file)
			bcast = TRUE;
		db_rep->perm_lsn = permlsn;
		if ((ret = send_permlsn(env, generation, bcast, &permlsn)) != 0)
			return (ret);

		break;

	case DB_REP_NOTPERM: /* FALLTHROUGH */
	case DB_REP_IGNORE: /* FALLTHROUGH */
	case DB_LOCK_DEADLOCK:
		break;

	case DB_REP_JOIN_FAILURE:
		RPRINT(env, (env, DB_VERB_REPMGR_MISC,
			"repmgr fires join failure event"));
		DB_EVENT(env, DB_EVENT_REP_JOIN_FAILURE, NULL);
		break;

	default:
		__db_err(env, ret, "DB_ENV->rep_process_message");
		return (ret);
	}
DB_TEST_RECOVERY_LABEL
	return (0);
}

/*
 * Handle replication-related events.  Returns only 0 or DB_EVENT_NOT_HANDLED;
 * no other error returns are tolerated.
 *
 * PUBLIC: int __repmgr_handle_event __P((ENV *, u_int32_t, void *));
 */
int
__repmgr_handle_event(env, event, info)
	ENV *env;
	u_int32_t event;
	void *info;
{
	DB_REP *db_rep;

	db_rep = env->rep_handle;

	if (db_rep->selector == NULL) {
		/* Repmgr is not in use, so all events go to application. */
		return (DB_EVENT_NOT_HANDLED);
	}

	switch (event) {
	case DB_EVENT_REP_ELECTED:
		DB_ASSERT(env, info == NULL);
		db_rep->takeover_pending = TRUE;

		/*
		 * The application doesn't really need to see this, because the
		 * purpose of this event is to tell the winning site that it
		 * should call rep_start(MASTER), and in repmgr we do that
		 * automatically.  Still, they could conceivably be curious, and
		 * it doesn't hurt anything to let them know.
		 */
		break;
	case DB_EVENT_REP_NEWMASTER:
		DB_ASSERT(env, info != NULL);

		/* Application still needs to see this. */
		break;
	default:
		break;
	}
	return (DB_EVENT_NOT_HANDLED);
}

static int
send_permlsn(env, generation, bcast, lsn)
	ENV *env;
	u_int32_t generation;
	int bcast;
	DB_LSN *lsn;
{
	DB_REP *db_rep;
	REP *rep;
	REPMGR_CONNECTION *conn;
	REPMGR_SITE *site;
	int master, ret;
	u_int eid;

	db_rep = env->rep_handle;
	rep = db_rep->region;
	ret = 0;
	master = rep->master_id;
	LOCK_MUTEX(db_rep->mutex);

	/*
	 * All non-bcast perms go to the master site.
	 * If we're not in touch with the master, we drop it, since
	 * there's not much else we can do for now.
	 */
	if (!bcast && (!IS_VALID_EID(master) || master == SELF_EID)) {
		RPRINT(env, (env, DB_VERB_REPMGR_MISC,
		    "dropping ack with master %d", master));
		goto unlock;
	}

	/*
	 * We always need to send the perm LSN to the master, and
	 * we need to send to both primary and subordinate connections.
	 * Do that first.
	 */
	site = SITE_FROM_EID(master);
	if (bcast)
		VPRINT(env, (env, DB_VERB_REPMGR_MISC,
		    "send_permlsn: send [%lu][%lu] to master %d",
		    (u_long)lsn->file, (u_long)lsn->offset, master));
	/*
	 * Send the ack out on any/all connections that need it,
	 * rather than going to the trouble of trying to keep
	 * track of what LSN's each connection may be waiting for.
	 */
	if (IS_SITE_AVAILABLE(site) &&
	    (ret = send_permlsn_conn(env, site->ref.conn, generation,
	    lsn)) != 0)
		goto unlock;
	TAILQ_FOREACH(conn, &site->sub_conns, entries) {
		if ((ret = send_permlsn_conn(env, conn, generation, lsn)) != 0)
			goto unlock;
	}
	if (bcast) {
		/*
		 * Send our information to everyone.
		 */
		for (eid = 0; eid < db_rep->site_cnt; eid++) {
			/*
			 * Skip the master, we've already sent to that site.
			 */
			if ((int)eid == master)
				continue;
			site = SITE_FROM_EID(eid);
			VPRINT(env, (env, DB_VERB_REPMGR_MISC,
			    "send_permlsn: send permlsn to eid %d", eid));

			/*
			 * Send the ack out on primary connection only.
			 */
			if (site->state == SITE_CONNECTED &&
			    (ret = send_permlsn_conn(env,
			    site->ref.conn, generation, lsn)) != 0)
				goto unlock;
		}
	}

unlock:
	UNLOCK_MUTEX(db_rep->mutex);
	return (ret);
}

/*
 * Sends an perm LSN message on one connection, if it needs it.
 *
 * !!! Called with mutex held.
 */
static int
send_permlsn_conn(env, conn, generation, lsn)
	ENV *env;
	REPMGR_CONNECTION *conn;
	u_int32_t generation;
	DB_LSN *lsn;
{
	DBT control2, rec2;
	__repmgr_permlsn_args permlsn;
	u_int8_t buf[__REPMGR_PERMLSN_SIZE];
	int ret;

	ret = 0;

	if (conn->state == CONN_READY) {
		DB_ASSERT(env, conn->version > 0);
		permlsn.generation = generation;
		memcpy(&permlsn.lsn, lsn, sizeof(DB_LSN));
		if (conn->version == 1) {
			control2.data = &permlsn;
			control2.size = sizeof(permlsn);
		} else {
			__repmgr_permlsn_marshal(env, &permlsn, buf);
			control2.data = buf;
			control2.size = __REPMGR_PERMLSN_SIZE;
		}
		rec2.size = 0;
		/*
		 * It's hard to imagine anyone would care about a lost ack if
		 * the path to the master is so congested as to need blocking;
		 * so pass "blockable" argument as FALSE.
		 */
		if ((ret = __repmgr_send_one(env, conn, REPMGR_PERMLSN,
		    &control2, &rec2, FALSE)) == DB_REP_UNAVAIL)
			ret = __repmgr_bust_connection(env, conn);
	}
	return (ret);
}

/*
 * Does everything necessary to handle the processing of a NEWSITE return.
 */
static int
handle_newsite(env, rec)
	ENV *env;
	const DBT *rec;
{
	DB_REP *db_rep;
	REPMGR_SITE *site;
	SITE_STRING_BUFFER buffer;
	size_t hlen;
	u_int16_t port;
	int ret;
	char *host;

	db_rep = env->rep_handle;
	/*
	 * Check if we got sent connect information and if we did, if
	 * this is me or if we already have a connection to this new
	 * site.  If we don't, establish a new one.
	 *
	 * Unmarshall the cdata: a 2-byte port number, in network byte order,
	 * followed by the host name string, which should already be
	 * null-terminated, but let's make sure.
	 */
	if (rec->size < sizeof(port) + 1) {
		__db_errx(env, "unexpected cdata size, msg ignored");
		return (0);
	}
	memcpy(&port, rec->data, sizeof(port));
	port = ntohs(port);

	host = (char*)((u_int8_t*)rec->data + sizeof(port));
	hlen = (rec->size - sizeof(port)) - 1;
	host[hlen] = '\0';

	/* It's me, do nothing. */
	if (strcmp(host, db_rep->my_addr.host) == 0 &&
	    port == db_rep->my_addr.port) {
		VPRINT(env, (env, DB_VERB_REPMGR_MISC,
		    "repmgr ignores own NEWSITE info"));
		return (0);
	}

	LOCK_MUTEX(db_rep->mutex);
	if ((ret = __repmgr_add_site(env,
	    host, port, &site, 0, FALSE)) == EEXIST) {
		VPRINT(env, (env, DB_VERB_REPMGR_MISC,
		    "NEWSITE info from %s was already known",
		    __repmgr_format_site_loc(site, buffer)));
		/*
		 * This is a good opportunity to look up the host name, if
		 * needed, because we're on a message thread (not the critical
		 * select() thread).
		 */
		if ((ret = __repmgr_check_host_name(env,
		    EID_FROM_SITE(site))) != 0)
			return (ret);

		if (site->state == SITE_CONNECTED)
			goto unlock; /* Nothing to do. */
	} else {
		if (ret != 0)
			goto unlock;
		RPRINT(env, (env, DB_VERB_REPMGR_MISC,
		    "NEWSITE info added %s",
		    __repmgr_format_site_loc(site, buffer)));
	}

	/*
	 * Wake up the main thread to connect to the new or reawakened
	 * site.
	 */
	ret = __repmgr_wake_main_thread(env);

unlock: UNLOCK_MUTEX(db_rep->mutex);
	return (ret);
}
