/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2005,2008 Oracle.  All rights reserved.
 *
 * $Id: repmgr_util.c,v 1.45 2008/04/30 02:33:34 alexg Exp $
 */

#include "db_config.h"

#define	__INCLUDE_NETWORKING	1
#include "db_int.h"

/*
 * Schedules a future attempt to re-establish a connection with the given site.
 * Usually, we wait the configured retry_wait period.  But if the "immediate"
 * parameter is given as TRUE, we'll make the wait time 0, and put the request
 * at the _beginning_ of the retry queue.  Note how this allows us to preserve
 * the property that the queue stays in time order simply by appending to the
 * end.
 *
 * PUBLIC: int __repmgr_schedule_connection_attempt __P((ENV *, u_int, int));
 *
 * !!!
 * Caller should hold mutex.
 *
 * Unless an error occurs, we always attempt to wake the main thread;
 * __repmgr_bust_connection relies on this behavior.
 */
int
__repmgr_schedule_connection_attempt(env, eid, immediate)
	ENV *env;
	u_int eid;
	int immediate;
{
	DB_REP *db_rep;
	REPMGR_RETRY *retry;
	REPMGR_SITE *site;
	db_timespec t;
	int ret;

	db_rep = env->rep_handle;
	if ((ret = __os_malloc(env, sizeof(*retry), &retry)) != 0)
		return (ret);

	__os_gettime(env, &t, 1);
	if (immediate)
		TAILQ_INSERT_HEAD(&db_rep->retries, retry, entries);
	else {
		TIMESPEC_ADD_DB_TIMEOUT(&t, db_rep->connection_retry_wait);
		TAILQ_INSERT_TAIL(&db_rep->retries, retry, entries);
	}
	retry->eid = eid;
	retry->time = t;

	site = SITE_FROM_EID(eid);
	site->state = SITE_IDLE;
	site->ref.retry = retry;

	return (__repmgr_wake_main_thread(env));
}

/*
 * Initialize the necessary control structures to begin reading a new input
 * message.
 *
 * PUBLIC: void __repmgr_reset_for_reading __P((REPMGR_CONNECTION *));
 */
void
__repmgr_reset_for_reading(con)
	REPMGR_CONNECTION *con;
{
	con->reading_phase = SIZES_PHASE;
	__repmgr_iovec_init(&con->iovecs);
	__repmgr_add_buffer(&con->iovecs, &con->msg_type,
	    sizeof(con->msg_type));
	__repmgr_add_buffer(&con->iovecs, &con->control_size_buf,
	    sizeof(con->control_size_buf));
	__repmgr_add_buffer(&con->iovecs, &con->rec_size_buf,
	    sizeof(con->rec_size_buf));
}

/*
 * Constructs a DB_REPMGR_CONNECTION structure, and puts it on the main list of
 * connections.  It does not initialize eid, since that isn't needed and/or
 * immediately known in all cases.
 *
 * PUBLIC:  int __repmgr_new_connection __P((ENV *, REPMGR_CONNECTION **,
 * PUBLIC:				   socket_t, int));
 */
int
__repmgr_new_connection(env, connp, s, state)
	ENV *env;
	REPMGR_CONNECTION **connp;
	socket_t s;
	int state;
{
	DB_REP *db_rep;
	REPMGR_CONNECTION *c;
	int ret;

	db_rep = env->rep_handle;
	if ((ret = __os_calloc(env, 1, sizeof(REPMGR_CONNECTION), &c)) != 0)
		return (ret);
	if ((ret = __repmgr_alloc_cond(&c->drained)) != 0) {
		__os_free(env, c);
		return (ret);
	}
	c->blockers = 0;

	c->fd = s;
	c->state = state;

	STAILQ_INIT(&c->outbound_queue);
	c->out_queue_length = 0;

	__repmgr_reset_for_reading(c);
	TAILQ_INSERT_TAIL(&db_rep->connections, c, entries);
	*connp = c;

	return (0);
}

/*
 * PUBLIC: int __repmgr_new_site __P((ENV *, REPMGR_SITE**,
 * PUBLIC:     const repmgr_netaddr_t *, int));
 *
 * !!!
 * Caller must hold mutex.
 */
int
__repmgr_new_site(env, sitep, addr, state)
	ENV *env;
	REPMGR_SITE **sitep;
	const repmgr_netaddr_t *addr;
	int state;
{
	DB_REP *db_rep;
	REPMGR_SITE *site;
	SITE_STRING_BUFFER buffer;
	u_int new_site_max, eid;
	int ret;

	db_rep = env->rep_handle;
	if (db_rep->site_cnt >= db_rep->site_max) {
#define	INITIAL_SITES_ALLOCATION	10		/* Arbitrary guess. */
		new_site_max = db_rep->site_max == 0 ?
		    INITIAL_SITES_ALLOCATION : db_rep->site_max * 2;
		if ((ret = __os_realloc(env,
		     sizeof(REPMGR_SITE) * new_site_max, &db_rep->sites)) != 0)
			 return (ret);
		db_rep->site_max = new_site_max;
	}
	eid = db_rep->site_cnt++;

	site = &db_rep->sites[eid];

	memcpy(&site->net_addr, addr, sizeof(*addr));
	ZERO_LSN(site->max_ack);
	site->flags = 0;
	timespecclear(&site->last_rcvd_timestamp);
	site->state = state;

	RPRINT(env, DB_VERB_REPMGR_MISC,
	    (env, "EID %u is assigned for %s", eid,
	    __repmgr_format_site_loc(site, buffer)));
	*sitep = site;
	return (0);
}

/*
 * Destructor for a repmgr_netaddr_t, cleans up any allocated memory pointed to
 * by the addr.
 *
 * PUBLIC: void __repmgr_cleanup_netaddr __P((ENV *, repmgr_netaddr_t *));
 */
void
__repmgr_cleanup_netaddr(env, addr)
	ENV *env;
	repmgr_netaddr_t *addr;
{
	if (addr->address_list != NULL) {
		__os_freeaddrinfo(env, addr->address_list);
		addr->address_list = addr->current = NULL;
	}
	if (addr->host != NULL) {
		__os_free(env, addr->host);
		addr->host = NULL;
	}
}

/*
 * PUBLIC: void __repmgr_iovec_init __P((REPMGR_IOVECS *));
 */
void
__repmgr_iovec_init(v)
	REPMGR_IOVECS *v;
{
	v->offset = v->count = 0;
	v->total_bytes = 0;
}

/*
 * PUBLIC: void __repmgr_add_buffer __P((REPMGR_IOVECS *, void *, size_t));
 *
 * !!!
 * There is no checking for overflow of the vectors[5] array.
 */
void
__repmgr_add_buffer(v, address, length)
	REPMGR_IOVECS *v;
	void *address;
	size_t length;
{
	v->vectors[v->count].iov_base = address;
	v->vectors[v->count++].iov_len = length;
	v->total_bytes += length;
}

/*
 * PUBLIC: void __repmgr_add_dbt __P((REPMGR_IOVECS *, const DBT *));
 */
void
__repmgr_add_dbt(v, dbt)
	REPMGR_IOVECS *v;
	const DBT *dbt;
{
	v->vectors[v->count].iov_base = dbt->data;
	v->vectors[v->count++].iov_len = dbt->size;
	v->total_bytes += dbt->size;
}

/*
 * Update a set of iovecs to reflect the number of bytes transferred in an I/O
 * operation, so that the iovecs can be used to continue transferring where we
 * left off.
 *     Returns TRUE if the set of buffers is now fully consumed, FALSE if more
 * remains.
 *
 * PUBLIC: int __repmgr_update_consumed __P((REPMGR_IOVECS *, size_t));
 */
int
__repmgr_update_consumed(v, byte_count)
	REPMGR_IOVECS *v;
	size_t byte_count;
{
	db_iovec_t *iov;
	int i;

	for (i = v->offset; ; i++) {
		DB_ASSERT(NULL, i < v->count && byte_count > 0);
		iov = &v->vectors[i];
		if (byte_count > iov->iov_len) {
			/*
			 * We've consumed (more than) this vector's worth.
			 * Adjust count and continue.
			 */
			byte_count -= iov->iov_len;
		} else {
			/* 
			 * Adjust length of remaining portion of vector. 
			 * byte_count can never be greater than iov_len, or we
			 * would not be in this section of the if clause.
			 */
			iov->iov_len -= (u_int32_t)byte_count;
			if (iov->iov_len > 0) {
				/*
				 * Still some left in this vector.  Adjust base
				 * address too, and leave offset pointing here.
				 */
				iov->iov_base = (void *)
				    ((u_int8_t *)iov->iov_base + byte_count);
				v->offset = i;
			} else {
				/*
				 * Consumed exactly to a vector boundary.
				 * Advance to next vector for next time.
				 */
				v->offset = i+1;
			}
			/*
			 * If offset has reached count, the entire thing is
			 * consumed.
			 */
			return (v->offset >= v->count);
		}
	}
}

/*
 * Builds a buffer containing our network address information, suitable for
 * publishing as cdata via a call to rep_start, and sets up the given DBT to
 * point to it.  The buffer is dynamically allocated memory, and the caller must
 * assume responsibility for it.
 *
 * PUBLIC: int __repmgr_prepare_my_addr __P((ENV *, DBT *));
 */
int
__repmgr_prepare_my_addr(env, dbt)
	ENV *env;
	DBT *dbt;
{
	DB_REP *db_rep;
	size_t size, hlen;
	u_int16_t port_buffer;
	u_int8_t *ptr;
	int ret;

	db_rep = env->rep_handle;

	/*
	 * The cdata message consists of the 2-byte port number, in network byte
	 * order, followed by the null-terminated host name string.
	 */
	port_buffer = htons(db_rep->my_addr.port);
	size = sizeof(port_buffer) +
	    (hlen = strlen(db_rep->my_addr.host) + 1);
	if ((ret = __os_malloc(env, size, &ptr)) != 0)
		return (ret);

	DB_INIT_DBT(*dbt, ptr, size);

	memcpy(ptr, &port_buffer, sizeof(port_buffer));
	ptr = &ptr[sizeof(port_buffer)];
	memcpy(ptr, db_rep->my_addr.host, hlen);

	return (0);
}

/*
 * Provide the appropriate value for nsites, the number of sites in the
 * replication group.  If the application has specified a value, use that.
 * Otherwise, just use the number of sites we know of.
 *
 * !!!
 * This may only be called after the environment has been opened, because we
 * assume we have a rep region.  That should be OK, because we only need this
 * for starting an election, or counting acks after sending a PERM message.
 *
 * PUBLIC: u_int __repmgr_get_nsites __P((DB_REP *));
 */
u_int
__repmgr_get_nsites(db_rep)
	DB_REP *db_rep;
{
	REP *rep;

	rep = db_rep->region;
	if (rep->config_nsites > 0)
		return ((u_int)rep->config_nsites);

	/*
	 * The number of other sites in our table, plus 1 to count ourself.
	 */
	return (db_rep->site_cnt + 1);
}

/*
 * PUBLIC: void __repmgr_thread_failure __P((ENV *, int));
 */
void
__repmgr_thread_failure(env, why)
	ENV *env;
	int why;
{
	(void)__repmgr_stop_threads(env);
	(void)__env_panic(env, why);
}

/*
 * Format a printable representation of a site location, suitable for inclusion
 * in an error message.  The buffer must be at least as big as
 * MAX_SITE_LOC_STRING.
 *
 * PUBLIC: char *__repmgr_format_eid_loc __P((DB_REP *, int, char *));
 */
char *
__repmgr_format_eid_loc(db_rep, eid, buffer)
	DB_REP *db_rep;
	int eid;
	char *buffer;
{
	if (IS_VALID_EID(eid))
		return (__repmgr_format_site_loc(SITE_FROM_EID(eid), buffer));

	snprintf(buffer, MAX_SITE_LOC_STRING, "(unidentified site)");
	return (buffer);
}

/*
 * PUBLIC: char *__repmgr_format_site_loc __P((REPMGR_SITE *, char *));
 */
char *
__repmgr_format_site_loc(site, buffer)
	REPMGR_SITE *site;
	char *buffer;
{
	snprintf(buffer, MAX_SITE_LOC_STRING, "site %s:%lu",
	    site->net_addr.host, (u_long)site->net_addr.port);
	return (buffer);
}

/*
 * PUBLIC: int __repmgr_repstart __P((ENV *, u_int32_t));
 */
int
__repmgr_repstart(env, flags)
	ENV *env;
	u_int32_t flags;
{
	DBT my_addr;
	int ret;

	if ((ret = __repmgr_prepare_my_addr(env, &my_addr)) != 0)
		return (ret);
	ret = __rep_start(env->dbenv, &my_addr, flags);
	__os_free(env, my_addr.data);
	if (ret != 0)
		__db_err(env, ret, "rep_start");
	return (ret);
}
