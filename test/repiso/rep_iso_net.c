/*-
 * test/repiso/rep_iso_net.c --
 *	Minimal Base-Replication-API transport for the two-site harness.
 *
 * Derived from examples/c/ex_rep/base/rep_net.c, cut down to exactly two
 * sites so there is no machine table, no election, and no site discovery to
 * go wrong.  What remains is the smallest thing that is still a REAL
 * transport: one TCP connection, the ex_rep wire format
 * (4-byte rec size, rec, 4-byte control size, control), and a reader thread
 * that hands every message to DB_ENV->rep_process_message.
 *
 * Why not reuse ex_rep directly: ex_rep's rep_net.c pulls in the machtab, the
 * election threads and the connect-all logic, all of which want an N-site
 * world.  Two sites with fixed roles need ~200 lines, and a harness that is
 * mostly its own transport is a harness whose failures are its own fault.
 */
#include <errno.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

#include "rep_iso.h"

/*
 * The peer.  Two sites, so a single socket and a single mutex serializing
 * writes to it is the whole "network layer".
 *
 * ponytail: one global peer, because the harness is two sites by
 * construction.  If this ever grows to three sites, put these in a struct
 * keyed by eid -- but a third site cannot observe anything the second cannot.
 */
static int		 peer_fd = -1;
static pthread_mutex_t	 send_lock = PTHREAD_MUTEX_INITIALIZER;
static DB_ENV		*msg_dbenv;
static volatile int	 reader_stop;
static pthread_t	 reader_thr;
static int		 reader_started;

/* Counters the driver prints; plain ints, single writer each. */
volatile int		 riso_startupdone;
volatile int		 riso_is_master;
volatile unsigned long	 riso_msgs_in;
volatile unsigned long	 riso_txns_applied;

static ssize_t
readn(int fd, void *vp, size_t n)
{
	size_t nleft;
	ssize_t nr;
	u_int8_t *p;

	p = vp;
	nleft = n;
	while (nleft > 0) {
		if ((nr = read(fd, p, nleft)) < 0) {
			if (errno == EINTR)
				continue;
			return (-1);
		}
		if (nr == 0)
			break;			/* EOF */
		nleft -= (size_t)nr;
		p += nr;
	}
	return ((ssize_t)(n - nleft));
}

static int
writen(int fd, const void *vp, size_t n)
{
	size_t nleft;
	ssize_t nw;
	const u_int8_t *p;

	p = vp;
	nleft = n;
	while (nleft > 0) {
		if ((nw = write(fd, p, nleft)) <= 0) {
			if (nw < 0 && errno == EINTR)
				continue;
			return (-1);
		}
		nleft -= (size_t)nw;
		p += nw;
	}
	return (0);
}

/*
 * riso_send --
 *	The f_send function for DB_ENV->rep_set_transport.  Both DB_EID_BROADCAST
 *	and the peer's eid mean "the one peer", since there is only one.
 */
int
riso_send(DB_ENV *dbenv, const DBT *control, const DBT *rec,
    const DB_LSN *lsnp, int eid, u_int32_t flags)
{
	u_int32_t csize, rsize;
	int fd, ret;

	COMPQUIET(lsnp, NULL);
	COMPQUIET(eid, 0);
	COMPQUIET(flags, 0);

	if ((fd = peer_fd) < 0)
		return (DB_REP_UNAVAIL);

	rsize = rec == NULL ? 0 : rec->size;
	csize = control == NULL ? 0 : control->size;

	(void)pthread_mutex_lock(&send_lock);
	ret = writen(fd, &rsize, 4);
	if (ret == 0 && rsize > 0)
		ret = writen(fd, rec->data, rsize);
	if (ret == 0)
		ret = writen(fd, &csize, 4);
	if (ret == 0 && csize > 0)
		ret = writen(fd, control->data, csize);
	(void)pthread_mutex_unlock(&send_lock);

	if (ret != 0) {
		/*
		 * A dead peer must be reported, not swallowed: swallowing it
		 * makes a master think a PERMANENT record was delivered.
		 */
		return (DB_REP_UNAVAIL);
	}
	return (0);
}

/*
 * riso_msg_loop --
 *	Read messages off the socket forever, feeding each to
 *	rep_process_message.  This is the thread that performs APPLY on the
 *	client -- the code path issue #140 is about.
 */
static void *
riso_msg_loop(void *arg)
{
	DBT control, rec;
	DB_LSN permlsn;
	u_int32_t csize, rsize;
	int r;

	COMPQUIET(arg, NULL);
	memset(&control, 0, sizeof(control));
	memset(&rec, 0, sizeof(rec));

	while (!reader_stop) {
		if (readn(peer_fd, &rsize, 4) != 4)
			break;
		if (rsize > 0) {
			if ((rec.data = realloc(rec.data, rsize)) == NULL)
				break;
			if (readn(peer_fd, rec.data, rsize) !=
			    (ssize_t)rsize)
				break;
		}
		rec.size = rsize;
		if (readn(peer_fd, &csize, 4) != 4)
			break;
		if (csize > 0) {
			if ((control.data =
			    realloc(control.data, csize)) == NULL)
				break;
			if (readn(peer_fd, control.data, csize) !=
			    (ssize_t)csize)
				break;
		}
		control.size = csize;

		riso_msgs_in++;
		r = msg_dbenv->rep_process_message(msg_dbenv, &control, &rec,
		    RISO_PEER_EID, &permlsn);
		switch (r) {
		case 0:
		case DB_REP_ISPERM:
		case DB_REP_NOTPERM:
		case DB_REP_NEWSITE:
		case DB_REP_IGNORE:
			break;
		default:
			msg_dbenv->err(msg_dbenv, r, "rep_process_message");
			break;
		}
	}
	if (rec.data != NULL)
		free(rec.data);
	if (control.data != NULL)
		free(control.data);
	return (NULL);
}

void
riso_event(DB_ENV *dbenv, u_int32_t which, void *info)
{
	COMPQUIET(info, NULL);

	switch (which) {
	case DB_EVENT_REP_CLIENT:
		riso_is_master = 0;
		break;
	case DB_EVENT_REP_MASTER:
		riso_is_master = 1;
		break;
	case DB_EVENT_REP_STARTUPDONE:
		riso_startupdone = 1;
		break;
	case DB_EVENT_REP_NEWMASTER:
		break;
	default:
		COMPQUIET(dbenv, NULL);
		break;
	}
}

/*
 * riso_listen --
 *	Bind, listen and accept exactly one connection.  The master listens;
 *	the client connects.  Fixed roles remove the whole "who calls an
 *	election" question from the harness.
 */
int
riso_listen(int port)
{
	struct sockaddr_in si;
	socklen_t silen;
	int ls, on;

	if ((ls = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)) < 0) {
		perror("socket");
		return (-1);
	}
	on = 1;
	(void)setsockopt(ls, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on));

	memset(&si, 0, sizeof(si));
	si.sin_family = AF_INET;
	si.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
	si.sin_port = htons((u_int16_t)port);
	if (bind(ls, (struct sockaddr *)&si, sizeof(si)) != 0) {
		perror("bind");
		(void)close(ls);
		return (-1);
	}
	if (listen(ls, 1) != 0) {
		perror("listen");
		(void)close(ls);
		return (-1);
	}
	silen = sizeof(si);
	if ((peer_fd = accept(ls, (struct sockaddr *)&si, &silen)) < 0) {
		perror("accept");
		(void)close(ls);
		return (-1);
	}
	(void)close(ls);
	on = 1;
	(void)setsockopt(peer_fd, IPPROTO_TCP, TCP_NODELAY, &on, sizeof(on));
	return (0);
}

/*
 * riso_connect --
 *	Connect to the master, retrying while it comes up.  The retry budget is
 *	generous on purpose: a shared CI runner can take seconds to schedule
 *	the peer process, and "peer not up yet" must not look like a failure.
 */
int
riso_connect(const char *host, int port, int timeout_secs)
{
	struct sockaddr_in si;
	struct hostent *hp;
	int fd, i, on;

	if ((hp = gethostbyname(host)) == NULL) {
		fprintf(stderr, "host not found: %s\n", host);
		return (-1);
	}
	for (i = 0; i < timeout_secs * 10; i++) {
		if ((fd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)) < 0) {
			perror("socket");
			return (-1);
		}
		memset(&si, 0, sizeof(si));
		si.sin_family = AF_INET;
		memcpy(&si.sin_addr, hp->h_addr, (size_t)hp->h_length);
		si.sin_port = htons((u_int16_t)port);
		if (connect(fd, (struct sockaddr *)&si, sizeof(si)) == 0) {
			peer_fd = fd;
			on = 1;
			(void)setsockopt(peer_fd, IPPROTO_TCP, TCP_NODELAY,
			    &on, sizeof(on));
			return (0);
		}
		(void)close(fd);
		usleep(100000);
	}
	fprintf(stderr, "could not connect to %s:%d in %ds\n", host, port,
	    timeout_secs);
	return (-1);
}

int
riso_start_reader(DB_ENV *dbenv)
{
	int ret;

	msg_dbenv = dbenv;
	reader_stop = 0;
	if ((ret = pthread_create(&reader_thr, NULL, riso_msg_loop,
	    NULL)) != 0) {
		fprintf(stderr, "pthread_create: %s\n", strerror(ret));
		return (ret);
	}
	reader_started = 1;
	return (0);
}

void
riso_stop_reader(void)
{
	reader_stop = 1;
	if (peer_fd >= 0) {
		(void)shutdown(peer_fd, SHUT_RDWR);
		(void)close(peer_fd);
		peer_fd = -1;
	}
	if (reader_started) {
		(void)pthread_join(reader_thr, NULL);
		reader_started = 0;
	}
}
