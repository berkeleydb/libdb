/*-
 * test/repiso/rep_iso.h --
 *	Shared declarations for the two-site replication isolation harness.
 */
#ifndef	_REP_ISO_H_
#define	_REP_ISO_H_

#include <sys/types.h>
#include "db.h"

#ifndef COMPQUIET
#define	COMPQUIET(x, y)	((void)(x))
#endif

/*
 * Environment IDs.  Fixed roles, two sites: whoever I am, the other one is
 * RISO_PEER_EID.  Base replication requires SELF and PEER to be distinct and
 * stable for the life of the process, which is all these are.
 */
#define	RISO_SELF_EID	1
#define	RISO_PEER_EID	2

/* The database the harness replicates. */
#define	RISO_DBFILE	"iso.db"

/*
 * The two keys of the trigger shape.  They must land on DIFFERENT B-tree leaf
 * pages, which RISO_FILL filler keys sorting between them arranges (they sort
 * as WKEY < f#### < RKEY).  The driver verifies the split with DB->stat's
 * bt_leaf_pg rather than assuming it.
 */
#define	RISO_WKEY	"aaa_write"
#define	RISO_RKEY	"zzz_read"
#define	RISO_FILL	64
#define	RISO_PAGESIZE	512
#define	RISO_PAD	100

/* Transport (rep_iso_net.c). */
int  riso_send __P((DB_ENV *, const DBT *, const DBT *, const DB_LSN *,
	int, u_int32_t));
void riso_event __P((DB_ENV *, u_int32_t, void *));
int  riso_listen __P((int));
int  riso_connect __P((const char *, int, int));
int  riso_start_reader __P((DB_ENV *));
void riso_stop_reader __P((void));

extern volatile int		riso_startupdone;
extern volatile int		riso_is_master;
extern volatile unsigned long	riso_msgs_in;
extern volatile unsigned long	riso_txns_applied;

#endif /* !_REP_ISO_H_ */
