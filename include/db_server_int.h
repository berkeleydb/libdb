/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2000
 *	Sleepycat Software.  All rights reserved.
 *
 * $Id: db_server_int.h,v 1.8 2000/05/18 17:43:20 sue Exp $
 */

#ifndef _DB_SERVER_INT_H_
#define	_DB_SERVER_INT_H_

#define	DB_SERVER_TIMEOUT	300	/* 5 minutes */
#define	DB_SERVER_MAXTIMEOUT	1200	/* 20 minutes */
#define	DB_SERVER_IDLETIMEOUT	86400	/* 1 day */

enum h_type { H_ENV = 1, H_TXN, H_DB, H_CURSOR };

typedef struct home_entry home_entry;
struct home_entry {
	LIST_ENTRY(home_entry) entries;
	char *home;
	char *dir;
	char *name;
};

typedef struct ct_entry ct_entry;
struct ct_entry {
	LIST_ENTRY(ct_entry) entries;
	union {
		DB_ENV *envp;
		DB_TXN *txnp;
		DB *dbp;
		DBC *dbc;
		void *anyp;
	} handle_u;
	long ct_id;
	long ct_active;
	long ct_timeout;
	long ct_idle;
	enum h_type ct_type;
	struct ct_entry *ct_parent;
	struct ct_entry *ct_envparent;
};

#define	ct_envp handle_u.envp
#define	ct_txnp handle_u.txnp
#define	ct_dbp handle_u.dbp
#define	ct_dbc handle_u.dbc
#define	ct_anyp handle_u.anyp

extern int __dbsrv_verbose;

/*
 * Get ctp and activate it.
 * Assumes local variable 'replyp'.
 * NOTE: May 'return' from macro.
 */
#define	ACTIVATE_CTP(ctp, id, type) {		\
	(ctp) = get_tableent(id);		\
	if ((ctp) == NULL) {			\
		replyp->status = DB_NOSERVER_ID;\
		return;				\
	}					\
	DB_ASSERT((ctp)->ct_type == (type));	\
	__dbsrv_active(ctp);			\
}

#endif	/* _DB_SERVER_INT_H_ */
