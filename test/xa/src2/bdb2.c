/*
*      Copyright (c) 1997 BEA Systems, Inc.
*       All Rights Reserved
*
*       THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF
*       BEA Systems, Inc.
*       The copyright notice above does not evidence any
*       actual or intended publication of such source code.
*
* This server is only called by bdb1.  It takes the data sent by bdb1 and 
* inserts it into db2.
*/

#include <db.h>
#include <xa.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <atmi.h>	/* TUXEDO Header File */
#include <userlog.h>	/* TUXEDO Header File */
#include <string.h>
#include <time.h>
#include <tpadm.h>

#define	DATABASE1	"data1.db"
#define	DATABASE2	"data2.db"

static DB *dbp1;
static DB *dbp2;

static int opendb(){
	int ret;
	u_int32_t open_flags = DB_CREATE | DB_AUTO_COMMIT;

	/* Create and initialize database object, open the database. */
	if ((ret = db_create(&dbp1, NULL, DB_XA_CREATE)) != 0) {
		userlog("db_create: %s", db_strerror(ret));
		return (EXIT_FAILURE);
	}

	if ((ret = db_create(&dbp2, NULL, DB_XA_CREATE)) != 0) {
		userlog("db_create: %s", db_strerror(ret));
		return (EXIT_FAILURE);
	}

	if ((ret = dbp1->open(dbp1, NULL, DATABASE1, NULL, DB_BTREE, open_flags, 
	    0664)) != 0) {
		userlog("open: %s", db_strerror(ret));
		return (EXIT_FAILURE);
	}
	if ((ret = dbp2->open(dbp2, NULL, DATABASE2, NULL, DB_BTREE, open_flags, 
	    0664)) != 0) {
		userlog("open: %s", db_strerror(ret));
		return (EXIT_FAILURE);
	}
	return 0;
}

static void closedb(){
	(void)dbp1->close(dbp1, 0);
	(void)dbp2->close(dbp2, 0);
}

/* Write the given data into the given database. */
static int writedb(DB * dbp, void *buf, u_int32_t size){
	DBT key, data;
	size_t len;
	int ch, ret;

	memset(&key, 0, sizeof(DBT));
	memset(&data, 0, sizeof(DBT));

	key.data = buf;
	data.data = buf;
	data.size = key.size = size;
	ret = dbp->put(dbp, NULL, &key, &data, DB_NOOVERWRITE);
	switch (ret) {
	case 0:
		return (EXIT_SUCCESS);
	case DB_KEYEXIST:
	        return (EXIT_SUCCESS);
	case DB_LOCK_DEADLOCK:
	        return (EXIT_SUCCESS);
	default:
		userlog("put: %s", db_strerror(ret));
		return -1;
	}
}

/* Open the databases used by this server when it is started. */
int
tpsvrinit(argc, argv)
int argc;
char **argv;
{
	int ret;
	/* Some compilers warn if argc and argv aren't used. */
	argc = argc;
	argv = argv;

	tpopen();
	if (ret = opendb() != 0){
		userlog("put: %s", db_strerror(ret));
	}

	return(0);
}

/* Close the database when the server is shutdown. */
void
tpsvrdone(void)
{
	closedb();
	tpclose();
}

/* 
 * Get the data the calling server just inserted into db1 and insert it into
 * db2.  Fgets32 is used to get the key passed by the calling server. 
 */
int
WRITE2(rqst)
TPSVCINFO *rqst;

{
	char buf[100];
	DBT key, data;
	FBFR32 *reqbuf = (FBFR32*)rqst->data;
	int ret;
	
	Fgets32(reqbuf, TA_REPOSPARAM, 0, buf);
	userlog("buf:[%s]", buf);

	memset(&key, 0, sizeof(DBT));
	memset(&data, 0, sizeof(DBT));
     
	key.data = buf;
	key.size = (u_int32_t)strlen(buf);
	data.flags = DB_DBT_MALLOC;
		
	/* Get the data that the calling server inserted into db1 */
	switch (ret = dbp1->get(dbp1, NULL, &key, &data, DB_READ_UNCOMMITTED)){
	case 0:
		break;
	case DB_LOCK_DEADLOCK:
	        tpreturn(TPSUCCESS, 1L, rqst->data, 0L, 0);
	default:
		userlog("get: %s", db_strerror(ret));
		tpreturn(TPFAIL, 0L, rqst->data, 0L, 0);
	}
	
	/* Write the data to db2 */
	if(writedb(dbp2, data.data, data.size) != 0){
		tpreturn(TPSUCCESS, 1L, rqst->data, 0L, 0);
	}
	tpreturn(TPSUCCESS, 0, rqst->data, 0L, 0);
}

