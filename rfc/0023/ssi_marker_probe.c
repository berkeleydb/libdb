/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 *
 * ssi_marker_probe.c -- diagnostic: where does a serializable read's SIREAD
 * marker land in a multi-level btree, and does a write to the same key lock
 * the same page?  Prints the lock region so we can see the pgno of each
 * SIREAD marker vs the leaf the key lives on.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include "db.h"

static DB_ENV *env;
static DB *db;

static void ck(const char *w, int rc){ if(rc){fprintf(stderr,"%s: %s\n",w,db_strerror(rc));exit(2);} }

static int put(DB_TXN *t,int key,int val){
	DBT k,d; char kb[16]; char buf[200];
	snprintf(kb,sizeof(kb),"k%08d",key);
	memset(&k,0,sizeof(k)); memset(&d,0,sizeof(d)); memset(buf,0,sizeof(buf));
	memcpy(buf,&val,sizeof(val));
	k.data=kb; k.size=strlen(kb); d.data=buf; d.size=sizeof(buf);
	return db->put(db,t,&k,&d,0);
}
static int get(DB_TXN *t,int key){
	DBT k,d; char kb[16]; char buf[256];
	snprintf(kb,sizeof(kb),"k%08d",key);
	memset(&k,0,sizeof(k)); memset(&d,0,sizeof(d));
	k.data=kb; k.size=strlen(kb); d.data=buf; d.ulen=sizeof(buf); d.flags=DB_DBT_USERMEM;
	return db->get(db,t,&k,&d,0);
}

int main(void){
	int i,rc,ky; DB_TXN *t1; DB_BTREE_STAT *bst;
	system("find SMPDIR -mindepth 1 -delete 2>/dev/null"); mkdir("SMPDIR",0755);
	ck("env_create",db_env_create(&env,0));
	ck("set_cache",env->set_cachesize(env,0,16*1024*1024,1));
	ck("open",env->open(env,"SMPDIR",DB_CREATE|DB_INIT_LOCK|DB_INIT_LOG|DB_INIT_MPOOL|DB_INIT_TXN|DB_THREAD,0600));
	ck("db_create",db_create(&db,env,0));
	ck("pagesize",db->set_pagesize(db,512));
	ck("db_open",db->open(db,NULL,"t.db",NULL,DB_BTREE,DB_CREATE|DB_MULTIVERSION|DB_AUTO_COMMIT|DB_THREAD,0600));
	for(i=0;i<402;i++) ck("load",put(NULL,i,0));
	ck("stat",db->stat(db,NULL,&bst,0));
	ky=401;
	printf("tree: %lu leaf pages, %lu levels; reading key %d\n",
	    (u_long)bst->bt_leaf_pg,(u_long)bst->bt_levels,ky); free(bst);

	ck("txn",env->txn_begin(env,NULL,&t1,DB_TXN_SERIALIZABLE));
	rc=get(t1,ky); ck("get",rc);
	printf("--- lock region after serializable read of key %d (look for SIREAD + its pgno) ---\n",ky);
	ck("lock_stat_print",env->lock_stat_print(env,DB_STAT_LOCK_OBJECTS|DB_STAT_ALL));
	ck("commit",t1->commit(t1,0));
	db->close(db,0); env->close(env,0);
	system("find SMPDIR -mindepth 1 -delete 2>/dev/null");
	return 0;
}
