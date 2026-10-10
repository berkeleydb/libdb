/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/* Switch-engagement probe: run N threads doing txn begin/commit, report
 * st_lockers_wait/st_lockers_nowait.  P12 ON should drive wait ~0; P12 OFF
 * (DB_NO_LOCKER_SHARD) should look like base.  This proves the env vars are
 * read, not ignored -- a 13/13 TCL pass alone would not. */
#include <db.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
static DB_ENV *e; static DB *db; static int nthr=16, iters=4000;
static void *w(void *a){ int i; DB_TXN *t; DBT k,d; char kb[32]; long id=(long)a;
  for(i=0;i<iters;i++){ if(e->txn_begin(e,NULL,&t,0)) return NULL;
    memset(&k,0,sizeof k); memset(&d,0,sizeof d);
    snprintf(kb,sizeof kb,"k%ld_%d",id,i%64); k.data=kb; k.size=strlen(kb)+1;
    d.data=kb; d.size=k.size;
    if(db->put(db,t,&k,&d,0)){ t->abort(t); continue; }
    t->commit(t,0);} return NULL;}
int main(int c,char**v){ pthread_t th[256]; long i; DB_LOCK_STAT *s; char *dir=v[1];
  if(db_env_create(&e,0)) return 2;
  e->set_cachesize(e,0,64*1024*1024,1);
  e->set_flags(e,DB_TXN_NOSYNC,1);
  if(e->open(e,dir,DB_CREATE|DB_INIT_LOCK|DB_INIT_LOG|DB_INIT_MPOOL|DB_INIT_TXN|DB_THREAD,0600)) return 3;
  if(db_create(&db,e,0)) return 4;
  if(db->open(db,NULL,"t.db",NULL,DB_BTREE,DB_CREATE|DB_AUTO_COMMIT|DB_THREAD,0600)) return 5;
  for(i=0;i<nthr;i++) pthread_create(&th[i],NULL,w,(void*)i);
  for(i=0;i<nthr;i++) pthread_join(th[i],NULL);
  if(e->lock_stat(e,&s,0)) return 6;
  printf("lockers_wait=%lu lockers_nowait=%lu region_wait=%lu region_nowait=%lu\n",
    (unsigned long)s->st_lockers_wait,(unsigned long)s->st_lockers_nowait,
    (unsigned long)s->st_region_wait,(unsigned long)s->st_region_nowait);
  db->close(db,0); e->close(e,0); return 0;}
