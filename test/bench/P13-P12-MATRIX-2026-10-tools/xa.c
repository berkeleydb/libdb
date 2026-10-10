/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/* Cross-attach probe: open an env with binary A, then ATTACH with binary B.
 * A moved signature must produce DB_VERSION_MISMATCH / BDB1539, which is what
 * "users must recreate their environment" actually means.  Mode 'c' creates,
 * mode 'a' attaches only (no DB_CREATE). */
#include <db.h>
#include <stdio.h>
#include <string.h>
int main(int c,char**v){
  DB_ENV *e; int r; u_int32_t fl;
  if(c<3){fprintf(stderr,"usage: xa <dir> c|a\n");return 2;}
  if((r=db_env_create(&e,0))!=0){printf("create err %d\n",r);return 3;}
  e->set_errpfx(e,"xa");
  fl=DB_INIT_LOCK|DB_INIT_LOG|DB_INIT_MPOOL|DB_INIT_TXN|DB_THREAD;
  if(v[2][0]=='c') fl|=DB_CREATE;
  r=e->open(e,v[1],fl,0600);
  printf("open rc=%d (%s)%s\n", r, r?db_strerror(r):"success",
     r==DB_VERSION_MISMATCH?"  <== DB_VERSION_MISMATCH":"");
  if(r==0) e->close(e,0);
  return r==0?0:1;}
