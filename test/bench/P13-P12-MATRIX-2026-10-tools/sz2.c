/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

#include "db_config.h"
#include "db_int.h"
#include "dbinc/lock.h"
#include <stdio.h>
int main(void){
  printf("sizeof(DB_LOCKREGION)=%zu\n",sizeof(DB_LOCKREGION));
#ifdef LOCK_LOCKER_ALLOC_STRIPE
  printf("sizeof(DB_LOCKERSTRIPE)=%zu\n",sizeof(DB_LOCKERSTRIPE));
  printf("LOCK_LOCKER_STRIPES=%d\n",(int)LOCK_LOCKER_STRIPES);
  printf("stripe_array_bytes=%zu\n",sizeof(DB_LOCKERSTRIPE)*LOCK_LOCKER_STRIPES);
  printf("region_minus_stripe_array=%zu\n",sizeof(DB_LOCKREGION)-sizeof(DB_LOCKERSTRIPE)*LOCK_LOCKER_STRIPES);
  printf("hypothetical_if_stripe_were_40B=%zu\n",
     sizeof(DB_LOCKREGION)-sizeof(DB_LOCKERSTRIPE)*LOCK_LOCKER_STRIPES + 40*(size_t)LOCK_LOCKER_STRIPES);
#endif
  printf("sizeof(DB_LOCK_STAT)=%zu\n",sizeof(DB_LOCK_STAT));
  return 0;}
