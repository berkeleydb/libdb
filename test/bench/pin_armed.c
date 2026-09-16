/*-
 * pin_armed: prove each arm's build is actually ARMED, i.e. that the change
 * under test is compiled in rather than silently absent.  Six vacuous-green
 * measurements have shipped in this project; an arm that is byte-identical to
 * the baseline would produce a perfectly reproducible "NULL result" that means
 * nothing.
 *
 * Prints, from the build's OWN headers:
 *   - __env_struct_sig(), and the public sizes, so a region-layout change is
 *     visible (perf/lock-readpath grows DB_LOCKOBJ, perf/bhpin-r1 grows
 *     DB_MPOOL_HASH, the mpool-pin port pads struct __bh);
 *   - sizeof(BH) / sizeof(DB_MPOOL_HASH) / sizeof(DB_LOCKOBJ) / sizeof(BTREE),
 *     each of which is the fingerprint of exactly one arm.
 */
#include <stdio.h>
#include <stddef.h>
#include "db_config.h"
#include "db_int.h"
#include "dbinc/db_page.h"
#include "dbinc/btree.h"
#include "dbinc/lock.h"
#include "dbinc/mp.h"

int
main(void)
{
	printf("ARMED sig=0x%08x DB=%zu DBC=%zu DB_ENV=%zu DB_TXN=%zu "
	    "BH=%zu MPOOL_HASH=%zu LOCKOBJ=%zu BTREE=%zu",
	    (unsigned)__env_struct_sig(),
	    sizeof(DB), sizeof(DBC), sizeof(DB_ENV), sizeof(DB_TXN),
	    sizeof(BH), sizeof(DB_MPOOL_HASH), sizeof(DB_LOCKOBJ),
	    sizeof(BTREE));
#ifdef MPOOL_HOTFIELDS_ISOLATED
	printf(" MPOOL_HOTFIELDS_ISOLATED=1");
#else
	printf(" MPOOL_HOTFIELDS_ISOLATED=0");
#endif
	printf("\n");
	return (0);
}
