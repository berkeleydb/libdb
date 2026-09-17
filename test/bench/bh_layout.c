/*
 * bh_layout.c -- print the real byte layout of struct __bh, including holes.
 *
 * RFC 0007's Phase 0 table stops at `hq`; this prints every member's offset so
 * the padding question ("is there more than one spare byte?") is answered from
 * the compiler rather than from the table.
 *
 * Build (from a configured build dir):
 *   cc -I<build> -I<src>/src -o /tmp/bh_layout test/bench/bh_layout.c
 */
#include "db_config.h"
#include "db_int.h"
#include "dbinc/mp.h"

#include <stdio.h>

#define	P(f)	printf("  %-12s off=%3zu size=%3zu\n", #f,		\
		    (size_t)offsetof(BH, f), sizeof(((BH *)0)->f))

int
main()
{
	printf("sizeof(BH)            = %zu\n", sizeof(BH));
	printf("sizeof(db_mutex_t)    = %zu\n", sizeof(db_mutex_t));
	printf("sizeof(roff_t)        = %zu\n", sizeof(roff_t));
	printf("sizeof(db_atomic_t)   = %zu\n", sizeof(db_atomic_t));
	printf("sizeof(SH_TAILQ_ENTRY)= %zu\n", sizeof(SH_TAILQ_ENTRY));
	printf("sizeof(SH_CHAIN_ENTRY)= %zu\n", sizeof(SH_CHAIN_ENTRY));
	printf("members:\n");
	P(mtx_buf);
	P(ref);
	P(flags);
	P(wired);
#ifdef DB_BH_HAS_GEN
	P(gen);
#endif
	P(priority);
	P(hq);
	P(pgno);
	P(mf_offset);
	P(bucket);
	P(region);
	P(td_off);
	P(vc);
	P(buf);
	printf("public: DB=%zu DBC=%zu DB_ENV=%zu DB_TXN=%zu\n",
	    sizeof(DB), sizeof(DBC), sizeof(DB_ENV), sizeof(DB_TXN));
	return (0);
}
