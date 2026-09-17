#!/bin/sh
# lsc_probe.sh -- build ONE library with a runtime-switched, DELIBERATELY UNSAFE
# ceiling probe, to measure the size of the prize before designing a safe fix.
#
# The probe changes exactly one thing: in __db_new, the PGNO_BASE_MD write lock
# is released with __LPUT (unconditional put) instead of __TLPUT (no-op inside a
# txn), when the env var LSC_META_EARLY=1 is set.
#
# THIS IS NOT A CANDIDATE FIX.  It breaks 2PL for the meta page: another txn can
# allocate from a free list this txn has mutated but not committed, and if this
# txn aborts, __db_pg_alloc_recover's UNDO writes meta->free = argp->pgno and
# meta->last_pgno = argp->last_pgno BLINDLY (guarded only by the meta LSN), so it
# would clobber the second txn's allocation.  It exists ONLY to answer "if the
# meta hold were free, how much of the 285 ms would go away".  Never shipped.
set -e
WT=$HOME/wt-lockscope
cd $WT

python3 - <<'EOF'
import re
p = 'src/db/db_meta.c'
s = open(p).read()

# 1. The probe hook: a file-static predicate read once.
hook = '''
/*
 * CEILING PROBE, NOT A FIX.  See test/bench/lsc_probe.sh and
 * test/bench/BTREE-LOCK-SCOPE-2026-09.md.  When LSC_META_EARLY=1, __db_new
 * releases the PGNO_BASE_MD write lock unconditionally rather than holding it
 * to commit.  That BREAKS 2PL and abort-correctness on purpose, to measure the
 * ceiling.  Never enabled in any shipped build; guarded so the stock path is
 * byte-identical when the variable is unset.
 */
static int lsc_meta_early = -1;
static int
lsc_meta_early_on()
{
	char *e;

	if (lsc_meta_early < 0) {
		e = getenv("LSC_META_EARLY");
		lsc_meta_early = (e != NULL && *e == '1');
	}
	return (lsc_meta_early);
}
'''
anchor = '/*\n * __db_init_meta --'
assert anchor in s
s = s.replace(anchor, hook + '\n' + anchor, 1)

# 2. __db_new success path: line 263 "if ((ret = __TLPUT(dbc, metalock)) != 0)"
old = '''	if ((ret = __TLPUT(dbc, metalock)) != 0)
		return (ret);
	*pagepp = h;'''
new = '''	if ((ret = lsc_meta_early_on() ?
	    __LPUT(dbc, metalock) : __TLPUT(dbc, metalock)) != 0)
		return (ret);
	*pagepp = h;'''
assert s.count(old) == 1, s.count(old)
s = s.replace(old, new, 1)

open(p,'w').write(s)
print("patched")
EOF

cd $WT/build_unix && make -j96 > /tmp/probe_build.log 2>&1 && echo PROBE_BUILD_OK
cd $WT/test/bench && make BDB=../../build_unix commit_bench 2>&1 | tail -1
echo PROBE_READY
