#!/bin/sh -
#
# Copyright (c) 2026 libdb contributors.  All rights reserved.
#
# SPDX-License-Identifier: Sleepycat
#
# test/dst-rt/scenarios/s2_io_errors.sh <seed> -- RFC 0024 S2.
#
# I/O error handling (one node).  For each libfiu I/O plan, inject the fault
# at a seeded probability during a workload, then check (§2.4 rules 1-3, 7):
# does the injected EIO/ENOSPC/short I/O return an error or panic, never
# corrupt or silently lose an ACKed commit, and is the env recoverable?
#
# Arms, one per plan (§2.2):
#   s2-eio-write    posix pwrite/write -> EIO
#   s2-fsync-fail   fdatasync/fsync -> EIO  (must never report a non-durable
#                   commit as durable)
#   s2-enospc       pwrite/write -> ENOSPC
#   s2-short        pwrite/pread return 0 (short count)
#
# The loader only ACKs a commit when commit() returned 0; an injected fault
# makes the commit (or an earlier op) return non-zero, logged FAIL, so rtcheck
# requires it NOT be visible.  The oracle therefore catches BOTH a lost ACK and
# a phantom FAIL.
#
# SABOTAGE arm (s2-delete-behind): after a clean run, delete a key directly
# behind the oracle's back (bypassing the ack log) -- an ACKed put whose value
# is then removed.  rtcheck MUST catch the missing acked key.  Teeth for the
# whole S2 oracle.
#
# Usage:  sh scenarios/s2_io_errors.sh <seed>

set -u
HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
DSTRT_DIR=${DSTRT_DIR:-$(CDPATH= cd -- "$HERE/.." && pwd)}
. "$DSTRT_DIR/lib/fi_env.sh"
. "$DSTRT_DIR/lib/fi_fiu.sh"
. "$DSTRT_DIR/../harness.sh"

SEED=${1:-1}
OPS=${OPS:-1500}
KEYS=${KEYS:-250}
VALSZ=${VALSZ:-200}

echo "# reproduce: BUILD=$BUILD DSTRT_DIR=$DSTRT_DIR sh $0 $SEED"
hi_init "dst-rt" "$DSTRT_DIR/.."

fi_have libdb || { echo "SKIP: no libdb under $BUILD"; exit 77; }
[ -x "$BUILD/rtload" ] || fi_build_node rtload || { echo "SKIP: rtload build"; exit 77; }
[ -x "$BUILD/rtcheck" ] || fi_build_node rtcheck || { echo "SKIP: rtcheck build"; exit 77; }
fi_have fiu || { echo "SKIP: libfiu not available"; hi_emit s2-eio-write skip
	hi_emit s2-fsync-fail skip; hi_emit s2-enospc skip; hi_emit s2-short skip
	hi_emit s2-delete-behind skip; exit 77; }

RUN=$(fi_mkrun s2)
trap 'rm -rf "$RUN" 2>/dev/null' 0

rc=0

# run_plan NAME PLAN PROB -- run a loader under PLAN, recover, check, emit.
run_plan() {
	_name=$1 _plan=$2 _prob=$3
	_home="$RUN/$_name/home"; mkdir -p "$_home"
	_ack="$RUN/$_name/ack.log"; : > "$_ack"

	echo "== $_name =="
	_cmd=$(fi_fiu_cmd "$_plan" "$SEED" "$_prob")
	# shellcheck disable=SC2086
	eval "$_cmd" "$BUILD/rtload" --home "$_home" --ack "$_ack" \
	    --node "$_name" --seed "$SEED" --ops "$OPS" --keys "$KEYS" \
	    --valsz "$VALSZ" >/dev/null 2>&1
	_lrc=$?
	# A non-zero loader rc is fine (the fault surfaced as an error); a
	# SIGSEGV/SIGABRT without DB_RUNRECOVERY would be a rule-7 violation,
	# but rtcheck's recovery open is the real gate.
	echo "    loader rc=$_lrc"

	_out=$("$BUILD/rtcheck" --home "$_home" --ack "$_ack" 2>&1)
	echo "$_out" | sed 's/^/    /'
	if echo "$_out" | grep -q '^PASS'; then
		hi_emit "$_name" pass
	else
		# A violation here is a genuine finding: an injected I/O fault
		# led to a lost acked commit, a phantom, corruption, or an
		# unrecoverable env.
		hi_emit "$_name" fail; rc=1
		echo "    FINDING: $_plan produced an oracle violation (see FINDINGS.md)"
	fi
}

run_plan s2-eio-write  io-eio-write  0.01
run_plan s2-fsync-fail io-fsync-fail 0.03
run_plan s2-enospc     io-enospc     0.01
run_plan s2-short      io-short      0.02

# ---- SABOTAGE: delete a key behind the oracle's back ----------------------
# Prove the oracle catches a real data loss.  Run a clean (no-fault) loader,
# then open the db and delete one key that the ack log says is ACKed+present.
# rtcheck MUST report it missing.
echo "== s2-delete-behind =="
_home="$RUN/sab/home"; mkdir -p "$_home"
_ack="$RUN/sab/ack.log"; : > "$_ack"
"$BUILD/rtload" --home "$_home" --ack "$_ack" --node sab --seed "$SEED" \
    --ops 400 --keys "$KEYS" --valsz "$VALSZ" >/dev/null 2>&1
# find one ACKed put key from the log (last put with an ACKed txn).
victim=$(awk '$3=="put"{k[$2]=$5} $3=="txn"&&$7=="ACK"{print k[$2]}' \
    "$_ack" | grep . | tail -1)
if [ -z "$victim" ]; then
	echo "    SKIP: no acked key to delete"; hi_emit s2-delete-behind skip
else
	# delete it behind the oracle with db_dump|db_load round-trip? simpler:
	# a tiny driver that opens the db and deletes the key.  Use the stock
	# db utilities: db_dump the key out, then a db with the key removed via
	# a rtdel helper would be heavy; instead use the loader's engine through
	# a one-shot C action compiled on the fly is overkill.  Use rtcheck's
	# own sibling: run a loader that does a single del and does NOT log it.
	# Simplest portable path: db_load to overwrite... but a del is cleaner.
	# We shell out to a small inline program via the already-built rtload?
	# rtload cannot del a specific key.  Use the db tools: there is no
	# "delete one key" tool, so use a 6-line C sabotage helper.
	cat > "$RUN/sab_del.c" <<'CEOF'
#include <db.h>
#include <string.h>
int main(int c,char**v){DB_ENV*e;DB*d;DBT k;int r;
 if(c<3)return 2; db_env_create(&e,0);
 if((r=e->open(e,v[1],DB_CREATE|DB_INIT_LOCK|DB_INIT_LOG|DB_INIT_MPOOL|DB_INIT_TXN|DB_RECOVER,0))!=0)return 3;
 db_create(&d,e,0);
 if((r=d->open(d,NULL,"rt.db",NULL,DB_UNKNOWN,DB_AUTO_COMMIT,0))!=0)return 4;
 memset(&k,0,sizeof k);k.data=v[2];k.size=(u_int32_t)strlen(v[2]);
 r=d->del(d,NULL,&k,DB_AUTO_COMMIT); d->close(d,0); e->close(e,0); return r?5:0;}
CEOF
	# shellcheck disable=SC2086
	"${CC:-cc}" -g -O1 -I"$BUILD" "$RUN/sab_del.c" "$LIBDB_LIB" \
	    $LIBDB_RPATH $LIBDB_EXTRA -o "$RUN/sab_del" 2>/dev/null
	"$RUN/sab_del" "$_home" "$victim" >/dev/null 2>&1
	echo "    deleted acked key $victim behind the oracle"
	_out=$("$BUILD/rtcheck" --home "$_home" --ack "$_ack" 2>&1)
	echo "$_out" | sed 's/^/    /'
	# PASSES iff the oracle FAILED (caught the missing acked key).
	if echo "$_out" | grep -q '^FAIL'; then
		echo "    oracle caught the behind-the-back delete (teeth)"
		hi_emit s2-delete-behind pass
	else
		echo "    SABOTAGE NOT CAUGHT: oracle missed a deleted acked key"
		hi_emit s2-delete-behind fail; rc=1
	fi
fi

exit $rc
