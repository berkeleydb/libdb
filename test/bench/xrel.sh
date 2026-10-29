#!/bin/bash
# Copyright (c) 2026 libdb contributors.  All rights reserved.
# SPDX-License-Identifier: Sleepycat
# See the file LICENSE for redistribution information.
# xrel.sh -- cross-release benchmark driver (bdb 5.3.28 / lts v2026.10.7.1 / master).
# Runs on the c6id.metal under /data (XFS, local NVMe).  Every phase appends rows to
# /data/out/<phase>.tsv as they complete and drops /data/out/<phase>.done at the end,
# so a phase can be resumed and the data copied off at any point.
#
#   bash xrel.sh kv | tpcb | tpcc | micro | all
#
# Method: the three arms run back-to-back for each (cell, rep), and the order
# rotates every rep (bdb,lts,master / lts,master,bdb / master,bdb,lts), so a slow
# drift on the host cannot favour one arm.  Each arm loads its own database fresh
# for every rep: on-disk formats differ between the releases and no state is shared.
set -u
OUT=/data/out; mkdir -p $OUT
ARMS=(bdb lts master)
REPS=${REPS:-5}
THREADS=${THREADS:-"1 4 16 32 64 128"}
SECS=${SECS:-10}
RECS=${RECS:-1000000}

order() {	# order <rep>: print the arms rotated by rep
	local r=$1 i n=${#ARMS[@]}
	for i in 0 1 2; do echo -n "${ARMS[$(( (i + r) % n ))]} "; done
}

fresh() {	# fresh <dir>: an empty directory
	mkdir -p "$1"; find "$1" -mindepth 1 -delete
}

phase_kv() {	# YCSB A/B/C/F/E/I over all five access methods, transactional
	local f=$OUT/kv.tsv
	[ -f $f ] || echo -e "arm\trep\tam\twl\tthreads\tsecs\tcommitted\tops_per_s\taborts\tp50_us\tp99_us" > $f
	for am in btree hash recno queue heap; do
		for wl in C B A F E I; do
			[ "$wl" = E ] && { [ $am = hash ] || [ $am = heap ]; } && continue
			for r in $(seq 1 $REPS); do
				for arm in $(order $r); do
					h=/data/run/kv_$arm; fresh $h
					/data/$arm/build_rel/xrel_kv $h $am C $RECS 1 1 load || { echo "LOAD FAIL $arm $am" >&2; continue; }
					for t in $THREADS; do
						row=$(/data/$arm/build_rel/xrel_kv $h $am $wl $RECS $t $SECS 2>>$OUT/kv.err) || row=""
						[ -n "$row" ] && echo -e "$arm\t$r\t$row" >> $f
						# I and E grow the table; reload so every thread count starts equal.
						if [ $wl = I ] || [ $wl = E ]; then
							fresh $h; /data/$arm/build_rel/xrel_kv $h $am C $RECS 1 1 load
						fi
					done
				done
			done
		done
	done
	touch $OUT/kv.done
}

phase_tpcb() {	# TPC-B-like debit/credit, scale 128 (12.8M accounts), DB_RMW reads, sync and wnosync
	local f=$OUT/tpcb.tsv
	[ -f $f ] || echo -e "arm\trep\tdurability\tthreads\ttxn_per_s\tcommitted\tretries" > $f
	for dur in wnosync sync; do
		for r in $(seq 1 $REPS); do
			for arm in $(order $r); do
				h=/data/run/tpcb_$arm; fresh $h
				/data/$arm/build_rel/tproc_b -i -h $h -S 128 -c $((8<<30)) -d $dur > /dev/null 2>&1
				for t in $THREADS; do
					l=$(TPROC_B_RMW=1 /data/$arm/build_rel/tproc_b -h $h -S 128 -c $((8<<30)) -d $dur -t $t -s $SECS 2>>$OUT/tpcb.err | grep '^tproc-b')
					# tproc-b NN threads  X txn/s  committed=C retries=R (Ts)
					v=$(echo "$l" | sed -E 's/.* ([0-9]+) txn\/s +committed=([0-9]+) retries=([0-9]+).*/\1\t\2\t\3/')
					[ -n "$l" ] && echo -e "$arm\t$r\t$dur\t$t\t$v" >> $f
				done
			done
		done
	done
	touch $OUT/tpcb.done
}

phase_tpcc() {	# TPC-C-like, 5 transaction types, 64 warehouses, wnosync
	local f=$OUT/tpcc.tsv
	[ -f $f ] || echo -e "arm\trep\tthreads\ttpmC_like\ttxn_per_s" > $f
	for r in $(seq 1 $REPS); do
		for arm in $(order $r); do
			h=/data/run/tpcc_$arm; fresh $h
			/data/$arm/build_rel/tproc_c -i -h $h -S 64 -c $((16<<30)) -d wnosync > /dev/null 2>&1
			for t in $THREADS; do
				l=$(/data/$arm/build_rel/tproc_c -h $h -S 64 -c $((16<<30)) -d wnosync -t $t -s $SECS 2>>$OUT/tpcc.err | grep '^tproc-c')
				# tproc-c NN threads  X tpmC-like (Y txn/s over Ts)
				v=$(echo "$l" | sed -E 's/.* ([0-9]+) tpmC-like \(([0-9]+) txn\/s.*/\1\t\2/')
				[ -n "$l" ] && echo -e "$arm\t$r\t$t\t$v" >> $f
			done
		done
	done
	touch $OUT/tpcc.done
}

phase_micro() {	# Berkeley DB's own test_micro run.std (122 single-threaded tests)
	local f=$OUT/micro.tsv
	[ -f $f ] || echo -e "arm\trep\ttest\tdesc\tops_per_s" > $f
	for r in $(seq 1 ${MREPS:-3}); do
		for arm in $(order $r); do
			d=/data/run/micro_$arm; fresh $d
			(cd /data/$arm/build_rel && ./test_micro -d $d -i /data/$arm/test/micro/configs/run.std > /dev/null 2>>$OUT/micro.err)
			for t in $(ls $d | grep -E '^[0-9]+$' | sort -n); do
				echo -e "$arm\t$r\t$t\t$(head -1 $d/$t | cut -c3- | tr '\t' ' ')\t$(tail -1 $d/$t | cut -f2)" >> $f
			done
		done
	done
	touch $OUT/micro.done
}

case "${1:-all}" in
kv) phase_kv ;; tpcb) phase_tpcb ;; tpcc) phase_tpcc ;; micro) phase_micro ;;
all) phase_kv; phase_tpcb; phase_tpcc; phase_micro ;;
*) echo "usage: xrel.sh kv|tpcb|tpcc|micro|all" >&2; exit 1 ;;
esac
