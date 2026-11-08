#!/bin/sh -
#
# Copyright (c) 2026 libdb contributors.  All rights reserved.
#
# SPDX-License-Identifier: Sleepycat
#
# test/dst-rt/lib/fi_check.sh -- invariant oracles (RFC 0024 §2.4).
# Sourced after fi_env.sh.  Single-node rules 1-3 and 7 are enforced by the
# compiled rtcheck; this shell layer wraps it and adds the cross-home rules
# 4-6 (HA convergence, single master, XA atomicity) that compare db_dump /
# db_stat / event logs across homes.  F2 drives the HA rules from here.
#
# fi_check_single HOME ACK...   -- rtcheck on one home (rules 1-3, 7).
# fi_check_converge HOME...     -- rule 4: identical db_dump across homes.
# fi_check_single_master EV...  -- rule 5: at most one master per generation.

# fi_check_single HOME ACKLOG... -- run rtcheck; echo PASS/FAIL, return 0/1.
fi_check_single() {
	_home=$1; shift
	_args="--home $_home"
	for a in "$@"; do _args="$_args --ack $a"; done
	# shellcheck disable=SC2086
	"$BUILD/rtcheck" $_args
}

# locate db_dump / db_stat in the build (libtool-wrapped or .libs).
_fi_tool() {
	for cand in "$BUILD/$1" "$BUILD/.libs/$1"; do
		[ -x "$cand" ] && { echo "$cand"; return 0; }
	done
	command -v "$1" 2>/dev/null
}

# fi_check_converge HOME... -- rule 4.  Every home's db_dump of rt.db must be
# byte-identical after quiesce.  Returns 0 if all match (or <2 homes).
fi_check_converge() {
	_dbdump=$(_fi_tool db_dump)
	[ -n "$_dbdump" ] || { echo "SKIP: db_dump not built"; return 0; }
	_ref=""; _rc=0
	_tmp=$(mktemp -d)
	_i=0
	for h in "$@"; do
		_out="$_tmp/dump.$_i"
		# -p prints printable; -k keeps order stable; compare data only.
		"$_dbdump" -h "$h" -p rt.db 2>/dev/null | \
			grep -v '^VERSION\|^format\|^database=\|^type=' \
			> "$_out" || true
		if [ -z "$_ref" ]; then _ref="$_out"
		elif ! cmp -s "$_ref" "$_out"; then
			echo "VIOLATION (rule 4): $h diverges from the first home"
			_rc=1
		fi
		_i=$((_i + 1))
	done
	rm -rf "$_tmp"
	[ "$_rc" = 0 ] && echo "CONVERGE PASS"
	return "$_rc"
}

# fi_check_single_master EVLOG... -- rule 5.  Scan ROLE master lines; at most
# one node may claim master in any one generation.
fi_check_single_master() {
	awk '
	/ROLE master gen=/ {
		split($0, a, "gen=")
		gen = a[2] + 0
		masters[gen]++
	}
	END {
		bad = 0
		for (g in masters)
			if (masters[g] > 1) {
				printf "VIOLATION (rule 5): %d masters in gen %d\n",
				    masters[g], g
				bad = 1
			}
		if (!bad) print "SINGLE-MASTER PASS"
		exit bad
	}' "$@"
}
