#!/bin/sh
# lockmode_inventory.sh -- CHECKED inventory of every lock-MODE enumeration
# site in libdb, plus the set of DB_LOCK_* modes declared in src/dbinc/db.in.
#
# WHY (GitHub issue #140): SSI added DB_LOCK_SIREAD=9 to db_lockmode_t without
# revisiting the pre-existing sites that enumerate lock modes exhaustively.  One
# of them -- __lock_vec's DB_LOCK_PUT_READ path -- sized a descriptor array from
# sh_locker->nwrites while its population loop named the read modes by hand;
# SIREAD matched neither, fell through, and wrote past the allocation (a heap
# overflow in every release build, the only bounds check being a DIAGNOSTIC-only
# DB_ASSERT), and truncated the replication commit lock list.
#
# This script is the AUTHORITATIVE guard for that class, because Coccinelle
# cannot express "a switch over db_lockmode_t that is missing a case" in the
# spatch build we use (`... when != case X:` inside a switch is a parse error).
# dist/cocci/rule_lock_mode_enum.cocci covers the two expression-level shapes;
# this covers the enum itself and the switch sites.
#
# WHAT IT CHECKS (all three are hard failures):
#
#  1. MODE SET.  The db_lockmode_t members in src/dbinc/db.in must exactly equal
#     the committed list in dist/cocci/lockmode_inventory.txt.  Add a mode =>
#     this fails => you must walk the SITES list below and record a verdict for
#     each, then update the inventory in the same commit.
#
#  2. SITES.  Every file:function recorded in the inventory must still exist.
#     A site that is renamed away silently would otherwise drop out of review.
#
#  3. EXHAUSTIVE SWITCHES.  Every switch marked `exhaustive` in the inventory
#     must contain a case arm for EVERY mode in the mode set.  This is the check
#     that #140's class cannot evade: a new mode with no arm fails CI.
#
# Usage:  sh dist/cocci/lockmode_inventory.sh [repo-root]
#         sh dist/cocci/lockmode_inventory.sh --print   (emit the current mode
#                                                        set, for updating .txt)
set -eu

if [ "${1:-}" = "--print" ]; then
	shift
	PRINT=1
else
	PRINT=0
fi
ROOT="${1:-$(pwd)}"
cd "$ROOT"

DBIN=src/dbinc/db.in
INV=dist/cocci/lockmode_inventory.txt

# ---------------------------------------------------------------------------
# The declared mode set: the DB_LOCK_* members of the db_lockmode_t enum.
# Anchored on the typedef block so the unrelated DB_LOCK_* #defines above it
# (detection policies) and the db_lockop_t enum below it are not picked up.
# ---------------------------------------------------------------------------
modes() {
	awk '
	  /^typedef enum \{/ { inenum = 1; next }
	  inenum && /\} db_lockmode_t;/ { exit }
	  inenum && match($0, /DB_LOCK_[A-Z_]+=[0-9]+/) {
		s = substr($0, RSTART, RLENGTH)
		sub(/=.*/, "", s)
		print s
	  }
	' "$DBIN" | sort -u
}

if [ "$PRINT" = 1 ]; then
	modes
	exit 0
fi

[ -f "$INV" ] || { echo "FAIL: missing $INV" >&2; exit 1; }

rc=0

# ---- 1. mode set vs inventory ---------------------------------------------
modes > /tmp/lmi-actual.$$
awk '$1 == "mode" { print $2 }' "$INV" | sort -u > /tmp/lmi-recorded.$$
if ! cmp -s /tmp/lmi-actual.$$ /tmp/lmi-recorded.$$; then
	echo "FAIL: db_lockmode_t in $DBIN differs from $INV"
	echo "  added (in db.in, not in inventory):"
	comm -23 /tmp/lmi-actual.$$ /tmp/lmi-recorded.$$ | sed 's/^/    /'
	echo "  removed (in inventory, not in db.in):"
	comm -13 /tmp/lmi-actual.$$ /tmp/lmi-recorded.$$ | sed 's/^/    /'
	echo "  => A new lock mode must be reviewed against EVERY 'site' line in"
	echo "     $INV (see rfc/0003/lock-mode-audit.md), then recorded here."
	rc=1
fi

# ---- 2. every recorded site still exists ----------------------------------
# site <path> <function> <verdict> <note...>
awk '$1 == "site" { print $2 "\t" $3 }' "$INV" |
while IFS="$(printf '\t')" read -r path fn; do
	[ -f "$path" ] || { echo "FAIL: inventory site file gone: $path"; echo x >> /tmp/lmi-fail.$$; continue; }
	# K&R definition: the function name at column 0 followed by '('.
	grep -q "^$fn(" "$path" || grep -q "^$fn(" "$path" ||
	  { echo "FAIL: inventory site function gone: $fn in $path"; echo x >> /tmp/lmi-fail.$$; }
done

# ---- 3. switches declared exhaustive cover every mode ---------------------
# exhaustive <path> <function>
awk '$1 == "exhaustive" { print $2 "\t" $3 }' "$INV" |
while IFS="$(printf '\t')" read -r path fn; do
	[ -f "$path" ] || { echo "FAIL: exhaustive-switch file gone: $path"; echo x >> /tmp/lmi-fail.$$; continue; }
	# Body = from the K&R definition line to the next line that is exactly '}'.
	body=$(awk -v fn="$fn" '
	  $0 ~ "^" fn "\\(" { inf = 1 }
	  inf { print }
	  inf && /^\}/ { exit }
	' "$path")
	if [ -z "$body" ]; then
		echo "FAIL: exhaustive-switch function gone: $fn in $path"
		echo x >> /tmp/lmi-fail.$$
		continue
	fi
	while read -r m; do
		printf '%s\n' "$body" | grep -q "case[ 	]*$m[ 	]*:" || {
			echo "FAIL: $path:$fn (declared exhaustive) has no 'case $m:'"
			echo "  => add an arm for $m, or drop the 'exhaustive' marker in $INV."
			echo x >> /tmp/lmi-fail.$$
		}
	done < /tmp/lmi-actual.$$
done

[ -f /tmp/lmi-fail.$$ ] && rc=1
rm -f /tmp/lmi-actual.$$ /tmp/lmi-recorded.$$ /tmp/lmi-fail.$$

if [ "$rc" = 0 ]; then
	echo "lock-mode inventory: OK ($(awk '$1=="mode"' "$INV" | wc -l | tr -d ' ') modes, $(awk '$1=="site"' "$INV" | wc -l | tr -d ' ') sites, $(awk '$1=="exhaustive"' "$INV" | wc -l | tr -d ' ') exhaustive switches)"
fi
exit "$rc"
