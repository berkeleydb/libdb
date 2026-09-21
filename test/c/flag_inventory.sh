#!/bin/sh
# test/c/flag_inventory.sh -- the HONEST accounting of public API flag coverage.
#
# WHY A SCRIPT RATHER THAN A NUMBER IN A DOCUMENT
#
# test/TESTING-PROGRAM.md reports "112 of 229 public API flags
# referenced by NO test."  A number in a document goes stale the moment someone
# adds a flag or a test, and cannot be re-derived without guessing the method.
# This script IS the method, so the figure in
# test/TESTING-IMPROVEMENTS.md can be regenerated and disputed.
#
# THE THREE-WAY DISTINCTION THAT MATTERS
#
# "Referenced" is the weak bar the 2026-09 document warned about: G15 showed a
# flag can be referenced, counted as covered, and still be completely broken
# (cov_api_surface.c "covered" DB_DIRECT_DB by checking set_flags ACCEPTED it,
# while no database could be opened under it -- defect P2).  So this counts three
# populations, not two:
#
#   ASSERTED    the flag appears in a behaviour driver's CODE, where a verdict
#               depends on its observable consequence.
#   MENTIONED   the flag appears somewhere under test/ -- possibly only in a
#               comment, a doc block, or an acceptance check.  NOT coverage.
#   UNTESTED    the flag appears nowhere under test/ at all.
#
# The ASSERTED figure is deliberately the harsh one.  A flag named only in a
# comment of the very file that tests its neighbours is counted MENTIONED, not
# ASSERTED -- which is why the number this prints is lower than a naive grep
# would give, and why it is the number worth quoting.
#
# Usage:
#	test/c/flag_inventory.sh            # the summary
#	test/c/flag_inventory.sh --list     # plus every flag in each bucket
#	test/c/flag_inventory.sh --untested # just the untested names, one per line
#
# Exits 0 always: this is a measurement, not a gate.  (A gate on the count would
# have to choose a threshold, and the useful pressure here is the MANIFEST's --
# a new flag with no verdict fails check_manifest.sh, not this.)

set -eu

HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
ROOT=$(CDPATH= cd -- "$HERE/../.." && pwd)
FLAGS="$ROOT/src/dbinc_auto/api_flags.in"
MODE=${1:-}

[ -f "$FLAGS" ] || { echo "flag_inventory: no $FLAGS" >&2; exit 2; }

# The behaviour drivers.  test/c/cov_api_surface.c is deliberately NOT here -- it
# is the acceptance-check file whose weakness prompted all of this.
DRIVERS="$HERE/flag_behaviour.c $HERE/flag_archive.c $HERE/flag_misc.c"

# ---------------------------------------------------------------------------
# SUBJECT flags: the flags a graded verdict is ABOUT.
#
# This list is explicit, and it has to be.  Simply grepping the drivers'
# non-comment code overcounts badly: those drivers necessarily USE DB_CREATE,
# DB_INIT_MPOOL, DB_AUTO_COMMIT, DB_PRIVATE, DB_FORCE, DB_MPOOL_DIRTY and so on
# as scaffolding to reach the flag under test.  Counting scaffolding as coverage
# is a softer version of exactly the mistake G15 named -- inflating a number with
# references that no verdict depends on.  Measured, the difference is 48 flags
# "appearing in driver code" versus 33 that a verdict is actually about.
#
# Each name here is cross-checked below against the stripped driver code, so a
# typo or a deleted mode makes this script FAIL rather than quietly shrink the
# count.
#
# From test/c/flag_behaviour.c (tier `flag`, gap G15, first wave).
#
# DB_LOG_WRNOSYNC is NOT in this list even though the `syncs` mode is entirely
# about it.  It is not settable directly: __txn_commit passes it to log_put when
# the environment has DB_TXN_WRITE_NOSYNC (LOG_FLAGS() in src/txn/txn.c), so the
# driver sets the trigger and asserts the log-sync count.  The flag therefore
# never appears by name in the code, and the cross-check below would reject it --
# correctly, since "the name is present" is not the property being claimed.
# Flags in this position are listed as INDIRECT and reported separately, because
# silently folding them into the main count would be a claim the code cannot
# support.
SUBJECT_FLAGS="DB_DIRECT DB_DIRECT_DB DB_DSYNC_DB DB_LOG_DIRECT DB_LOG_DSYNC
DB_NOSYNC"
# Asserted through a trigger flag rather than by name.  Each entry is
# "flag:trigger" so the mechanism is recorded, not just the claim.
INDIRECT_FLAGS="DB_LOG_WRNOSYNC:DB_TXN_WRITE_NOSYNC"
# From test/c/flag_archive.c (tier `flagapi`, archive + backup):
SUBJECT_FLAGS="$SUBJECT_FLAGS DB_ARCH_ABS DB_ARCH_DATA DB_ARCH_LOG
DB_ARCH_REMOVE DB_BACKUP_NO_LOGS DB_BACKUP_UPDATE"
# From test/c/flag_misc.c (tier `flagapi`, everything else):
SUBJECT_FLAGS="$SUBJECT_FLAGS DB_SEQ_INC DB_SEQ_DEC DB_SEQ_WRAP
DB_SEQ_RANGE_SET DB_TXN_FAMILY DB_TXN_WAIT DB_CURSOR_BULK DB_INORDER
DB_FREELIST_ONLY DB_NOLOCKING DB_OVERWRITE DB_NOFLUSH
DB_HOTBACKUP_IN_PROGRESS DB_STAT_LOCK_CONF DB_STAT_LOCK_OBJECTS
DB_STAT_LOCK_PARAMS DB_STAT_SUMMARY DB_AGGRESSIVE DB_PRINTABLE
DB_ORDERCHKONLY DB_SALVAGE"

# ---------------------------------------------------------------------------
# SKIP_FLAGS -- flags that genuinely CANNOT be given a behaviour assertion from
# a test, each with the reason.  Format: <flag>|<reason>
#
# The brief for this work requires an explicit SKIP verdict rather than silent
# omission, "so the list stays honest".  Everything here is skipped for a
# STRUCTURAL reason -- the flag is not reachable from the public API, or it is an
# output-only value, or asserting it needs infrastructure this fork does not have
# -- never because it is inconvenient.
#
# The categories, and why each is a genuine structural block:
#
#  INTERNAL-ONLY:  the flag shares a bit-space with the public API (it is in
#      api_flags.in because dist/s_apiflags generates every flag from
#      dist/api_flags) but only ever passed between internal functions.  An
#      application cannot set it: __db_fchk on the public entry point REJECTS it.
#      The DB_ST_* / DB_SA_* verify-descent flags are the clearest case --
#      bt_verify.c passes them down its own recursion.
#
#  FEEDBACK VALUE:  DB_UPGRADE / DB_VERIFY are the `opcode` ARGUMENT the library
#      passes TO an application's set_feedback callback.  There is nothing to
#      set; the assertion would be "the callback was called with this constant",
#      which is a binding test, not a behaviour test.  Worth doing, but it
#      belongs with the callback coverage in cov_api_surface.c.
#
#  NEEDS MULTI-SITE REPLICATION:  the DB_REP_* transport flags are arguments the
#      library passes to an application's send() callback, and asserting them
#      needs two live sites plus a controllable network.  test/repiso and the
#      rep0NN harness are where that belongs.
#
#  NEEDS A CRASHED PROCESS / EXTERNAL STATE:  DB_FAILCHK_ISALIVE,
#      DB_MUTEX_ALLOCATED and friends are only observable across a process that
#      died holding a resource.
#
# Listing a flag here is a CLAIM, and it is checked: the loop below refuses any
# entry that is already asserted by a driver (that would be a stale skip hiding
# real coverage) and any entry that is not a real flag (a typo).
SKIP_FLAGS='
DB_ST_TOPLEVEL|INTERNAL-ONLY: verify-descent state passed down bt_verify.c/hash_verify.c recursion; DB->verify rejects it via VERIFY_FLAGS (db_vrfy.c:61)
DB_ST_DUPOK|INTERNAL-ONLY: as DB_ST_TOPLEVEL -- descent state, not settable
DB_ST_DUPSET|INTERNAL-ONLY: as DB_ST_TOPLEVEL -- descent state, not settable
DB_ST_DUPSORT|INTERNAL-ONLY: as DB_ST_TOPLEVEL -- descent state, not settable
DB_ST_IS_RECNO|INTERNAL-ONLY: as DB_ST_TOPLEVEL -- descent state, not settable
DB_ST_OVFL_LEAF|INTERNAL-ONLY: as DB_ST_TOPLEVEL -- descent state, not settable
DB_ST_RECNUM|INTERNAL-ONLY: as DB_ST_TOPLEVEL -- descent state, not settable
DB_ST_RELEN|INTERNAL-ONLY: as DB_ST_TOPLEVEL -- descent state, not settable
DB_SA_SKIPFIRSTKEY|INTERNAL-ONLY: salvage-descent state set by bt_verify.c itself
DB_SA_UNKNOWNKEY|INTERNAL-ONLY: salvage-descent state set by bt_verify.c itself
DB_UNREF|INTERNAL-ONLY: db_vrfy.c:112 SETS this itself for non-salvage verifies; an application cannot distinguish it
DB_UPGRADE|FEEDBACK VALUE: the opcode the library passes TO set_feedback, not a flag to set
DB_VERIFY|FEEDBACK VALUE: the opcode the library passes TO set_feedback, not a flag to set
DB_REP_ANYWHERE|NEEDS MULTI-SITE REPLICATION: an argument the library passes to the application send() callback; belongs with the rep0NN harness
DB_REP_NOBUFFER|NEEDS MULTI-SITE REPLICATION: as DB_REP_ANYWHERE
DB_REP_REREQUEST|NEEDS MULTI-SITE REPLICATION: as DB_REP_ANYWHERE
DB_REP_ELECTION|NEEDS MULTI-SITE REPLICATION: repmgr_start mode needing a real quorum
DB_MUTEX_ALLOCATED|NEEDS A CRASHED PROCESS: mutex state inspected by failchk after a holder dies (mut_failchk.c)
DB_MUTEX_LOCKED|NEEDS A CRASHED PROCESS: as DB_MUTEX_ALLOCATED
DB_MUTEX_LOGICAL_LOCK|NEEDS A CRASHED PROCESS: as DB_MUTEX_ALLOCATED
DB_FAILCHK_ISALIVE|NEEDS A CRASHED PROCESS: only observable when a registered process dies holding a resource; env012/DB_REGISTER territory
DB_INTERNAL_PERSISTENT_DB|INTERNAL-ONLY: set by fop_util.c/rep_method.c for the library own metadata databases
DB_INTERNAL_TEMPORARY_DB|INTERNAL-ONLY: as DB_INTERNAL_PERSISTENT_DB
DB_LOG_NOCOPY|INTERNAL-ONLY in practice: log_put optimisation whose only observable is that the caller buffer is not copied -- indistinguishable from outside
DB_SHALLOW_DUP|INTERNAL-ONLY: bt_compress.c cursor-duplication detail
DB_EXTENT|INTERNAL-ONLY: queue-extent bookkeeping passed by qam_files.c to mpool
DB_ODDFILESIZE|INTERNAL-ONLY: set by db_vrfy.c/db.c when opening a file whose size is not a page multiple
DB_DURABLE_UNKNOWN|INTERNAL-ONLY: mp_fopen/dbreg use it when durability cannot yet be determined
DB_PR_PAGE|INTERNAL-ONLY: db_dump/printlog page-dump selector, not part of DB->verify OKFLAGS
DB_PR_RECOVERYTEST|INTERNAL-ONLY: as DB_PR_PAGE
'
: "${SKIP_FLAGS:=}"

TMP=${TMPDIR:-/tmp}/libdb-flaginv.$$
mkdir -p "$TMP" || exit 2
trap 'find "$TMP" -mindepth 1 -delete 2>/dev/null; rmdir "$TMP" 2>/dev/null' 0

# Every public flag name, sorted.
awk '/^#define[ \t]+DB_/ { print $2 }' "$FLAGS" | sort -u > "$TMP/all"
total=$(wc -l < "$TMP/all" | tr -d ' ')

# strip_comments FILE -- FILE with C comment bodies removed, so a flag named only
# in a comment is not counted as asserted.
#
# awk with an explicit in-comment state, not sed: this file's style is one long
# block comment per function, and the obvious sed range expression for that is
# both unportable and easy to get wrong.  The first version here used
# `sed -e '\,/\*,,\*/,d'`, which is a malformed address in GNU sed -- it
# emitted "unterminated address regex", produced an EMPTY stripped file, and so
# reported 0 flags asserted.  An empty input silently answering "nothing is
# covered" is the same vacuous shape as an empty input answering "everything is
# covered"; the check below refuses both.
strip_comments() {
	awk '
	{
		line = $0
		out = ""
		while (length(line) > 0) {
			if (incomment) {
				i = index(line, "*/")
				if (i == 0) { line = ""; break }
				incomment = 0
				line = substr(line, i + 2)
			} else {
				i = index(line, "/*")
				if (i == 0) { out = out line; line = "" }
				else {
					out = out substr(line, 1, i - 1)
					line = substr(line, i + 2)
					incomment = 1
				}
			}
		}
		# Drop // comments too.
		sub(/\/\/.*/, "", out)
		print out
	}' "$1" 2>/dev/null
}

for f in $DRIVERS; do
	[ -f "$f" ] && strip_comments "$f"
done > "$TMP/drivercode" 2>/dev/null || :

# A comment-stripper that produced nothing would report "0 flags asserted",
# which looks like a finding and is actually a broken tool.  That happened (see
# strip_comments).  Refuse to print a number derived from an empty input.
code_lines=$(wc -l < "$TMP/drivercode" | tr -d ' ')
if [ "${code_lines:-0}" -lt 100 ]; then
	echo "flag_inventory: the comment-stripped driver code is only" \
	    "${code_lines:-0} lines -- strip_comments is broken, and every" \
	    "count below would be wrong in the direction that looks like a" \
	    "finding.  Refusing to report." >&2
	exit 2
fi

: > "$TMP/asserted"
: > "$TMP/mentioned"
: > "$TMP/untested"

while IFS= read -r flag; do
	if grep -qw -- "$flag" "$TMP/drivercode" 2>/dev/null; then
		echo "$flag" >> "$TMP/asserted"
	elif (cd "$ROOT" && git grep -qw -- "$flag" -- test/ \
	    ':!test/c/flag_inventory.sh' 2>/dev/null); then
		echo "$flag" >> "$TMP/mentioned"
	else
		echo "$flag" >> "$TMP/untested"
	fi
done < "$TMP/all"

# Cross-check the SUBJECT list against the stripped driver code.  A subject flag
# that no longer appears in any driver means a mode was deleted or renamed, and
# the count would silently overstate coverage.  That is a hard error.
: > "$TMP/subject"
missing_subj=
for sf in $SUBJECT_FLAGS; do
	if grep -qw -- "$sf" "$TMP/drivercode" 2>/dev/null; then
		echo "$sf" >> "$TMP/subject"
	else
		missing_subj="$missing_subj $sf"
	fi
done
if [ -n "$missing_subj" ]; then
	echo "flag_inventory: these SUBJECT flags appear in no driver's code:" \
	    "$missing_subj" >&2
	echo "A mode was deleted or renamed.  Refusing to report a coverage" \
	    "count that would overstate what is actually asserted." >&2
	exit 2
fi
sort -u "$TMP/subject" -o "$TMP/subject"
ns=$(wc -l < "$TMP/subject" | tr -d ' ')

# Indirect subjects: the flag is what the verdict is about, but it is reached
# through a trigger flag.  Require the TRIGGER to be present in driver code, so
# the claim still rests on something checkable.
: > "$TMP/indirect"
for pair in $INDIRECT_FLAGS; do
	fl=${pair%%:*}; tr=${pair#*:}
	if grep -qw -- "$tr" "$TMP/drivercode" 2>/dev/null; then
		echo "$fl (via $tr)" >> "$TMP/indirect"
	else
		echo "flag_inventory: $fl is claimed to be asserted via $tr," \
		    "but $tr appears in no driver code -- the mechanism is" \
		    "gone, so the claim is false." >&2
		exit 2
	fi
done
ni=$(wc -l < "$TMP/indirect" | tr -d ' ')

na=$(wc -l < "$TMP/asserted" | tr -d ' ')
nm=$(wc -l < "$TMP/mentioned" | tr -d ' ')
nu=$(wc -l < "$TMP/untested" | tr -d ' ')

# Partition the untested set into explicitly-SKIPPED (with a reason) and
# genuinely OPEN.  Both claims are checked, because a stale skip would hide real
# coverage and a typo'd skip would silently shrink the open list.
: > "$TMP/skipped"
: > "$TMP/skipped_mentioned"
: > "$TMP/skipreason"
bad_skip=
# NB: fed from a FILE, not a pipe.  `printf ... | while read` runs the loop in a
# subshell, so the appends below would be lost and the partition would come out
# empty -- which it did: SKIPPED and OPEN summed to more than UNTESTED.
printf '%s\n' "$SKIP_FLAGS" > "$TMP/skiplist"
while IFS='|' read -r sf reason; do
	[ -n "${sf:-}" ] || continue
	if [ -z "${reason:-}" ]; then
		echo "$sf|NO REASON GIVEN" >> "$TMP/skipreason"
		continue
	fi
	if ! grep -qx -- "$sf" "$TMP/all"; then
		echo "$sf|NOT A REAL FLAG" >> "$TMP/skipreason"
		continue
	fi
	if grep -qx -- "$sf" "$TMP/subject" 2>/dev/null; then
		echo "$sf|ALREADY ASSERTED" >> "$TMP/skipreason"
		continue
	fi
	# A skip only partitions the UNTESTED set.  A flag that is merely
	# MENTIONED (named in a comment, or used as scaffolding) is already
	# outside `untested`, so counting its skip there would make SKIPPED plus
	# OPEN exceed UNTESTED -- it did, by exactly one: DB_UNREF, which
	# db_vrfy.c:112 sets itself and which this list explains, but which a
	# driver comment also names.  Reported separately rather than dropped:
	# the explanation is still worth having.
	if ! grep -qx -- "$sf" "$TMP/untested"; then
		echo "$sf" >> "$TMP/skipped_mentioned"
		echo "$sf|$reason" >> "$TMP/skipreason"
		continue
	fi
	echo "$sf" >> "$TMP/skipped"
	echo "$sf|$reason" >> "$TMP/skipreason"
done < "$TMP/skiplist"
# A bad entry is a hard error: an unjustified or stale skip is exactly the
# dishonesty the SKIP list exists to prevent.
if grep -qE '\|(NO REASON GIVEN|NOT A REAL FLAG|ALREADY ASSERTED)$' \
    "$TMP/skipreason" 2>/dev/null; then
	echo "flag_inventory: bad SKIP_FLAGS entries:" >&2
	grep -E '\|(NO REASON GIVEN|NOT A REAL FLAG|ALREADY ASSERTED)$' \
	    "$TMP/skipreason" | sed -n 's/^/    /p' >&2
	echo "A skip with no reason, a skip of a nonexistent flag, or a skip of" \
	    "a flag that IS asserted all make the inventory a lie.  Fix the" \
	    "list." >&2
	exit 2
fi
sort -u "$TMP/skipped" -o "$TMP/skipped" 2>/dev/null || :
nsk=$(wc -l < "$TMP/skipped" 2>/dev/null | tr -d ' ')
nskm=$(wc -l < "$TMP/skipped_mentioned" 2>/dev/null | tr -d ' ')
: "${nskm:=0}"
comm -23 "$TMP/untested" "$TMP/skipped" > "$TMP/open" 2>/dev/null || cp "$TMP/untested" "$TMP/open"
nop=$(wc -l < "$TMP/open" | tr -d ' ')

if [ "$MODE" = "--untested" ]; then
	cat "$TMP/untested"
	exit 0
fi
if [ "$MODE" = "--open" ]; then
	cat "$TMP/open"
	exit 0
fi
if [ "$MODE" = "--skipped" ]; then
	sed 's/|/\t/' "$TMP/skipreason"
	exit 0
fi

pct() { awk -v n="$1" -v d="$2" 'BEGIN { printf "%.1f", d ? 100 * n / d : 0 }'; }

echo "== public API flag coverage =="
echo "  source:  $FLAGS"
echo "  drivers: $(echo $DRIVERS | sed "s,$ROOT/,,g")"
echo
printf '  %-40s %4d\n' "public API flags declared" "$total"
printf '  %-40s %4d  (%s%%)\n' "SUBJECT of a graded verdict" "$ns" "$(pct "$ns" "$total")"
printf '  %-40s %4d\n' "  ..plus asserted via a trigger flag" "$ni"
printf '  %-40s %4d  (%s%%)\n' "  ..appearing in driver code at all" "$na" "$(pct "$na" "$total")"
printf '  %-40s %4d  (%s%%)\n' "mentioned under test/ only" "$nm" "$(pct "$nm" "$total")"
printf '  %-40s %4d  (%s%%)\n' "UNTESTED (no reference at all)" "$nu" "$(pct "$nu" "$total")"
printf '  %-40s %4d\n' "  ..of those, SKIPPED with a reason" "$nsk"
printf '  %-40s %4d\n' "  ..of those, genuinely OPEN" "$nop"
printf '  %-40s %4d\n' "explained-but-only-mentioned (not untested)" "$nskm"
echo
echo "  SUBJECT is the number worth quoting: the flag is what a verdict is"
echo "  ABOUT.  The larger \"appearing in driver code\" figure includes the"
echo "  scaffolding those drivers need to reach the flag under test (DB_CREATE,"
echo "  DB_INIT_MPOOL, DB_AUTO_COMMIT and so on); counting that as coverage is a"
echo "  softer form of the mistake G15 named."
echo
echo "  'mentioned' is NOT coverage either: G15 established that a flag can be"
echo "  referenced, counted, and still be wholly non-functional (P2)."

if [ "$MODE" = "--list" ]; then
	echo
	echo "-- SUBJECT of a graded verdict ($ns) --"
	sed -n 's/^/  /p' "$TMP/subject"
	echo
	echo "-- asserted via a trigger flag ($ni) --"
	sed -n 's/^/  /p' "$TMP/indirect"
	echo
	echo "-- appearing in driver code, incl. scaffolding ($na) --"
	sed -n 's/^/  /p' "$TMP/asserted"
	echo
	echo "-- mentioned only ($nm) --"
	sed -n 's/^/  /p' "$TMP/mentioned"
	echo
	echo "-- UNTESTED but explicitly SKIPPED, with the reason ($nsk) --"
	sed 's/|/\n        /' "$TMP/skipreason" | sed -n 's/^/  /p'
	echo
	echo "-- genuinely OPEN: no test, no stated reason ($nop) --"
	sed -n 's/^/  /p' "$TMP/open"
fi

exit 0
