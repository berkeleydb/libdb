#!/bin/sh
# test/config/option_sweep.sh -- the configure-option sweep (gap G14).
#
# FINDING: 42 of libdb's 54 configure options were NEVER BUILT in CI.  One of
# them was --enable-o_direct, and defect P2 -- a documented public flag under
# which no database can be opened at all -- survived because nothing ever
# compiled or ran that path.
#
# WHAT THIS DOES
#
# ONE OPTION AT A TIME, never combinatorially (2^54 is not a test plan):
#
#   for each option:  configure + build
#   for each option that changes RUNTIME behaviour:  ALSO run the smoke suite
#
# The runtime half is not optional garnish.  --enable-o_direct builds perfectly
# and fails at runtime, so a build-only sweep would have reported it green.
# test/config/config_smoke.c is the runtime half: access-method round-trips with
# byte-compared read-back, commit/abort, stat, log, compact, mpool, verify.
#
# THE COMPLETENESS CHECK IS THE POINT
#
# --check-complete enumerates every AC_ARG_ENABLE / AC_ARG_WITH in
# dist/aclocal/options.m4 and fails if an option is in NEITHER the sweep NOR the
# commented exclusion list below.  Adding a configure option therefore forces a
# testing decision, which is the same discipline test/MANIFEST applies to tests.
# An option that legitimately cannot build here is EXCLUDED WITH A REASON, never
# silently skipped.
#
# Usage:
#   ./option_sweep.sh --check-complete        # completeness gate only (fast)
#   ./option_sweep.sh --list                  # print the plan and exit
#   ./option_sweep.sh [--jobs N] [--only PAT] [--build-dir DIR]
#
# Env: SWEEP_DIR (scratch root, default ./sweep), JOBS, CC
#
# Emits RESULT lines for the manifest gate (tier `config`) so a leg that did not
# run is a MISSING entry rather than a silent absence.

set -u

HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
TOP=$(CDPATH= cd -- "$HERE/../.." && pwd)
OPTIONS_M4="$TOP/dist/aclocal/options.m4"
SWEEP_DIR=${SWEEP_DIR:-"$HERE/sweep"}
JOBS=${JOBS:-$( (nproc 2>/dev/null || echo 4) )}
ONLY=''
MODE=sweep

while [ $# -gt 0 ]; do
	case $1 in
	--check-complete)	MODE=check ;;
	--list)			MODE=list ;;
	--jobs)	shift; JOBS=$1 ;;
	--only)	shift; ONLY=$1 ;;
	--sweep-dir) shift; SWEEP_DIR=$1 ;;
	-h|--help) sed -n '2,38p' "$0"; exit 0 ;;
	*) echo "unknown option: $1" >&2; exit 2 ;;
	esac
	shift
done

# ===========================================================================
# THE PLAN.  Three lists, and every configure option must be in exactly one.
#
# Format of SWEEP_BUILD / SWEEP_SMOKE:  <option-name> <configure-args...>
# The option NAME is what options.m4 declares (so the completeness check can
# match it); the ARGS are what configure is actually given, which is not always
# the same string -- --disable-hash tests the `hash` option, and
# --enable-perfmon-statistics is declared as `perfmon_statistics`.
# ===========================================================================

# ---------------------------------------------------------------------------
# SWEEP_SMOKE -- options that change RUNTIME behaviour.  Build AND run the
# smoke suite.  These are the ones where "it compiled" proves nothing.
# ---------------------------------------------------------------------------
SWEEP_SMOKE='
o_direct|--enable-o_direct
atomicsupport|--disable-atomicsupport
atomicfileread|--enable-atomicfileread
mutexsupport|--disable-mutexsupport
mutex|--with-mutex=POSIX/pthreads/library
posixmutexes|--enable-posixmutexes
umrw|--enable-umrw
stacksize|--with-stacksize=262144
log_checksum|--disable-log_checksum
partition|--disable-partition
hash|--disable-hash
heap|--disable-heap
queue|--disable-queue
replication|--disable-replication
statistics|--disable-statistics
verify|--disable-verify
compression|--disable-compression
smallbuild|--enable-smallbuild
debug_rop|--enable-debug_rop
debug_wop|--enable-debug_wop
diagnostic|--enable-diagnostic
debug|--enable-debug
dst|--enable-dst
faultinject|--enable-faultinject
handoff-trace|--enable-handoff-trace
cryptography|--with-cryptography=no
localization|--enable-localization
stripped_messages|--enable-stripped_messages
uniquename|--with-uniquename=_libdbsweep
'

# ---------------------------------------------------------------------------
# SWEEP_BUILD -- build only.  These affect what gets BUILT (bindings, extra
# libraries, utilities, compiler output) rather than how the core engine
# behaves, so a successful build is the whole assertion.  The smoke driver
# links the C library, which these do not change.
# ---------------------------------------------------------------------------
SWEEP_BUILD='
cxx|--enable-cxx
stl|--enable-cxx --enable-stl
dbm|--enable-dbm
compat185|--enable-compat185
'

# ---------------------------------------------------------------------------
# SWEEP_PAIRS -- KNOWN-INTERACTING option pairs, built and smoked together.
#
# One-at-a-time sweeping cannot see interactions, and 2^54 is not a target.  This
# is a small NAMED list where each entry carries a reason it is worth a leg --
# not a random sample.  test/TESTING-PROGRAM.md names the first
# four; each reason below says what specifically could break that neither option
# alone exercises.
#
# Format: <pair-name>|<configure-args>|<reason>
#
# The reason is not decoration: it is what a future reader needs in order to
# decide whether a failing leg matters or the pair should be dropped.  A pair
# with no reason is a random sample, and a random sample of a 2^54 space tells
# you nothing.
# ---------------------------------------------------------------------------
SWEEP_PAIRS='
diagnostic-x-o_direct|--enable-diagnostic --enable-o_direct|DIAGNOSTIC adds assertions about buffer alignment and page state that only O_DIRECT can violate -- P2 and P3 were both unaligned-buffer defects reachable only under o_direct, and diagnostic is the build most likely to catch the next one at its source rather than as an EINVAL far away.
smallbuild-x-statistics|--enable-smallbuild --disable-statistics|smallbuild already implies --disable-statistics among others, so requesting both is the case where two mechanisms disable the same code; if either forgets the other has acted, the result is a double-disable that removes a symbol the remaining code still references.
mutexsupport-x-atomicsupport|--disable-mutexsupport --disable-atomicsupport|XFAIL:inherits the pre-existing single-leg failure. U7 showed --disable-mutexsupport had NEVER built; it BUILDS now, but the smoke driver opens its environment with DB_INIT_LOCK|DB_INIT_TXN, which a library built without mutex support cannot provide -- it correctly answers "library build did not include support for locking" and panics. So the leg fails for a TEST-DESIGN reason, not a defect, and it fails identically with --disable-mutexsupport ALONE (verified on pristine master: same SMOKE pass=0 fail=1). The pair therefore adds no information until config_smoke.c learns to request a mutex-free environment; kept, XFAILed, and named so the fix has somewhere to land.
replication-x-cryptography|--disable-replication --with-cryptography=no|the replication message path is the largest caller of the crypto/HMAC layer, so removing both at once is where a #ifdef that guards a call on one option but its declaration on the other would surface as an unresolved symbol.
diagnostic-x-smallbuild|--enable-diagnostic --enable-smallbuild|smallbuild strips code that DIAGNOSTIC assertions refer to (statistics counters, verify paths), so this pair is where an assertion can outlive the field it asserts about -- a compile failure that neither option alone produces.
'

# ---------------------------------------------------------------------------
# EXCLUDED -- with the reason, which is the only acceptable form of a skip.
# Format: <option-name>|<reason>
#
# Everything here is excluded for an ENVIRONMENTAL or STRUCTURAL reason, not
# because it is inconvenient to test.  If one of these becomes buildable, move
# it into a sweep list; do not add new entries without a concrete reason.
#
# TWELVE of these were MEASURED into this list, not assumed.  They were swept on
# a c6id.24xlarge (96 vCPU, Debian 12, gcc 12.2, clang, tcl8.6, liburing) and
# configure or make REFUSED them with the message quoted below.  Five options are
# RETIRED -- configure errors out on purpose -- which is not a test gap and must
# not be reported as one.
# ---------------------------------------------------------------------------
EXCLUDED='
java|needs a JDK plus the Java build chain; not installed on the benchmark box
jdbc|needs a JDK and an external SQLite JDBC source tree (--with-jdbc=DIR)
tcl|needs tclsh + tcl headers; the tcl BINDINGS are already covered by the tcl tier in ci.yml
mingw|cross-compiles for Windows; needs a mingw toolchain and cannot be run here
dtrace|needs a DTrace-capable kernel (Solaris/macOS); Linux has no dtrace provider here
systemtap|needs systemtap headers/probes (sys/sdt.h) and a stap-capable kernel
rpc|RETIRED: configure errors out by design -- RPC support has been removed from Berkeley DB
pthread_self|RETIRED no-op: configure only warns --enable-pthread_self is now always enabled
pthread_api|RETIRED no-op: configure only warns --enable-pthread_api is now always enabled
mutexalign|RETIRED: configure errors --with-mutexalign no longer supported, use DbEnv::mutex_set_align
bigfile|RETIRED: configure errors --enable-bigfile no longer supported, use --enable-largefile
uimutexes|needs the Solaris/UI mutex interfaces; configure errors unable to find UI mutex interfaces on Linux
perfmon_statistics|requires --enable-dtrace which is itself excluded; configure errors Enabling perfmon statistics requires --enable-dtrace
compile-commands|requires the bear tool; configure errors --enable-compile-commands requires bear to be installed
test|requires --enable-tcl which is excluded; configure errors --enable-test requires --enable-tcl
dump185|needs the SYSTEM DB 1.85/1.86 db.h and object library, documented as an external prerequisite in dist/Makefile.in
sql|needs the bundled SQLite source at lang/sql/sqlite/, absent from this tree (configure cannot find lang/sql/sqlite/configure)
sql_compat|same missing lang/sql/sqlite/ tree as --enable-sql
sql_codegen|same missing lang/sql/sqlite/ tree as --enable-sql
amalgamation|same missing lang/sql/sqlite/ tree as --enable-sql
readline|only reachable through --enable-sql, which cannot configure here
'

# ===========================================================================

# opt_names LIST -- the option names in one of the lists above.
opt_names() {
	printf '%s\n' "$1" | sed -n 's/^\([a-z0-9_-]*\)|.*/\1/p'
}

# declared_options -- every option name options.m4 declares.
declared_options() {
	sed -n 's/.*AC_ARG_ENABLE(\[*\([a-z0-9_-]*\).*/\1/p;
		s/.*AC_ARG_WITH(\[*\([a-z0-9_-]*\).*/\1/p' "$OPTIONS_M4" |
	    sort -u
}

# ---------------------------------------------------------------------------
# THE COMPLETENESS GATE.  Every declared option must be in the sweep or on the
# exclusion list.  Both directions are checked: a stale entry naming an option
# that no longer exists is also a failure, because it silently reduces
# coverage while looking like coverage.
# ---------------------------------------------------------------------------
check_complete() {
	[ -f "$OPTIONS_M4" ] || {
		echo "option_sweep: no $OPTIONS_M4" >&2
		return 2
	}
	tmp_d=$(mktemp) || return 2
	tmp_p=$(mktemp) || return 2
	trap 'rm -f "$tmp_d" "$tmp_p"' 0 1 2 3 13 15

	declared_options > "$tmp_d"
	{ opt_names "$SWEEP_SMOKE"; opt_names "$SWEEP_BUILD";
	  opt_names "$EXCLUDED"; } | sort -u > "$tmp_p"

	nd=$(wc -l < "$tmp_d" | tr -d ' ')
	ns=$(opt_names "$SWEEP_SMOKE" | sort -u | wc -l | tr -d ' ')
	nb=$(opt_names "$SWEEP_BUILD" | sort -u | wc -l | tr -d ' ')
	nx=$(opt_names "$EXCLUDED" | sort -u | wc -l | tr -d ' ')
	echo "== configure-option completeness gate =="
	echo "declared in dist/aclocal/options.m4: $nd"
	echo "swept with a smoke run:              $ns"
	echo "swept build-only:                    $nb"
	echo "excluded with a reason:              $nx"
	echo

	crc=0
	miss=$(comm -23 "$tmp_d" "$tmp_p")
	if [ -n "$miss" ]; then
		echo "UNTESTED OPTIONS -- each is in neither a sweep list nor the"
		echo "exclusion list.  Add it to SWEEP_SMOKE (it changes runtime"
		echo "behaviour), SWEEP_BUILD (it only changes what is built), or"
		echo "EXCLUDED with a concrete reason:"
		printf '    %s\n' $miss
		crc=1
	fi
	stale=$(comm -13 "$tmp_d" "$tmp_p")
	if [ -n "$stale" ]; then
		echo "STALE ENTRIES -- named in a list but no longer declared by"
		echo "options.m4.  Remove them: an entry for an option that does"
		echo "not exist looks like coverage and is not:"
		printf '    %s\n' $stale
		crc=1
	fi
	if [ "$crc" = 0 ]; then
		echo "completeness gate: OK -- every configure option is swept" \
		    "or excluded with a reason."
	else
		echo
		echo "completeness gate: FAILED."
	fi
	return "$crc"
}

if [ "$MODE" = check ]; then
	check_complete
	exit $?
fi

if [ "$MODE" = list ]; then
	check_complete || exit 1
	echo
	echo "== smoke legs (build + run) =="
	printf '%s\n' "$SWEEP_SMOKE" | sed -n 's/^\([a-z0-9_-]*\)|\(.*\)/    \1\t\2/p'
	echo "== build-only legs =="
	printf '%s\n' "$SWEEP_BUILD" | sed -n 's/^\([a-z0-9_-]*\)|\(.*\)/    \1\t\2/p'
	echo "== excluded =="
	printf '%s\n' "$EXCLUDED" | sed -n 's/^\([a-z0-9_-]*\)|\(.*\)/    \1\t\2/p'
	echo
	echo "KNOWN-INTERACTING PAIRS (extra legs; not part of the completeness"
	echo "accounting -- every option is still in exactly one list above):"
	printf '%s\n' "$SWEEP_PAIRS" |
	    sed -n 's/^\([a-z0-9_-]*\)|\([^|]*\)|\(.*\)/    \1\n        args: \2\n        why:  \3/p'
	exit 0
fi

# ---------------------------------------------------------------------------
# THE SWEEP ITSELF.
# ---------------------------------------------------------------------------
. "$TOP/test/harness.sh"
hi_init config "$TOP/test"

# The completeness gate runs FIRST and is itself a recorded verdict, so a sweep
# that ran every leg against an incomplete list still fails.
if check_complete; then
	hi_emit complete pass
else
	hi_emit complete fail
	sweep_rc=1
fi
sweep_rc=${sweep_rc:-0}
echo

mkdir -p "$SWEEP_DIR" || exit 2
SUMMARY="$SWEEP_DIR/summary.tsv"
printf 'option\tkind\tconfigure\tconfig_rc\tbuild_rc\tsmoke\tdetail\n' > "$SUMMARY"

# run_leg NAME KIND ARGS... -- configure + build (+ smoke when KIND=smoke).
run_leg() {
	name=$1; kind=$2; shift 2
	cargs="$*"
	case "$ONLY" in
	'') ;;
	*) case "$name" in *$ONLY*) ;; *) return 0 ;; esac ;;
	esac

	# The leg name doubles as the build DIRECTORY name, and it must not
	# contain a colon: ':' is the dynamic loader's path separator, so a
	# RUNPATH of ".../pair:foo/.libs" is parsed as two nonexistent
	# directories and the smoke driver dies with "cannot open shared object
	# file" even though the library is right there.  Measured: all five pair
	# legs reported exit 127 for exactly this reason, which looks like five
	# broken pairs and is one broken directory name.
	#
	# Verdict names keep the colon (they are the manifest's identifiers);
	# only the path is sanitised.
	dirname_safe=$(printf '%s' "$name" | tr ':' '-')
	d="$SWEEP_DIR/$dirname_safe"
	if [ -d "$d" ]; then
		find "$d" -mindepth 1 -delete
	elif ! mkdir -p "$d"; then
		echo "--- $name: HARNESS ERROR (cannot create $d)"
		hi_emit "$name" fail
		sweep_rc=1
		return
	fi

	echo "=== $name ($kind): configure $cargs"
	crc=0
	# shellcheck disable=SC2086
	( cd "$d" && "$TOP/dist/configure" $cargs ) >"$d/configure.log" 2>&1 ||
	    crc=$?
	if [ "$crc" != 0 ]; then
		detail=$(tail -3 "$d/configure.log" | tr '\n' ' ')
		echo "--- $name: FAIL (configure exit $crc)"
		echo "    $detail"
		printf '%s\t%s\t%s\t%d\t-\t-\t%s\n' \
		    "$name" "$kind" "$cargs" "$crc" "$detail" >> "$SUMMARY"
		hi_emit "$name" fail
		sweep_rc=1
		return
	fi

	echo "=== $name: make -j$JOBS"
	brc=0
	( cd "$d" && make -j"$JOBS" ) >"$d/build.log" 2>&1 || brc=$?
	if [ "$brc" != 0 ]; then
		detail=$(grep -m3 -E 'error:|Error [0-9]' "$d/build.log" |
		    tr '\n' ' ')
		echo "--- $name: FAIL (build exit $brc)"
		echo "    $detail"
		printf '%s\t%s\t%s\t0\t%d\t-\t%s\n' \
		    "$name" "$kind" "$cargs" "$brc" "$detail" >> "$SUMMARY"
		hi_emit "$name" fail
		sweep_rc=1
		return
	fi

	# smoke and smoke-xfail both RUN the smoke driver; only the grading of a
	# failure differs (see the bottom of this function).  Anything else is
	# build-only.
	if [ "$kind" != smoke ] && [ "$kind" != smoke-xfail ]; then
		echo "--- $name: PASS (build only)"
		printf '%s\t%s\t%s\t0\t0\tn/a\tbuild ok\n' \
		    "$name" "$kind" "$cargs" >> "$SUMMARY"
		hi_emit "$name" pass
		return
	fi

	# ------------------------------------------------------------------
	# The runtime half.  A build-only result here would have reported
	# --enable-o_direct green, which is the entire reason this exists.
	# ------------------------------------------------------------------
	lib=$(ls "$d"/.libs/libdb-*.so 2>/dev/null | head -1)
	if [ -z "$lib" ]; then
		echo "--- $name: FAIL (no shared library produced, cannot smoke)"
		printf '%s\t%s\t%s\t0\t0\tno-lib\tno .libs/libdb-*.so\n' \
		    "$name" "$kind" "$cargs" >> "$SUMMARY"
		hi_emit "$name" fail
		sweep_rc=1
		return
	fi
	src="$HERE/config_smoke.c"
	if ! ${CC:-cc} -g -O1 -I"$d" -I"$TOP/src" "$src" "$lib" \
	    -lpthread -Wl,-rpath,"$d/.libs" -o "$d/config_smoke" \
	    >"$d/smokebuild.log" 2>&1; then
		detail=$(grep -m2 'error:' "$d/smokebuild.log" | tr '\n' ' ')
		echo "--- $name: FAIL (smoke driver did not compile) $detail"
		printf '%s\t%s\t%s\t0\t0\tcc-fail\t%s\n' \
		    "$name" "$kind" "$cargs" "$detail" >> "$SUMMARY"
		hi_emit "$name" fail
		sweep_rc=1
		return
	fi

	rundir="$d/smokerun"
	mkdir -p "$rundir/TESTDIR_config_smoke"
	find "$rundir/TESTDIR_config_smoke" -mindepth 1 -delete
	srcode=0
	( cd "$rundir" && timeout 300 "$d/config_smoke" ) \
	    >"$d/smoke.log" 2>&1 || srcode=$?
	sed -n 's/^/    /p' "$d/smoke.log"

	# The verdict is the SMOKE line, not the exit status.  A driver that
	# exits 0 having printed nothing (a library that dies before main, an
	# empty run) must not read as a pass.
	sline=$(grep '^SMOKE ' "$d/smoke.log" | tail -1)
	if [ -z "$sline" ]; then
		echo "--- $name: FAIL (exit $srcode, NO SMOKE LINE -- the" \
		    "driver produced no verdict, so nothing was measured)"
		printf '%s\t%s\t%s\t0\t0\tno-verdict\texit %d\n' \
		    "$name" "$kind" "$cargs" "$srcode" >> "$SUMMARY"
		hi_emit "$name" fail
		sweep_rc=1
		return
	fi
	sp=$(printf '%s\n' "$sline" | sed -n 's/.*pass=\([0-9]*\).*/\1/p')
	sf=$(printf '%s\n' "$sline" | sed -n 's/.*fail=\([0-9]*\).*/\1/p')
	sk=$(printf '%s\n' "$sline" | sed -n 's/.*skip=\([0-9]*\).*/\1/p')

	# An "all skipped" run is the vacuous shape: every check declining to
	# run looks exactly like every check passing if only fail= is read.
	if [ "${sp:-0}" -lt 4 ] || [ "${sf:-1}" != 0 ]; then
		# The failure shapes: too few checks passed (an "all skipped"
		# run looks like a clean one if only fail= is read), or a check
		# actually failed.
		if [ "${sp:-0}" -lt 4 ]; then
			why="fewer than 4 checks PASSED, so the smoke run proved almost nothing"
			tag=vacuous
		else
			why="a check FAILED"
			tag=fail
		fi
		if [ "$kind" = smoke-xfail ]; then
			# A recorded, understood expectation.  It ran, it failed
			# as stated, and that does not fail the sweep -- but it
			# is reported as XFAIL rather than quietly skipped, so it
			# stays visible.
			echo "--- $name: XFAIL ($sline -- $why; expected, see" \
			    "the stated reason)"
			printf '%s\t%s\t%s\t0\t0\txfail\t%s\n' \
			    "$name" "$kind" "$cargs" "$sline" >> "$SUMMARY"
			hi_emit "$name" pass
		else
			echo "--- $name: FAIL ($sline -- $why)"
			printf '%s\t%s\t%s\t0\t0\t%s\t%s\n' \
			    "$name" "$kind" "$cargs" "$tag" "$sline" >> "$SUMMARY"
			hi_emit "$name" fail
			sweep_rc=1
		fi
	elif [ "$kind" = smoke-xfail ]; then
		# It PASSED while recorded as expected-to-fail.  That is news,
		# and it must not pass silently: either the underlying problem
		# was fixed (drop the XFAIL: prefix) or the leg stopped
		# asserting anything.  Both need a human, so both fail.
		echo "--- $name: FAIL (UNEXPECTED PASS: smoke pass=$sp skip=$sk," \
		    "but this leg is recorded XFAIL.  Either the stated problem" \
		    "is fixed -- remove the XFAIL: prefix from its reason in" \
		    "SWEEP_PAIRS -- or the leg stopped checking anything.)"
		printf '%s\t%s\t%s\t0\t0\tunexpected-pass\tpass=%s skip=%s\n' \
		    "$name" "$kind" "$cargs" "$sp" "$sk" >> "$SUMMARY"
		hi_emit "$name" fail
		sweep_rc=1
	else
		echo "--- $name: PASS (smoke pass=$sp skip=$sk)"
		printf '%s\t%s\t%s\t0\t0\tpass\tpass=%s skip=%s\n' \
		    "$name" "$kind" "$cargs" "$sp" "$sk" >> "$SUMMARY"
		hi_emit "$name" pass
	fi
}

# Walk both lists.  IFS games rather than arrays: this has to run in POSIX sh.
walk() {
	kind=$1; list=$2
	printf '%s\n' "$list" | while IFS='|' read -r nm args; do
		[ -n "${nm:-}" ] || continue
		# shellcheck disable=SC2086
		run_leg "$nm" "$kind" $args
	done
}

# NB: `walk` runs in a subshell (the pipe), so sweep_rc set inside is lost.
# Grade from the SUMMARY file afterwards instead -- the file is the evidence,
# and reading the evidence back is also what catches a leg that never wrote a
# row at all.
walk smoke "$SWEEP_SMOKE"
walk build "$SWEEP_BUILD"

# ---------------------------------------------------------------------------
# The PAIR legs.  Same machinery, a third field to ignore (the reason).
#
# Deliberately AFTER the single-option legs: a pair failure is only interesting
# once both its options are known to build alone, and running them first means
# the summary reads in that order.
#
# Pairs are NOT part of the completeness accounting.  Every configure option is
# still in exactly one of SWEEP_SMOKE / SWEEP_BUILD / EXCLUDED, and adding a pair
# does not remove an option from that reckoning -- so --check-complete is
# untouched by this list, which is what keeps the "an unlisted option fails the
# gate" property intact.
# ---------------------------------------------------------------------------
walk_pairs() {
	printf '%s\n' "$SWEEP_PAIRS" | while IFS='|' read -r nm args reason; do
		[ -n "${nm:-}" ] || continue
		# A pair with no stated reason is a random sample.  Refuse it:
		# the whole justification for this list is that each entry earns
		# its minutes, and an unjustified entry silently erodes that.
		if [ -z "${reason:-}" ]; then
			echo "--- pair:$nm: FAIL (no reason given -- a pair" \
			    "without a stated interaction is a random sample" \
			    "of a 2^54 space, which proves nothing)"
			hi_emit "pair:$nm" fail
			continue
		fi
		echo
		echo "### pair $nm"
		echo "    why: $reason"
		# A reason beginning "XFAIL:" records a leg known to fail for a
		# stated, understood reason.  It still RUNS -- the point is to
		# notice when it starts passing -- but its failure does not fail
		# the sweep, and its verdict says which it was.  A bare skip
		# would hide it; a hard failure would make the sweep permanently
		# red and train everyone to ignore it.
		case "$reason" in
		XFAIL:*)
			echo "    (XFAIL: expected to fail; see the reason above)"
			# shellcheck disable=SC2086
			run_leg "pair:$nm" smoke-xfail $args ;;
		*)
			# shellcheck disable=SC2086
			run_leg "pair:$nm" smoke $args ;;
		esac
	done
}
walk_pairs

echo
echo "=== sweep summary ($SUMMARY)"
column -t -s"$(printf '\t')" "$SUMMARY" 2>/dev/null || cat "$SUMMARY"

nrows=$(($(wc -l < "$SUMMARY") - 1))
# The bad tags.  "unexpected-pass" is one of them: an XFAIL leg that started
# passing needs a human either way, and leaving it off this list is how the
# verdict printed by run_leg fails to reach the exit status.  Measured -- the
# teeth probe for the XFAIL arm reported UNEXPECTED PASS on the leg and the
# sweep still exited 0, because walk_pairs runs in a subshell (see the note on
# walk) and the SUMMARY file is the only channel that survives.
#
# "xfail" is NOT bad: it is a recorded expectation that held.
nbad=$(awk -F'\t' 'NR > 1 && ($4 != 0 || $5 != 0 || $6 == "fail" ||
    $6 == "vacuous" || $6 == "no-verdict" || $6 == "cc-fail" ||
    $6 == "no-lib" || $6 == "unexpected-pass") { n++ }
    END { print n + 0 }' "$SUMMARY")
# Planned legs: the single options, PLUS the pairs (which are extra legs, not
# options).  Counting only the options would make a pair that never ran invisible
# to the "a leg that wrote no row did not run" check below.
nexpect=$( { opt_names "$SWEEP_SMOKE"; opt_names "$SWEEP_BUILD"; } |
    sort -u | wc -l | tr -d ' ')
npairs=$(printf '%s\n' "$SWEEP_PAIRS" | sed -n 's/^\([a-z0-9_-]*\)|.*/\1/p' |
    sort -u | wc -l | tr -d ' ')
nexpect=$((nexpect + npairs))

echo
echo "legs run: $nrows   failing: $nbad   planned: $nexpect ($npairs of them pairs)"
if [ -z "$ONLY" ] && [ "$nrows" -ne "$nexpect" ]; then
	echo "SWEEP INCOMPLETE: $nrows legs recorded but $nexpect were planned."
	echo "A leg that wrote no row did not run; that is not a pass."
	sweep_rc=1
fi
[ "$nbad" = 0 ] || sweep_rc=1

if [ "$sweep_rc" = 0 ]; then
	echo "option sweep: OK"
else
	echo "option sweep: FAILED -- see the rows above."
fi
exit "$sweep_rc"
