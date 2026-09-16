#!/bin/sh
# test/tcl/run_targeted.sh -- run the per-push targeted TCL subset and emit
# verdicts for the test-execution manifest gate (test/MANIFEST).
#
# Replaces the inline heredoc in ci.yml.  The test list comes from the manifest,
# not from a copy in the workflow: trap 3 was `ssi` missing from $subs in
# testparams.tcl, so the ssi procs were never sourced and the job exited 0
# having tested nothing.  Here, a test that fails to source is a FAIL verdict
# rather than silence.
#
# Usage (from build_unix):  sh ../test/tcl/run_targeted.sh
# Env: TCLSH (default tclsh), TCL_TIMEOUT (default 3600)

set -u

HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
. "$HERE/../harness.sh"
hi_init tcl "$HERE/.."

MANIFEST=${LIBDB_MANIFEST:-"$HERE/../MANIFEST"}
TCLSH=${TCLSH:-tclsh}
TCL_TIMEOUT=${TCL_TIMEOUT:-3600}

tests=$(awk '$1 == "tcl" && $1 !~ /^#/ { print $2 }' "$MANIFEST")
[ -n "$tests" ] || { echo "run_targeted.sh: no tcl entries in $MANIFEST" >&2; exit 1; }

script=$(mktemp /tmp/libdb-tcl.XXXXXX) || exit 1
out=$(mktemp /tmp/libdb-tcl-out.XXXXXX) || exit 1
trap 'rm -f "$script" "$out"' 0 1 2 3 13 15

# One catch per test, and a VERDICT line per test either way.  The old inline
# script exited on the first failure, so the tests after it produced neither a
# pass nor a fail -- indistinguishable from never having been listed.
{
	echo "source $HERE/test.tcl"
	for t in $tests; do
		a=''
		# test001..testNNN take an access method argument.
		case $t in test[0-9]*) a='btree' ;; esac
		cat <<TCL
if {[catch {source $HERE/$t.tcl} res]} {
	puts "VERDICT $t fail (source: \$res)"
} elseif {[catch {eval $t $a} res]} {
	puts "VERDICT $t fail (\$res)"
} else {
	puts "VERDICT $t pass"
}
TCL
	done
} > "$script"

rc=0
timeout "$TCL_TIMEOUT" "$TCLSH" "$script" > "$out" 2>&1 || rc=$?
cat "$out"

# Translate. A test whose VERDICT line never appeared (the tclsh process died
# mid-run) gets no RESULT line, and the manifest gate then reports it MISSING --
# which is the honest answer: it did not finish, so nothing was proved.
awk '$1 == "VERDICT" { print $2, $3 }' "$out" | while read -r name verdict; do
	hi_emit "$name" "$verdict"
	[ "$verdict" = fail ] && echo "run_targeted.sh: $name FAILED"
done

# A fail verdict must fail the script even though tclsh itself exited 0.
if grep -q '^VERDICT .* fail' "$out"; then
	rc=1
fi
exit "$rc"
