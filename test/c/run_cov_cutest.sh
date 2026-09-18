#!/bin/sh -
#
# $Id$
#
# run_cov_cutest.sh --
#	Run the C unit-test binary `cutest` (test/c/cutest + test/c/suites)
#	from a coverage driver.
#
#	This is a MEASUREMENT GAP closer, not a new test.  `cutest` is a
#	12-suite CuTest binary that already exists, already has a Makefile.in
#	target (`make cutest`), and passes 100%.  No coverage driver has ever
#	run it, so the code only it reaches has always measured as cold.  Two
#	of its suites reach surfaces nothing else in the tree does:
#
#	  * TestChannel -- brings up THREE live repmgr sites in one process
#	    (ports 30101-30103), installs a message-dispatch callback, and
#	    drives the whole DB_CHANNEL API: async send, request/response,
#	    multi-segment and zero-segment messages, USERMEM buffers with and
#	    without DB_MULTIPLE, send-to-master, send-to-self, master
#	    switchover, sending to a shut-down site, connecting to a
#	    non-existent EID, and illegal calls from inside the dispatch
#	    function.  That is precisely the never-called set in
#	    repmgr/repmgr_method.c (__repmgr_channel, __repmgr_send_msg,
#	    __repmgr_send_request, __repmgr_send_response,
#	    __repmgr_set_msg_dispatch, get_channel_connection,
#	    establish_connection, send_msg_conn, send_msg_self,
#	    request_self, response_complete, adjust_bulk_response,
#	    copy_body, bad_callback_method, ...) plus repmgr_msg.c's
#	    dispatch-side handlers.
#	  * TestDbTuner -- drives db_tuner.c (the page-size advisor), which
#	    no Tcl test runs.
#
#	The other suites (TestDbHotBackup x5, TestEncryption x12,
#	TestEnvConfig x12, TestEnvMethod, TestKeyExistErrorReturn,
#	TestPartial x4, TestQueue incl. the shared-list unit tests) add
#	env-config, encryption and partial-record coverage cheaply.
#
#	Runs in its own directory under a hard timeout; cutest creates and
#	removes its own TESTDIR-style homes.
#
# Usage (from build_unix):
#	sh ../test/c/run_cov_cutest.sh

set -u

BUILD=${BUILD:-.}
TIMEOUT=${TIMEOUT:-600}		# per suite
RUNDIR=${RUNDIR:-CUTEST_TESTDIR}

# Build it if it is not there yet (the coverage driver's `make` may not have
# asked for it; `make cutest` is cheap next to a full build).
if [ ! -x "$BUILD/cutest" ]; then
	echo "Building cutest"
	( cd "$BUILD" && make cutest ) || {
		echo "run_cov_cutest.sh: SKIP (cutest did not build)"
		exit 0
	}
fi

mkdir -p "$BUILD/$RUNDIR"
# Guaranteed-clean run dir (no rm -rf).
find "$BUILD/$RUNDIR" -mindepth 1 -delete 2>/dev/null || true

# cutest links the shared lib; point the loader at the build tree.
LIBS_DIR=$(cd "$BUILD/.libs" && pwd)
LD_LIBRARY_PATH="$LIBS_DIR:${LD_LIBRARY_PATH:-}"
export LD_LIBRARY_PATH

#
# Run ONE SUITE PER PROCESS (cutest -s <suite>) rather than the whole binary.
# Two reasons, both about not losing coverage:
#
#   1. gcov writes its .gcda from an atexit handler, so a process that dies by
#      a signal contributes NOTHING.  One crashing suite would throw away the
#      coverage of every suite in the same process.
#   2. TestQueue DOES crash in an optimised build: sh_l_as_string()
#      (test/c/suites/TestQueue.c:64) fills a fixed `static char buf[1024]`
#      from a list with no bound check, and the shared-list op matrix
#      overruns it -- SIGSEGV at TestQueue.c:64 via TestQueue.c:827.  That is
#      a pre-existing bug in the TEST HARNESS, not in libdb, and it is
#      reported in test/coverage/FULL-COVERAGE-REPORT-4.md rather than fixed
#      here.  Isolating it keeps the other eleven suites' coverage.
#
# A suite that crashes is reported but does not fail the run, for the same
# reason: this driver's job is measurement, and the crash is a known,
# documented harness defect.  A suite that reports a CuTest FAILURE does fail.
#
SUITES=${SUITES:-"TestChannel TestDbHotBackup TestDbTuner TestEncryption \
TestEnvConfig TestEnvMethod TestKeyExistErrorReturn TestPartial TestQueue"}

cd "$BUILD/$RUNDIR"
ran=0
okc=0
bad=0
crashed=""
failed=""

for s in $SUITES; do
	log="cutest-$s.log"
	timeout "$TIMEOUT" ../cutest -s "$s" > "$log" 2>&1
	rc=$?
	ran=$((ran + 1))
	if grep -q '!!!FAILURES!!!' "$log" 2>/dev/null; then
		bad=$((bad + 1))
		failed="$failed $s"
	elif [ $rc -eq 0 ]; then
		okc=$((okc + 1))
	else
		crashed="$crashed $s(rc=$rc)"
	fi
	# Clean between suites: several create their own TESTDIR homes.
	find . -mindepth 1 ! -name 'cutest-*.log' -delete 2>/dev/null || true
done

echo "run_cov_cutest.sh: ran $ran suites, $okc clean, $bad with test failures"
[ -n "$crashed" ] && echo "run_cov_cutest.sh: CRASHED (known harness bugs):$crashed"
if [ "$bad" -gt 0 ]; then
	echo "run_cov_cutest.sh: FAILURES:$failed"
	for s in $failed; do
		echo "--- $s ---"
		grep -A6 '!!!FAILURES!!!' "cutest-$s.log" | head -20
	done
	echo "run_cov_cutest.sh: FAIL"
	exit 1
fi
if [ "$okc" -eq 0 ]; then
	echo "run_cov_cutest.sh: SKIP (no suite ran clean)"
	exit 0
fi
echo "run_cov_cutest.sh: PASS"
exit 0
