#!/bin/sh

TOP=`dirname $0`
TOP=`cd $TOP && /bin/pwd`
SQLITE=$TOP/../../lang/sql/sqlite

BDB_TESTS_PASSING="\
	aggerror.test alter.test alter3.test alter4.test altermalloc.test
	analyze.test analyze2.test analyze3.test async.test async2.test
	async3.test async4.test async5.test auth.test auth2.test auth3.test
	autoinc.test autovacuum.test autovacuum_ioerr2.test backup.test
	backup2.test backup_malloc.test badutf.test bdb_deadlock.test
	bdb_dupset.test bdb_logsize.test bdb_exclusive.test bdb_inmem_memleak.test
	bdb_mvcc.test bdb_persistent_pragma.test bdb_pragmas.test
	bdb_rdonly.test bdb_replication.test bdb_sequence.test between.test
	bigrow.test bind.test bindxfer.test bitvec.test blob.test
	boundary1.test boundary2.test boundary3.test boundary4.test
	capi3d.test cast.test check.test coalesce.test collate1.test
	collate2.test collate3.test collate4.test collate5.test collate6.test
	collate7.test collate8.test collate9.test collateA.test colmeta.test
	colname.test count.test createtab.test cse.test date.test default.test
	delete.test delete2.test delete3.test descidx1.test descidx2.test
	descidx3.test distinctagg.test enc.test enc3.test exec.test expr.test
	fkey1.test fkey2.test fkey3.test fkey_malloc.test func.test func2.test
	fuzz2.test hook.test icu.test in.test in2.test in3.test in4.test
	incrblob.test incrblob2.test incrblob_err.test incrvacuum.test
	incrvacuum2.test incrvacuum_ioerr.test index.test index2.test
	index3.test indexedby.test insert.test insert2.test insert3.test
	insert4.test insert5.test intarray.test interrupt.test intpkey.test
	join.test join2.test join3.test join4.test join5.test join6.test
	keyword1.test lastinsert.test laststmtchanges.test like.test like2.test
	limit.test loadext.test loadext2.test lookaside.test malloc4.test
	malloc6.test malloc7.test malloc8.test malloc9.test mallocA.test
	mallocB.test mallocD.test mallocE.test mallocF.test mallocG.test
	mallocH.test mallocJ.test manydb.test memdb.test memsubsys2.test
	minmax.test minmax2.test minmax3.test misc2.test misc3.test misc4.test
	misc6.test misuse.test nan.test notify1.test notify2.test notnull.test
	null.test openv2.test pagesize.test printf.test ptrchng.test quote.test
	randexpr1.test rdonly.test reindex.test rollback.test rowhash.test
	rowid.test rtree.test savepoint2.test savepoint3.test savepoint5.test
	schema.test schema2.test securedel.test select1.test select2.test
	select3.test select4.test select5.test select6.test select7.test
	select8.test select9.test selectA.test selectB.test selectC.test
	server1.test shared2.test shared3.test shared4.test shared6.test
	shared7.test sidedelete.test sort.test sqllimits1.test subquery.test
	subselect.test substr.test table.test tempdb.test temptable.test
	temptrigger.test thread001.test thread003.test thread004.test
	thread005.test thread1.test thread2.test tokenize.test trace.test
	trans.test trans2.test trans3.test trigger1.test trigger2.test
	trigger3.test trigger4.test trigger5.test trigger6.test trigger7.test
	trigger8.test trigger9.test triggerA.test triggerB.test triggerC.test
	triggerD.test types.test types2.test types3.test unique.test
	update.test utf16align.test vacuum.test vacuum2.test vacuum4.test
	view.test vtab1.test vtab2.test vtab3.test vtab4.test vtab5.test
	vtab6.test vtab7.test vtab8.test vtab9.test vtabA.test vtabB.test
	vtabC.test vtabD.test vtab_alter.test vtab_err.test vtab_shared.test
	where.test where2.test where3.test where4.test where5.test where6.test
	where7.test where8.test where8m.test where9.test whereA.test
	whereB.test wherelimit.test zeroblob.test"

BDB_TESTS_ERRORS="eval.test progress.test"

BDB_TESTS_HANGS=""

BDB_TESTS_ALL="$BDB_TESTS_PASSING $BDB_TESTS_ERRORS $BDB_TESTS_HANGS"

# Define extension test suites (fts3, rtree, etc)
BDB_FTS3_TESTS="fts3.test fts3malloc.test fts3rnd.test e_fts3.test"
BDB_RTREE_TESTS="rtree1.test rtree2.test rtree3.test rtree4.test rtree5.test rtree6.test"

# Clean up any old log
rm -f test.log

exe_suffix=""
cygwin=`uname | grep -i "cygwin"`
if [ "$cygwin" != "" ]; then
	exe_suffix=".exe"
fi

# kill tests if still running after 30 minutes
TIMEOUT=1800
alarm() { perl -e 'alarm shift; exec @ARGV' "$@"; }

# number of threads
PARALLEL=0 ; NPROCESS=1
TESTFIXTURE=./testfixture$exe_suffix

case "$1" in
-j)	PARALLEL=1 ; NPROCESS="$2" ; TESTFIXTURE="../$TESTFIXTURE"
	shift; shift;;
esac

case "$1" in
passing) TEST_CASES="$BDB_TESTS_PASSING";;
errors)  TEST_CASES="$BDB_TESTS_ERRORS";;
hangs)   TEST_CASES="$BDB_TESTS_HANGS";;
fts3)	 TEST_CASES="$BDB_FTS3_TESTS"
	 TIMEOUT=7200
	 ;;
rtree)	 TEST_CASES="$BDB_RTREE_TESTS"
	 TIMEOUT=7200
	 ;;
*)       TEST_CASES="$BDB_TESTS_ALL";;
esac

trap "kill 0" INT TERM

PROCESS=0
LOG="current_test.log"
while [ $PROCESS -lt $NPROCESS ] ; do
	(if [ $PARALLEL -ne 0 ] ; then
		rm -rf TEST$PROCESS && mkdir TEST$PROCESS && cd TEST$PROCESS
	fi
	i=0
	for t in $TEST_CASES ; do
		i=`expr $i + 1`
		[ `expr $i % $NPROCESS` -ne $PROCESS ] && continue

		case "$t" in
		bdb*) tpath=$TOP/$t ;;
		*) tpath=$SQLITE/test/$t ;;
		esac

		# Adjust path for rtree tests
		if [ "$TEST_CASES" = "$BDB_RTREE_TESTS" ]; then
			tpath=$SQLITE/ext/rtree/$t
		fi

		alarm $TIMEOUT $TESTFIXTURE $tpath > $LOG 2>&1

		# Detect test result
		result=`grep "errors out of" $LOG || echo "failed"`

		# Detect memory leak
		is_leaked=`grep "Unfreed memory:" $LOG || echo "no memleaks"`

		# Save log and clean environment
		cat $LOG >> test.log
		rm -rf *.db*
		rm -f $LOG

		# Output summary
		echo "$t: ${result}, ${is_leaked}"
	done) &
	PROCESS=`expr $PROCESS + 1`
done

wait
trap - INT TERM
