#!/bin/sh

TOP=`dirname $0`
SQLITE=$TOP/../sqlite

BDB_TESTS_PASSING="aggerror.test alter.test alter3.test \
	alter4.test altermalloc.test async.test async2.test async3.test \
	async4.test async5.test autoinc.test badutf.test bdb_logsize.test \
	between.test bigrow.test bitvec.test blob.test boundary1.test \
	boundary2.test boundary3.test boundary4.test \
	cast.test check.test collate1.test collate2.test collate3.test \
	collate4.test collate5.test collate6.test collate7.test collate8.test \
	collate9.test collateA.test colmeta.test colname.test count.test \
	createtab.test cse.test date.test default.test delete.test \
	delete2.test delete3.test descidx1.test descidx2.test descidx3.test \
	distinctagg.test expr.test fkey1.test func.test in.test \
	in2.test in3.test in4.test incrblob.test incrblob_err.test \
	incrblob2.test index.test index2.test index3.test indexedby.test \
	insert.test insert2.test insert3.test insert4.test insert5.test \
	interrupt.test intpkey.test lastinsert.test like.test like2.test \
	limit.test lookaside.test manydb.test memdb.test \
	minmax.test minmax2.test minmax3.test \
	nan.test notnull.test null.test pagesize.test \
	ptrchng.test rdonly.test reindex.test rollback.test rowhash.test \
	rowid.test rtree.test schema.test schema2.test select1.test \
	select2.test select3.test select4.test select5.test select6.test \
	select7.test select8.test select9.test selectA.test selectB.test \
	selectC.test server1.test shared2.test shared3.test shared4.test \
	shared6.test shared7.test \
	sort.test sqllimits1.test subquery.test subselect.test substr.test \
	table.test tempdb.test temptable.test temptrigger.test \
	thread001.test thread003.test thread004.test thread005.test \
	thread1.test thread2.test \
	trans.test trans2.test trans3.test trigger1.test trigger2.test \
	trigger3.test trigger4.test trigger5.test trigger6.test trigger7.test \
	trigger8.test trigger9.test triggerA.test triggerB.test types.test \
	types2.test types3.test unique.test update.test utf16align.test \
	view.test vtab1.test vtab2.test vtab3.test vtab4.test vtab5.test \
	vtab6.test vtab7.test vtab8.test vtab9.test vtab_alter.test vtabA.test \
	vtabB.test vtabC.test vtabD.test vtab_err.test vtab_shared.test \
	where.test where2.test where3.test where4.test where5.test where6.test \
	where7.test where8.test where8m.test where9.test whereA.test \
	wherelimit.test zeroblob.test"

BDB_TESTS_ERRORS=""

BDB_TESTS_HANGS=""

BDB_TESTS_ALL="$BDB_TESTS_PASSING $BDB_TESTS_ERRORS $BDB_TESTS_HANGS"

# Clean up any old log
rm -f test.log

# kill tests if still running after 30 minutes
TIMEOUT=1800
alarm() { perl -e 'alarm shift; exec @ARGV' "$@"; }

case "$1" in
passing) TEST_CASES="$BDB_TESTS_PASSING";;
errors)  TEST_CASES="$BDB_TESTS_ERRORS";;
hangs)   TEST_CASES="$BDB_TESTS_HANGS";;
*)       TEST_CASES="$BDB_TESTS_ALL";;
esac

for t in $TEST_CASES ; do
	case "$t" in
	bdb*) tpath=$TOP/test/$t ;;
	*) tpath=$SQLITE/test/$t ;;
	esac

	echo $t: `alarm $TIMEOUT ./testfixture $tpath 2>&1 | tee -a test.log | grep "errors out of" || echo "failed"`
done
