/*
 * Copyright (c) 2011
 *	Donald D. Anderson.  All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted.
 * This software is provided 'as is' and any express or
 * implied warranties, including, but not limited to, the implied
 * warranties of merchantability, fitness for a particular purpose, or
 * non-infringement, are disclaimed.
 */

// This code 'mostly' implements the '3n+1 NoSQL/Key-Value/Schema-Free/
// Schema-Less Database Benchmark' described here:
//   https://docs.google.com/View?id=dd5f3337_12fzjpqbc2
// At the moment, we don't honor the benchmark's input/output
// requirements (reading config values on stdin, all the things
// we need to report on output)
//
// Note: this code could be refactored a bit, and better error
// handling added.  I actually removed some layers, and moved
// everything into just two files so that small portions could be
// examined on their own, for the purposes of illustration, and for
// verification of the benchmark.

#include "db.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <iostream>
#include <sstream>
#include <algorithm>
#include "bench3n.h"

// std::string used 
using namespace std;

// Do the 'enhanced' benchmark - all values stored as strings
#define STORE_AS_STRING  1

typedef int cyclelength_t;
typedef long maxvalue_t;

// more like a struct - we allow free access to all
class Results {
public:
    Results() : cyclelength(-1), maxvalue(-1L) { }
    cyclelength_t cyclelength;
    maxvalue_t maxvalue;

};

enum TxnType {
    SYNC, NOSYNC, WRITENOSYNC, NONE
};

// Each thread evaluates a chunk of values of N.
// For example, the first thread in evaluates the first 1000 values,
// The next thread the next 1000, etc.  Doing in chunks at a time
// removes a point of contention.
const int compute_chunk_size = 1000;
const char *testdir = NULL;

// Used by trickle thead to know when to quit
static volatile int running = 1;

// The next_value and the associated mutex is used by the
// 'chunk allocator'
static long next_value = -1;
mutex_handle get_value_mutex;

// These are arguments - input and output, passed to each worker thread
struct bench_args {
    // these are input args, only read by the thread
    DB_ENV *dbenv;
    DB *benchdb;
    DB *resultdb;
    int minn;
    int maxn;
    int threads;
    int cachek;
    int logbufsizek;
    int logflags;
    int tricklepercent;
    int trickleinterval;
    int partition;
    TxnType txn;
    bool sortbylength;

    // these are output or 'return' args, only written by the thread
    long ret_nput;
    long ret_nget;
};

// Used by the 'enhanced' benchmark
const char *digits[] =
{
    "zero", "eins", "deux",
    "tres", "quattro", "пять",
    "ستة", "सात", "捌", "ஒன்பது"
};

static void dump_digits()
{
    for (int i=0; i<10; i++)
    {
        fprintf(stderr, "digit[%d] len=%d \"%s\"\n", i, (int)strlen(digits[i]), digits[i]);
    }
}

inline int nspace(unsigned char *p, int len)
{
    int count = 0;
    while (len-- > 0) {
        if (*p++ == ' ')
            count++;
    }
    return count;
}

int key_digit_length_compare(DB *db, const DBT *dbt1, const DBT *dbt2)
{
    unsigned char *p1 = (unsigned char *)dbt1->data;
    unsigned char *p2 = (unsigned char *)dbt2->data;
    int nspace1 = nspace(p1, dbt1->size);
    int nspace2 = nspace(p2, dbt2->size);

    if (nspace1 < nspace2)
        return -1;
    else if (nspace1 > nspace2)
        return 1;

    // quick compare - we know that all keys have length > 0
    if (*p1 < *p2)
        return -1;
    else if (*p1 > *p2)
        return 1;

    int len = dbt1->size;
    if (dbt2->size < len)
        len = dbt2->size;

    while (len-- > 0) {
        if (*p1 < *p2)
            return -1;
        else if (*p1 > *p2)
            return 1;
        p1++;
        p2++;
    }
    if (dbt1->size < dbt2->size)
        return -1;
    else if (dbt1->size > dbt2->size)
        return 1;

    return 0;
}

// Uses Fowler/Noll/Vo hash as taken from src/hash/hash_func.c in DB source

u_int32_t partitioner(DB *db, DBT *key)
{
    struct bench_args *args = (bench_args *)db->app_private;

    const u_int8_t *k = (u_int8_t *)key->data;
    const u_int8_t *e = k + key->size;
    u_int32_t h = 0;

    for (h = 0; k < e; ++k) {
        h *= 16777619;
        h ^= *k;
    }
    return (h % args->partition);
}


// Forces values to be stored in MSB format - that's important for locality
// This class is used by the unenhanced benchmark
class DatabaseLong
{
private:
    unsigned char bytes[8];
public:
    DatabaseLong(long l)
    {
        setLong(l);
    }

    long getLong()
    {
        long val = 0;
        for (int i=0; i<sizeof(bytes); i++)
        {
            val <<= 8;
            val |= bytes[i];
        }
        return val;
    }

    void setLong(long l)
    {
        for (int i=sizeof(bytes)-1; i>=0; i--)
        {
            bytes[i] = (l & 0xff);
            l >>= 8;
        }
    }
    
    unsigned char *getBytes()
    {
        return bytes;
    }

    size_t getSize()
    {
        return sizeof(long);
    }
};

// This class is used by the enhanced benchmark
class DatabaseDigits
{
private:
    string s;

public:
    DatabaseDigits(long l)
    {
        setLong(l);
    }

    DatabaseDigits(string &sparam)
    {
        s = sparam;
    }

    DatabaseDigits(DBT *dbt)
    {
        string str((char *)dbt->data, dbt->size);
        s = str;
    }

    long getLong()
    {
        long val = 0;
        istringstream iss(s);
        do {
            string sub;
            iss >> sub;
            if (sub == "")
                break;
            val *= 10;
            for (int i=0; i<10; i++) {
                if (sub == digits[i]) {
                    val += i;
                    break;
                }
            }
        } while (iss);
        return val;
    }

    void setLong(long l)
    {
        if (l < 10) {
            if (l < 0) {
                cerr << "bad call to setLong(" << l << ")\n";
                exit(1);                // TODO: throw exception
            }
            s = digits[(int)l];
        }
        else {
            s = "";
            while (l != 0) {
                if (s.length() != 0)
                    s.insert(0, " ");
                s.insert(0, digits[(int)(l % 10)]);
                l = l / 10;
            }
        }
    }
    
    unsigned char *getBytes()
    {
        return (unsigned char *)s.c_str();
    }

    size_t getSize()
    {
        return s.length();
    }

};

void init_dbt(DBT *dbt, const void *data, size_t size)
{
    memset(dbt, 0, sizeof(DBT));
    if (data != 0) {
        dbt->data = (void *)data;
        // for some APIs (get) we'll need size set,
        // for some (put), we need ulen set.
        // By setting both, this method can be used in either case.
        dbt->size = size;
        dbt->ulen = size;
    }
    dbt->flags = DB_DBT_USERMEM;
}

Results get_result(bench_args *args)
{
    DBT keydbt;
    DBT valdbt;
    int ret;
    Results result;

    init_dbt(&valdbt, &result, sizeof(result));
    init_dbt(&keydbt, "", 0);

    DEADLOCK_RETRY(args->resultdb->get(args->resultdb, NULL, &keydbt, &valdbt, DB_READ_UNCOMMITTED), 5, "db", "get", ret);
    if (ret != 0 && ret != DB_NOTFOUND) {
        fprintf(stderr, "bench3n: error: getting result: %s\n", db_strerror(ret));
        exit(1);                  // TODO: throw exception
    }

    return result;
}

void update_result(bench_args *args, Results result)
{
    DB_TXN *txn;
    DBT keydbt;
    DBT valdbt;
    int ret;
    Results curresult;

    init_dbt(&keydbt, "", 0);

    int deadtries = 0;
    while (deadtries++ < 5) {

        init_dbt(&valdbt, &curresult, sizeof(curresult));

        // Always sync changes to results.
        CHK(args->dbenv->txn_begin(args->dbenv, NULL, &txn, DB_TXN_SYNC), "DB_ENV", "txn_begin");

        // since we're in a loop, reinitialize curresult.
        curresult.cyclelength = -1;
        curresult.maxvalue = -1;
        if ((ret = (args->resultdb->get(args->resultdb, txn, &keydbt, &valdbt, DB_RMW))) == 0
          || ret == DB_NOTFOUND) {

            if (curresult.cyclelength >= result.cyclelength && curresult.maxvalue >= result.maxvalue) {
                // no changes needed, we're done.
                CHK(txn->commit(txn, 0), "txn", "commit");
                break;
            }
            curresult.cyclelength = max(curresult.cyclelength, result.cyclelength);
            curresult.maxvalue = max(curresult.maxvalue, result.maxvalue);

            if ((ret = (args->resultdb->put(args->resultdb, txn, &keydbt, &valdbt, 0))) == 0) {
                // changed, we're done.
                CHK(txn->commit(txn, 0), "txn", "commit");
                break;
            }
        }
        if (ret != DB_LOCK_DEADLOCK) {
            fprintf(stderr, "bench3n: error: updating result: %s\n", db_strerror(ret));
            exit(1);                  // TODO: throw exception
        }
        CHK(txn->abort(txn), "txn", "abort");
    }
}

// Get the start of the next 'chunk' of values to evaluate
long get_next_value()
{
    long retval = 0;

    /*Critical section*/
    mutex_lock(&get_value_mutex);
    retval = next_value;
    next_value += compute_chunk_size;
    mutex_unlock(&get_value_mutex);
    /*End Critical section*/

    return retval;
}

Results compute_cycles(bench_args *args, long n, long maxseen)
{
    if (n >= maxseen) {
        maxseen = n;
    }
    Results result;
    result.maxvalue = maxseen;

    if (n == 1) {
        result.cyclelength = 1;
        return result;
    }
    if (n <= 0) {
        fprintf(stderr, "bench3n: overflow/underflow\n");
        exit(1);                  // TODO: throw exception
    }

#if STORE_AS_STRING
    DatabaseDigits key(n);
    DatabaseDigits val(0L);
    char stored[512];          // TODO: should avoid fixed size array.
#else
    DatabaseLong key(n);
    cyclelength_t val = 0;
#endif

    DBT keydbt;
    DBT valdbt;

    init_dbt(&keydbt, key.getBytes(), key.getSize());

#if STORE_AS_STRING
    init_dbt(&valdbt, stored, sizeof(stored));
#else
    init_dbt(&valdbt, &val, sizeof(val));
#endif

    int ret;
    int flags = 0;
    flags |= DB_READ_UNCOMMITTED;
    args->ret_nget++;
    DEADLOCK_RETRY(args->benchdb->get(args->benchdb, NULL, &keydbt, &valdbt, flags), 5, "db", "get", ret);
    if (ret == 0)
    {
#if STORE_AS_STRING
        DatabaseDigits d(&valdbt);
        result.cyclelength = d.getLong();
#else
        /*fprintf(stderr, "  found (%d) => %d\n", n, val);*/
        result.cyclelength = val;
#endif
        return result;
    }
    else if (ret != DB_NOTFOUND)
    {
        // Note: no exception, we can recover on get by doing more work.
        fprintf(stderr, "bench3n: warning: getting value %ld: %s\n", key.getLong(), db_strerror(ret));
    }

    long nextn = ((n % 2) == 0) ? (n/2) : (3*n + 1);
    result = compute_cycles(args, nextn, maxseen);
    if (result.cyclelength <= 0)
        return result;             // error return
    result.cyclelength++;

#if STORE_AS_STRING
    DatabaseDigits d(result.cyclelength);
    init_dbt(&valdbt, d.getBytes(), d.getSize());
#else
    val = result.cyclelength;
#endif
    args->ret_nput++;
    DEADLOCK_RETRY(args->benchdb->put(args->benchdb, NULL, &keydbt, &valdbt, 0), 5, "db", "put", ret);
    if (ret != 0)
    {
        fprintf(stderr, "bench3n: error: getting value %ld: %s\n", key.getLong(), db_strerror(ret));
        exit(-1);                  // TODO: throw exception
    }
    return result;
}

void *bench_thread_main(void *thread_args)
{
    bench_args *args = (bench_args*)thread_args;
    for (long chunkstart = get_next_value(); chunkstart <= args->maxn; chunkstart = get_next_value()) {
        for (long n = chunkstart; n < chunkstart + compute_chunk_size; n++) {
            Results results = compute_cycles(args, n, -1L);
            // Update_result (especially when everyone is doing it) is expensive,
            // so first peek at the results to see if we might need to update.
            Results curresult = get_result(args);
            if (curresult.cyclelength < results.cyclelength ||
              curresult.maxvalue < results.maxvalue) {
                update_result(args, results);
            }
        }
    }
    return NULL;
}

void *trickle_thread_main(void *thread_args)
{
    bench_args *args = (bench_args*)thread_args;

    int pct = 5;
    int nsecs = args->trickleinterval;

    while (running) {
        if (args->tricklepercent < 0) {
            int npages = 0;
            args->dbenv->memp_trickle(args->dbenv, pct, &npages);
            if (npages > 0) {
                nsecs = 7;
                if (pct > 3)
                    pct--;
            }
            else if (pct < 5) {
                pct++;
            }
            else {
                nsecs = 20;
                pct = 5;
            }
        }
        else {
            args->dbenv->memp_trickle(args->dbenv, args->tricklepercent, NULL);
        }
        sleep(nsecs);
    }
}


void runbench(bench_args *args)
{
    thread_handle tids[args->threads];
    bench_args targs[args->threads];
    int nthreads = args->threads;
    int ret;
    Results result;
    long nput = 0;
    long nget = 0;

    next_value = args->minn;

    time_t startt, endt;
    int nseconds;

    time(&startt);
    for (int t=0; t<nthreads; t++) {
        targs[t] = *args;
        if ((ret = thread_start(bench_thread_main, &targs[t], &tids[t])) != 0) {
            fprintf(stderr, "thread_start error: %s\n", strerror(ret));
        }
        //fprintf(stderr, "Thread[%d] = %p started\n", t, tids[t]);
    }

    for (int t=0; t<nthreads; t++) {
        CHK(thread_join(tids[t]), "thread", "join");
        //fprintf(stderr, "Thread[%d] = %p joined\n", t, tids[t]);
    }
    time(&endt);
    nseconds = (int)(endt - startt);

    // collect the per-thread statistics
    for (int t=0; t<nthreads; t++) {
        nput += targs[t].ret_nput;
        nget += targs[t].ret_nget;
    }
    result = get_result(args);

    fprintf(stderr, "  N=%d\n  result=%d\n  maxvalue=%ld\n  time=%d\n", args->maxn, result.cyclelength, result.maxvalue, nseconds);
    fprintf(stderr, "  nputs=%ld (%.2f puts/second)\n  ngets=%ld (%.2f gets/second)\n  ops=%ld (%.2f ops/second)\n\n", nput, ((double)nput)/nseconds, nget, ((double)nget)/nseconds, (nput+nget), ((double)(nput+nget)/nseconds));
}

int openrunbench(bench_args *args)
{
    DB_ENV *env;
    DB *db;
    int envflags;

    // Set up environment configured according to input parameters
    envflags = DB_CREATE | DB_INIT_MPOOL | DB_INIT_LOCK | DB_THREAD;
    CHK(db_env_create(&env, 0), "dbenv", "create");
    if (args->cachek != 0) {
        CHK(env->set_cachesize(env, 0, args->cachek * 1024, 0), "dbenv", "set_cachesize");
    }
    CHK(env->set_lk_detect(env, DB_LOCK_DEFAULT), "dbenv", "set_lk_detect");
    if (args->logbufsizek != 0) {
        CHK(env->set_lg_bsize(env, args->logbufsizek * 1024), "dbenv", "set_lg_bufsize");
    }
    if (args->logflags != 0) {
        CHK(env->log_set_config(env, args->logflags, 1), "dbenv", "log_set_config");
    }
    if (args->txn != NONE) {
        envflags |= DB_INIT_TXN;
        if (args->txn == NOSYNC) {
            CHK(env->set_flags(env, DB_TXN_NOSYNC, 1), "dbenv", "set_flags");
        }
        else if (args->txn == WRITENOSYNC) {
            CHK(env->set_flags(env, DB_TXN_WRITE_NOSYNC, 1), "dbenv", "set_flags");
        }
    }
    CHK(env->open(env, testdir, envflags, 0), "dbenv->open", testdir);
    args->dbenv = env;

    // Set up cycles database, contains intermediate results.
    const char *pathnm = "3ncycles.db";
    int flags = DB_CREATE;
    if (args->txn != NONE) {
        flags |= DB_AUTO_COMMIT;
    }
    flags |= DB_READ_UNCOMMITTED;

    CHK(db_create(&db, env, 0), "db", "create");
    db->app_private = args;
    if (args->sortbylength) {
        CHK(db->set_bt_compare(db, key_digit_length_compare), "db", "set compare");
    }
    if (args->partition > 0) {
        CHK(db->set_partition(db, args->partition, NULL, partitioner), "db", "set_partition");
    }
    CHK(db->open(db, NULL, pathnm, NULL, DB_BTREE, flags, 0), "db->open", pathnm);
    args->benchdb = db;

    // Set up result database
    pathnm = "3nresult.db";
    flags = DB_CREATE | DB_AUTO_COMMIT;
    CHK(db_create(&db, env, 0), "db", "create");
    db->app_private = args;
    CHK(db->open(db, NULL, pathnm, NULL, DB_BTREE, flags, 0), "db->open", pathnm);
    args->resultdb = db;

    // Set up trickle thread if specified
    thread_handle trickle_thread;
    if (args->tricklepercent != 0) {
        CHK(thread_start(trickle_thread_main, args, &trickle_thread), "dbenv", "tricklethread");
    }

    // Run the benchmark
    runbench(args);

    // Clean up: close and wait for any additional threads.
    CHK(args->benchdb->close(args->benchdb, 0), "db", "close");
    CHK(args->resultdb->close(args->resultdb, 0), "db", "close");
    running = 0;
    if (args->tricklepercent != 0) {
        CHK(thread_join(trickle_thread), "trickle thread", "join");
    }
    CHK(env->close(env, 0), "dbenv", "close");

    return (0);
}

int main(int argc, char **argv) {
    bench_args args;

    memset(&args, 0, sizeof(args));
    args.threads = 1;
    args.minn = 1;
    args.cachek = 512;
    //args.tricklepercent = 20;
    //args.trickleinterval = 10;
    args.txn = NOSYNC;

    while (argc > 2) {
        const char *arg = argv[1];
        if (strcmp(arg, "-l") == 0) {
            args.minn = atoi(argv[2]);
            argv += 2;
            argc -= 2;
        }
        else if (strcmp(arg, "-n") == 0) {
            args.maxn = atoi(argv[2]);
            argv += 2;
            argc -= 2;
        }
        else if (strcmp(arg, "-c") == 0) {
            args.cachek = atoi(argv[2]);
            argv += 2;
            argc -= 2;
        }
        else if (strcmp(arg, "-t") == 0) {
            args.threads = atoi(argv[2]);
            argv += 2;
            argc -= 2;
        }
        else if (strcmp(arg, "-nosynctxn") == 0) {
            args.txn = NOSYNC;
            argv += 1;
            argc -= 1;
        }
        else if (strcmp(arg, "-writenosynctxn") == 0) {
            args.txn = WRITENOSYNC;
            argv += 1;
            argc -= 1;
        }
        else if (strcmp(arg, "-notxn") == 0) {
            args.txn = NONE;
            argv += 1;
            argc -= 1;
        }
        else if (strcmp(arg, "-synctxn") == 0) {
            args.txn = SYNC;
            argv += 1;
            argc -= 1;
        }
        else if (strcmp(arg, "-tricklepct") == 0) {
            args.tricklepercent = atoi(argv[2]);
            argv += 2;
            argc -= 2;
        }
        else if (strcmp(arg, "-logbufsize") == 0) {
            args.logbufsizek = atoi(argv[2]);
            argv += 2;
            argc -= 2;
        }
        else if (strcmp(arg, "-logdsync") == 0) {
            args.logflags |= DB_LOG_DSYNC;
            argv += 1;
            argc -= 1;
        }
        else if (strcmp(arg, "-logdirect") == 0) {
            args.logflags |= DB_LOG_DIRECT;
            argv += 1;
            argc -= 1;
        }
        else if (strcmp(arg, "-logzero") == 0) {
            args.logflags |= DB_LOG_ZERO;
            argv += 1;
            argc -= 1;
        }
        else if (strcmp(arg, "-trickleinterval") == 0) {
            args.trickleinterval = atoi(argv[2]);
            argv += 2;
            argc -= 2;
        }
        else if (strcmp(arg, "-sortbylength") == 0) {
            args.sortbylength = true;
            argv += 1;
            argc -= 1;
        }
        else if (strcmp(arg, "-partition") == 0) {
            args.partition = atoi(argv[2]);
            argv += 2;
            argc -= 2;
        }
        else {
            fprintf(stderr, "bench3n: bad arg=%s\n", arg);
            break;
        }
    }
    if (argc != 2 || args.maxn == 0) {
        fprintf(stderr, "Usage: bench3n "
          "-n maxN [ -l minN ] [ -t nthreads ] [ -c cachesize-in-kbytes ] dir\n");
        exit(1);
    }
    testdir = argv[1];

    //dump_digits();
    mutex_init(&get_value_mutex);
    return openrunbench(&args);
}
