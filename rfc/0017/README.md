# RFC 0017 working directory

Measurement plan, results and notes for RFC 0017 (HASH concurrency and online
resize). Every number recorded here must come from a run on libdb, with the
host, commit, build flags and command line beside it.

Before RFC 0017 can leave Draft, this directory must contain the following.

1. **Concurrency benchmark results.** `test/bench/hash_conc_bench.c` runs T
   threads doing point inserts and lookups against one HASH database at a fixed
   fill factor. It covers transactional and non-transactional runs, reports
   throughput against thread count and a latency histogram, and includes a
   B-tree control. The A/B against the base is run on one host, with arms
   alternated over repeated runs.
2. **Split-stall measurement.** How long concurrent cursors wait on the hash
   metapage while a split is in progress (`__ham_expand_table` through
   `__ham_split_page`, until the splitting transaction ends), before and after
   the change.
3. **Correctness evidence.** The `test/db` tier, the TCL hash tests, DST with
   `BUGGIFY_HASH_EXPAND_EARLY`, and a multi-process hash test, all green, with
   the region signature for change 1 compared against master.
