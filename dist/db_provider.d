/*
 * DO NOT EDIT: automatically built by dist/s_include.
 * Oracle Berkeley DB DTrace Provider
 */
provider bdb {
	probe alloc__btree_split(char *db, unsigned pgno, struct __db_dbt *dbt);
	probe alloc__hash_bucket_overflow(char *db, unsigned pgno, struct __db_dbt *dbt);
	probe alloc__hash_table_split(char *db, unsigned pgno, struct __db_dbt *dbt);
	probe alloc__offpage_duplicate(char *db, unsigned pgno, struct __db_dbt *dbt);
	probe alloc__queue_extend(char *db, unsigned pgno, struct __db_dbt *dbt);
	probe alloc__record_overflow(char *db, unsigned pgno, struct __db_dbt *dbt);
	probe lock__suspend(char *db, unsigned pgno, unsigned partition);
	probe lock__resume(char *db, unsigned pgno, unsigned partition);
	probe log__put(unsigned logfile, unsigned logoffset, unsigned type);
	probe log__flush(unsigned logfile, unsigned logoffset);
	probe mpool__evict(char *db, unsigned pgno, struct __bh *buf);
	probe mpool__hash_examined(unsigned examined);
	probe mpool__hash_longest(unsigned longest);
	probe mpool__hash_search(unsigned searched);
	probe mpool__hits(unsigned hits);
	probe mpool__map(unsigned map);
	probe mpool__miss(unsigned hits);
	probe mpool__page_create(unsigned page_create);
	probe mpool__page_in(unsigned page_in);
	probe mpool__read(char *db, unsigned pgno, struct __bh *buf);
	probe mpool__write(char *db, unsigned pgno, struct __bh *buf);
	probe mutex__suspend(unsigned mutex, unsigned alloc_id);
	probe mutex__resume(unsigned mutex, unsigned alloc_id);
};
