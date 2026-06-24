/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2002, 2013 Oracle and/or its affiliates.  All rights reserved.
 */

#include <sys/types.h>

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "db_185.h"

void	err(char *);
int	mycmp(const DBT *, const DBT *);
void	ops(DB *, int);

int
main()
{
	DB *dbp;
	HASHINFO h_info;
	BTREEINFO b_info;
	RECNOINFO r_info;

	printf("\tBtree...\n");
	memset(&b_info, 0, sizeof(b_info));
	b_info.flags = R_DUP;
	b_info.cachesize = 100 * 1024;
	b_info.psize = 512;
	b_info.lorder = 4321;
	b_info.compare = mycmp;
	(void)remove("a.db");
	if ((dbp =
	   dbopen("a.db", O_CREAT | O_RDWR, 0664, DB_BTREE, &b_info)) == NULL)
		err("dbopen: btree");
	ops(dbp, DB_BTREE);

	printf("\tHash...\n");
	memset(&h_info, 0, sizeof(h_info));
	h_info.bsize = 512;
	h_info.ffactor = 6;
	h_info.nelem = 1000;
	h_info.cachesize = 100 * 1024;
	h_info.lorder = 1234;
	(void)remove("a.db");
	if ((dbp =
	    dbopen("a.db", O_CREAT | O_RDWR, 0664, DB_HASH, &h_info)) == NULL)
		err("dbopen: hash");
	ops(dbp, DB_HASH);

	printf("\tRecno...\n");
	memset(&r_info, 0, sizeof(r_info));
	r_info.flags = R_FIXEDLEN;
	r_info.cachesize = 100 * 1024;
	r_info.psize = 1024;
	r_info.reclen = 37;
	(void)remove("a.db");
	if ((dbp =
	   dbopen("a.db", O_CREAT | O_RDWR, 0664, DB_RECNO, &r_info)) == NULL)
		err("dbopen: recno");
	ops(dbp, DB_RECNO);

	return (0);
}

int
mycmp(a, b)
	const DBT *a, *b;
{
	size_t len;
	u_int8_t *p1, *p2;

	len = a->size > b->size ? b->size : a->size;
	for (p1 = a->data, p2 = b->data; len--; ++p1, ++p2)
		if (*p1 != *p2)
			return ((long)*p1 - (long)*p2);
	return ((long)a->size - (long)b->size);
}

void
ops(dbp, type)
	DB *dbp;
	int type;
{
	FILE *outfp;
	DBT key, data;
	recno_t recno;
	int i, ret;
	char buf[64];

	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));

	for (i = 1; i < 100; ++i) {		/* Test DB->put. */
		sprintf(buf, "abc_%d_efg", i);
		if (type == DB_RECNO) {
			recno = i;
			key.data = &recno;
			key.size = sizeof(recno);
		} else {
			key.data = data.data = buf;
			key.size = data.size = strlen(buf);
		}

		data.data = buf;
		data.size = strlen(buf);
		if (dbp->put(dbp, &key, &data, 0))
			err("DB->put");
	}

	if (type == DB_RECNO) {			/* Test DB->get. */
		recno = 97;
		key.data = &recno;
		key.size = sizeof(recno);
	} else {
		key.data = buf;
		key.size = strlen(buf);
	}
	sprintf(buf, "abc_%d_efg", 97);
	if (dbp->get(dbp, &key, &data, 0) != 0)
		err("DB->get");
	if (memcmp(data.data, buf, strlen(buf)))
		err("DB->get: wrong data returned");

	if (type == DB_RECNO) {			/* Test DB->put no-overwrite. */
		recno = 42;
		key.data = &recno;
		key.size = sizeof(recno);
	} else {
		key.data = buf;
		key.size = strlen(buf);
	}
	sprintf(buf, "abc_%d_efg", 42);
	if (dbp->put(dbp, &key, &data, R_NOOVERWRITE) == 0)
		err("DB->put: no-overwrite succeeded");

	if (type == DB_RECNO) {			/* Test DB->del. */
		recno = 35;
		key.data = &recno;
		key.size = sizeof(recno);
	} else {
		sprintf(buf, "abc_%d_efg", 35);
		key.data = buf;
		key.size = strlen(buf);
	}
	if (dbp->del(dbp, &key, 0))
		err("DB->del");

						/* Test DB->seq. */
	if ((outfp = fopen("output", "w")) == NULL)
		err("fopen: output");
	while ((ret = dbp->seq(dbp, &key, &data, R_NEXT)) == 0) {
		if (type == DB_RECNO)
			fprintf(outfp, "%d\n", *(int *)key.data);
		else
			fprintf(outfp,
			    "%.*s\n", (int)key.size, (char *)key.data);
		fprintf(outfp, "%.*s\n", (int)data.size, (char *)data.data);
	}
	if (ret != 1)
		err("DB->seq");
	fclose(outfp);
	switch (type) {
	case DB_BTREE:
		ret = system("cmp output test_db185/O.BH");
		break;
	case DB_HASH:
		ret = system("sort output | cmp - test_db185/O.BH");
		break;
	case DB_RECNO:
		ret = system("cmp output test_db185/O.R");
		break;
	}
	if (ret != 0)
		err("output comparison failed");

	if (dbp->sync(dbp, 0))			/* Test DB->sync. */
		err("DB->sync");

	test_db_boundary_conditions(dbp, type);
	test_db_cursor(dbp, type);
	test_db_statistics(dbp, type);

	if (dbp->close(dbp))			/* Test DB->close. */
		err("DB->close");
}

static void test_db_boundary_conditions(DB *dbp, int type) {
    DBT key, data;
    char small_key[1] = "";
    char large_key[1024 * 1024];
    int ret;

    memset(&key, 0, sizeof(key));
    memset(&data, 0, sizeof(data));

    key.data = small_key;
    key.size = 0;
    data.data = "empty_key_value";
    data.size = 15;
    if ((ret = dbp->put(dbp, &key, &data, 0)) != 0) {
        fprintf(stderr, "Empty key test failed: %s\n", strerror(errno));
        return;
    }

    memset(large_key, 'x', sizeof(large_key));
    key.data = large_key;
    key.size = sizeof(large_key);
    data.data = "large_data";
    data.size = 10;
    if ((ret = dbp->put(dbp, &key, &data, 0)) != 0) {
        fprintf(stderr, "Large key test failed: %s\n", strerror(errno));
        return;
    }

    key.data = "nonexistent_key_12345";
    key.size = 20;
    if ((ret = dbp->get(dbp, &key, &data, 0)) == 0) {
        fprintf(stderr, "Nonexistent key test failed\n");
        return;
    }

    printf("\t\tBoundary conditions test passed\n");
}

static void test_db_cursor(DB *dbp, int type) {
    DBC *cursor;
    DBT key, data;
    int ret;

    memset(&key, 0, sizeof(key));
    memset(&data, 0, sizeof(data));

    if ((ret = dbp->cursor(dbp, NULL, &cursor, 0)) != 0) {
        fprintf(stderr, "Cursor open failed: %s\n", strerror(errno));
        return;
    }

    int count = 0;
    while ((ret = cursor->c_get(cursor, &key, &data, DB_NEXT)) == 0) {
        count++;
    }
    if (ret != DB_NOTFOUND) {
        fprintf(stderr, "Cursor iteration failed: %s\n", strerror(errno));
        cursor->c_close(cursor);
        return;
    }
    printf("\t\tCursor traversed %d records\n", count);

    if (type != DB_RECNO) {
        sprintf((char *)key.data, "abc_%d_efg", 50);
        key.size = strlen((char *)key.data);
        if ((ret = cursor->c_get(cursor, &key, &data, DB_SET)) != 0) {
            fprintf(stderr, "Cursor DB_SET failed: %s\n", strerror(errno));
            cursor->c_close(cursor);
            return;
        }
        printf("\t\tCursor DB_SET test passed\n");
    }

    if (type != DB_RECNO) {
        key.data = "cursor_insert_key";
        key.size = 17;
        data.data = "cursor_insert_data";
        data.size = 19;
        if ((ret = cursor->c_put(cursor, &key, &data, DB_KEYFIRST)) != 0) {
            fprintf(stderr, "Cursor c_put failed: %s\n", strerror(errno));
            cursor->c_close(cursor);
            return;
        }
        printf("\t\tCursor c_put test passed\n");
    }

    if ((ret = cursor->c_close(cursor)) != 0) {
        fprintf(stderr, "Cursor close failed: %s\n", strerror(errno));
        return;
    }

    printf("\t\tCursor operations test passed\n");
}

static void test_db_statistics(DB *dbp, int type) {
    DB_BTREE_STAT *bt_stat;
    DB_HASH_STAT *hash_stat;
    DB_RECNO_STAT *rec_stat;
    void *statp;
    int ret;

    if ((ret = dbp->stat(dbp, NULL, &statp, 0)) != 0) {
        fprintf(stderr, "DB->stat failed: %s\n", strerror(errno));
        return;
    }

    switch (type) {
        case DB_BTREE:
            bt_stat = (DB_BTREE_STAT *)statp;
            printf("\t\tBtree stat: pages=%lu, entries=%lu\n",
                (u_long)bt_stat->bt_npage, (u_long)bt_stat->bt_nrecs);
            break;
        case DB_HASH:
            hash_stat = (DB_HASH_STAT *)statp;
            printf("\t\tHash stat: buckets=%lu, entries=%lu\n",
                (u_long)hash_stat->hash_nbucket, (u_long)hash_stat->hash_nrecs);
            break;
        case DB_RECNO:
            rec_stat = (DB_RECNO_STAT *)statp;
            printf("\t\tRecno stat: pages=%lu, entries=%lu\n",
                (u_long)rec_stat->r_npage, (u_long)rec_stat->r_nrecs);
            break;
    }

    switch (type) {
        case DB_BTREE:
            bt_stat = (DB_BTREE_STAT *)statp;
            if (bt_stat->bt_nrecs != 99) {  /* 99 entries inserted */
                fprintf(stderr, "Btree stat validation failed\n");
                __os_free(NULL, statp);
                return;
            }
            break;
        case DB_HASH:
            hash_stat = (DB_HASH_STAT *)statp;
            if (hash_stat->hash_nrecs != 99) {
                fprintf(stderr, "Hash stat validation failed\n");
                __os_free(NULL, statp);
                return;
            }
            break;
        case DB_RECNO:
            rec_stat = (DB_RECNO_STAT *)statp;
            if (rec_stat->r_nrecs != 99) {
                fprintf(stderr, "Recno stat validation failed\n");
                __os_free(NULL, statp);
                return;
            }
            break;
    }

    __os_free(NULL, statp);
    printf("\t\tStatistics test passed\n");
}

void
err(s)
	char *s;
{
	fprintf(stderr, "\t%s: %s\n", s, strerror(errno));
	exit(EXIT_FAILURE);
}
