/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 *
 *	@(#)DbMpoolStat.java	11.1 (Sleepycat) 7/25/99
 */

package com.sleepycat.db;

/*
 * This is filled in and returned by the
 * DbMpool.stat() method.
 */
public class DbMpoolStat
{
    /**
     * @deprecated As of Berkeley DB 2.8.2, cachesize for mpool unavailable.
     */
    public int st_cachesize;            // Cache size.
    public int st_cache_hit;            // Pages found in the cache.
    public int st_cache_miss;           // Pages not found in the cache.
    public int st_map;                  // Pages from mapped files.
    public int st_page_create;          // Pages created in the cache.
    public int st_page_in;              // Pages read in.
    public int st_page_out;             // Pages written out.
    public int st_ro_evict;             // Clean pages forced from the cache.
    public int st_rw_evict;             // Dirty pages forced from the cache.
    public int st_hash_buckets;         // Number of hash buckets.
    public int st_hash_searches;        // Total hash chain searches.
    public int st_hash_longest;         // Longest hash chain searched.
    public int st_hash_examined;        // Total hash entries searched.
    public int st_page_clean;           // Clean pages.
    public int st_page_dirty;           // Dirty pages.
    public int st_page_trickle;         // Pages written by memp_trickle.
    public int st_region_wait;          // Region lock granted after wait.
    public int st_region_nowait;        // Region lock granted without wait.
    public int st_regsize;              // Region size.
}

// end of DbMpoolStat.java
