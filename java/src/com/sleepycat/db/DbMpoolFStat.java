/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998
 *	Sleepycat Software.  All rights reserved.
 *
 *	@(#)DbMpoolFStat.java	10.5 (Sleepycat) 11/2/98
 */

package com.sleepycat.db;

/*
 * This is filled in and returned by the
 * DbMpool.fstat() method.
 */
public class DbMpoolFStat
{
    public String file_name;            // File name.
    public int st_pagesize;             // Page size.
    public int st_cache_hit;            // Pages found in the cache.
    public int st_cache_miss;           // Pages not found in the cache.
    public int st_map;                  // Pages from mapped files.
    public int st_page_create;          // Pages created in the cache.
    public int st_page_in;              // Pages read in.
    public int st_page_out;             // Pages written out.
}

// end of DbMpoolFStat.java
