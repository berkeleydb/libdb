/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 *
 *	@(#)DbBtreeStat.java	11.1 (Sleepycat) 7/25/99
 */

package com.sleepycat.db;

/*
 * This is filled in and returned by the
 * Db.stat() method.
 */
public class DbBtreeStat
{
    public int bt_metaflags;            // Meta-data flags.
    public int bt_maxkey;               // Maxkey value.
    public int bt_minkey;               // Minkey value.
    public int bt_re_len;               // Fixed-length record length.
    public int bt_re_pad;               // Fixed-length record pad.
    public int bt_pagesize;             // Page size.
    public int bt_levels;               // Tree levels.
    public int bt_nrecs;                // Number of records.
    public int bt_int_pg;               // Internal pages.
    public int bt_leaf_pg;              // Leaf pages.
    public int bt_dup_pg;               // Duplicate pages.
    public int bt_over_pg;              // Overflow pages.
    public int bt_free;                 // Pages on the free list.
    public int bt_int_pgfree;           // Bytes free in internal pages.
    public int bt_leaf_pgfree;          // Bytes free in leaf pages.
    public int bt_dup_pgfree;           // Bytes free in duplicate pages.
    public int bt_over_pgfree;          // Bytes free in overflow pages.
    public int bt_magic;                // Magic number.
    public int bt_version;              // Version number.
}

// end of DbBtreeStat.java
