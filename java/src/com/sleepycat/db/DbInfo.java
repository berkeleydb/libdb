/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998
 *	Sleepycat Software.  All rights reserved.
 *
 *	@(#)DbInfo.java	10.4 (Sleepycat) 10/23/98
 */

package com.sleepycat.db;

/**
 *
 * @author Donald D. Anderson
 */
public class DbInfo
{
    // methods
    //
    public DbInfo()
    {
        init_from(null);
    }

    public DbInfo(DbInfo that)
    {
        init_from(that);
    }

    private native void init_from(DbInfo that);

    protected native void finalize()
         throws Throwable;

    // set methods
    //

    // Byte order.
    public native void set_lorder(int lorder);

    // Underlying cache size.
    public native void set_cachesize(/*size_t*/ long cachesize);

    // Underlying page size.
    public native void set_pagesize(/*size_t*/ long pagesize);

    // Note: this callback is not implemented
    // Duplicate compare function.
    // public native void set_compare(DbCompare compare)

    // Local heap allocation.
    // Note: there is no access to the underlying "malloc" field, since
    // it is a C function that makes little sense in the Java world.

    ////////////////////////////////////////////////////////////////
    // Btree access method.

    // Maximum keys per page.
    public native void set_bt_maxkey(int bt_maxkey);

    // Minimum keys per page.
    public native void set_bt_minkey(int bt_minkey);

    // Note: this callback is not implemented
    // Comparison function.
    // public native void set_bt_compare(DbBtreeCompare bt_compare);

    // Note: this callback is not implemented
    // Prefix function.
    // public native void set_bt_prefix(DbBtreePrefix bt_prefix);

    ////////////////////////////////////////////////////////////////
    // Hash access method.

    // Fill factor.
    public native void set_h_ffactor(/*unsigned*/ int h_ffactor);

    // Number of elements.
    public native void set_h_nelem(/*unsigned*/ int h_nelem);

    // Note: this callback is not implemented
    // Hash function.
    // public native void set_h_hash(DbHash h_hash);

    ////////////////////////////////////////////////////////////////
    // Recno access method.

    // Fixed-length padding byte.
    public native void set_re_pad(int re_pad);

    // Variable-length delimiting byte.
    public native void set_re_delim(int re_delim);

    // Length for fixed-length records.
    public native void set_re_len(/*u_int32_t*/ int re_len);

    // Source file name.
    public native void set_re_source(String re_source);

    // Flags.
    public native void set_flags(/*u_int32_t*/ int flags);

    // private data
    //
    private long private_info_ = 0;

    static {
        Db.load_db();
    }
}

// end of DbInfo.java
