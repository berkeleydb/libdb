/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997-2001
 *      Sleepycat Software.  All rights reserved.
 *
 * $Id: DbLock.java,v 11.7 2001/07/26 23:22:34 dda Exp $
 */

package com.sleepycat.db;

/**
 *
 * @author Donald D. Anderson
 */
public class DbLock
{
    protected native void finalize()
         throws Throwable;

    // get/set methods
    //

    // private data
    //
    private long private_dbobj_ = 0;

    static {
        Db.load_db();
    }
}

// end of DbLock.java
