/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997
 *	Sleepycat Software.  All rights reserved.
 *
 *	@(#)DbErrcall.java	10.1 (Sleepycat) 11/10/97
 */

package com.sleepycat.db;

/**
 *
 * @author Donald D. Anderson
 */
public interface DbErrcall
{
    // methods
    //
    public abstract void errcall(String prefix, String buffer);
}

// end of DbErrcall.java
