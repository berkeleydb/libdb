/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 *
 *	@(#)DbErrcall.java	11.1 (Sleepycat) 7/25/99
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
