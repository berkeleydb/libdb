/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 *
 *	@(#)DbFeedback.java	11.2 (Sleepycat) 7/30/99
 */

package com.sleepycat.db;

/**
 *
 * @author Donald D. Anderson
 */
public interface DbFeedback
{
    // methods
    //
    public abstract void feedback(Db db, int opcode, int pct);
}

// end of DbFeedback.java
