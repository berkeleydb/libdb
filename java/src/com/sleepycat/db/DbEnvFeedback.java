/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1999
 *	Sleepycat Software.  All rights reserved.
 *
 *	@(#)DbEnvFeedback.java	11.1 (Sleepycat) 7/31/99
 */

package com.sleepycat.db;

public interface DbEnvFeedback
{
    // methods
    //
    public abstract void feedback(DbEnv env, int opcode, int pct);
}

// end of DbFeedback.java
