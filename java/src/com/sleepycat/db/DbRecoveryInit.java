/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997-2001
 *      Sleepycat Software.  All rights reserved.
 *
 * $Id: DbRecoveryInit.java,v 11.5 2001/05/16 13:09:30 bostic Exp $
 */

package com.sleepycat.db;

/**
 *
 * @author Donald D. Anderson
 */
public interface DbRecoveryInit
{
    // methods
    //
    public abstract void recovery_init(DbEnv dbenv);
}

// end of DbRecoveryInit.java
