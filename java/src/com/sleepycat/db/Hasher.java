/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2000-2005
 *	Sleepycat Software.  All rights reserved.
 *
 * $Id: Hasher.java,v 12.1 2005/06/16 20:23:01 bostic Exp $
 */
package com.sleepycat.db;

public interface Hasher {
    int hash(Database db, byte[] data, int len);
}
