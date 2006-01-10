/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997-2005
 *	Sleepycat Software.  All rights reserved.
 *
 * $Id: PanicHandler.java,v 12.1 2005/06/16 20:23:03 bostic Exp $
 */
package com.sleepycat.db;

public interface PanicHandler {
    void panic(Environment dbenv, DatabaseException e);
}
