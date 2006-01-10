/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997-2005
 *	Sleepycat Software.  All rights reserved.
 *
 * $Id: ErrorHandler.java,v 12.1 2005/06/16 20:23:01 bostic Exp $
 */
package com.sleepycat.db;

public interface ErrorHandler {
    void error(Environment dbenv, String errpfx, String msg);
}
