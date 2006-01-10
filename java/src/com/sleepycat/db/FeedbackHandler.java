/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997-2005
 *	Sleepycat Software.  All rights reserved.
 *
 * $Id: FeedbackHandler.java,v 12.1 2005/06/16 20:23:01 bostic Exp $
 */
package com.sleepycat.db;

public interface FeedbackHandler {
    void recoveryFeedback(Environment dbenv, int percent);
    void upgradeFeedback(Database db, int percent);
    void verifyFeedback(Database db, int percent);
}
