/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2000-2003
 *      Sleepycat Software.  All rights reserved.
 *
 * $Id: TransactionWorker.java,v 1.9 2003/10/18 19:54:22 mhayes Exp $
 */

package com.sleepycat.bdb;

/**
 * The interface implemented to perform the work within a transaction.
 * To run a transaction, an instance of this interface is passed to the
 * {@link TransactionRunner#run} method.
 *
 * @author Mark Hayes
 */
public interface TransactionWorker {

    /**
     * Perform the work for a single transaction.
     *
     * @see TransactionRunner#run
     */
    void doWork()
        throws Exception;
}
