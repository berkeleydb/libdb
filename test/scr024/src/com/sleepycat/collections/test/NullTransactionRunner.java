/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2002,2008 Oracle.  All rights reserved.
 *
 * $Id: NullTransactionRunner.java,v 12.6 2008/01/08 20:58:57 bostic Exp $
 */

package com.sleepycat.collections.test;

import com.sleepycat.collections.TransactionRunner;
import com.sleepycat.collections.TransactionWorker;
import com.sleepycat.db.Environment;
import com.sleepycat.util.ExceptionUnwrapper;

class NullTransactionRunner extends TransactionRunner {

    NullTransactionRunner(Environment env) {

        super(env);
    }

    public void run(TransactionWorker worker)
        throws Exception {

        try {
            worker.doWork();
        } catch (Exception e) {
            throw ExceptionUnwrapper.unwrap(e);
        }
    }
}
