/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2002-2003
 *	Sleepycat Software.  All rights reserved.
 *
 * $Id: TestKeyAssigner.java,v 1.5 2003/10/18 19:51:29 mhayes Exp $
 */

package com.sleepycat.bdb.test;

import com.sleepycat.bdb.bind.DataBuffer;
import com.sleepycat.bdb.PrimaryKeyAssigner;
import com.sleepycat.db.DbException;
import java.io.IOException;

/**
 * @author Mark Hayes
 */
class TestKeyAssigner implements PrimaryKeyAssigner {

    private byte next = 1;
    private boolean isRecNum;

    TestKeyAssigner(boolean isRecNum) {

        this.isRecNum = isRecNum;
    }

    public void assignKey(DataBuffer keyData)
        throws DbException, IOException {

        if (isRecNum) {
            TestStore.RECNO_FORMAT.recordNumberToData(next, keyData);
        } else {
            keyData.setData(new byte[] { next }, 0, 1);
        }
        next += 1;
    }

    void reset() {

        next = 1;
    }
}
