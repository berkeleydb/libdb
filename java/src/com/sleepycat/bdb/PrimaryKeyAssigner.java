/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2000-2003
 *      Sleepycat Software.  All rights reserved.
 *
 * $Id: PrimaryKeyAssigner.java,v 1.11 2003/10/18 19:54:21 mhayes Exp $
 */

package com.sleepycat.bdb;

import com.sleepycat.bdb.bind.DataBuffer;
import com.sleepycat.db.DbException;
import com.sleepycat.bdb.collection.StoredList;
import com.sleepycat.bdb.collection.StoredMap;
import java.io.IOException;

/**
 * An interface implemented to assign new primary key values.
 * An implementation of this interface is passed to the {@link DataStore}
 * constructor to assign primary keys for that store. Key assignment occurs
 * when {@link StoredMap#append} or {@link StoredList#append} is called.
 *
 * @author Mark Hayes
 */
public interface PrimaryKeyAssigner {

    /**
     * Assigns a new primary key value into the given data buffer.
     */
    void assignKey(DataBuffer keyData)
        throws DbException, IOException;
}
