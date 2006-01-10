/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2000-2005
 *      Sleepycat Software.  All rights reserved.
 *
 * $Id: ByteArrayBinding.java,v 12.1 2005/01/31 19:27:29 mark Exp $
 */

package com.sleepycat.bind;

import com.sleepycat.db.DatabaseEntry;

/**
 * A pass-through <code>EntryBinding</code> that uses the entry's byte array as
 * the key or data object.
 *
 * @author Mark Hayes
 */
public class ByteArrayBinding implements EntryBinding {

    /**
     * Creates a byte array binding.
     */
    public ByteArrayBinding() {
    }

    // javadoc is inherited
    public Object entryToObject(DatabaseEntry entry) {

        byte[] bytes = new byte[entry.getSize()];
        System.arraycopy(entry.getData(), entry.getOffset(),
                         bytes, 0, bytes.length);
        return bytes;
    }

    // javadoc is inherited
    public void objectToEntry(Object object, DatabaseEntry entry) {

        byte[] bytes = (byte[]) object;
        entry.setData(bytes, 0, bytes.length);
    }
}
