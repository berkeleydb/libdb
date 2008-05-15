/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2002,2008 Oracle.  All rights reserved.
 *
 * $Id: MultipleEntry.java,v 12.10 2008/01/17 05:04:53 mjc Exp $
 */

package com.sleepycat.db;

import com.sleepycat.db.internal.DbConstants;

import java.nio.ByteBuffer;

/**
An abstract class representing a DatabaseEntry that holds multiple results
returned by a single {@link com.sleepycat.db.Cursor Cursor} get method.  Use one of the concrete
subclasses depending on whether you need:
<ul>
<li>multiple data items only: {@link com.sleepycat.db.MultipleDataEntry MultipleDataEntry}</li>
<li>multiple key / data item pairs: {@link com.sleepycat.db.MultipleKeyDataEntry MultipleKeyDataEntry}</li>
<li>multiple record number / data item pairs:
{@link com.sleepycat.db.MultipleRecnoDataEntry MultipleRecnoDataEntry}</li>
</ul>
*/
public abstract class MultipleEntry extends DatabaseEntry {
    /* package */ int pos;

    /* package */ MultipleEntry(final byte[] data, final int offset, final int size) {
        super(data, offset, size);
        setUserBuffer((data != null) ? (data.length - offset) : 0, true);
    }

    /* package */ MultipleEntry(final ByteBuffer data) {
        super(data);
    }

    public void setUserBuffer(final int length, final boolean usermem) {
        if (!usermem)
            throw new IllegalArgumentException("User buffer required");
        super.setUserBuffer(length, usermem);
    }
}
