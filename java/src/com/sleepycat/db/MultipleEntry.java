/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2002-2005
 *	Sleepycat Software.  All rights reserved.
 *
 * $Id: MultipleEntry.java,v 12.2 2005/06/16 20:23:03 bostic Exp $
 */

package com.sleepycat.db;

import com.sleepycat.db.internal.DbConstants;

public abstract class MultipleEntry extends DatabaseEntry {
    protected int pos;

    protected MultipleEntry(final byte[] data, final int offset, final int size) {
        super(data, offset, size);
        setUserBuffer((data != null) ? (data.length - offset) : 0, true);
        this.flags |= DbConstants.DB_DBT_USERMEM;
    }

    public void setUserBuffer(final int length, final boolean usermem) {
        if (!usermem)
            throw new IllegalArgumentException("User buffer required");
        super.setUserBuffer(length, usermem);
    }
}
