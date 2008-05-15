/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2000,2008 Oracle.  All rights reserved.
 *
 * $Id: EntryBinding.java,v 12.7 2008/02/08 20:12:36 mark Exp $
 */

package com.sleepycat.bind;

import com.sleepycat.db.DatabaseEntry;

/**
 * A binding between a key or data entry and a key or data object.
 *
 * <p><em>WARNING:</em> Binding instances are typically shared by multiple
 * threads and binding methods are called without any special synchronization.
 * Therefore, bindings must be thread safe.  In general no shared state should
 * be used and any caching of computed values must be done with proper
 * synchronization.</p>
 *
 * @author Mark Hayes
 */
public interface EntryBinding {

    /**
     * Converts a entry buffer into an Object.
     *
     * @param entry is the source entry buffer.
     *
     * @return the resulting Object.
     */
    Object entryToObject(DatabaseEntry entry);

    /**
     * Converts an Object into a entry buffer.
     *
     * @param object is the source Object.
     *
     * @param entry is the destination entry buffer.
     */
    void objectToEntry(Object object, DatabaseEntry entry);
}
