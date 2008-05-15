/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2000,2008 Oracle.  All rights reserved.
 *
 * $Id: EntityBinding.java,v 12.7 2008/02/08 20:12:36 mark Exp $
 */

package com.sleepycat.bind;

import com.sleepycat.db.DatabaseEntry;

/**
 * A binding between a key-value entry pair and an entity object.
 *
 * <p><em>WARNING:</em> Binding instances are typically shared by multiple
 * threads and binding methods are called without any special synchronization.
 * Therefore, bindings must be thread safe.  In general no shared state should
 * be used and any caching of computed values must be done with proper
 * synchronization.</p>
 *
 * @author Mark Hayes
 */
public interface EntityBinding {

    /**
     * Converts key and data entry buffers into an entity Object.
     *
     * @param key is the source key entry.
     *
     * @param data is the source data entry.
     *
     * @return the resulting Object.
     */
    Object entryToObject(DatabaseEntry key, DatabaseEntry data);

    /**
     * Extracts the key entry from an entity Object.
     *
     * @param object is the source Object.
     *
     * @param key is the destination entry buffer.
     */
    void objectToKey(Object object, DatabaseEntry key);

    /**
     * Extracts the data entry from an entity Object.
     *
     * @param object is the source Object.
     *
     * @param data is the destination entry buffer.
     */
    void objectToData(Object object, DatabaseEntry data);
}
