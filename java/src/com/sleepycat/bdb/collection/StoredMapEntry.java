/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2000-2003
 *      Sleepycat Software.  All rights reserved.
 *
 * $Id: StoredMapEntry.java,v 1.7 2003/10/18 19:54:30 mhayes Exp $
 */

package com.sleepycat.bdb.collection;

/**
 * @author Mark Hayes
 */
final class StoredMapEntry extends MapEntry {

    private StoredIterator iter;
    private StoredCollection coll;

    StoredMapEntry(Object key, Object value, StoredCollection coll,
                   StoredIterator iter) {

        super(key, value);
        // Assert: coll, coll.keyBinding/valueBinding
        this.coll = coll;
        this.iter = iter;
    }

    public Object setValue(Object newValue) {

        Object oldValue;
        if (iter != null && iter.isCurrentData(this)) {
            oldValue = getValue();
            iter.set(newValue);
        } else {
            oldValue = coll.put(getKey(), newValue);
        }
        super.setValue(newValue);
        return oldValue;
    }
}
