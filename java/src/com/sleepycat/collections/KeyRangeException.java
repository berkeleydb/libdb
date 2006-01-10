/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2000-2005
 *      Sleepycat Software.  All rights reserved.
 *
 * $Id: KeyRangeException.java,v 12.1 2005/01/31 19:27:32 mark Exp $
 */

package com.sleepycat.collections;

/**
 * An exception thrown when a key is out of range.
 *
 * @author Mark Hayes
 */
class KeyRangeException extends IllegalArgumentException {

    /**
     * Creates a key range exception.
     */
    public KeyRangeException(String msg) {

        super(msg);
    }
}
