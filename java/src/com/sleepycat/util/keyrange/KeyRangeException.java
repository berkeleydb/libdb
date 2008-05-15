/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2000,2008 Oracle.  All rights reserved.
 *
 * $Id: KeyRangeException.java,v 1.5 2008/01/08 20:58:39 bostic Exp $
 */

package com.sleepycat.util.keyrange;

/**
 * An exception thrown when a key is out of range.
 *
 * @author Mark Hayes
 */
public class KeyRangeException extends IllegalArgumentException {

    /**
     * Creates a key range exception.
     */
    public KeyRangeException(String msg) {

        super(msg);
    }
}
