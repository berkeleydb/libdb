/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2002,2008 Oracle.  All rights reserved.
 *
 * $Id: KeyLocation.java,v 1.1 2008/02/07 17:12:27 mark Exp $
 */

package com.sleepycat.persist.impl;

/**
 * Holder for the input and format of a key.  Used when copying secondary keys.
 * Returned by RecordInput.getKeyLocation().
 *
 * @author Mark Hayes
 */
class KeyLocation {

    RecordInput input;
    Format format;

    KeyLocation(RecordInput input, Format format) {
        this.input = input;
        this.format = format;
    }
}
