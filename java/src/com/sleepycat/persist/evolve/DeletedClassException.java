/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2002,2008 Oracle.  All rights reserved.
 *
 * $Id: DeletedClassException.java,v 1.1 2008/02/07 17:12:27 mark Exp $
 */

package com.sleepycat.persist.evolve;


/**
 * While reading from an index, an instance of a deleted class version was
 * encountered.
 *
 * @see com.sleepycat.persist.evolve Class Evolution
 * @author Mark Hayes
 */
public class DeletedClassException extends RuntimeException {

    public DeletedClassException(String msg) {
        super(msg);
    }
}
