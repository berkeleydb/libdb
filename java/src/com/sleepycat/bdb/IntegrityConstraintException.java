/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2000-2003
 *      Sleepycat Software.  All rights reserved.
 *
 * $Id: IntegrityConstraintException.java,v 1.6 2003/10/18 19:54:21 mhayes Exp $
 */

package com.sleepycat.bdb;

/**
 * Thrown when an integrity constraint violation occurs.  This normally occurs
 * when an attempt is made to delete a store element, and the primary key of
 * that element is referenced by a foreign key index with a ON_DELETE_ABORT
 * delete action.  It may also occur when an internal index inconsistency is
 * detected, in processing a ForeignKeyIndex or a DataIndex.
 *
 * @see ForeignKeyIndex
 * @author Mark Hayes
 */
public class IntegrityConstraintException extends RuntimeException {

    /**
     * Creates an integrity constraint exception.
     */
    public IntegrityConstraintException(String msg) {

        super(msg);
    }
}

