/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2000,2008 Oracle.  All rights reserved.
 *
 * $Id: IOExceptionWrapper.java,v 12.7 2008/01/08 20:58:39 bostic Exp $
 */

package com.sleepycat.util;

import java.io.IOException;

/**
 * An IOException that can contain nested exceptions.
 *
 * @author Mark Hayes
 */
public class IOExceptionWrapper
    extends IOException implements ExceptionWrapper {

    private Throwable e;

    public IOExceptionWrapper(Throwable e) {

        super(e.getMessage());
        this.e = e;
    }

    /**
     * @deprecated replaced by {@link #getCause}.
     */
    public Throwable getDetail() {

        return e;
    }

    public Throwable getCause() {

        return e;
    }
}
