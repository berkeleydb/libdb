/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2000-2005
 *      Sleepycat Software.  All rights reserved.
 *
 * $Id: IOExceptionWrapper.java,v 12.2 2005/08/01 20:25:23 mark Exp $
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
