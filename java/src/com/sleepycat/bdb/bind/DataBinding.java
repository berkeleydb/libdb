/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2000-2003
 *      Sleepycat Software.  All rights reserved.
 *
 * $Id: DataBinding.java,v 1.7 2003/10/18 19:54:23 mhayes Exp $
 */

package com.sleepycat.bdb.bind;

import java.io.IOException;

/**
 * The interface implemented by all data-to-object bindings.
 *
 * @author Mark Hayes
 */
public interface DataBinding {

    /**
     * Converts a data buffer into an Object.
     *
     * @param data is the source data buffer.
     *
     * @return the resulting Object.
     */
    Object dataToObject(DataBuffer data)
        throws IOException;

    /**
     * Converts an Object into a data buffer.
     *
     * @param object is the source Object.
     *
     * @param data is the destination data buffer.
     */
    void objectToData(Object object, DataBuffer data)
        throws IOException;

    /**
     * Returns the format used for the data of this binding.
     *
     * @return the data format.
     */
    DataFormat getDataFormat();
}
