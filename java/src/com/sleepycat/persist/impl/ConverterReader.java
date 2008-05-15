/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2002,2008 Oracle.  All rights reserved.
 *
 * $Id: ConverterReader.java,v 1.1 2008/02/07 17:12:27 mark Exp $
 */

package com.sleepycat.persist.impl;

import com.sleepycat.persist.evolve.Converter;
import com.sleepycat.persist.raw.RawObject;

/**
 * Reader for invoking a class Converter mutation.
 *
 * @author Mark Hayes
 */
public class ConverterReader implements Reader {

    private static final long serialVersionUID = -305788321064984348L;

    private Converter converter;
    private transient Format oldFormat;

    ConverterReader(Converter converter) {
        this.converter = converter;
    }

    public void initializeReader(Catalog catalog,
                                 int initVersion,
                                 Format oldFormat) {
        this.oldFormat = oldFormat;
    }

    public Object newInstance(EntityInput input, boolean rawAccess) {
        /* Create the old format RawObject. */
        return oldFormat.newInstance(input, true);
    }

    public void readPriKey(Object o, EntityInput input, boolean rawAccess) {
        /* Read the old format RawObject's primary key. */
        oldFormat.readPriKey(o, input, true);
    }

    public Object readObject(Object o, EntityInput input, boolean rawAccess) {
        Catalog catalog = input.getCatalog();

        /* Read the old format RawObject and convert it. */
        o = oldFormat.readObject(o, input, true);
        o = converter.getConversion().convert(o);

        /* Convert the current format RawObject to a live Object. */
        if (!rawAccess && o instanceof RawObject) {
            o = catalog.convertRawObject((RawObject) o, null);
        }
        return o;
    }
}
