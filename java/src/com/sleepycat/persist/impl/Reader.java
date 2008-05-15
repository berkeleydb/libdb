/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2002,2008 Oracle.  All rights reserved.
 *
 * $Id: Reader.java,v 1.1 2008/02/07 17:12:27 mark Exp $
 */

package com.sleepycat.persist.impl;

import java.io.Serializable;

/**
 * Interface to the "read object" methods of the Format class.  For the
 * latest version format, the Format object provides the implementation of
 * these methods.  For an older version format, an evolver object implements
 * this interface to convert from the old to new format.
 *
 * See {@link Format} for a description of each method.
 * @author Mark Hayes
 */
interface Reader extends Serializable {

    void initializeReader(Catalog catalog, int initVersion, Format oldFormat);

    Object newInstance(EntityInput input, boolean rawAccess);

    void readPriKey(Object o, EntityInput input, boolean rawAccess);

    Object readObject(Object o, EntityInput input, boolean rawAccess);
}
