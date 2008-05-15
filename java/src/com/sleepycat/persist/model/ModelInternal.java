/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2002,2008 Oracle.  All rights reserved.
 *
 * $Id: ModelInternal.java,v 1.1 2008/02/07 17:12:28 mark Exp $
 */

package com.sleepycat.persist.model;

import com.sleepycat.persist.impl.PersistCatalog;

/**
 * Internal access class that should not be used by applications.
 *
 * @author Mark Hayes
 */
public class ModelInternal {

    /**
     * Internal access method that should not be used by applications.
     */
    public static void setCatalog(EntityModel model, PersistCatalog catalog) {
        model.setCatalog(catalog);
    }
}
