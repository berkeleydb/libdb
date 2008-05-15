/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2002,2008 Oracle.  All rights reserved.
 *
 * $Id: EvolveInternal.java,v 1.1 2008/02/07 17:12:27 mark Exp $
 */

package com.sleepycat.persist.evolve;

/**
 * Internal access class that should not be used by applications.
 *
 * @author Mark Hayes
 */
public class EvolveInternal {

    /**
     * Internal access method that should not be used by applications.
     */
    public static EvolveEvent newEvent() {
        return new EvolveEvent();
    }

    /**
     * Internal access method that should not be used by applications.
     */
    public static void updateEvent(EvolveEvent event,
                                   String entityClassName,
                                   int nRead,
                                   int nConverted) {
        event.update(entityClassName);
        event.getStats().add(nRead, nConverted);
    }
}
