/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2002-2003
 *	Sleepycat Software.  All rights reserved.
 *
 * $Id: PartKey.java,v 1.9 2003/10/18 19:50:38 mhayes Exp $
 */

package com.sleepycat.examples.bdb.shipment.tuple;

/**
 * A PartKey serves as the key in the key/value pair for a part entity.
 *
 * <p> In this sample, PartKey is bound to the key's tuple storage data using
 * a TupleBinding.  Because it is not used directly as storage data, it does
 * not need to be Serializable. </p>
 *
 * @author Mark Hayes
 */
public class PartKey {

    private String number;

    public PartKey(String number) {

        this.number = number;
    }

    public final String getNumber() {

        return number;
    }

    public String toString() {

        return "[PartKey: number=" + number + ']';
    }
}
