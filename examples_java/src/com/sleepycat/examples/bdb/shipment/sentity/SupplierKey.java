/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2002-2003
 *	Sleepycat Software.  All rights reserved.
 *
 * $Id: SupplierKey.java,v 1.9 2003/10/18 19:50:37 mhayes Exp $
 */

package com.sleepycat.examples.bdb.shipment.sentity;

/**
 * A SupplierKey serves as the key in the key/value pair for a supplier entity.
 *
 * <p> In this sample, SupplierKey is bound to the key's tuple storage data
 * using a TupleBinding.  Because it is not used directly as storage data, it
 * does not need to be Serializable. </p>
 *
 * @author Mark Hayes
 */
public class SupplierKey {

    private String number;

    public SupplierKey(String number) {

        this.number = number;
    }

    public final String getNumber() {

        return number;
    }

    public String toString() {

        return "[SupplierKey: number=" + number + ']';
    }
}
