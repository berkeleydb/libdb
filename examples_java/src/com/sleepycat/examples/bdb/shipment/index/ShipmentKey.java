/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2002-2003
 *	Sleepycat Software.  All rights reserved.
 *
 * $Id: ShipmentKey.java,v 1.4 2003/10/18 19:50:29 mhayes Exp $
 */

package com.sleepycat.examples.bdb.shipment.index;

import java.io.Serializable;

/**
 * A ShipmentKey serves as the key in the key/value pair for a shipment entity.
 *
 * <p> In this sample, ShipmentKey is used both as the storage data for the key
 * as well as the object binding to the key.  Because it is used directly as
 * storage data using SerialFormat, it must be Serializable. </p>
 *
 * @author Mark Hayes
 */
public class ShipmentKey implements Serializable {

    private String partNumber;
    private String supplierNumber;

    public ShipmentKey(String partNumber, String supplierNumber) {

        this.partNumber = partNumber;
        this.supplierNumber = supplierNumber;
    }

    public final String getPartNumber() {

        return partNumber;
    }

    public final String getSupplierNumber() {

        return supplierNumber;
    }

    public String toString() {

        return "[ShipmentKey: supplier=" + supplierNumber +
                " part=" + partNumber + ']';
    }
}
