/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2002-2003
 *	Sleepycat Software.  All rights reserved.
 *
 * $Id: ShipmentValue.java,v 1.8 2003/10/18 19:50:22 mhayes Exp $
 */

package com.sleepycat.examples.bdb.shipment.entity;

import java.io.Serializable;

/**
 * A ShipmentValue serves as the value in the key/value pair for a shipment
 * entity.
 *
 * <p> In this sample, ShipmentValue is used only as the storage data for the
 * value, while the Shipment object is used as the value's object
 * representation.  Because it is used directly as storage data using
 * SerialFormat, it must be Serializable. </p>
 *
 * @author Mark Hayes
 */
public class ShipmentValue implements Serializable {

    private int quantity;

    public ShipmentValue(int quantity) {

        this.quantity = quantity;
    }

    public final int getQuantity() {

        return quantity;
    }

    public String toString() {

        return "[ShipmentValue: quantity=" + quantity + ']';
    }
}
