/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2002-2003
 *	Sleepycat Software.  All rights reserved.
 *
 * $Id: Part.java,v 1.8 2003/10/18 19:50:19 mhayes Exp $
 */

package com.sleepycat.examples.bdb.shipment.entity;

/**
 * A Part represents the combined key/value pair for a part entity.
 *
 * <p> In this sample, Part is created from the stored key/value data using a
 * SerialSerialBinding.  See {@link SampleViews.PartBinding} for details.  Since
 * this class is not used directly for data storage, it does not need to be
 * Serializable. </p>
 *
 * @author Mark Hayes
 */
public class Part {

    private String number;
    private String name;
    private String color;
    private Weight weight;
    private String city;

    public Part(String number, String name, String color, Weight weight,
                String city) {

        this.number = number;
        this.name = name;
        this.color = color;
        this.weight = weight;
        this.city = city;
    }

    public final String getNumber() {

        return number;
    }

    public final String getName() {

        return name;
    }

    public final String getColor() {

        return color;
    }

    public final Weight getWeight() {

        return weight;
    }

    public final String getCity() {

        return city;
    }

    public String toString() {

        return "[Part: number=" + number +
               " name=" + name +
               " color=" + color +
               " weight=" + weight +
               " city=" + city + ']';
    }
}
