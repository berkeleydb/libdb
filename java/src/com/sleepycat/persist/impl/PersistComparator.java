/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2002,2008 Oracle.  All rights reserved.
 *
 * $Id: PersistComparator.java,v 1.1 2008/02/07 17:12:27 mark Exp $
 */

package com.sleepycat.persist.impl;

import java.io.Serializable;
import java.util.Comparator;
import java.util.List;

import com.sleepycat.persist.model.FieldMetadata;

/**
 * The btree comparator for persistent key classes.  The serialized form of
 * this comparator is stored in the BDB JE database descriptor so that the
 * comparator can be re-created during recovery.
 *
 * @author Mark Hayes
 */
public class PersistComparator implements Comparator<Object>, Serializable {

    private static final long serialVersionUID = 5221576538843355317L;

    private String keyClassName;
    private String[] comositeFieldOrder;
    private transient PersistKeyBinding binding;

    public PersistComparator(String keyClassName,
                             List<FieldMetadata> compositeKeyFields,
                             PersistKeyBinding binding) {
        this.keyClassName = keyClassName;
        this.binding = binding;

        if (compositeKeyFields != null) {
            comositeFieldOrder =
                CompositeKeyFormat.getFieldNameArray(compositeKeyFields);
        }
    }

    public int compare(Object o1, Object o2) {

        /*
         * The binding will be null after the comparator is deserialized, i.e.,
         * during BDB JE recovery.  We must construct it here, without access
         * to the stored catalog since recovery is not complete.
         */
        if (binding == null) {
            Class keyClass;
            try {
                keyClass = SimpleCatalog.classForName(keyClassName);
            } catch (ClassNotFoundException e) {
                throw new IllegalStateException(e);
            }
            binding = new PersistKeyBinding(keyClass, comositeFieldOrder);
        }

        byte[] b1 = (byte[]) o1;
        byte[] b2 = (byte[]) o2;

        Comparable k1 = (Comparable) binding.bytesToObject(b1, 0, b1.length);
        Comparable k2 = (Comparable) binding.bytesToObject(b2, 0, b2.length);

        return k1.compareTo(k2);
    }
}
