/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2002, 2011 Oracle and/or its affiliates.  All rights reserved.
 *
 */

package com.sleepycat.bind.serial.test;

import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.ObjectStreamClass;
import java.util.HashMap;

import com.sleepycat.bind.serial.ClassCatalog;
import com.sleepycat.compat.DbCompat;
import com.sleepycat.db.DatabaseException;
import com.sleepycat.util.FastInputStream;
import com.sleepycat.util.FastOutputStream;
import com.sleepycat.util.RuntimeExceptionWrapper;

/**
 * @author Mark Hayes
 */
public class TestClassCatalog implements ClassCatalog {

    private final HashMap idToDescMap = new HashMap();
    private final HashMap nameToIdMap = new HashMap();
    private int nextId = 1;

    public TestClassCatalog() {
    }

    public void close() {
    }

    public synchronized byte[] getClassID(ObjectStreamClass desc) {
        String className = desc.getName();
        byte[] id = (byte[]) nameToIdMap.get(className);
        if (id == null) {
            String strId = String.valueOf(nextId);
            id = strId.getBytes();
            nextId += 1;

            idToDescMap.put(strId, desc);
            nameToIdMap.put(className, id);
        }
        return id;
    }

    public synchronized ObjectStreamClass getClassFormat(byte[] id)
        throws DatabaseException {

        String strId = new String(id);
        ObjectStreamClass desc = (ObjectStreamClass) idToDescMap.get(strId);
        if (desc == null) {
            throw new RuntimeException("classID not found");
        }
        
        /*
         * Workaround for a Harmony bug that appears on Android.  The
         * ObjectStreamClass is not properly initialized, and using it later
         * will cause NullPointerException.  Serializing it and then
         * deserializing it causes is to be initialized properly.  [#18163]
         */
        if (DbCompat.isDalvik()) {
            try {
                /* Serialize desc first. */
                FastOutputStream fo = new FastOutputStream();
                ObjectOutputStream oos = new ObjectOutputStream(fo);
                oos.writeObject(desc);
                byte[] bytes = fo.toByteArray();
                /* Then deserialize classFormat. */
                FastInputStream fi = new FastInputStream(bytes);
                ObjectInputStream ois = new ObjectInputStream(fi);
                desc = (ObjectStreamClass) ois.readObject();
            } catch (Exception e) {
                throw RuntimeExceptionWrapper.wrapIfNeeded(e);
            }
        }
        return desc;
    }
}
