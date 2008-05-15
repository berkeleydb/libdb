/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2002,2008 Oracle.  All rights reserved.
 *
 * $Id: FieldInfo.java,v 1.1 2008/02/07 17:12:27 mark Exp $
 */

package com.sleepycat.persist.impl;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.sleepycat.persist.raw.RawField;

/**
 * A field definition used by ComplexFormat and CompositeKeyFormat.
 *
 * <p>Note that the equals(), compareTo() and hashCode() methods only use the
 * name field in this class.  Comparing two FieldInfo objects is only done when
 * both are declared in the same class, so comparing the field name is
 * sufficient.</p>
 *
 * @author Mark Hayes
 */
class FieldInfo implements RawField, Serializable, Comparable<FieldInfo> {

    private static final long serialVersionUID = 2062721100372306296L;

    /**
     * Returns a list of all non-transient non-static fields that are declared
     * in the given class.
     */
    static List<FieldInfo> getInstanceFields(Class cls) {
        Field[] declaredFields = cls.getDeclaredFields();
        List<FieldInfo> fields =
            new ArrayList<FieldInfo>(declaredFields.length);
        for (Field field : declaredFields) {
            int mods = field.getModifiers();
            if (!Modifier.isTransient(mods) && !Modifier.isStatic(mods)) {
                fields.add(new FieldInfo(field));
            }
        }
        return fields;
    }

    static FieldInfo getField(List<FieldInfo> fields, String fieldName) {
        int i = getFieldIndex(fields, fieldName);
        if (i >= 0) {
            return fields.get(i);
        } else {
            return null;
        }
    }

    static int getFieldIndex(List<FieldInfo> fields, String fieldName) {
        for (int i = 0; i < fields.size(); i += 1) {
            FieldInfo field = fields.get(i);
            if (fieldName.equals(field.getName())) {
                return i;
            }
        }
        return -1;
    }

    private String name;
    private String className;
    private Format format;
    private transient Class cls;

    private FieldInfo(Field field) {
        name = field.getName();
        cls = field.getType();
        className = cls.getName();
    }

    void collectRelatedFormats(Catalog catalog,
                               Map<String,Format> newFormats) {
        format = catalog.createFormat(cls, newFormats);
    }

    void migrateFromBeta(Map<String,Format> formatMap) {
        if (format == null) {
            format = formatMap.get(className);
            if (format == null) {
                throw new IllegalStateException(className);
            }
        }
    }

    void initialize(Catalog catalog, int initVersion) {
    }

    Class getFieldClass() {
        if (cls == null) {
            try {
                cls = SimpleCatalog.classForName(className);
            } catch (ClassNotFoundException e) {
                throw new IllegalStateException(e);
            }
        }
        return cls;
    }

    String getClassName() {
        return className;
    }

    public String getName() {
        return name;
    }

    public Format getType() {
        return format;
    }

    public int compareTo(FieldInfo o) {
        return name.compareTo(o.name);
    }

    @Override
    public boolean equals(Object other) {
        if (other instanceof FieldInfo) {
            FieldInfo o = (FieldInfo) other;
            return name.equals(o.name);
        } else {
            return false;
        }
    }

    @Override
    public int hashCode() {
        return name.hashCode();
    }

    @Override
    public String toString() {
        return "[Field name: " + name + " class: " + className + ']';
    }
}
