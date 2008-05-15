/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2002,2008 Oracle.  All rights reserved.
 *
 * $Id: EnumFormat.java,v 1.1 2008/02/07 17:12:27 mark Exp $
 */

package com.sleepycat.persist.impl;

import java.lang.reflect.Array;
import java.util.Arrays;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.sleepycat.persist.raw.RawObject;

/**
 * Format for all enum types.
 *
 * In this class we resort to using reflection to allocate arrays of enums.
 * If there is a need for it, reflection could be avoided in the future by
 * generating code as new array formats are encountered.
 *
 * @author Mark Hayes
 */
public class EnumFormat extends Format {

    private static final long serialVersionUID = 1069833955604373538L;

    private String[] names;
    private transient Object[] values;

    EnumFormat(Class type) {
        super(type);
        values = type.getEnumConstants();
        names = new String[values.length];
        for (int i = 0; i < names.length; i += 1) {
            names[i] = ((Enum) values[i]).name();
        }
    }

    @Override
    public boolean isEnum() {
        return true;
    }

    @Override
    public List<String> getEnumConstants() {
        return Arrays.asList(names);
    }

    @Override
    void collectRelatedFormats(Catalog catalog,
                               Map<String,Format> newFormats) {
    }

    @Override
    void initialize(Catalog catalog, int initVersion) {
        if (values == null) {
            Class cls = getType();
            if (cls != null) {
                values = new Object[names.length];
                for (int i = 0; i < names.length; i += 1) {
                    values[i] = Enum.valueOf(cls, names[i]);
                }
            }
        }
    }

    @Override
    Object newArray(int len) {
        return Array.newInstance(getType(), len);
    }

    @Override
    public Object newInstance(EntityInput input, boolean rawAccess) {
        int index = input.readEnumConstant(names);
        if (rawAccess) {
            return new RawObject(this, names[index]);
        } else {
            return values[index];
        }
    }

    @Override
    public Object readObject(Object o, EntityInput input, boolean rawAccess) {
        /* newInstance reads the value -- do nothing here. */
        return o;
    }

    @Override
    void writeObject(Object o, EntityOutput output, boolean rawAccess) {
        if (rawAccess) {
            String name = ((RawObject) o).getEnum();
            for (int i = 0; i < names.length; i += 1) {
                if (names[i].equals(name)) {
                    output.writeEnumConstant(names, i);
                    return;
                }
            }
        } else {
            for (int i = 0; i < values.length; i += 1) {
                if (o == values[i]) {
                    output.writeEnumConstant(names, i);
                    return;
                }
            }
        }
        throw new IllegalStateException("Bad enum: " + o);
    }

    @Override
    Object convertRawObject(Catalog catalog,
                            boolean rawAccess,
                            RawObject rawObject,
                            IdentityHashMap converted) {
        String name = rawObject.getEnum();
        for (int i = 0; i < names.length; i += 1) {
            if (names[i].equals(name)) {
                Object o = values[i];
                converted.put(rawObject, o);
                return o;
            }
        }
        throw new IllegalArgumentException
            ("Enum constant is not defined: " + name);
    }

    @Override
    void skipContents(RecordInput input) {
        input.skipFast(input.getPackedIntByteLength());
    }

    @Override
    boolean evolve(Format newFormatParam, Evolver evolver) {
        if (!(newFormatParam instanceof EnumFormat)) {
            evolver.addEvolveError
                (this, newFormatParam,
                 "Incompatible enum type changed detected",
                 "An enum class may not be changed to a non-enum type");
            /* For future:
            evolver.addMissingMutation
                (this, newFormatParam,
                 "Converter is required when an enum class is changed to " +
                 "a non-enum type");
            */
            return false;
        }
        EnumFormat newFormat = (EnumFormat) newFormatParam;
        if (Arrays.equals(names, newFormat.names)) {
            evolver.useOldFormat(this, newFormat);
            return true;
        } else {
            Set<String> oldNames = new HashSet<String>(Arrays.asList(names));
            List<String> newNames = Arrays.asList(newFormat.names);
            if (newNames.containsAll(oldNames)) {
                evolver.useEvolvedFormat(this, newFormat, newFormat);
                return true;
            } else {
                oldNames.removeAll(newNames);
                evolver.addEvolveError
                    (this, newFormat,
                     "Incompatible enum type changed detected",
                     "Enum values may not be removed: " + oldNames);
                /* For future:
                evolver.addMissingMutation
                    (this, newFormatParam,
                     "Converter is required when a value is removed from an " +
                     "enum: " + oldNames);
                */
                return false;
            }
        }
    }
}
