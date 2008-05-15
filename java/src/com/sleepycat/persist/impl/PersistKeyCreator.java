/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2002,2008 Oracle.  All rights reserved.
 *
 * $Id: PersistKeyCreator.java,v 1.1 2008/02/07 17:12:27 mark Exp $
 */

package com.sleepycat.persist.impl;

import java.util.Collection;
import java.util.Set;

import com.sleepycat.bind.tuple.TupleBase;
import com.sleepycat.db.DatabaseEntry;
import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.ForeignMultiKeyNullifier;
import com.sleepycat.db.SecondaryDatabase;
import com.sleepycat.db.SecondaryKeyCreator;
import com.sleepycat.db.SecondaryMultiKeyCreator;
import com.sleepycat.persist.model.EntityMetadata;
import com.sleepycat.persist.model.Relationship;
import com.sleepycat.persist.model.SecondaryKeyMetadata;
import com.sleepycat.persist.raw.RawObject;

/**
 * A persistence secondary key creator/nullifier.  This class always uses
 * rawAccess=true to avoid depending on the presence of the proxy class.
 *
 * @author Mark Hayes
 */
public class PersistKeyCreator implements SecondaryKeyCreator,
                                          SecondaryMultiKeyCreator,
                                          ForeignMultiKeyNullifier {

    static boolean isManyType(Class cls) {
        return cls.isArray() || Collection.class.isAssignableFrom(cls);
    }

    private Catalog catalog;
    private int priKeyFormatId;
    private String keyName;
    private Format keyFormat;
    private boolean toMany;

    /**
     * Creates a key creator/nullifier for a given entity class and key name.
     */
    public PersistKeyCreator(Catalog catalog,
                             EntityMetadata entityMeta,
                             String keyClassName,
                             SecondaryKeyMetadata secKeyMeta) {
        this.catalog = catalog;
        Format priKeyFormat =
            catalog.getFormat(entityMeta.getPrimaryKey().getClassName());
        priKeyFormatId = priKeyFormat.getId();
        keyName = secKeyMeta.getKeyName();
        keyFormat = catalog.getFormat(keyClassName);
        if (keyFormat == null) {
            throw new IllegalArgumentException
                ("Not a key class: " + keyClassName);
        }
        if (keyFormat.isPrimitive()) {
            throw new IllegalArgumentException
                ("Use a primitive wrapper class instead of class: " +
                 keyFormat.getClassName());
        }
        Relationship rel = secKeyMeta.getRelationship();
        toMany = (rel == Relationship.ONE_TO_MANY ||
                  rel == Relationship.MANY_TO_MANY);
    }

    public boolean createSecondaryKey(SecondaryDatabase secondary,
				      DatabaseEntry key,
				      DatabaseEntry data,
				      DatabaseEntry result)
	throws DatabaseException {

        if (toMany) {
            throw new IllegalStateException();
        }
        KeyLocation loc = moveToKey(key, data);
        if (loc != null) {
            RecordOutput output = new RecordOutput
                (catalog, true /*rawAccess*/);
            loc.format.copySecKey(loc.input, output);
            TupleBase.outputToEntry(output, result);
            return true;
        } else {
            /* Key field is not present or null. */
            return false;
        }
    }

    public void createSecondaryKeys(SecondaryDatabase secondary,
				    DatabaseEntry key,
				    DatabaseEntry data,
				    Set results)
	throws DatabaseException {

        if (!toMany) {
            throw new IllegalStateException();
        }
        KeyLocation loc = moveToKey(key, data);
        if (loc != null) {
            loc.format.copySecMultiKey(loc.input, keyFormat, results);
        }
        /* Else key field is not present or null. */
    }

    public boolean nullifyForeignKey(SecondaryDatabase secondary,
                                     DatabaseEntry key,
                                     DatabaseEntry data,
                                     DatabaseEntry secKey)
	throws DatabaseException {

        /* Deserialize the entity and get its current class format. */
        RawObject entity = (RawObject) PersistEntityBinding.readEntity
            (catalog, key, data, true /*rawAccess*/);
        Format entityFormat = (Format) entity.getType();

        /*
         * Set the key to null.  For a TO_MANY key, pass the key object to be
         * removed from the array/collection.
         */
        Object secKeyObject = null;
        if (toMany) {
            secKeyObject = PersistKeyBinding.readKey
                (keyFormat, catalog, secKey.getData(), secKey.getOffset(),
                 secKey.getSize(), true /*rawAccess*/);
        }
        if (entityFormat.nullifySecKey
            (catalog, entity, keyName, secKeyObject)) {

            /*
             * Using the current format for the entity, serialize the modified
             * entity back to the data entry.
             */
            PersistEntityBinding.writeEntity
                (entityFormat, catalog, entity, data, true /*rawAccess*/);
            return true;
        } else {
            /* Key field is not present or null. */
            return false;
        }
    }

    /**
     * Returns the location from which the secondary key field can be copied.
     */
    private KeyLocation moveToKey(DatabaseEntry priKey, DatabaseEntry data) {

        RecordInput input = new RecordInput
            (catalog, true /*rawAccess*/, priKey, priKeyFormatId,
             data.getData(), data.getOffset(), data.getSize());
        int formatId = input.readPackedInt();
        Format entityFormat = catalog.getFormat(formatId);
        Format fieldFormat = entityFormat.skipToSecKey(input, keyName);
        if (fieldFormat != null) {
            /* Returns null if key field is null. */
            return input.getKeyLocation(fieldFormat);
        } else {
            /* Key field is not present in this class. */
            return null;
        }
    }
}
