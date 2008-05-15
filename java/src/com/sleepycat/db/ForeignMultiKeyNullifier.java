package com.sleepycat.db;

public interface ForeignMultiKeyNullifier {
    boolean nullifyForeignKey(SecondaryDatabase secondary, DatabaseEntry key, DatabaseEntry data, DatabaseEntry secKey)
	    throws DatabaseException;
}
