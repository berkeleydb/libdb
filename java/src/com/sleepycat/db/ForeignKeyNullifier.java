package com.sleepycat.db;

public interface ForeignKeyNullifier {
    boolean nullifyForeignKey(SecondaryDatabase secondary, DatabaseEntry data)
	    throws DatabaseException;
}
