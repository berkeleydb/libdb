/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 *
 *	@(#)DbLockStat.java	11.1 (Sleepycat) 7/25/99
 */

package com.sleepycat.db;

/*
 * This is filled in and returned by the
 * DbLockTab.stat() method.
 */
public class DbLockStat
{
    public int st_maxlocks;             // Maximum number of locks in table.
    public int st_nmodes;               // Number of lock modes.
    public int st_nlockers;             // Number of lockers.
    public int st_nconflicts;           // Number of lock conflicts.
    public int st_nrequests;            // Number of lock gets.
    public int st_nreleases;            // Number of lock puts.
    public int st_ndeadlocks;           // Number of lock deadlocks.
    public int st_region_wait;          // Region lock granted after wait.
    public int st_region_nowait;        // Region lock granted without wait.
    public int st_regsize;              // Region size.
}

// end of DbLockStat.java
