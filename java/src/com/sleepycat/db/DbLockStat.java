/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998
 *	Sleepycat Software.  All rights reserved.
 *
 *	@(#)DbLockStat.java	1.4 (Sleepycat) 11/2/98
 */

package com.sleepycat.db;

/*
 * This is filled in and returned by the
 * DbLockTab.stat() method.
 */
public class DbLockStat
{
    public int st_magic;                // Lock file magic number.
    public int st_version;              // Lock file version number.
    public int st_maxlocks;             // Maximum number of locks in table.
    public int st_nmodes;               // Number of lock modes.
    public int st_numobjs;              // Number of objects.
    public int st_nlockers;             // Number of lockers.
    public int st_nconflicts;           // Number of lock conflicts.
    public int st_nrequests;            // Number of lock gets.
    public int st_nreleases;            // Number of lock puts.
    public int st_ndeadlocks;           // Number of lock deadlocks.
    public int st_region_wait;          // Region lock granted after wait.
    public int st_region_nowait;        // Region lock granted without wait.
    public int st_refcnt;               // Region reference count.
    public int st_regsize;              // Region size.
}

// end of DbLockStat.java
