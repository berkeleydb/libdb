/*-
 * Automatically built by dist/s_java_stat.
 * Only the javadoc comments can be edited.
 *
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2002,2008 Oracle.  All rights reserved.
 */

package com.sleepycat.db;

/**
Lock statistics for a database environment.
*/
public class LockStats {
    // no public constructor
    /* package */ LockStats() {}

    private int st_id;
    /**
    The last allocated locker ID.
    */
    public int getId() {
        return st_id;
    }

    private int st_cur_maxid;
    public int getCurMaxId() {
        return st_cur_maxid;
    }

    private int st_maxlocks;
    /**
    The maximum number of locks possible.
    */
    public int getMaxLocks() {
        return st_maxlocks;
    }

    private int st_maxlockers;
    /**
    The maximum number of lockers possible.
    */
    public int getMaxLockers() {
        return st_maxlockers;
    }

    private int st_maxobjects;
    /**
    The maximum number of lock objects possible.
    */
    public int getMaxObjects() {
        return st_maxobjects;
    }

    private int st_partitions;
    /** 
    The number of lock table partitions.
    */
    public int getPartitions() {
        return st_partitions;
    }

    private int st_nmodes;
    /**
    The number of lock modes.
    */
    public int getNumModes() {
        return st_nmodes;
    }

    private int st_nlockers;
    /**
    The number of current lockers.
    */
    public int getNumLockers() {
        return st_nlockers;
    }

    private int st_nlocks;
    /**
    The number of current locks.
    */
    public int getNumLocks() {
        return st_nlocks;
    }

    private int st_maxnlocks;
    /**
    The maximum number of locks at any one time.  Note that if there is more than one partition, this is the sum of the maximum across all partitions.
    */
    public int getMaxNlocks() {
        return st_maxnlocks;
    }

    private int st_maxhlocks;
    /** 
    The maximum number of locks in any hash bucket at any one time.
    */
    public int getMaxHlocks() {
        return st_maxhlocks;
    }

    private int st_locksteals;
    /** 
    The maximum number of locks stolen for an empty partition. 
    */
    public int getLocksteals() {
        return st_locksteals;
    }

    private int st_maxlsteals;
    /**
    The maximum number of lock steals for any one partition. 
    */
    public int getMaxLsteals() {
        return st_maxlsteals;
    }

    private int st_maxnlockers;
    /**
    The maximum number of lockers at any one time.
    */
    public int getMaxNlockers() {
        return st_maxnlockers;
    }

    private int st_nobjects;
    /**
    The number of current lock objects.
    */
    public int getNobjects() {
        return st_nobjects;
    }

    private int st_maxnobjects;
    /**
    The maximum number of lock objects at any one time.  Note that if there is more than one partition this is the sum of the maximum across all partitions.
    */
    public int getMaxNobjects() {
        return st_maxnobjects;
    }

    private int st_maxhobjects;
    /**
    The maximum number of objects in any hash bucket at any one time.
    */
    public int getMaxHobjects() {
        return st_maxhobjects;
    }

    private int st_objectsteals;
    /**
    The maximum number of objects stolen for an empty partition.
    */
    public int getObjectsteals() {
        return st_objectsteals;
    }

    private int st_maxosteals;
    /**
    The maximum number of object steals for any one partition.
    */
    public int getMaxOsteals() {
        return st_maxosteals;
    }

    private int st_nrequests;
    /**
    The total number of locks requested.
    */
    public int getNumRequests() {
        return st_nrequests;
    }

    private int st_nreleases;
    /**
    The total number of locks released.
    */
    public int getNumReleases() {
        return st_nreleases;
    }

    private int st_nupgrade;
    /** The total number of locks upgraded. **/
    public int getNumUpgrade() {
        return st_nupgrade;
    }

    private int st_ndowngrade;
    /** The total number of locks downgraded. **/
    public int getNumDowngrade() {
        return st_ndowngrade;
    }

    private int st_lock_wait;
    /**
    The number of lock requests not immediately available due to conflicts,
    for which the thread of control waited.
    */
    public int getLockWait() {
        return st_lock_wait;
    }

    private int st_lock_nowait;
    /**
    The number of lock requests not immediately available due to conflicts,
    for which the thread of control did not wait.
    */
    public int getLockNowait() {
        return st_lock_nowait;
    }

    private int st_ndeadlocks;
    /**
    The number of deadlocks.
    */
    public int getNumDeadlocks() {
        return st_ndeadlocks;
    }

    private int st_locktimeout;
    /**
    Lock timeout value.
    */
    public int getLockTimeout() {
        return st_locktimeout;
    }

    private int st_nlocktimeouts;
    /**
    The number of lock requests that have timed out.
    */
    public int getNumLockTimeouts() {
        return st_nlocktimeouts;
    }

    private int st_txntimeout;
    /**
    Transaction timeout value.
    */
    public int getTxnTimeout() {
        return st_txntimeout;
    }

    private int st_ntxntimeouts;
    /**
    The number of transactions that have timed out.  This value is also
    a component of st_ndeadlocks, the total number of deadlocks detected.
    */
    public int getNumTxnTimeouts() {
        return st_ntxntimeouts;
    }

    private int st_part_wait;
    /** 
     The number of times that a thread of control was forced to wait before 
     obtaining a lock partition mutex.
     * */
    public int getPartWait() {
        return st_part_wait;
    }

    private int st_part_nowait;
    /** 
     The number of times that a thread of control was able to obtain a lock 
     partition mutex without waiting.
     * */
    public int getPartNowait() {
        return st_part_nowait;
    }

    private int st_part_max_wait;
    /** 
     The maximum number of times that a thread of control was forced to wait 
     before obtaining any one lock partition mutex.
     * */
    public int getPartMaxWait() {
        return st_part_max_wait;
    }

    private int st_part_max_nowait;
    /** 
     The number of times that a thread of control was able to obtain any one 
     lock partition mutex without waiting.
     * */
    public int getPartMaxNowait() {
        return st_part_max_nowait;
    }

    private int st_objs_wait;
    /**
    The number of requests to allocate or deallocate an object for which the
    thread of control waited.
    */
    public int getObjsWait() {
        return st_objs_wait;
    }

    private int st_objs_nowait;
    /**
    The number of requests to allocate or deallocate an object for which the
    thread of control did not wait.
    */
    public int getObjsNowait() {
        return st_objs_nowait;
    }

    private int st_lockers_wait;
    /**
    The number of requests to allocate or deallocate a locker for which the
    thread of control waited.
    */
    public int getLockersWait() {
        return st_lockers_wait;
    }

    private int st_lockers_nowait;
    /**
    The number of requests to allocate or deallocate a locker for which the
    thread of control did not wait.
    */
    public int getLockersNowait() {
        return st_lockers_nowait;
    }

    private int st_region_wait;
    /**
    The number of times that a thread of control was forced to wait
    before obtaining the region lock.
    */
    public int getRegionWait() {
        return st_region_wait;
    }

    private int st_region_nowait;
    /**
    The number of times that a thread of control was able to obtain the
    region lock without waiting.
    */
    public int getRegionNowait() {
        return st_region_nowait;
    }

    private int st_hash_len;
    /**
    Maximum length of a lock hash bucket.
    */
    public int getHashLen() {
        return st_hash_len;
    }

    private int st_regsize;
    /**
    The size of the lock region.
    */
    public int getRegSize() {
        return st_regsize;
    }

    /**
    For convenience, the LockStats class has a toString method
    that lists all the data fields.
    */
    public String toString() {
        return "LockStats:"
            + "\n  st_id=" + st_id
            + "\n  st_cur_maxid=" + st_cur_maxid
            + "\n  st_maxlocks=" + st_maxlocks
            + "\n  st_maxlockers=" + st_maxlockers
            + "\n  st_maxobjects=" + st_maxobjects
            + "\n  st_partitions=" + st_partitions
            + "\n  st_nmodes=" + st_nmodes
            + "\n  st_nlockers=" + st_nlockers
            + "\n  st_nlocks=" + st_nlocks
            + "\n  st_maxnlocks=" + st_maxnlocks
            + "\n  st_maxhlocks=" + st_maxhlocks
            + "\n  st_locksteals=" + st_locksteals
            + "\n  st_maxlsteals=" + st_maxlsteals
            + "\n  st_maxnlockers=" + st_maxnlockers
            + "\n  st_nobjects=" + st_nobjects
            + "\n  st_maxnobjects=" + st_maxnobjects
            + "\n  st_maxhobjects=" + st_maxhobjects
            + "\n  st_objectsteals=" + st_objectsteals
            + "\n  st_maxosteals=" + st_maxosteals
            + "\n  st_nrequests=" + st_nrequests
            + "\n  st_nreleases=" + st_nreleases
            + "\n  st_nupgrade=" + st_nupgrade
            + "\n  st_ndowngrade=" + st_ndowngrade
            + "\n  st_lock_wait=" + st_lock_wait
            + "\n  st_lock_nowait=" + st_lock_nowait
            + "\n  st_ndeadlocks=" + st_ndeadlocks
            + "\n  st_locktimeout=" + st_locktimeout
            + "\n  st_nlocktimeouts=" + st_nlocktimeouts
            + "\n  st_txntimeout=" + st_txntimeout
            + "\n  st_ntxntimeouts=" + st_ntxntimeouts
            + "\n  st_part_wait=" + st_part_wait
            + "\n  st_part_nowait=" + st_part_nowait
            + "\n  st_part_max_wait=" + st_part_max_wait
            + "\n  st_part_max_nowait=" + st_part_max_nowait
            + "\n  st_objs_wait=" + st_objs_wait
            + "\n  st_objs_nowait=" + st_objs_nowait
            + "\n  st_lockers_wait=" + st_lockers_wait
            + "\n  st_lockers_nowait=" + st_lockers_nowait
            + "\n  st_region_wait=" + st_region_wait
            + "\n  st_region_nowait=" + st_region_nowait
            + "\n  st_hash_len=" + st_hash_len
            + "\n  st_regsize=" + st_regsize
            ;
    }
}
