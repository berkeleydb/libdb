/*-
 * Automatically built by dist/s_java_stat.
 * Only the javadoc comments can be edited.
 *
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2002,2008 Oracle.  All rights reserved.
 */

package com.sleepycat.db;

import com.sleepycat.db.internal.DbUtil;

/**
Transaction statistics for a database environment.
*/
public class TransactionStats {
    // no public constructor
    /* package */ TransactionStats() {}

    public static class Active {
        // no public constructor
        /* package */ Active() {}

        private int txnid;
        /**
    The transaction ID of the transaction.
    */
    public int getTxnId() {
            return txnid;
        }

        private int parentid;
        /**
    The transaction ID of the parent transaction (or 0, if no parent).
    */
    public int getParentId() {
            return parentid;
        }

        private int pid;
        /** The process ID of the process that owns the transaction. **/
    public int getPid() {
            return pid;
        }

        private LogSequenceNumber lsn;
        /**
    The log sequence number of the transaction's first log record.
    */
    public LogSequenceNumber getLsn() {
            return lsn;
        }

        private LogSequenceNumber read_lsn;
        /**
    The log sequence number of reads for snapshot transactions.
    */
    public LogSequenceNumber getReadLsn() {
            return read_lsn;
        }

        private int mvcc_ref;
        /**
    The number of buffer copies created by this transaction that remain in
    cache.
    */
    public int getMultiversionRef() {
            return mvcc_ref;
        }

        private int status;
        /**
    The status of the transaction.
    */
    public int getStatus() {
            return status;
        }

        private int xa_status;
        /**
    If the transaction is an XA transaction, the status of the
    transaction, otherwise 0.
    */
    public int getXaStatus() {
            return xa_status;
        }

        private byte[] xid;
        public byte[] getXId() {
            return xid;
        }

        private String name;
        /**
        The transaction name, including the thread name if available.
        */
        public String getName() {
            return name;
        }

        /** {@inheritDoc} */
    public String toString() {
            return "Active:"
                + "\n      txnid=" + txnid
                + "\n      parentid=" + parentid
                + "\n      pid=" + pid
                + "\n      lsn=" + lsn
                + "\n      read_lsn=" + read_lsn
                + "\n      mvcc_ref=" + mvcc_ref
                + "\n      status=" + status
                + "\n      xa_status=" + xa_status
                + "\n      xid=" + DbUtil.byteArrayToString(xid)
                + "\n      name=" + name
                ;
        }
    };

    private int st_nrestores;
    /**
    The number of transactions that have been restored.
    */
    public int getNumRestores() {
        return st_nrestores;
    }

    private LogSequenceNumber st_last_ckp;
    /**
    The LSN of the last checkpoint.
    */
    public LogSequenceNumber getLastCkp() {
        return st_last_ckp;
    }

    private long st_time_ckp;
    /**
    The time the last completed checkpoint finished (as the number of
    seconds since the Epoch, returned by the IEEE/ANSI Std 1003.1
    (POSIX) time interface).
    */
    public long getTimeCkp() {
        return st_time_ckp;
    }

    private int st_last_txnid;
    /**
    The last transaction ID allocated.
    */
    public int getLastTxnId() {
        return st_last_txnid;
    }

    private int st_maxtxns;
    /**
    The maximum number of active transactions configured.
    */
    public int getMaxTxns() {
        return st_maxtxns;
    }

    private int st_naborts;
    /**
    The number of transactions that have aborted.
    */
    public int getNaborts() {
        return st_naborts;
    }

    private int st_nbegins;
    /**
    The number of transactions that have begun.
    */
    public int getNumBegins() {
        return st_nbegins;
    }

    private int st_ncommits;
    /**
    The number of transactions that have committed.
    */
    public int getNumCommits() {
        return st_ncommits;
    }

    private int st_nactive;
    /**
    The number of transactions that are currently active.
    */
    public int getNactive() {
        return st_nactive;
    }

    private int st_nsnapshot;
    /**
    The number of transactions on the snapshot list.  These are transactions
    which modified a database opened with {@link
    DatabaseConfig#setMultiversion}, and which have committed or aborted, but
    the copies of pages they created are still in the cache.
    */
    public int getNumSnapshot() {
        return st_nsnapshot;
    }

    private int st_maxnactive;
    /**
    The maximum number of active transactions at any one time.
    */
    public int getMaxNactive() {
        return st_maxnactive;
    }

    private int st_maxnsnapshot;
    /**
    The maximum number of transactions on the snapshot list at any one time.
    */
    public int getMaxNsnapshot() {
        return st_maxnsnapshot;
    }

    private Active[] st_txnarray;
    public Active[] getTxnarray() {
        return st_txnarray;
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

    private int st_regsize;
    /**
    The size of the region.
    */
    public int getRegSize() {
        return st_regsize;
    }

    /** {@inheritDoc} */
    public String toString() {
        return "TransactionStats:"
            + "\n  st_nrestores=" + st_nrestores
            + "\n  st_last_ckp=" + st_last_ckp
            + "\n  st_time_ckp=" + st_time_ckp
            + "\n  st_last_txnid=" + st_last_txnid
            + "\n  st_maxtxns=" + st_maxtxns
            + "\n  st_naborts=" + st_naborts
            + "\n  st_nbegins=" + st_nbegins
            + "\n  st_ncommits=" + st_ncommits
            + "\n  st_nactive=" + st_nactive
            + "\n  st_nsnapshot=" + st_nsnapshot
            + "\n  st_maxnactive=" + st_maxnactive
            + "\n  st_maxnsnapshot=" + st_maxnsnapshot
            + "\n  st_txnarray=" + DbUtil.objectArrayToString(st_txnarray, "st_txnarray")
            + "\n  st_region_wait=" + st_region_wait
            + "\n  st_region_nowait=" + st_region_nowait
            + "\n  st_regsize=" + st_regsize
            ;
    }
}
