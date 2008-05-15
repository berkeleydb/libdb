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
Replication Manager statistics for a database environment.
*/
public class ReplicationManagerStats {
    // no public constructor
    /* package */ ReplicationManagerStats() {}

    private int st_perm_failed;
    /**
    The number of times a PERM message originating at this site did
    not receive sufficient acknowledgement from clients, according to the
    configured acknowledgement policy and acknowledgement timeout..
    */
    public int getPermFailed() {
        return st_perm_failed;
    }

    private int st_msgs_queued;
    /**
    The number of messages queued due to a network delay..
    */
    public int getMsgsQueued() {
        return st_msgs_queued;
    }

    private int st_msgs_dropped;
    /**
    The number of messages discarded due to queue length overflows..
    */
    public int getMsgsDropped() {
        return st_msgs_dropped;
    }

    private int st_connection_drop;
    /**
    The number of existing connections that have been dropped since the
    statistics were last reset..
    */
    public int getConnectionDrop() {
        return st_connection_drop;
    }

    private int st_connect_fail;
    /**
    The number of times new connection attempts have failed..
    */
    public int getConnectFail() {
        return st_connect_fail;
    }

    /**
    For convenience, the ReplicationManagerStats class has a toString method
    that lists all the data fields.
    */
    public String toString() {
        return "ReplicationManagerStats:"
            + "\n  st_perm_failed=" + st_perm_failed
            + "\n  st_msgs_queued=" + st_msgs_queued
            + "\n  st_msgs_dropped=" + st_msgs_dropped
            + "\n  st_connection_drop=" + st_connection_drop
            + "\n  st_connect_fail=" + st_connect_fail
            ;
    }
}
