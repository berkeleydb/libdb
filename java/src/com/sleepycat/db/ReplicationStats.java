/*-
 * Automatically built by dist/s_java_stat.
 * Only the javadoc comments can be edited.
 *
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2002,2008 Oracle.  All rights reserved.
 */

package com.sleepycat.db;

import com.sleepycat.db.internal.DbConstants;

/**
Replication statistics for a database environment.
*/
public class ReplicationStats {
    // no public constructor
    /* package */ ReplicationStats() {}
    /**
    The environment is configured as a replication client, as reported by {@link #getStatus}.
    */
    public static final int REP_CLIENT = DbConstants.DB_REP_CLIENT;

    /**
    The environment is configured as a replication master, as reported by {@link #getStatus}.
    */
    public static final int REP_MASTER = DbConstants.DB_REP_MASTER;

    /**
    Replication is not configured for this environment, as reported by {@link #getStatus}.
    */
    public static final int REP_NONE = 0;

    private int st_log_queued;
    /**
    The number of log records currently queued.
    */
    public int getLogQueued() {
        return st_log_queued;
    }

    private int st_startup_complete;
    /**
    The client site has completed its startup procedures and is now
    handling live records from the master.
    */
    public boolean getStartupComplete() {
        return (st_startup_complete != 0);
    }

    private int st_status;
    /**
    The current replication mode.  Set to REP_MASTER if the environment
    is a replication master, REP_CLIENT if the environment is a
    replication client, or REP_NONE if replication is not configured.
    */
    public int getStatus() {
        return st_status;
    }

    private LogSequenceNumber st_next_lsn;
    /**
    In replication environments configured as masters, the next LSN
    expected.  In replication environments configured as clients, the next
    LSN to be used.
    */
    public LogSequenceNumber getNextLsn() {
        return st_next_lsn;
    }

    private LogSequenceNumber st_waiting_lsn;
    /**
    The LSN of the first log record we have after missing log records
    being waited for, or 0 if no log records are currently missing.
    */
    public LogSequenceNumber getWaitingLsn() {
        return st_waiting_lsn;
    }

    private LogSequenceNumber st_max_perm_lsn;
    /**
    The LSN of the maximum permanent log record, or 0 if there are no
    permanent log records.
    */
    public LogSequenceNumber getMaxPermLsn() {
        return st_max_perm_lsn;
    }

    private int st_next_pg;
    /**
    The next page number we expect to receive.
    */
    public int getNextPages() {
        return st_next_pg;
    }

    private int st_waiting_pg;
    /**
    The page number of the first page we have after missing pages being
    waited for, or 0 if no pages are currently missing.
    */
    public int getWaitingPages() {
        return st_waiting_pg;
    }

    private int st_dupmasters;
    /**
    The number of duplicate master conditions detected.
    */
    public int getDupmasters() {
        return st_dupmasters;
    }

    private int st_env_id;
    /**
    The current environment ID.
    */
    public int getEnvId() {
        return st_env_id;
    }

    private int st_env_priority;
    /**
    The current environment priority.
    */
    public int getEnvPriority() {
        return st_env_priority;
    }

    private int st_bulk_fills;
    /**
    The number of times the bulk buffer filled up, forcing the buffer content
    to be sent.
    **/
    public int getBulkFills() {
        return st_bulk_fills;
    }

    private int st_bulk_overflows;
    /**
    The number of times a record was bigger than the entire bulk buffer,
    and therefore had to be sent as a singleton.
    **/
    public int getBulkOverflows() {
        return st_bulk_overflows;
    }

    private int st_bulk_records;
    /** The number of records added to a bulk buffer.**/
    public int getBulkRecords() {
        return st_bulk_records;
    }

    private int st_bulk_transfers;
    /**
    The number of bulk buffers transferred (via a call to the
    application's {@link ReplicationTransport#send} method).
    **/
    public int getBulkTransfers() {
        return st_bulk_transfers;
    }

    private int st_client_rerequests;
    /**
    The number of times this client site received a "re-request" message,
    indicating that a request it previously sent to another client could
    not be serviced by that client.  (Compare to {@link #getClientSvcMiss}.)
    **/
    public int getClientRerequests() {
        return st_client_rerequests;
    }

    private int st_client_svc_req;
    /**
    The number of "request" type messages received by this client.
    ("Request" messages are usually sent from a client to the master, but
    a message marked with <code>anywhere = true</code> in the
    invocation of the application's {@link ReplicationTransport#send}
    method may be sent to another client instead.)
    **/
    public int getClientSvcReq() {
        return st_client_svc_req;
    }

    private int st_client_svc_miss;
    /**
    The number of "request" type messages received by this client that
    could not be processed, forcing the originating requestor to try
    sending the request to the master (or another client).
    **/
    public int getClientSvcMiss() {
        return st_client_svc_miss;
    }

    private int st_gen;
    /**
    The current generation number.
    */
    public int getGen() {
        return st_gen;
    }

    private int st_egen;
    /**
    The current election generation number.
    */
    public int getEgen() {
        return st_egen;
    }

    private int st_log_duplicated;
    /**
    The number of duplicate log records received.
    */
    public int getLogDuplicated() {
        return st_log_duplicated;
    }

    private int st_log_queued_max;
    /**
    The maximum number of log records ever queued at once.
    */
    public int getLogQueuedMax() {
        return st_log_queued_max;
    }

    private int st_log_queued_total;
    /**
    The total number of log records queued.
    */
    public int getLogQueuedTotal() {
        return st_log_queued_total;
    }

    private int st_log_records;
    /**
    The number of log records received and appended to the log.
    */
    public int getLogRecords() {
        return st_log_records;
    }

    private int st_log_requested;
    /**
    The number of log records missed and requested.
    */
    public int getLogRequested() {
        return st_log_requested;
    }

    private int st_master;
    /**
    The current master environment ID.
    */
    public int getMaster() {
        return st_master;
    }

    private int st_master_changes;
    /**
    The number of times the master has changed.
    */
    public int getMasterChanges() {
        return st_master_changes;
    }

    private int st_msgs_badgen;
    /**
    The number of messages received with a bad generation number.
    */
    public int getMsgsBadgen() {
        return st_msgs_badgen;
    }

    private int st_msgs_processed;
    /**
    The number of messages received and processed.
    */
    public int getMsgsProcessed() {
        return st_msgs_processed;
    }

    private int st_msgs_recover;
    /**
    The number of messages ignored due to pending recovery.
    */
    public int getMsgsRecover() {
        return st_msgs_recover;
    }

    private int st_msgs_send_failures;
    /**
    The number of failed message sends.
    */
    public int getMsgsSendFailures() {
        return st_msgs_send_failures;
    }

    private int st_msgs_sent;
    /**
    The number of messages sent.
    */
    public int getMsgsSent() {
        return st_msgs_sent;
    }

    private int st_newsites;
    /**
    The number of new site messages received.
    */
    public int getNewsites() {
        return st_newsites;
    }

    private int st_nsites;
    /**
    The number of sites believed to be in the replication group.
    */
    public int getNumSites() {
        return st_nsites;
    }

    private int st_nthrottles;
    /**
    Transmission limited. This indicates the number of times data
    transmission was stopped to limit the amount of data sent in response to
    a single call to {@link com.sleepycat.db.Environment#processReplicationMessage Environment.processReplicationMessage}.
    */
    public int getNumThrottles() {
        return st_nthrottles;
    }

    private int st_outdated;
    /**
    The number of outdated conditions detected.
    */
    public int getOutdated() {
        return st_outdated;
    }

    private int st_pg_duplicated;
    /**
    The number of duplicate pages received.
    */
    public int getPagesDuplicated() {
        return st_pg_duplicated;
    }

    private int st_pg_records;
    /**
    The number of pages received and stored.
    */
    public int getPagesRecords() {
        return st_pg_records;
    }

    private int st_pg_requested;
    /**
    The number of pages missed and requested from the master.
    */
    public int getPagesRequested() {
        return st_pg_requested;
    }

    private int st_txns_applied;
    /**
    The number of transactions applied.
    */
    public int getTxnsApplied() {
        return st_txns_applied;
    }

    private int st_startsync_delayed;
    /**
    The number of times the client had to delay the start of a cache flush
    operation (initiated by the master for an impending checkpoint) because it
    was missing some previous log record(s).
    */
    public int getStartSyncDelayed() {
        return st_startsync_delayed;
    }

    private int st_elections;
    /**
    The number of elections held.
    */
    public int getElections() {
        return st_elections;
    }

    private int st_elections_won;
    /**
    The number of elections won.
    */
    public int getElectionsWon() {
        return st_elections_won;
    }

    private int st_election_cur_winner;
    /**
    The election winner.
    */
    public int getElectionCurWinner() {
        return st_election_cur_winner;
    }

    private int st_election_gen;
    /**
    The election generation number.
    */
    public int getElectionGen() {
        return st_election_gen;
    }

    private LogSequenceNumber st_election_lsn;
    /**
    The maximum LSN of election winner.
    */
    public LogSequenceNumber getElectionLsn() {
        return st_election_lsn;
    }

    private int st_election_nsites;
    /**
    The number sites expected to participate in elections.
    */
    public int getElectionNumSites() {
        return st_election_nsites;
    }

    private int st_election_nvotes;
    /**
    The number of votes required to complete the election.
    */
    public int getElectionNumVotes() {
        return st_election_nvotes;
    }

    private int st_election_priority;
    /**
    The election priority.
    */
    public int getElectionPriority() {
        return st_election_priority;
    }

    private int st_election_status;
    /**
    The current election phase (0 if no election is in progress).
    */
    public int getElectionStatus() {
        return st_election_status;
    }

    private int st_election_tiebreaker;
    /**
    The election tiebreaker value.
    */
    public int getElectionTiebreaker() {
        return st_election_tiebreaker;
    }

    private int st_election_votes;
    /**
    The votes received this election round.
    */
    public int getElectionVotes() {
        return st_election_votes;
    }

    private int st_election_sec;
    /**
    The number of seconds the last election took (the total election
    time is this plus {@link #getElectionUsec}).
    **/
    public int getElectionSec() {
        return st_election_sec;
    }

    private int st_election_usec;
    /**
    The number of microseconds the last election took (the total election
    time is this plus {@link #getElectionSec}).
    **/
    public int getElectionUsec() {
        return st_election_usec;
    }

    private int st_max_lease_sec;
    /** 
    The number of seconds of the longest lease (the total lease time
    is this plus {@link #getMaxLeaseUsec}).
    */
    public int getMaxLeaseSec() {
        return st_max_lease_sec;
    }

    private int st_max_lease_usec;
    /** 
    The number of microseconds of the longest lease (the total lease
    time is this plus {@link #getMaxLeaseSec}).
    */
    public int getMaxLeaseUsec() {
        return st_max_lease_usec;
    }

    /**
    For convenience, the ReplicationStats class has a toString method
    that lists all the data fields.
    */
    public String toString() {
        return "ReplicationStats:"
            + "\n  st_log_queued=" + st_log_queued
            + "\n  st_startup_complete=" + (st_startup_complete != 0)
            + "\n  st_status=" + st_status
            + "\n  st_next_lsn=" + st_next_lsn
            + "\n  st_waiting_lsn=" + st_waiting_lsn
            + "\n  st_max_perm_lsn=" + st_max_perm_lsn
            + "\n  st_next_pg=" + st_next_pg
            + "\n  st_waiting_pg=" + st_waiting_pg
            + "\n  st_dupmasters=" + st_dupmasters
            + "\n  st_env_id=" + st_env_id
            + "\n  st_env_priority=" + st_env_priority
            + "\n  st_bulk_fills=" + st_bulk_fills
            + "\n  st_bulk_overflows=" + st_bulk_overflows
            + "\n  st_bulk_records=" + st_bulk_records
            + "\n  st_bulk_transfers=" + st_bulk_transfers
            + "\n  st_client_rerequests=" + st_client_rerequests
            + "\n  st_client_svc_req=" + st_client_svc_req
            + "\n  st_client_svc_miss=" + st_client_svc_miss
            + "\n  st_gen=" + st_gen
            + "\n  st_egen=" + st_egen
            + "\n  st_log_duplicated=" + st_log_duplicated
            + "\n  st_log_queued_max=" + st_log_queued_max
            + "\n  st_log_queued_total=" + st_log_queued_total
            + "\n  st_log_records=" + st_log_records
            + "\n  st_log_requested=" + st_log_requested
            + "\n  st_master=" + st_master
            + "\n  st_master_changes=" + st_master_changes
            + "\n  st_msgs_badgen=" + st_msgs_badgen
            + "\n  st_msgs_processed=" + st_msgs_processed
            + "\n  st_msgs_recover=" + st_msgs_recover
            + "\n  st_msgs_send_failures=" + st_msgs_send_failures
            + "\n  st_msgs_sent=" + st_msgs_sent
            + "\n  st_newsites=" + st_newsites
            + "\n  st_nsites=" + st_nsites
            + "\n  st_nthrottles=" + st_nthrottles
            + "\n  st_outdated=" + st_outdated
            + "\n  st_pg_duplicated=" + st_pg_duplicated
            + "\n  st_pg_records=" + st_pg_records
            + "\n  st_pg_requested=" + st_pg_requested
            + "\n  st_txns_applied=" + st_txns_applied
            + "\n  st_startsync_delayed=" + st_startsync_delayed
            + "\n  st_elections=" + st_elections
            + "\n  st_elections_won=" + st_elections_won
            + "\n  st_election_cur_winner=" + st_election_cur_winner
            + "\n  st_election_gen=" + st_election_gen
            + "\n  st_election_lsn=" + st_election_lsn
            + "\n  st_election_nsites=" + st_election_nsites
            + "\n  st_election_nvotes=" + st_election_nvotes
            + "\n  st_election_priority=" + st_election_priority
            + "\n  st_election_status=" + st_election_status
            + "\n  st_election_tiebreaker=" + st_election_tiebreaker
            + "\n  st_election_votes=" + st_election_votes
            + "\n  st_election_sec=" + st_election_sec
            + "\n  st_election_usec=" + st_election_usec
            + "\n  st_max_lease_sec=" + st_max_lease_sec
            + "\n  st_max_lease_usec=" + st_max_lease_usec
            ;
    }
}
