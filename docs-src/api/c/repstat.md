---
title: "DB_ENV->rep_stat()"
api-name: "DB_ENV->rep_stat()"
source: docs/api_reference/C/repstat.html
---
## DB_ENV-\>rep_stat()

``` c
#include <db.h>

int
DB_ENV->rep_stat(DB_ENV *env, DB_REP_STAT **statp, u_int32_t flags);  
```

The `DB_ENV->rep_stat()` method returns the replication subsystem statistics.

The `DB_ENV->rep_stat()` method creates a statistical structure of type `DB_REP_STAT` and copies a pointer to it into a user-specified memory location.

Statistical structures are stored in allocated memory. If application-specific allocation routines have been declared (see <a href="envset_alloc.md" class="xref" title="DB_ENV-&gt;set_alloc()">DB_ENV-&gt;set_alloc()</a> for more information), they are used to allocate the memory; otherwise, the standard C library **malloc**(3) is used. The caller is responsible for deallocating the memory. To deallocate the memory, free the memory reference; references inside the returned memory need not be individually freed.

The following `DB_REP_STAT` fields will be filled in:

- **uintmax_t st_bulk_fills;**

  The number of times the bulk buffer filled up, forcing the buffer content to be sent.

- **uintmax_t st_bulk_overflows;**

  The number of times a record was bigger than the entire bulk buffer, and therefore had to be sent as a singleton.

- **uintmax_t st_bulk_records;**

  The number of records added to a bulk buffer.

- **uintmax_t st_bulk_transfers;**

  The number of bulk buffers transferred (via a call to the application's **send** function).

- **uintmax_t st_client_rerequests;**

  The number of times this client site received a "re-request" message, indicating that a request it previously sent to another client could not be serviced by that client. (Compare to **st_client_svc_miss**.)

- **uintmax_t st_client_svc_miss;**

  The number of "request" type messages received by this client that could not be processed, forcing the originating requester to try sending the request to the master (or another client).

- **uintmax_t st_client_svc_req;**

  The number of "request" type messages received by this client. ("Request" messages are usually sent from a client to the master, but a message marked with the <a href="reptransport.md#transport_DB_REP_ANYWHERE" class="link">DB_REP_ANYWHERE</a> flag in the invocation of the application's **send** function may be sent to another client instead.)

- **u_int32_t st_dupmasters;**

  The number of duplicate master conditions originally detected at this site.

- **u_int32_t st_egen;**

  The election generation number for the current or next election.

- **int st_election_cur_winner;**

  The environment ID of the winner of the current or last election.

- **u_int32_t st_election_datagen;**

  The master data generation number of the winner of the current or last election.

- **u_int32_t st_election_gen;**

  The master generation number of the winner of the current or last election.

- **DB_LSN st_election_lsn;**

  The maximum LSN of the winner of the current or last election.

- **u_int32_t st_election_nsites;**

  The number of sites responding to this site during the current election.

- **u_int32_t st_election_nvotes;**

  The number of votes required in the current or last election.

- **u_int32_t st_election_priority;**

  The priority of the winner of the current or last election.

- **u_int32_t st_election_sec;**

  The number of seconds the last election took (the total election time is **st_election_sec** plus **st_election_usec**).

- **int st_election_status;**

  The current election phase (0 if no election is in progress).

- **u_int32_t st_election_tiebreaker;**

  The tiebreaker value of the winner of the current or last election.

- **u_int32_t st_election_usec;**

  The number of microseconds the last election took (the total election time is **st_election_sec** plus **st_election_usec**).

- **u_int32_t st_election_votes;**

  The number of votes received during the current election.

- **uintmax_t st_elections;**

  The number of elections held.

- **uintmax_t st_elections_won;**

  The number of elections won.

- **int st_env_id;**

  The current environment ID.

- **u_int32_t st_env_priority;**

  The current environment priority.

- **u_int32_t st_gen;**

  The current master generation number.

- **uintmax_t st_lease_chk;**

  The number of lease validity checks.

- **uintmax_t st_lease_chk_misses;**

  The number of invalid lease validity checks.

- **uintmax_t st_lease_chk_refresh;**

  The number of lease refresh attempts during lease validity checks.

- **uintmax_t st_lease_sends;**

  The number of live messages sent while using leases.

- **uintmax_t st_log_duplicated;**

  The number of duplicate log records received.

- **uintmax_t st_log_queued;**

  The number of log records currently queued.

- **uintmax_t st_log_queued_max;**

  The maximum number of log records ever queued at once.

- **uintmax_t st_log_queued_total;**

  The total number of log records queued.

- **uintmax_t st_log_records;**

  The number of log records received and appended to the log.

- **uintmax_t st_log_requested;**

  The number of times log records were missed and requested.

- **int st_master;**

  The current master environment ID.

- **uintmax_t st_master_changes;**

  The number of times the master has changed.

- **u_int32_t st_max_lease_sec;**

  The number of seconds of the longest lease (the total lease time is **st_max_lease_sec** plus **st_max_lease_usec**).

- **u_int32_t st_max_lease_usec;**

  The number of microseconds of the longest lease (the total lease time is **st_max_lease_sec** plus **st_max_lease_usec**).

- **DB_LSN st_max_perm_lsn;**

  The LSN of the maximum permanent log record, or 0 if there are no permanent log records.

- **uintmax_t st_msgs_badgen;**

  The number of messages received with a bad generation number.

- **uintmax_t st_msgs_processed;**

  The number of messages received and processed.

- **uintmax_t st_msgs_recover;**

  The number of messages ignored due to pending recovery.

- **uintmax_t st_msgs_send_failures;**

  The number of failed message sends.

- **uintmax_t st_msgs_sent;**

  The number of messages sent.

- **uintmax_t st_newsites;**

  The number of new site messages received.

- **DB_LSN st_next_lsn;**

  In replication environments configured as masters, the next LSN to be used. In replication environments configured as clients, the next LSN expected.

- **u_int32_t st_next_pg;**

  The next page number we expect to receive.

- **u_int32_t st_nsites;**

  The number of sites used in the last election.

- **uintmax_t st_nthrottles;**

  Transmission limited. This indicates the number of times that data transmission was stopped to limit the amount of data sent in response to a single call to <a href="repmessage.md" class="xref" title="DB_ENV-&gt;rep_process_message()">DB_ENV-&gt;rep_process_message()</a>.

- **uintmax_t st_outdated;**

  The number of outdated conditions detected.

- **uintmax_t st_pg_duplicated;**

  The number of duplicate pages received.

- **uintmax_t st_pg_records;**

  The number of pages received and stored.

- **uintmax_t st_pg_requested;**

  The number of pages missed and requested from the master.

- **uintmax_t st_startsync_delayed;**

  The number of times the client had to delay the start of a cache flush operation (initiated by the master for an impending checkpoint) because it was missing some previous log record(s).

- **u_int32_t st_startup_complete;**

  The client site has completed its startup procedures and is now handling live records from the master.

- **u_int32_t st_status;**

  The current replication mode. Set to <a href="repmgrstart.md#repmgrstart_DB_REP_MASTER" class="link">DB_REP_MASTER</a> if the environment is a replication master, <a href="repmgrstart.md#repmgrstart_DB_REP_CLIENT" class="link">DB_REP_CLIENT</a> if the environment is a replication client, or 0 if replication is not configured.

- **uintmax_t st_txns_applied;**

  The number of transactions applied.

- **DB_LSN st_waiting_lsn;**

  The LSN of the first log record we have after missing log records being waited for, or 0 if no log records are currently missing.

- **u_int32_t st_waiting_pg;**

  The page number of the first page we have after missing pages being waited for, or 0 if no pages are currently missing.

The `DB_ENV->rep_stat()` method may not be called before the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method is called.

The `DB_ENV->rep_stat()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### statp

The **statp** parameter references memory into which a pointer to the allocated statistics structure is copied.

#### flags

The **flags** parameter must be set to 0 or the following value:

- `DB_STAT_CLEAR`

  Reset statistics after returning their values.

### Errors

The `DB_ENV->rep_stat()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the database environment was not already opened; or if an invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="rep.md#replist" class="xref" title="Replication and Related Methods">Replication and Related Methods</a>
