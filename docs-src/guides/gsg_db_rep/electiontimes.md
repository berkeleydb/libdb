---
title: "Managing Election Times"
api-name: "Managing Election Times"
source: docs/gsg_db_rep/C/electiontimes.html
---
## Managing Election Times

<span class="sect2"> [Managing Election Timeouts](electiontimes.md#electiontimeout) </span>

<span class="sect2"> [Managing Election Retry Times](electiontimes.md#electretrytime) </span>

Where it comes to elections, there are two timeout values with which you should be concerned: election timeouts and election retries.

### Managing Election Timeouts

When an environment calls for an election, it will wait some amount of time for the other replicas in the replication group to respond. The amount of time that the environment will wait before declaring the election completed is the <span class="emphasis">*election timeout*</span>.

If the environment hears from all other known replicas before the election timeout occurs, the election is considered a success and a master is elected.

If only a subset of replicas respond, then the success or failure of the election is determined by how many replicas have participated in the election. It only takes a simple majority of replicas to elect a master. If there are enough votes for a given environment to meet that standard, then the master has been elected and the election is considered a success.

However, if not enough replicas have participated in the election when the election timeout value is reached, the election is considered a failure and a master is not elected. At this point, your replication group is operating without a master, which means that, essentially, your replicated application has been placed in read-only mode.

Note, however, that the Replication Manager will attempt a new election after a given amount of time has passed. See the next section for details.

You set the election timeout value using `DB_ENV->rep_set_timeout()`. To do so, specify the `DB_REP_ELECTION_TIMEOUT` value to the `which` parameter and then a timeout value in microseconds to the `timeout` parameter.

### Managing Election Retry Times

In the event that a election fails (see the previous section), an election will not be attempted again until the election retry timeout value has expired.

You set the retry timeout value using `DB_ENV->rep_set_timeout()`. To do so, specify the `DB_REP_ELECTION_RETRY` value to the `which` parameter and then a retry value in microseconds to the `timeout` parameter.

Note that this flag is only valid when you are using the Replication Manager. If you are using the Base APIs, then this flag is ignored.
