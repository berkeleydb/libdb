#!/usr/sbin/dtrace -qs
/*
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 * SPDX-License-Identifier: Sleepycat
 *
 * commitlat.d - Transaction commit latency against log flush latency.
 *
 * A durable commit waits for the log to reach disk, and group commit lets
 * one fsync cover many commits.  This script shows, for one process:
 *
 *	- commit latency (txn-commit to return of DB_TXN->commit), and
 *	- log fsync latency (log-flush_begin to log-flush_end), and
 *	- commits and fsyncs in total; commits / fsyncs is the group-commit
 *	  batch size.
 *
 * If commit latency tracks fsync latency the bottleneck is the device; if
 * commits are much slower than fsyncs, they are queueing for the log.
 *
 * usage: commitlat.d { -p <pid> | -c "<program> [<args>]" }
 *
 * Static probes used:
 *	txn-commit(unsigned txnid, unsigned flags)
 *	log-flush_begin(unsigned file, unsigned offset)
 *	log-flush_end(unsigned file, unsigned offset, int ret)
 * and the pid provider for the return of __txn_commit.
 */

bdb$target:::txn-commit
{
	self->c = timestamp;
}

pid$target::__txn_commit:return
/self->c/
{
	@commit["commit latency (us)"] = quantize((timestamp - self->c) / 1000);
	commits++;
	self->c = 0;
}

bdb$target:::log-flush_begin
{
	self->f = timestamp;
}

bdb$target:::log-flush_end
/self->f/
{
	@flush["log fsync latency (us)"] = quantize((timestamp - self->f) / 1000);
	@errors["fsync errors"] = sum(arg2 != 0);
	flushes++;
	self->f = 0;
}

dtrace:::END
{
	printa(@commit);
	printa(@flush);
	printa("%s: %@d\n", @errors);
	printf("commits %d, log fsyncs %d\n", commits, flushes);
}
