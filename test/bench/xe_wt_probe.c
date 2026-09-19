/*
 * xe_wt_probe.c -- ask WiredTiger itself which configuration options this
 * build accepts, instead of guessing from a header or a Python data file.
 *
 * WHY: every option in the wiredtiger_open string below is a fairness lever in
 * the cross-engine report, and three of my first guesses were wrong in ways
 * that would NOT have produced a clean failure:
 *
 *   - `direct_io=[data]` still PARSES (it is a retained, undocumented option)
 *     but api_data.py says "this option is no longer supported, retained for
 *     backward compatibility" -- so a run could set it, get rc=0, and report
 *     O_DIRECT parity it never had.
 *   - `read_only=true` is not a begin_transaction option at all.
 *   - the WT_STAT_CONN_* ids I assumed were absent under those names.
 *
 * WT validates unknown keys at open, so the authoritative test is to open a
 * connection with each option and see what it says.  This prints one line per
 * probe, and the report quotes it.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wiredtiger.h>

static const char *dir_;

static int
try_open(const char *label, const char *cfg)
{
	WT_CONNECTION *c;
	int ret;
	char buf[1024];

	(void)snprintf(buf, sizeof(buf), "create,%s", cfg);
	ret = wiredtiger_open(dir_, NULL, buf, &c);
	printf("%-28s %-6s %s\n", label, ret == 0 ? "ACCEPT" : "REJECT",
	    ret == 0 ? "" : wiredtiger_strerror(ret));
	if (ret == 0)
		(void)c->close(c, NULL);
	return ret;
}

int
main(int argc, char **argv)
{
	int a, b, p;

	dir_ = argc > 1 ? argv[1] : "/nvme/wtprobe";
	printf("# %s\n", wiredtiger_version(&a, &b, &p));
	printf("# probing config acceptance in %s\n\n", dir_);

	try_open("baseline", "cache_size=512M");
	try_open("direct_io=[data]", "cache_size=512M,direct_io=[data]");
	try_open("direct_io=[data,log]", "cache_size=512M,direct_io=[data,log]");
	try_open("direct_io=[data]+align", "cache_size=512M,direct_io=[data],buffer_alignment=4096");
	try_open("log=(enabled)", "cache_size=512M,log=(enabled=true,path=journal,file_max=1024MB)");
	try_open("txn_sync=off", "cache_size=512M,log=(enabled=true),transaction_sync=(enabled=true,method=none)");
	try_open("txn_sync=fsync", "cache_size=512M,log=(enabled=true),transaction_sync=(enabled=true,method=fsync)");
	try_open("txn_sync=dsync", "cache_size=512M,log=(enabled=true),transaction_sync=(enabled=true,method=dsync)");
	try_open("eviction threads", "cache_size=512M,eviction=(threads_min=8,threads_max=16)");
	try_open("eviction targets", "cache_size=512M,eviction_target=80,eviction_trigger=95");
	try_open("checkpoint wait", "cache_size=512M,checkpoint=(wait=60,log_size=2GB)");
	try_open("session_max", "cache_size=512M,session_max=512");
	try_open("statistics=(fast)", "cache_size=512M,statistics=(fast)");
	try_open("statistics=(all)", "cache_size=512M,statistics=(all)");
	try_open("mmap=false", "cache_size=512M,mmap=false");
	try_open("cache_cursors", "cache_size=512M,cache_cursors=true");
	try_open("BOGUS_OPTION (control)", "cache_size=512M,xe_bogus_option=1");

	printf("\n# NOTE: ACCEPT means the option parses, NOT that it has an\n"
	    "# effect.  direct_io is documented in dist/api_data.py as\n"
	    "# \"no longer supported, retained for backward compatibility\",\n"
	    "# so it can parse and do nothing -- which is exactly why the\n"
	    "# report must not infer O_DIRECT from a successful open.\n");
	return 0;
}
