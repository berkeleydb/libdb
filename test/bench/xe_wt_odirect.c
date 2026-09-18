/*
 * xe_wt_odirect.c -- does WiredTiger's `direct_io=[data]` actually open data
 * files with O_DIRECT in this build?
 *
 * `direct_io` PARSES (xe_wt_probe shows ACCEPT) but api_data.py marks it
 * "no longer supported, retained for backward compatibility".  A benchmark
 * that set it, saw rc=0, and reported "both engines used O_DIRECT" would be
 * publishing a fabricated parity claim -- the precise failure this campaign
 * has to avoid.
 *
 * So: create a table, write enough to force real file I/O, checkpoint, and let
 * the caller strace this process.  If O_DIRECT is honored, the open() of
 * *.wt carries it.  Run under:
 *   strace -f -e trace=openat -o out.txt ./xe_wt_odirect DIR [direct]
 * then grep the trace for O_DIRECT on a .wt file.  That is evidence from the
 * kernel interface, not from a config parser.
 *
 * NOTE on the table config: `type=row` is NOT a WiredTiger access method and
 * is REJECTED by this build ("unknown object type").  WT's `type` selects a
 * data SOURCE (file, lsm, or an extension), not a page format; the row-store
 * B-tree is what a default `table:`/`file:` URI already is.  The first version
 * of this probe used type=row, so every create failed, no .wt data file was
 * ever opened, and the strace legitimately showed zero O_DIRECT opens -- a
 * vacuous negative that agreed with the right answer for the wrong reason.
 * Worth stating because "the probe returned the expected result" is not the
 * same as "the probe ran".
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wiredtiger.h>

int
main(int argc, char **argv)
{
	WT_CONNECTION *conn;
	WT_SESSION *s;
	WT_CURSOR *c;
	char cfg[512];
	const char *dir = argc > 1 ? argv[1] : "/nvme/wtod";
	int direct = argc > 2 && strcmp(argv[2], "direct") == 0;
	int i, ret;
	char kbuf[16], vbuf[4096];

	(void)snprintf(cfg, sizeof(cfg),
	    "create,cache_size=64M,statistics=(fast)%s",
	    direct ? ",direct_io=[data],buffer_alignment=4096" : "");
	printf("# config: %s\n", cfg);
	if ((ret = wiredtiger_open(dir, NULL, cfg, &conn)) != 0) {
		fprintf(stderr, "open: %s\n", wiredtiger_strerror(ret));
		return 1;
	}
	if ((ret = conn->open_session(conn, NULL, NULL, &s)) != 0) return 1;
	if ((ret = s->create(s, "table:od",
	    "key_format=u,value_format=u,leaf_page_max=8k")) != 0 &&
	    ret != 17 /*EEXIST*/) {
		fprintf(stderr, "create: %s\n", wiredtiger_strerror(ret));
		return 1;
	}
	if ((ret = s->open_cursor(s, "table:od", NULL, NULL, &c)) != 0) return 1;

	memset(vbuf, 'x', sizeof(vbuf));
	/* 20k * 4KB = 80 MB > 64 MB cache, so this must reach the device. */
	for (i = 0; i < 20000; i++) {
		WT_ITEM ki, vi;
		(void)snprintf(kbuf, sizeof(kbuf), "%012d", i);
		ki.data = kbuf; ki.size = 12;
		vi.data = vbuf; vi.size = sizeof(vbuf);
		c->set_key(c, &ki);
		c->set_value(c, &vi);
		if ((ret = c->insert(c)) != 0) {
			fprintf(stderr, "insert: %s\n", wiredtiger_strerror(ret));
			return 1;
		}
	}
	(void)c->close(c);
	if ((ret = s->checkpoint(s, NULL)) != 0)
		fprintf(stderr, "checkpoint: %s\n", wiredtiger_strerror(ret));
	(void)s->close(s, NULL);
	(void)conn->close(conn, NULL);
	printf("VERDICT xe_wt_odirect wrote 20000 rows direct=%d\n", direct);
	return 0;
}
