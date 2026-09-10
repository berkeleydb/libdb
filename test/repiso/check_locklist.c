/*-
 * test/repiso/check_locklist.c --
 *	The DETERMINISTIC half of the #140 client-consequence test.
 *
 * The invariant, stated as a property of the replication log:
 *
 *	Every page a replicated transaction MODIFIED must appear in that
 *	transaction's commit lock list.
 *
 * That is exactly what makes apply safe on a client.  src/rep/rep_record.c's
 * __rep_process_txn calls __lock_get_list(..., DB_LOCK_WRITE, lock_dbt) and
 * takes NO other page locks (the apply cursor is DBC_RECOVER, which
 * __db_lget short-circuits for a replication client, src/db/db_meta.c).  So a
 * modified page missing from the list is a page apply will change while holding
 * no lock on it -- and a concurrent client reader holding a read lock on that
 * page gets a non-repeatable read.
 *
 * This checker reads db_printlog output and, for each committed transaction,
 * compares the set of (fileid, pgno) pairs its page-modification records touch
 * against the set the commit lock list names.  It needs no timing, no race and
 * no second process: the log either contains the omission or it does not.
 *
 * WHY PARSE db_printlog INSTEAD OF LINKING THE ENGINE
 *
 * The commit lock list is serialized by __lock_fix_list into a private format
 * (count, per-object page counts, coalesced-by-fileid page runs) that only
 * __lock_list_print and __lock_get_list decode.  __lock_list_print is exactly
 * that decoder and db_printlog already calls it, so the log dump IS the
 * engine's own reading of the list -- no second implementation to get wrong.
 *
 * Usage:
 *	db_printlog -h DIR | check_locklist [--verbose]
 *
 * Exit status: 0 = every committed transaction listed all the pages it
 * modified, 1 = at least one omission (a #140 reproduction), 2 = usage error
 * or no transactions found (which would mean the harness produced no evidence).
 */
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define	MAXLINE		4096
#define	MAXPAGES	256
#define	MAXTXN		4096
#define	FIDLEN		64

typedef struct {
	char		fid[FIDLEN];	/* fileid as printed */
	unsigned long	pgno;
} pageref;

typedef struct {
	char	txnid[32];
	pageref	modified[MAXPAGES];
	int	nmodified;
	pageref	locked[MAXPAGES];
	int	nlocked;
	int	committed;
} txninfo;

static txninfo	txns[MAXTXN];
static int	ntxns;
static int	verbose;

/* Fileid printed in the page records is a small integer ("fileid: 0"). */
static txninfo *
txn_find(const char *id)
{
	int i;

	for (i = 0; i < ntxns; i++)
		if (strcmp(txns[i].txnid, id) == 0)
			return (&txns[i]);
	if (ntxns == MAXTXN)
		return (NULL);
	(void)snprintf(txns[ntxns].txnid, sizeof(txns[ntxns].txnid), "%s", id);
	return (&txns[ntxns++]);
}

static void
add_page(pageref *set, int *n, const char *fid, unsigned long pgno)
{
	int i;

	for (i = 0; i < *n; i++)
		if (set[i].pgno == pgno && strcmp(set[i].fid, fid) == 0)
			return;
	if (*n == MAXPAGES)
		return;
	(void)snprintf(set[*n].fid, FIDLEN, "%s", fid);
	set[*n].pgno = pgno;
	(*n)++;
}

static int
has_page(const pageref *set, int n, unsigned long pgno)
{
	int i;

	/*
	 * Compare on pgno only.  The page records print `fileid: N' (the
	 * dbreg id) while the lock list prints the 5-byte raw fileid, so the
	 * two never spell the same file the same way.  The harness replicates
	 * ONE multiversion database, so pgno alone is unambiguous; a
	 * multi-database checker would have to join the two through the
	 * __dbreg_register records.
	 *
	 * ponytail: pgno-only match, because the harness has one interesting
	 * file.  Join through dbreg if this ever checks a multi-file txn.
	 */
	for (i = 0; i < n; i++)
		if (set[i].pgno == pgno)
			return (1);
	return (0);
}

/*
 * A log record header looks like:
 *	[1][52873]__bam_repl: rec: 58 txnp 80000046 prevlsn [0][0]
 * Returns the record name and txn id.
 */
static int
parse_header(const char *line, char *name, size_t namelen, char *txnid,
    size_t txnlen)
{
	const char *p, *q;
	size_t n;

	if (line[0] != '[')
		return (0);
	if ((p = strstr(line, "]__")) == NULL)
		return (0);
	p += 1;					/* at "__name" */
	if ((q = strchr(p, ':')) == NULL)
		return (0);
	n = (size_t)(q - p);
	if (n >= namelen)
		return (0);
	memcpy(name, p, n);
	name[n] = '\0';

	if ((p = strstr(line, "txnp ")) == NULL)
		return (0);
	p += 5;
	for (n = 0; n < txnlen - 1 && isxdigit((unsigned char)p[n]); n++)
		txnid[n] = p[n];
	txnid[n] = '\0';
	return (n > 0);
}

/*
 * The commit lock list is printed by __lock_list_print as, per object:
 *	\t(35 12 39 a 2d) 37 3 27 0 36
 * i.e. a parenthesised 5-byte fileid then one or more page numbers.  Any line
 * inside a __txn_regop record that starts with '(' after the tab is one.
 */
static void
parse_locklist_line(const char *line, txninfo *t)
{
	char fid[FIDLEN];
	const char *p, *q;
	size_t n;

	p = line;
	while (*p == '\t' || *p == ' ')
		p++;
	if (*p != '(')
		return;
	if ((q = strchr(p, ')')) == NULL)
		return;
	n = (size_t)(q - p + 1);
	if (n >= FIDLEN)
		n = FIDLEN - 1;
	memcpy(fid, p, n);
	fid[n] = '\0';

	for (p = q + 1; *p != '\0'; ) {
		while (*p == ' ' || *p == '\t')
			p++;
		if (!isdigit((unsigned char)*p))
			break;
		add_page(t->locked, &t->nlocked, fid, strtoul(p, (char **)&p,
		    10));
	}
}

/*
 * Records that MODIFY a page and are replayed by apply.  Anything that carries
 * a `pgno:' field and is dispatched with DB_TXN_APPLY counts; the meta/alloc
 * records are included because apply writes those pages too.
 *
 * A record NOT in this list simply contributes no modified pages, which can
 * only make the checker MISS an omission, never invent one.  That asymmetry is
 * deliberate: a false PASS here is caught by the two-process driver, a false
 * FAIL would be unfalsifiable noise.
 */
static int
is_page_modify(const char *name)
{
	static const char *const mods[] = {
		"__bam_split", "__bam_rsplit", "__bam_adj", "__bam_cadjust",
		"__bam_cdel", "__bam_repl", "__bam_root", "__bam_curadj",
		"__bam_rcuradj", "__bam_relink", "__bam_merge",
		"__bam_merge_44", "__bam_pgno",
		"__db_addrem", "__db_addrem_42", "__db_big", "__db_big_42",
		"__db_ovref", "__db_relink", "__db_relink_42",
		"__db_debug", "__db_noop", "__db_pg_alloc", "__db_pg_free",
		"__db_cksum", "__db_pg_freedata", "__db_pg_init",
		"__db_pg_sort_44", "__db_pg_trunc", "__db_realloc",
		"__db_pg_prepare", "__db_pg_new",
		"__ham_insdel", "__ham_newpage", "__ham_splitdata",
		"__ham_replace", "__ham_copypage", "__ham_metagroup",
		"__ham_groupalloc", "__ham_curadj", "__ham_chgpg",
		"__heap_addrem", "__heap_pg_alloc", "__heap_trunc_meta",
		"__heap_trunc_page",
		"__qam_inc", "__qam_incfirst", "__qam_mvptr", "__qam_del",
		"__qam_add", "__qam_delext",
		NULL
	};
	int i;

	/* __db_debug and __db_noop carry no pgno; harmless either way. */
	for (i = 0; mods[i] != NULL; i++)
		if (strcmp(name, mods[i]) == 0)
			return (1);
	return (0);
}

/*
 * Fields that unambiguously name a page a log record MODIFIES.
 *
 * Deliberately NOT here: `next:' and `prev:'.  They are used both as free-list
 * pointers (__db_pg_alloc prints `next: 0' for a page it does not touch) and
 * inside the embedded page dumps that __bam_split emits, where a single line
 * reads `prev: 22 next: 3 entries: 8 offset: 48'.  Including them produced
 * false omissions on a KNOWN-GOOD library, which is the one failure mode a
 * gate must not have.
 *
 * Excluding a field can only make the checker MISS an omission, never invent
 * one.  That asymmetry is the whole design: a missed omission is caught by the
 * behavioural check in the same run, an invented one would be unfalsifiable
 * noise that gets the tier disabled.
 */
static const char *const pgfields[] = {
	"pgno", "left", "right", "ppgno", "npgno", "meta_pgno",
	"new_pgno", "root_pgno", "pgno_ovfl", NULL
};

/*
 * parse_pgfield --
 *	Match a STRICT single-value field line: a leading tab or spaces, a name
 *	from pgfields, a colon, whitespace, a number, then nothing but
 *	whitespace.  The strictness is what rejects the multi-field page-dump
 *	lines.  Returns 1 and sets *pgnop on a match.
 */
static int
parse_pgfield(const char *line, unsigned long *pgnop)
{
	const char *p, *q;
	char *end;
	size_t n;
	int i;

	p = line;
	while (*p == '\t' || *p == ' ')
		p++;
	if ((q = strchr(p, ':')) == NULL)
		return (0);
	n = (size_t)(q - p);
	for (i = 0; pgfields[i] != NULL; i++)
		if (strlen(pgfields[i]) == n &&
		    memcmp(p, pgfields[i], n) == 0)
			break;
	if (pgfields[i] == NULL)
		return (0);

	p = q + 1;
	while (*p == ' ' || *p == '\t')
		p++;
	if (!isdigit((unsigned char)*p))
		return (0);
	*pgnop = strtoul(p, &end, 10);
	/* Anything but trailing whitespace means this was a page dump. */
	for (p = end; *p != '\0'; p++)
		if (*p != ' ' && *p != '\t' && *p != '\n' && *p != '\r')
			return (0);
	return (1);
}

static void
print_pageset(const pageref *set, int n)
{
	int i;

	printf("{");
	for (i = 0; i < n; i++)
		printf("%s%lu", i ? "," : "", set[i].pgno);
	printf("}");
}

int
main(int argc, char **argv)
{
	char cur_fid[FIDLEN], cur_name[128], cur_txn[32], line[MAXLINE];
	txninfo *t;
	unsigned long pgno;
	int i, in_locklist, j, omissions, reported, ntx_checked;

	for (i = 1; i < argc; i++) {
		if (strcmp(argv[i], "--verbose") == 0 ||
		    strcmp(argv[i], "-v") == 0)
			verbose = 1;
		else {
			fprintf(stderr, "usage: db_printlog -h DIR | %s "
			    "[--verbose]\n", argv[0]);
			return (2);
		}
	}

	t = NULL;
	in_locklist = 0;
	cur_name[0] = cur_txn[0] = cur_fid[0] = '\0';

	while (fgets(line, sizeof(line), stdin) != NULL) {
		if (parse_header(line, cur_name, sizeof(cur_name), cur_txn,
		    sizeof(cur_txn))) {
			t = txn_find(cur_txn);
			in_locklist = 0;
			cur_fid[0] = '\0';
			if (t != NULL && strcmp(cur_name, "__txn_regop") == 0) {
				t->committed = 1;
				in_locklist = 1;
			}
			continue;
		}
		if (t == NULL)
			continue;

		if (in_locklist) {
			parse_locklist_line(line, t);
			continue;
		}
		if (!is_page_modify(cur_name))
			continue;

		/*
		 * `fileid: N' then `pgno: N'.  Several records carry more than
		 * one pgno (split, relink); take every one.
		 */
		if (strstr(line, "fileid:") != NULL) {
			(void)snprintf(cur_fid, sizeof(cur_fid), "%s",
			    strstr(line, "fileid:") + 8);
			for (i = 0; cur_fid[i] != '\0'; i++)
				if (cur_fid[i] == '\n')
					cur_fid[i] = '\0';
			continue;
		}
		if (parse_pgfield(line, &pgno))
			add_page(t->modified, &t->nmodified,
			    cur_fid[0] != '\0' ? cur_fid : "?", pgno);
	}

	omissions = ntx_checked = reported = 0;
	for (i = 0; i < ntxns; i++) {
		t = &txns[i];
		/*
		 * Only a committed transaction that both modified pages AND
		 * shipped a lock list is evidence.  A commit with an EMPTY
		 * list is skipped: an empty list is what a non-master or a
		 * read-only commit produces, and the harness's own trigger
		 * always ships a non-empty one.
		 */
		if (!t->committed || t->nmodified == 0 || t->nlocked == 0)
			continue;
		ntx_checked++;
		for (j = 0; j < t->nmodified; j++) {
			if (has_page(t->locked, t->nlocked,
			    t->modified[j].pgno))
				continue;
			omissions++;
			/*
			 * Name BOTH sides.  A future regression should be
			 * self-diagnosing from the log line alone: which txn,
			 * which page was modified, and exactly what the lock
			 * list did contain instead.
			 */
			if (reported++ < 20) {
				printf("OMISSION txn %s modified pgno %lu "
				    "(fileid %s) but its commit lock list "
				    "contains only ", t->txnid,
				    t->modified[j].pgno, t->modified[j].fid);
				print_pageset(t->locked, t->nlocked);
				printf(" -- apply would modify pgno %lu while "
				    "holding no lock on it\n",
				    t->modified[j].pgno);
				printf("         (txn %s modified ", t->txnid);
				print_pageset(t->modified, t->nmodified);
				printf(", locked ");
				print_pageset(t->locked, t->nlocked);
				printf(")\n");
			}
		}
		if (verbose) {
			printf("txn %s modified ", t->txnid);
			print_pageset(t->modified, t->nmodified);
			printf(" locked ");
			print_pageset(t->locked, t->nlocked);
			printf("\n");
		}
	}

	printf("LOCKLIST: %d committed txn(s) with both modifications and a "
	    "lock list; %d omission(s)\n", ntx_checked, omissions);
	if (ntx_checked == 0) {
		printf("LOCKLIST: RESULT verdict=NO_EVIDENCE -- the log "
		    "contained no committed transaction that both modified a "
		    "page and shipped a lock list, so nothing was checked\n");
		return (2);
	}
	if (omissions != 0) {
		printf("LOCKLIST: RESULT verdict=OMISSION_FOUND -- apply on a "
		    "client would modify %d page(s) it holds no lock on "
		    "(issue #140 client consequence)\n", omissions);
		return (1);
	}
	printf("LOCKLIST: RESULT verdict=PASS -- every modified page appears "
	    "in its transaction's commit lock list\n");
	return (0);
}
