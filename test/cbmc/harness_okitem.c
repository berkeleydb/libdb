/*
 * CBMC harness: __db_ret_okitem (src/db/db_ret.c) -- the page-item bounds
 * validator whose absence was bug #67 (an OOB read on corrupt page input).
 *
 * This is the highest-value correctness proof in the suite.  __db_ret_okitem
 * is the guard that gates every access-method cursor return through __db_ret
 * (btree, hash, heap): before __db_ret / __db_retcopy dereference an on-page
 * item's offset+length, okitem must reject any item whose bytes would run off
 * the page.  We prove that safety guarantee actually holds:
 *
 *   MAIN PROPERTY (SAFETY):  for ANY page contents in a bounded page, if
 *   __db_ret_okitem returns 0 ("safe to read"), then the item's on-page
 *   extent [off, off + item_len) lies entirely within [0, pgsize).  i.e. it
 *   NEVER greenlights an item that would read past the page end.
 *
 * We model the page as a fully nondeterministic byte array of size PGSIZE and
 * an arbitrary index.  CBMC explores every btree-leaf / hash / heap item
 * layout the bytes can spell.  The function body below is copied VERBATIM
 * from src/db/db_ret.c (only the surrounding #includes are replaced by the
 * struct/macro definitions below, which MIRROR src/dbinc/db_page.h exactly).
 *
 * What is modelled (mirrors the real headers, byte-for-byte layout):
 *   - PAGE header (SIZEOF_PAGE == 26, entries/type fields at the real
 *     offsets), db_indx_t = u_int16_t, inp[] via P_INP.
 *   - BKEYDATA / BOVERFLOW / HEAPHDR / HOFFPAGE structs and their SIZE macros.
 *   - The DB struct with only pgsize + flags (chksum/encrypt off => P_INP has
 *     no chksum/crypto prefix, the common no-crypto case).
 *
 * Bound: PGSIZE = 64 bytes (small enough for CBMC, large enough to hold a
 *        header + inp[] + several items with room to over-run).  The function
 *        is loop-free.
 *
 * ===========================================================================
 * REAL BUG FOUND (see test/cbmc/README.md "BUG FOUND"):
 *   __db_ret_okitem's two hash-offpage guards call
 *       HPAGE_PTYPE((u_int8_t *)h + off)
 *   but the HPAGE_PTYPE(p) macro (src/dbinc/db_page.h:583) is
 *       #define HPAGE_PTYPE(p) (*(u_int8_t *)p)     <-- no () around p
 *   so with a compound argument it parses as
 *       *(u_int8_t *)(u_int8_t *)h + off   ==   page[0] + off
 *   i.e. it reads the page's first byte plus off, NOT the item's type byte
 *   at page[off].  okitem therefore mis-detects H_OFFPAGE items on hash
 *   pages and can pass an item whose HOFFPAGE header runs past the page end
 *   -- the exact OOB-read CLASS that bug #67 set out to close.  These are
 *   the ONLY two call sites in src/ that pass a compound expression to
 *   HPAGE_PTYPE; every other caller passes a bare pointer, so this is
 *   isolated to okitem.
 *
 *   Compile with -DHPAGE_PTYPE_FIXED to use a correctly-parenthesized macro
 *   and see the property VERIFY -- this both proves the harness has teeth
 *   and validates the one-line engine fix (add parens: (*(u_int8_t *)(p))).
 * ===========================================================================
 */

#include <stdint.h>
#include <stddef.h>

typedef uint8_t   u_int8_t;
typedef uint16_t  u_int16_t;
typedef uint32_t  u_int32_t;
typedef uint32_t  db_pgno_t;
typedef uint16_t  db_indx_t;

/* --- from db.h / db_page.h --- */
#define DB_PAGE_NOTFOUND (-30986)
#define DB_AM_CHKSUM  0x00000001
#define DB_AM_ENCRYPT 0x00000400

#define P_HASH_UNSORTED 8
#define P_LBTREE        5
#define P_LDUP          14
#define P_LRECNO        6
#define P_HASH          13
#define P_HEAP          15

#define H_KEYDATA   1
#define H_DUPLICATE 2
#define H_OFFPAGE   3

#define B_KEYDATA   1
#define B_DUPLICATE 2
#define B_OVERFLOW  3
#define B_DELETE    0x80
#define B_TYPE(t)   ((t) & ~B_DELETE)

#define HEAP_RECSPLIT 0x01
#define HEAP_RECFIRST 0x02

#define F_ISSET(p, f) ((p)->flags & (f))
#define P_TO_UINT16(p) ((u_int16_t)(uintptr_t)(p))
#define SSZA(name, field) P_TO_UINT16(&(((name *)0)->field[0]))
#define DB_ALIGN(v, bound) (((v) + (bound) - 1) & ~(((uintmax_t)(bound)) - 1))

/* PAGE header: layout mirrors src/dbinc/db_page.h struct _db_page. */
typedef struct { u_int32_t file, offset; } DB_LSN;
typedef struct _db_page {
	DB_LSN    lsn;        /* 00-07 */
	db_pgno_t pgno;       /* 08-11 */
	db_pgno_t prev_pgno;  /* 12-15 */
	db_pgno_t next_pgno;  /* 16-19 */
	db_indx_t entries;    /* 20-21 */
	db_indx_t hf_offset;  /* 22-23 */
	u_int8_t  level;      /*    24 */
	u_int8_t  type;       /*    25 */
} PAGE;
#define SIZEOF_PAGE 26
#define NUM_ENT(p) (((PAGE *)p)->entries)
#define TYPE(p)    (((PAGE *)p)->type)

typedef struct __pg_chksum { u_int8_t unused[2]; u_int8_t chksum[4]; } PG_CHKSUM;
typedef struct __pg_crypto { u_int8_t unused[2]; u_int8_t chksum[20]; u_int8_t iv[16]; } PG_CRYPTO;

#define P_INP(dbp, pg)							\
	((db_indx_t *)((u_int8_t *)(pg) + SIZEOF_PAGE +			\
	(F_ISSET((dbp), DB_AM_ENCRYPT) ? sizeof(PG_CRYPTO) :		\
	(F_ISSET((dbp), DB_AM_CHKSUM) ? sizeof(PG_CHKSUM) : 0))))
#define P_ENTRY(dbp, pg, indx) ((u_int8_t *)pg + P_INP(dbp, pg)[indx])

typedef struct _bkeydata { db_indx_t len; u_int8_t type; u_int8_t data[1]; } BKEYDATA;
typedef struct _boverflow { db_indx_t unused1; u_int8_t type; u_int8_t unused2; db_pgno_t pgno; u_int32_t tlen; } BOVERFLOW;
#define GET_BKEYDATA(dbp, pg, indx) ((BKEYDATA *)P_ENTRY(dbp, pg, indx))
#define BOVERFLOW_SIZE ((u_int16_t)DB_ALIGN(sizeof(BOVERFLOW), sizeof(u_int32_t)))

typedef struct _hoffpage { u_int8_t type; u_int8_t unused[3]; db_pgno_t pgno; u_int32_t tlen; } HOFFPAGE;
#define HOFFPAGE_SIZE (sizeof(HOFFPAGE))
#ifdef HPAGE_PTYPE_FIXED
#define HPAGE_PTYPE(p) (*(u_int8_t *)(p))   /* proposed engine fix: parens */
#else
#define HPAGE_PTYPE(p) (*(u_int8_t *)p)     /* VERBATIM src/dbinc/db_page.h:583 (buggy) */
#endif
#define HKEYDATA_SIZE(len) ((len) + SSZA(HKEYDATA, data))
typedef struct _hkeydata { u_int8_t type; u_int8_t data[1]; } HKEYDATA;

typedef struct __heaphdr { u_int8_t flags; u_int8_t unused; u_int16_t size; } HEAPHDR;
typedef struct __heappg {
	DB_LSN lsn; db_pgno_t pgno; db_pgno_t high_pgno;
	db_indx_t high_indx; db_indx_t free_indx; db_indx_t entries; db_indx_t hf_offset;
	u_int8_t level; u_int8_t type;
} HEAPPG;
#define HEAP_HIGHINDX(p) (((HEAPPG *)p)->high_indx)

typedef struct __db {
	size_t pgsize;
	u_int32_t flags;
} DB;

/* --- BEGIN verbatim copy of __db_ret_okitem from src/db/db_ret.c --- */
static int
__db_ret_okitem(dbp, h, indx)
	DB *dbp;
	PAGE *h;
	u_int32_t indx;
{
	BKEYDATA *bk;
	HEAPHDR *hdr;
	db_indx_t *inp, off, prev;
	size_t pgsize;

	pgsize = dbp->pgsize;
	inp = P_INP(dbp, h);

	if (TYPE(h) == P_HEAP) {
		if (indx > HEAP_HIGHINDX(h))
			return (DB_PAGE_NOTFOUND);
		off = inp[indx];
		if (off == 0 || (size_t)off + sizeof(HEAPHDR) > pgsize)
			return (DB_PAGE_NOTFOUND);
		hdr = (HEAPHDR *)((u_int8_t *)h + off);
		if (!F_ISSET(hdr, (HEAP_RECSPLIT | HEAP_RECFIRST)) &&
		    (size_t)off + sizeof(HEAPHDR) + hdr->size > pgsize)
			return (DB_PAGE_NOTFOUND);
		return (0);
	}

	if (indx >= NUM_ENT(h))
		return (DB_PAGE_NOTFOUND);

	off = inp[indx];
	if ((size_t)((u_int8_t *)(inp + indx + 1) - (u_int8_t *)h) > pgsize)
		return (DB_PAGE_NOTFOUND);
	if ((size_t)off < (size_t)((u_int8_t *)(inp + NUM_ENT(h)) -
	    (u_int8_t *)h) || (size_t)off >= pgsize)
		return (DB_PAGE_NOTFOUND);

	switch (TYPE(h)) {
	case P_HASH_UNSORTED:
	case P_HASH:
		if ((size_t)off + 1 > pgsize)
			return (DB_PAGE_NOTFOUND);
		prev = indx == 0 ? (db_indx_t)pgsize : inp[indx - 1];
		if ((size_t)prev > pgsize || off >= prev)
			return (DB_PAGE_NOTFOUND);
		if (HPAGE_PTYPE((u_int8_t *)h + off) == H_OFFPAGE &&
		    (size_t)off + HOFFPAGE_SIZE > pgsize)
			return (DB_PAGE_NOTFOUND);
		if (HPAGE_PTYPE((u_int8_t *)h + off) != H_OFFPAGE &&
		    (size_t)(prev - off) < HKEYDATA_SIZE(0))
			return (DB_PAGE_NOTFOUND);
		break;
	case P_LBTREE:
	case P_LDUP:
	case P_LRECNO:
		if ((size_t)off + SSZA(BKEYDATA, data) > pgsize)
			return (DB_PAGE_NOTFOUND);
		bk = GET_BKEYDATA(dbp, h, indx);
		if (B_TYPE(bk->type) == B_OVERFLOW ||
		    B_TYPE(bk->type) == B_DUPLICATE) {
			if ((size_t)off + BOVERFLOW_SIZE > pgsize)
				return (DB_PAGE_NOTFOUND);
		} else if (B_TYPE(bk->type) == B_KEYDATA) {
			if ((size_t)off + SSZA(BKEYDATA, data) + bk->len > pgsize)
				return (DB_PAGE_NOTFOUND);
		} else
			return (DB_PAGE_NOTFOUND);
		break;
	default:
		break;
	}
	return (0);
}
/* --- END verbatim copy --- */

#define PGSIZE 64
u_int32_t nondet_u32(void);

void harness(void)
{
	static u_int8_t page[PGSIZE];
	DB db;
	PAGE *h = (PAGE *)page;
	u_int32_t indx = nondet_u32();
	db_indx_t *inp;
	db_indx_t off;
	int rc;
	unsigned i;

	/* No checksum / no encryption => P_INP has no prefix (common case). */
	db.pgsize = PGSIZE;
	db.flags = 0;

	/* Fill the entire page with nondeterministic bytes: models ANY
	 * (possibly corrupt) on-disk page content. */
	for (i = 0; i < PGSIZE; i++)
		page[i] = (u_int8_t)nondet_u32();

	/* Constrain the page type to one of the leaf types okitem handles
	 * (the __db_ret caller only invokes okitem for these). */
	__CPROVER_assume(
	    TYPE(h) == P_HASH_UNSORTED || TYPE(h) == P_HASH ||
	    TYPE(h) == P_HEAP || TYPE(h) == P_LBTREE ||
	    TYPE(h) == P_LDUP || TYPE(h) == P_LRECNO);

	/* A real page never claims more entries than could fit; bound NUM_ENT so
	 * okitem's end-pointer arithmetic (inp + NUM_ENT(h)) stays within the
	 * page object (avoids CBMC flagging out-of-object POINTER FORMATION,
	 * which is a modelling artifact, not the OOB-read safety property). */
	__CPROVER_assume(NUM_ENT(h) <= PGSIZE);

	/* Index is bounded; the caller passes a page index. */
	__CPROVER_assume(indx < PGSIZE);

	/*
	 * PRECONDITION (the function's implicit contract): the offset-table
	 * slot inp[indx] being consulted is itself on-page.  For btree/hash
	 * okitem enforces this internally via the
	 *   (u_int8_t*)(inp+indx+1) - h > pgsize
	 * check; for the P_HEAP path it does NOT -- it trusts the on-page
	 * (corrupt-influenced) HEAP_HIGHINDX and reads inp[indx] before any
	 * on-page bound on that slot.  We therefore constrain the slot to be
	 * on-page here so the harness verifies the SAFETY GUARANTEE (extent
	 * within page) rather than tripping on the offset-table read itself.
	 * The unconstrained heap over-read is reported separately in README.md
	 * as an engine observation (same bug CLASS as #67).
	 */
	inp = P_INP(&db, h);
	__CPROVER_assume(
	    (size_t)((u_int8_t *)(inp + indx + 1) - (u_int8_t *)h) <= PGSIZE);

	rc = __db_ret_okitem(&db, h, indx);

	if (rc == 0) {
		/*
		 * okitem said "safe to read".  Prove the EXACT downstream read
		 * that __db_ret / __db_retcopy would then perform stays within
		 * the page.  We reproduce __db_ret's (data-pointer, length)
		 * derivation per leaf type and assert the whole [data, data+len)
		 * region is on-page via __CPROVER_r_ok -- reading each page byte
		 * through ONE consistent access (avoids byte-vs-struct aliasing
		 * artifacts and directly checks the OOB-read that bug #67 was).
		 */
		void *data;
		size_t len;
		off = inp[indx];

		if (TYPE(h) == P_HEAP) {
			HEAPHDR *hdr = (HEAPHDR *)((u_int8_t *)h + off);
			/* Split records are copied out elsewhere; okitem skips
			 * their size check, so we do too. */
			if (F_ISSET(hdr, (HEAP_RECSPLIT | HEAP_RECFIRST)))
				return;
			len = hdr->size;
			data = (u_int8_t *)hdr + sizeof(HEAPHDR);
			__CPROVER_assert(__CPROVER_r_ok(data, len),
			    "okitem-safe HEAP: __db_ret data read on-page");
		} else if (TYPE(h) == P_HASH || TYPE(h) == P_HASH_UNSORTED) {
			u_int8_t *hk = (u_int8_t *)h + off;
			if (HPAGE_PTYPE(hk) == H_OFFPAGE) {
				/* __db_ret copies out sizeof(HOFFPAGE) then
				 * fetches the overflow page; the on-page read
				 * is the HOFFPAGE header. */
				__CPROVER_assert(
				    __CPROVER_r_ok(hk, HOFFPAGE_SIZE),
				    "okitem-safe HASH: HOFFPAGE header on-page");
			} else {
				/* len = LEN_HKEYDATA = (prev-off) - HKEYDATA_SIZE(0),
				 * data = hk + 1 (HKEYDATA_DATA). */
				db_indx_t prev = indx == 0 ?
				    (db_indx_t)PGSIZE : inp[indx - 1];
				len = (size_t)(prev - off) - HKEYDATA_SIZE(0);
				data = hk + SSZA(HKEYDATA, data);
				__CPROVER_assert(__CPROVER_r_ok(data, len),
				    "okitem-safe HASH: __db_ret data read on-page");
			}
		} else { /* P_LBTREE / P_LDUP / P_LRECNO */
			BKEYDATA *bk = GET_BKEYDATA(&db, h, indx);
			if (B_TYPE(bk->type) == B_OVERFLOW ||
			    B_TYPE(bk->type) == B_DUPLICATE) {
				__CPROVER_assert(
				    __CPROVER_r_ok(bk, BOVERFLOW_SIZE),
				    "okitem-safe BTREE: BOVERFLOW header on-page");
			} else { /* B_KEYDATA (okitem rejected any other type) */
				len = bk->len;
				data = bk->data;
				__CPROVER_assert(__CPROVER_r_ok(data, len),
				    "okitem-safe BTREE: __db_ret data read on-page");
			}
		}
	}
}
