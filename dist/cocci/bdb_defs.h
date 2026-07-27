/*
 * bdb_defs.h -- Coccinelle macro-hint file for the libdb (Berkeley DB) tree.
 *
 * Berkeley DB uses K&R-era prototype macros (__P(...)) and a family of
 * "shared memory" queue macros (SH_TAILQ_HEAD, SH_LIST_ENTRY, ...) that are
 * used *as declarators*:
 *
 *     typedef int (*TXNINFO_HANDLER) __P((DB_LOG_VRFY_INFO *, ...));
 *     typedef SH_TAILQ_HEAD(__hash_head) DB_HASHTAB;
 *     struct __foo { SH_TAILQ_ENTRY links; };
 *
 * spatch's built-in C parser cannot expand these on its own, so a bare
 *     spatch --sp-file r.cocci --dir src/
 * fails to parse most of the tree (measured: ~0 matches, hundreds of
 * "parse error" / "badcount" lines concentrated in build_unix/db_int.h).
 *
 * Feeding this file with --macro-file teaches spatch the expansions so it
 * parses the real declarations.  The WORKING invocation is:
 *
 *   spatch -I build_unix -I src -I src/dbinc -I src/dbinc_auto \
 *          --all-includes --macro-file dist/cocci/bdb_defs.h \
 *          --sp-file dist/cocci/rule_X.cocci <file-or---dir>
 *
 * Keep the expansions token-for-token compatible with the real macros in
 * src/dbinc/shqueue.h and build_unix/db.h so that struct layouts parse the
 * same way the compiler sees them.
 */

/* --- prototype / portability macros (build_unix/db.h) --------------------- */

/* __P(protos) expands to protos -- the K&R prototype wrapper. */
#define __P(protos)		protos

/* Windows DLL import decoration collapses to nothing on the analysis host. */
#define __DB_IMPORT

/*
 * The C++ guard `extern "C" {` trips the C parser inside headers.  spatch
 * predefines nothing for __cplusplus, so the guard body is what we parse; we
 * cannot #undef a builtin here, but the guard is only reached when the parser
 * follows the false branch -- documenting for maintainers.  See --all-includes.
 */

/* --- shared-memory queue declarators (src/dbinc/shqueue.h) ---------------- */
/*
 * These MUST match shqueue.h field-for-field so struct layouts parse
 * identically.  They are used both as type definitions and as struct members.
 */

#define SH_TAILQ_HEAD(name)						\
struct name {								\
	db_ssize_t stqh_first;						\
	db_ssize_t stqh_last;						\
}

#define SH_TAILQ_ENTRY							\
struct {								\
	db_ssize_t stqe_next;						\
	db_ssize_t stqe_prev;						\
}

#define SH_LIST_HEAD(name)						\
struct name {								\
	db_ssize_t slh_first;						\
}

#define SH_LIST_ENTRY							\
struct {								\
	db_ssize_t sle_next;						\
	db_ssize_t sle_prev;						\
}

#define SH_CHAIN_ENTRY							\
struct {								\
	db_ssize_t sce_next;						\
	db_ssize_t sce_prev;						\
}

/* --- BSD sys/queue.h declarators (src/dbinc/queue.h) ---------------------- */
/*
 * The non-shared (pointer) queue macros are also used as declarators in
 * struct bodies and typedefs.  TRACEBUF is empty in the non-debug build.
 */

#define TRACEBUF

#define LIST_HEAD(name, type)						\
struct name {								\
	struct type *lh_first;						\
}

#define LIST_ENTRY(type)						\
struct {								\
	struct type *le_next;						\
	struct type **le_prev;						\
}

#define TAILQ_HEAD(name, type)						\
struct name {								\
	struct type *tqh_first;						\
	struct type **tqh_last;						\
	TRACEBUF							\
}

#define TAILQ_ENTRY(type)						\
struct {								\
	struct type *tqe_next;						\
	struct type **tqe_prev;						\
	TRACEBUF							\
}

#define CIRCLEQ_HEAD(name, type)					\
struct name {								\
	struct type *cqh_first;						\
	struct type *cqh_last;						\
}

#define CIRCLEQ_ENTRY(type)						\
struct {								\
	struct type *cqe_next;						\
	struct type *cqe_prev;						\
}
