# RFC 0014 — Implementation specification (Phases 0 and 1)

- **Implements:** `rfc/0014-typed-data-and-constraints.md`, decisions D1–D8
  ("Maintainer decisions taken for implementation").
- **Branch:** `rfc14-typed`, cut from master `2d8397dfe`.
- **Audience:** the implementation agent. Every choice is made here. Where this
  document says "TODO-VERIFY", the implementer runs the named probe *first*; the
  spec says what to do for each outcome.
- **Citations** are `file:line` against master `2d8397dfe`.

Contents: §0 corrections to the RFC · §1 public API · §2 configure · §3 side
table (D2) · §4 catalog (D3) · §5 fingerprint · §6 OPTREAD gate (D7) · §7 test
plan · §8 Phase 0 · §9 work breakdown.

---

## §0 Corrections to the RFC (read first)

Each of these changes what gets built. Evidence is inline.

**C1. `DB->set_schema` / `DB_ENV->codec_register` cannot be methods under D2.**
A method is a function pointer in `struct __db` / `struct __db_env`
(`src/dbinc/db.in:1439` onward, the "DB PUBLIC HANDLE LIST"). Adding one changes
`sizeof(DB)` (pinned at 1744 by `test/db/handle_sizes.c:51`) and `sizeof(DB_ENV)`
(2088, `:53`), and both structs are hashed into the region signature
(`src/env/env_sig.c:121` `__ADD(__db)`, `:129` `__ADD(__db_env)`). D2 forbids
both. **Smallest change:** free functions taking the handle, exactly like the
existing `db_get_multiple(DB *, ...)` and `db_copy(DB_ENV *, ...)`
(`src/dbinc_auto/ext_prot.in`). See §1.

**C2. New public structs in `db.in` would change the region signature in every
build.** `dist/s_sig` emits `__ADD(x)` for every line in `db.in`, `db_int.in` and
`src/dbinc/*.h` matching `struct __x {` (the second sed in `dist/s_sig`, the
`s/.*[	 ]*struct[	 ]*\(__[a-z_]*\)[	 ]*{.*/	__ADD(\1);/p` rule). So
`struct __db_codec {` in `db.in` would be hashed the next time anyone runs
`s_sig` — in the *default* build too. **Fix:** add the three new tags to the
exclusion list at the top of that sed (the same mechanism that already excludes
`__addrinfo`, `__channel`, `__repmgr_*`), so regenerating `env_sig.c` is a no-op.
Internal typed structs go in `src/typed/typed_int.h`, which `s_sig` does not scan.

**C3. The side table cannot hang off `ENV`.** The brief suggests per-`ENV`.
`struct __env` (`src/dbinc/db_int.in:776`) is hashed (`env_sig.c:140`), and so is
`struct __db_globals` (`src/dbinc/globals.h:21`, a `typedef struct __db_globals {`
line that `s_sig` matches). Any new field there moves the signature. **Design:** a
process-global table in `src/typed/typed.c` (a file-static, not a struct `s_sig`
can see), indexed by a slot number stored in spare bits of `DB->flags2`
(`db.in:1844-1848`: only bits `0x1`, `0x2`, `0x4` are used). See §3.

**C4. Drop `codec_register` / `record_codec_register` from Phase 1.** Under C1
they would be free functions, but nothing in Phase 1 needs them: the schema
passed to `db_set_schema` already holds pointers to every codec it uses
(`DB_RECORD_CODEC.types[]`), and libdb cannot rebuild a record layout from a
codec name alone (field location is application code). The catalog check (§4,
§5) compares the codecs *in the passed schema* against the catalog. Add a
registry when Phase 2's FK persistence needs name→codec resolution without a
schema.

**C5. `DB_CODEC_MALFORMED (-30780)` is outside libdb's error namespace.**
`db.in:1374-1377`: "we own the error name space from -30,800 to -30,999";
`docs_src/guides/programmer_reference/program_errorret.md:19` says the same.
-30780 is free but is not ours. Public codes in use run -30999…-30967
(`db.in:1379-1411`); private codes -30899…-30887 (`:1414-1427`). **Use -30966,
-30965, -30964** (§1.3).

**C6. D3 (catalog subdatabase in the same file) works only for named
subdatabases.** A file opened with `dname == NULL` has no master database to hold
`__db_typed` (the RFC's own option (a) text says this). In-memory named databases
have no file at all. **Rule:** Phase 1 typed databases must be opened with
`dname != NULL` and `fname != NULL`; otherwise `DB->open` fails `EINVAL`
("typed databases must be named subdatabases in a file"). This also covers dbsql's
one-file layout, which the RFC gave as D3's motivation.

**C7. Phase 0 item 1 is no longer a Phase 0 fix.** D6 makes the `DB_KEYEXIST`
return conditional on the database being typed, so it needs the typed flag. It
moves into Phase 1 (commit 7, §9). Untyped behaviour is untouched.

**C8. Phase 0 item 2 is worse than a "dangling entry", and there is a second,
real leak next to it.** See §8.2: the stale entry causes wrong cascades, silently
disables a live FK check when the wrong foreign DB closes, and becomes a
use-after-free if the secondary closes first. Separately,
`__db_disassociate_foreign` mallocs a `DB_FOREIGN_INFO` into `tmp` and then
overwrites `tmp` (`src/db/db.c:1302-1309`), leaking one allocation on every close
of a secondary that has a foreign database.

**C9. The RFC's put-path hook TODO resolves to one function, `__dbc_put`, given
three Phase 1 refusals.** Every external put reaches `__dbc_put`
(`src/db/db_cam.c:2017`) except `DB_APPEND`, which is unreachable for Btree/Hash,
and compressed `DB_MULTIPLE`, which hands whole bulk buffers to `__dbc_put`. Phase
1 refuses compression, partitioning and partial puts on typed databases (§3.5).

**C10. The OPTREAD gate needs a totality flag on the record codec.** A generated
comparator calls the record codec's `field()` on torn bytes before it calls any
type codec. `DB_RECORD_CODEC` therefore gains a `flags` word that carries
`DB_CODEC_TOTAL` (§1.2, §6).

**C11. `DB_CODEC_MEMCMP` implies `DB_CODEC_EQUALIMAGE`.** If `memcmp` has the
sign of `compare`, then `compare == 0` iff the bytes are identical. `db_set_schema`
treats MEMCMP as setting EQUALIMAGE.

**C12. Phase 1 typed databases refuse `DB_DUP`/`DB_DUPSORT`.** The RFC's
generator table lists a generated `dup_compare`. Nothing in Phase 1's
constraint set needs duplicates, and a duplicate set makes "the record" of a
put ambiguous for `DB_CURRENT` and for UNIQUE (TODO-VERIFY: whether
`__db_associate_arg` already refuses a `DB_DUP` primary). Refused in
`db_set_schema` and again at open (§3.5). Lift it in Phase 2 with the
generated `dup_compare`.

---

## §1 Public API

### 1.1 Where things go in `src/dbinc/db.in`

All additions are **unconditional** text in `db.h`. A public header cannot test
`HAVE_TYPED`, which lives in `db_config.h`. Declarations and `#define`s produce no
code, so a default build's library is unchanged (§2.4 proves it). In a default
build the four functions are simply absent from `libdb.so`, so an application that
calls them fails at **link** time: fail-closed, and no stub enlarges the default
library.

| Item | Location in `db.in` |
|---|---|
| `typedef struct __db_codec DB_CODEC;` and the other two typedefs | the forward-typedef block near `typedef struct __db_dbt DBT;` |
| `struct __db_codec`, `struct __db_record_codec`, `struct __db_constraint`, `struct __db_schema` | new block after `struct __db_dbt` (after `db.in:258`'s `HAVE_MIXED_SIZE_ADDRESSING` block ends) headed `/* Typed data (RFC 0014); used only with --enable-typed. */` |
| 3 error codes | after `DB_SNAPSHOT_UNSAFE` at `db.in:1411` |
| `DB2_AM_TYPED`, `DB2_AM_TYPED_FILE`, slot field | after `DB2_AM_NOWAIT` at `db.in:1846` |
| prototypes | generated into `src/dbinc_auto/ext_prot.in` by `dist/s_include` from `EXTERN:` comments in `src/typed/typed.c` |

### 1.2 Structures

```c
/* Codec function result: the value's bytes are not well formed. */
/* (error code; see 1.3) */

struct __db_codec {
	const char *name;	/* Stable identity, <= 255 bytes, no NUL inside. */
	u_int32_t   version;	/* Bump whenever compare/equal/hash/sortkey change. */
#define	DB_CODEC_TOTAL		0x0001	/* Every function is total over any bytes. */
#define	DB_CODEC_MEMCMP		0x0002	/* memcmp order == compare order. Implies EQUALIMAGE. */
#define	DB_CODEC_EQUALIMAGE	0x0004	/* compare()==0 iff byte-identical. */
#define	DB_CODEC_FIXED		0x0008	/* Every value is exactly fixed_len bytes. */
	u_int32_t   flags;
	u_int32_t   fixed_len;	/* Used iff DB_CODEC_FIXED. */

	/*
	 * Reads bounded by [p, p + len).  Must not allocate, lock, call libdb,
	 * or fault.  Deterministic.  NULL compare is legal only with MEMCMP.
	 */
	int	  (*compare) __P((const DB_CODEC *, const u_int8_t *, u_int32_t,
		      const u_int8_t *, u_int32_t));
	int	  (*equal) __P((const DB_CODEC *, const u_int8_t *, u_int32_t,
		      const u_int8_t *, u_int32_t));	/* NULL: compare()==0 */
	u_int32_t (*hash) __P((const DB_CODEC *, const u_int8_t *, u_int32_t));
				/* NULL only with EQUALIMAGE (bytes are hashed). */
	int	  (*validate) __P((const DB_CODEC *, const u_int8_t *, u_int32_t));
				/* 0 or DB_CODEC_MALFORMED; NULL: always valid */
	int	  (*sortkey) __P((const DB_CODEC *, const u_int8_t *, u_int32_t,
		      u_int8_t *, u_int32_t, u_int32_t *));
				/* (in, inlen, out, outlen, &needed); optional.
				 * outlen < needed: write nothing, set *needed,
				 * return DB_BUFFER_SMALL. */
	void	   *app_private;

	const DBT  *vectors;	/* Golden vectors (data/size only). */
	u_int32_t   nvectors;	/* 0..256.  >= 2 required unless MEMCMP. */
};

struct __db_record_codec {
	const char *name;	/* <= 255 bytes */
	u_int32_t   version;
	u_int32_t   flags;	/* DB_CODEC_TOTAL only (C10). */
	u_int32_t   nfields;	/* 1..65535 */
	/* Locate field i.  Total, bounded by rec->size.  Malformed record:
	 * return DB_CODEC_MALFORMED.  *p/*len must lie inside rec. */
	int (*field) __P((const DB_RECORD_CODEC *, const DBT *, u_int32_t,
	    const u_int8_t **, u_int32_t *, int *));
	int (*validate) __P((const DB_RECORD_CODEC *, const DBT *)); /* optional */
	const DB_CODEC **types;	/* nfields entries, none NULL */
	void *app_private;
};

struct __db_constraint {
#define	DB_CONSTRAINT_NOT_NULL	1
#define	DB_CONSTRAINT_MAXLEN	2
#define	DB_CONSTRAINT_VALID	3	/* record + every field validates */
#define	DB_CONSTRAINT_UNIQUE	4
	u_int32_t   kind;
#define	DB_TYPED_KEY		1
#define	DB_TYPED_DATA		2
	u_int32_t   side;	/* DB_TYPED_KEY or DB_TYPED_DATA */
#define	DB_TYPED_WHOLE		0xffffffff	/* MAXLEN on the whole DBT */
	u_int32_t   field;	/* field number, or DB_TYPED_WHOLE (MAXLEN only) */
	u_int32_t   maxlen;	/* MAXLEN only */
#define	DB_CONSTRAINT_NULLS_NOT_DISTINCT 0x0001	/* UNIQUE only */
	u_int32_t   flags;
	const char *name;	/* Reported in errors; <= 255 bytes; unique in schema */
	DB	   *unique_db;	/* UNIQUE only: open, non-DUP, Btree, caller-owned */
};

struct __db_schema {
	DBTYPE	type;			/* DB_BTREE or DB_HASH */
	const DB_RECORD_CODEC *key;	/* NULL: key is untyped bytes */
	const DB_RECORD_CODEC *data;	/* NULL: data is untyped bytes */
	const DB_CONSTRAINT *constraints;
	u_int32_t nconstraints;		/* 0..64 */
	u_int32_t flags;		/* must be 0 in Phase 1 */
};
```

`DB_CONSTRAINT_UNIQUE` on a field whose codec is neither EQUALIMAGE (or MEMCMP)
nor has `sortkey` is refused by `db_set_schema` with `EINVAL`: a byte-compared
secondary key cannot express its equality.

### 1.3 Error codes

| Name | Value | Returned by | `db_strerror` text | `DB_STR` id |
|---|---|---|---|---|
| `DB_CONSTRAINT_VIOLATION` | **-30966** | a put that fails NOT NULL / MAXLEN / VALID | `DB_CONSTRAINT_VIOLATION: A typed-database constraint was violated` | next free (TODO-VERIFY: `grep -rho 'DB_STR("0[0-9]*"' src lang \| sort -u \| tail`; 0211/0212 are the SSI pair) |
| `DB_CODEC_MISMATCH` | **-30965** | `DB->open`: same codec name+version, different fingerprint | `DB_CODEC_MISMATCH: Codec behaviour differs from the database's record` | next free |
| `DB_CODEC_MALFORMED` | **-30964** | codec / record-codec callbacks to libdb. libdb never returns it from a public call: the put hook maps it to `DB_CONSTRAINT_VIOLATION` | `DB_CODEC_MALFORMED: Value bytes are not well formed for their codec` | next free |

The UNIQUE violation returns **`DB_KEYEXIST`** (D6), only on a typed primary.

All three go in the "DB (public) error return codes" block (`db.in:1378-1411`), in
numeric order after `DB_SNAPSHOT_UNSAFE`. Each needs a `case` in `db_strerror`
(`src/common/db_err.c:252-375`). `dist/validate/s_chk_err` extracts
`define.*DB_.*-309` from `db.in` and diffs it against the `case DB_` labels, so a
missing `case` fails `s_validate`.

### 1.4 Flags in `struct __db` (no size change)

```c
#define DB2_AM_TYPED		0x00000008 /* Handle has a schema (db_set_schema) */
#define DB2_AM_TYPED_FILE	0x00000010 /* Meta page carries BTM/HASH typed bit */
#define	DB2_TYPED_SLOT_SHIFT	16	   /* flags2 bits 16-31: side-table slot */
#define	DB2_TYPED_SLOT(dbp)	((dbp)->flags2 >> DB2_TYPED_SLOT_SHIFT)
```

`flags2` is a `u_int32_t` already in the struct (`db.in:1848`); its users are
`F2_SET/F2_CLR/F2_ISSET` (`db_int.in:268-270`) with three bits (`db.in:1844-1846`).
All writes to `flags2` today happen before open or in recovery
(`db_method.c:805-807`, `dbreg_util.c:699`, `log_print.c:360`), so storing a slot
number in the high half races with nothing.

### 1.5 Functions (prototypes generated from `EXTERN:` comments)

```c
int db_set_schema __P((DB *, const DB_SCHEMA *, u_int32_t));
int db_get_schema __P((DB *, const DB_SCHEMA **));
int db_record_fixed __P((DB_RECORD_CODEC *, const char *, u_int32_t,
    u_int32_t, const u_int32_t *, const u_int32_t *, const DB_CODEC **));
	/* (rc, name, version, nfields, off[], len[], types[]) */
```

- `db_set_schema(dbp, schema, flags)`: `flags` must be 0. Legal only before
  `DB->open` (`DB_ILLEGAL_AFTER_OPEN`), at most once per handle. **Deep-copies**
  the schema, both record codecs and their `types[]` arrays (the `DB_CODEC`
  structs themselves are *not* copied: the application keeps them alive until
  `DB->close`, as with any callback). Validates every rule in §3.4, computes the
  fingerprint (§5), installs the generated callbacks (§3.6), restricts
  `dbp->am_ok` to the schema's type (`DB_ILLEGAL_METHOD`), sets `DB2_AM_TYPED`
  and the slot.
- `db_get_schema`: returns the libdb-owned copy; `EINVAL` if none.
- `db_record_fixed`: fills an application-owned `DB_RECORD_CODEC` whose `field()`
  returns `(rec + off[i], len[i])`, `DB_CODEC_MALFORMED` if
  `off[i] + len[i] > rec->size` (overflow-safe: `len[i] > size || off[i] > size -
  len[i]`), `*isnull = 0`. Flags `DB_CODEC_TOTAL`. The arrays are referenced,
  not copied, until `db_set_schema` copies them.

### 1.6 Generators and bindings

| Generator | Phase 1 effect |
|---|---|
| `dist/api_flags` / `s_apiflags` | **Unchanged.** It generates *method-argument* flag values. The new constants are struct-field flags (like the `DB_DBT_*` set, which also lives directly in `db.in`), and `db_set_schema`'s flag word has no valid bits. |
| `dist/s_include` | Add `typed` to the directory loop (`for i in db btree clib common ... xa;`). That produces `src/dbinc_auto/typed_ext.h` and adds the three `EXTERN:` prototypes to `ext_prot.in` (and `ext_def.in` for unique names). Include `dbinc_auto/typed_ext.h` from `db_int.in` next to the other `*_ext.h` includes; its prototypes are harmless in a default build. |
| `dist/s_sig` | Exclusion lines `/struct __db_codec {/d`, `/struct __db_record_codec {/d`, `/struct __db_constraint {/d`, `/struct __db_schema {/d` in the second sed (C2). Gate: `cd dist && sh s_sig && git diff --exit-code ../src/env/env_sig.c`. |
| `dist/s_windows` | Regenerate `build_windows/db.h` (it seds `db.in`); no script change. |
| `dist/pubdef.in` | Add, alphabetically: `DB2_AM_TYPED * I *`, `DB2_AM_TYPED_FILE * I *`, `DB_CODEC_EQUALIMAGE D I *`, `DB_CODEC_FIXED D I *`, `DB_CODEC_MALFORMED D I *`, `DB_CODEC_MEMCMP D I *`, `DB_CODEC_MISMATCH D I *`, `DB_CODEC_TOTAL D I *`, `DB_CONSTRAINT_MAXLEN D I *`, `DB_CONSTRAINT_NOT_NULL D I *`, `DB_CONSTRAINT_NULLS_NOT_DISTINCT D I *`, `DB_CONSTRAINT_UNIQUE D I *`, `DB_CONSTRAINT_VALID D I *`, `DB_CONSTRAINT_VIOLATION D I *`, `DB_TYPED_DATA D I *`, `DB_TYPED_KEY D I *`, `DB_TYPED_WHOLE D I *`. No `J` column: Java gets nothing in Phase 1 (TODO-VERIFY how `DB2_AM_EXCL` is listed and copy that). |
| Java (`lang/java`, `s_java_const`) | **None.** `DbConstants.java` is generated from the `J` column of `pubdef.in`; with no `J`, nothing is emitted. No Java API for typed databases in Phase 1. |
| Tcl (`lang/tcl`) | **None.** Tcl cannot create a typed database, so it never sees the new codes. |
| C++ (`lang/cxx`) | **None.** `DbException` carries any integer code. |

---

## §2 The configure option

### 2.1 autoconf

`dist/aclocal/options.m4`, after the `handoff-trace` block (`:212-217`), same shape:

```m4
AC_MSG_CHECKING(if --enable-typed option specified)
AC_ARG_ENABLE(typed,
	[AS_HELP_STRING([--enable-typed],
			[Build the typed-data and constraint layer (RFC 0014).])],
	[db_cv_typed="$enable_typed"], [db_cv_typed="no"])
AC_MSG_RESULT($db_cv_typed)
```

`dist/configure.ac`, after the `HAVE_HANDOFF_TRACE` block (`:173-177`):

```sh
# Typed data and constraints (RFC 0014).  Off by default.  When off,
# HAVE_TYPED is undefined, src/typed/*.c compile to nothing, and every hook
# in the engine is #ifdef'd out: the library is the stock one.
if test "$db_cv_typed" = "yes"; then
	AC_DEFINE(HAVE_TYPED)
	AH_TEMPLATE(HAVE_TYPED,
    [Define to 1 to build the typed-data and constraint layer.])
fi
```

Regenerate `dist/configure` and `dist/config.hin` with `cd dist && sh s_config`
and commit both (`git add -f`). No library to detect, so the `--with-uring`
explicit/implicit split (`configure.ac:221-262`) does not apply; what is copied
from that precedent is the meson parity and the guarded-source pattern.

### 2.2 meson

`meson_options.txt`, after `uring`:

```meson
# Mirrors autoconf's --enable-typed.  OFF by default; 'disabled' must stay the
# default or run_meson_autoconf_parity.sh's HAVE_* comparison fails.
option('typed', type: 'feature', value: 'disabled',
  description: 'Typed-data and constraint layer (RFC 0014)')
```

`dist/meson.build`, after the uring block (`:132-151`):

```meson
if get_option('typed').enabled()
  cdata.set('HAVE_TYPED', 1)
endif
```

and `'src/typed/typed.c', 'src/typed/typed_catalog.c', 'src/typed/typed_gen.c'`
in `libdb_srcnames`, alphabetically between `'src/txn/txn.c'` and `'src/xa/xa_map.c'`.

### 2.3 Sources, guard and lists (the `--with-uring` precedent)

`os_aio_uring.c` is listed **unconditionally** in both builds
(`dist/meson.build:535`; `os_aio_uring@o@` in `DTRACE_OBJS`, `dist/Makefile.in:303`)
and is whole-file guarded (`src/os/os_aio_uring.c:30` `#ifdef HAVE_IO_URING`, `:231`
`#endif`). Copy that exactly:

- New files `src/typed/typed.c` (API, side table, put check, open/close hooks),
  `src/typed/typed_gen.c` (generated callbacks, `db_record_fixed`),
  `src/typed/typed_catalog.c` (catalog and fingerprint), `src/typed/typed_int.h`
  (internal structs; outside `src/dbinc/`, so `s_sig` does not see it). Each `.c`
  starts with the SPDX header from the handoff, then `#include "db_config.h"`,
  `#include "db_int.h"`, then `#ifdef HAVE_TYPED` … `#endif` around the whole body.
- `dist/srcfiles.in`: `src/typed/typed.c`, `src/typed/typed_catalog.c`,
  `src/typed/typed_gen.c`, alphabetically after `src/txn/txn_util.c`, **no**
  `android` tag.
- `dist/Makefile.in`: `typed@o@ typed_catalog@o@ typed_gen@o@` in `DTRACE_OBJS`
  (`:285-313`), plus three compile rules in the `$(srcdir)/...` section, for
  example:
  `typed@o@: $(srcdir)/typed/typed.c` / `	$(CC) $(CFLAGS) $(DEPFLAGS) $(srcdir)/typed/typed.c`.
- Windows: `test/db/run_windows_srclist.sh` (W1) fails if a `srcfiles.in` source
  is missing from `build_windows/VS10/db.vcxproj` or `build_windows/VS8/db.vcproj`.
  Add the three files to `dist/win_projects/projects.template.xml` and to both
  generated project files, following `log_handoff_trace.c`
  (`VS10/db.vcxproj:366`, `VS8/db.vcproj:245`). The whole-file guard is what makes
  that safe: Windows never defines `HAVE_TYPED`.

### 2.4 Parity, footprint and "byte-identical in behaviour"

- **`run_meson_autoconf_parity.sh` stays green** because it compares two
  *default* builds (it configures both itself and "deliberately takes NO build-dir
  argument"). Both defaults leave `HAVE_TYPED` undefined (`db_cv_typed="no"`,
  `value: 'disabled'`), so the `HAVE_*` sets match, and the signature is unchanged
  (§3), so both cross-attach arms pass.
- **Engine hooks** are all `#ifdef HAVE_TYPED`. With it undefined the preprocessed
  engine sources are token-identical to master's apart from line numbers, which
  reach code only through `__LINE__` under `DIAGNOSTIC`.
- **Footprint gate, `test/db/run_typed_footprint.sh`** (MANIFEST tier `db`). It
  builds master (`git worktree add` to a temp dir at `$(git merge-base HEAD
  master)`) and HEAD, both with plain `dist/configure`, and asserts:
  1. `size -A .libs/libdb-*.so` `.text` equal (byte-exact);
  2. `BUILD_DIR=… sh dist/env_sig_print.sh` equal;
  3. `nm -D .libs/libdb-*.so | grep -c 'db_set_schema\|__typed_'` is 0 in HEAD's
     default build and > 0 in a `--enable-typed` build;
  4. `env_sig_print.sh` equal between HEAD default and HEAD `--enable-typed`
     (the D2 promise).
  Verdict line `VERDICT typed_footprint PASS|FAIL <which>`. Sabotage arm: add one
  unguarded `int __typed_canary(void) { return 1; }` to `src/db/db_cam.c` → (1)
  must FAIL. Second arm: drop the `s_sig` exclusion, rerun `s_sig` → (2) and (4)
  must FAIL. If (1) fails on the unmodified branch because the empty `typed*.o`
  objects add bytes, move them from `DTRACE_OBJS` to a `TYPED_OBJS` list added to
  `ADDITIONAL_OBJS` only under `--enable-typed` (the `SIM_OBJS` precedent,
  `configure.ac:146-150`, `Makefile.in:220`) and make meson's list conditional too.

---

## §3 D2's side table

### 3.1 Data structure

All of it lives in `src/typed/typed_int.h` (not scanned by `s_sig`, C2) and
`src/typed/typed.c`.

```c
/* src/typed/typed_int.h */
typedef struct __typed_state {
	DB		*dbp;		/* Owner; DIAGNOSTIC asserts it. */
	DB_SCHEMA	 schema;	/* Deep copy (§1.5); points into the */
	DB_RECORD_CODEC	 key_rc, data_rc;	/* fields below, never into */
	const DB_CODEC	**key_types, **data_types;	/* caller memory */
	DB_CONSTRAINT	*constraints;	/* except DB_CODEC structs and */
	char		*strings;	/* unique_db handles.  One alloc. */
	u_int32_t	 nunique;	/* # of UNIQUE constraints (3.7) */
	u_int32_t	 key_memcmp;	/* every key field MEMCMP */
	u_int32_t	 key_equalimage;/* every key field EQUALIMAGE */
	u_int8_t	 fp[20];	/* Fingerprint (§5) */
} TYPED_STATE;

/* src/typed/typed.c */
#define	TYPED_NSLOTS	4096	/* ponytail: fixed table; 16 slot bits
				 * allow 65535 if a process ever needs more. */
static TYPED_STATE * volatile __typed_slot[TYPED_NSLOTS];  /* [0] unused */
```

- **Slot 0 means "untyped".** `DB2_TYPED_SLOT(dbp)` (§1.4) is 0 for every handle
  that never called `db_set_schema`, because `db_create` zeroes the handle and
  no code assigns `flags2` wholesale: every write is `F2_SET`/`F2_CLR` of a named
  bit (`db_method.c:805-806`, `dbreg_util.c:699`, `log_print.c:360`;
  `orig_flags2`, `db.in:1847`, is declared and never read or written anywhere in
  `src/`). `__db_refresh` restores `dbp->flags` from `orig_flags`
  (`db.c:1165`, `:1241`) and never touches `flags2`, so the slot survives a
  refresh.
- `DB2_AM_TYPED` is set iff the slot is non-zero. DIAGNOSTIC asserts the
  invariant in `__typed_put_check` and `__typed_release`.
- 4096 slots is 32 KB of `.bss`, present only in a `--enable-typed` build.
  Exhaustion: `db_set_schema` returns `ENOMEM` with "more than 4095 typed DB
  handles open in this process".

### 3.2 Locking: none on any hot path

No mutex is added, global or per-handle, on the put path or in a generated
callback. The table is **written only by `db_set_schema` (claim) and
`__typed_release` (free)**. Readers index it with the handle's own slot.

- **Claim** (`db_set_schema`, before `DB->open`, cold): scan from a static
  rotating hint for a `NULL` entry and install the state with
  `__os_atomic_cas_ptr` (`src/os/os_atomic.c:294`), retrying on CAS failure.
  The CAS is a full barrier, so the state's contents are visible before its
  pointer is. No lock, so no `mtx_regenv` traffic (RFC 0015's top cliff).
- **Read** (`__typed_put_check`, generated callbacks, hot): a plain load of
  `__typed_slot[DB2_TYPED_SLOT(dbp)]`. This is safe without a barrier because
  the slot is owned by that handle from `db_set_schema` to `DB->close`, and
  another thread can only use the handle after it was handed the `DB *`,
  which already required synchronisation after the CAS. Using a handle
  concurrently with its own `DB->close` is already undefined in libdb.
- **Free** (`__typed_release`, cold): store `NULL` into the slot with
  `__os_atomic_cas_ptr(env, slot, ts, NULL)`, then `__os_free` the state. The
  free cannot race a reader, by the same ownership argument.
- The cost on an **untyped** database in an enabled build is one
  `F2_ISSET(dbp, DB2_AM_TYPED)` test on a word in a cache line `__dbc_put`
  already reads (`dbp->flags` is tested at `db_cam.c:2044`). In a default
  build it is zero (`#ifdef HAVE_TYPED`).
- **Rejected:** a `DB *`-keyed hash table (needs a lock or a lock-free hash for
  a lookup the slot makes O(1)), and per-`ENV` storage (C3).

### 3.3 Lifecycle

| Event | Side-table effect | Where |
|---|---|---|
| `db_set_schema(dbp, s, 0)` | validate (3.4), allocate `TYPED_STATE` with `__os_malloc(dbp->env)`, claim slot, `F2_SET(dbp, DB2_AM_TYPED)`, store slot in bits 16-31, install callbacks (3.6) | `src/typed/typed.c` |
| `DB->open` | `__typed_open` (3.5, §4, §5, §6). No table write. | `db_open.c:267`, see 3.5 |
| failed `DB->open` | none: the caller must still `DB->close` the handle, which releases | — |
| put | `__typed_put_check` reads only | 3.7 |
| `DB->close`, including the deferred close of a handle whose close-log failed inside a txn | `__typed_release(dbp)`: free the state, free the slot, clear the bits | `db.c`, new lines between `:771` and `:773` (below) |
| `DB->remove`, `DB->rename` on a `DB` handle | none beyond close: `__db_remove` and `__db_rename` end in `__db_close` (`db_remove.c:222`, `db_rename.c:210`), so the release above runs. Catalog effects are §4.4. | — |
| `DB->truncate` | none: the schema belongs to the database, not its rows | — |
| recovery, `db_recover` | none: recovery creates its handles internally and never calls `db_set_schema`; `__typed_open` is skipped when `DB_AM_RECOVER` is set (D5) | — |

The release goes in `__db_close` (`src/db/db.c:749`) **after** the
`deferred_close` early return (`:770-771`) and **before** `env->db_ref` is
decremented and a `DB_ENV` private to the handle may be closed (`:783-788`),
because the state was allocated against `dbp->env`:

```c
	if (deferred_close)
		return (ret);
#ifdef HAVE_TYPED
	if (F2_ISSET(dbp, DB2_AM_TYPED))
		__typed_release(dbp);
#endif
```

A deferred close returns with the handle alive and comes back through
`__db_close` once the transaction resolves, so the release runs exactly once.
`__db_close` is the only place a `DB` is destroyed (`memset` + `__os_free`,
`db.c:791-792`), so no handle can leak a slot.

### 3.4 `db_set_schema` validation (all `EINVAL`, each with a `__db_errx` naming the item)

1. `flags == 0`; `schema->flags == 0`; `DB_ILLEGAL_AFTER_OPEN`; not already typed.
2. `schema->type` is `DB_BTREE` or `DB_HASH` (D4). Restrict `dbp->am_ok` with
   `DB_ILLEGAL_METHOD`-equivalent so a later `DB->open` with another type fails.
3. At least one of `key`, `data` non-NULL.
4. Each record codec: `name` non-NULL, `strnlen(name, 256) < 256`;
   `1 <= nfields <= 65535`; `field != NULL`; `types` non-NULL with no NULL
   entry; `flags & ~DB_CODEC_TOTAL == 0`.
5. Each distinct `DB_CODEC`: name as above; `flags` only the four defined bits;
   `FIXED` ⇒ `fixed_len > 0`; `compare == NULL` ⇒ `MEMCMP`; `hash == NULL` ⇒
   `EQUALIMAGE` (or `MEMCMP`, C11); `nvectors <= 256`; `nvectors >= 2` unless
   `MEMCMP`; `vectors != NULL` iff `nvectors > 0`. Two codecs with the same
   name and version must be the same pointer.
6. `nconstraints <= 64`, `constraints != NULL` iff `nconstraints > 0`. Each:
   known `kind`; `side` names a non-NULL record codec; `field < nfields`, or
   `DB_TYPED_WHOLE` only with `MAXLEN`; `flags` only `NULLS_NOT_DISTINCT`, only
   with `UNIQUE`; `name` non-NULL, ≤ 255 bytes, unique in the schema.
7. `UNIQUE`: `side == DB_TYPED_DATA` (a key is unique already);
   `unique_db != NULL`, opened (`F_ISSET(unique_db, DB_AM_OPEN_CALLED)`),
   `type == DB_BTREE`, not `DB_AM_DUP`, same `env`, not already a secondary;
   the field's codec is `EQUALIMAGE` (or `MEMCMP`) or has `sortkey` (§1.2); no
   two UNIQUE constraints share a `unique_db`.
8. The handle has no application callback already set: `bt_compare`,
   `bt_prefix`, `dup_compare`, `h_compare`, `h_hash` are the defaults
   (TODO-VERIFY the default pointer values in `__bam_db_create` /
   `__ham_db_create` and compare against those), and `DB_AM_DUP` is clear
   (C12).

### 3.5 Phase 1 refusals (C9, C6, C12) and where each is enforced

`__typed_open` is called from `__db_open` (`src/db/db_open.c:52`) after the
access method and partition opens succeed and before the post-open test hook,
i.e. inserted between `db_open.c:266` (`#endif` of the partition block) and
`:267` (`DB_TEST_RECOVERY(... DB_TEST_POSTOPEN ...)`):

```c
#ifdef HAVE_TYPED
	if (F2_ISSET(dbp, DB2_AM_TYPED) && !F_ISSET(dbp, DB_AM_RECOVER) &&
	    (ret = __typed_open(dbp, ip, txn, fname, dname, flags)) != 0)
		goto err;
#endif
```

A failure there is cleaned up by the caller exactly like any other open
failure: the child/auto transaction aborts, or, non-transactionally, a created
file or subdatabase is removed (`db_iface.c:1433-1445`).

| Refusal | Enforced at | Why there |
|---|---|---|
| **C6** `fname == NULL` or `dname == NULL` | `__db_open_arg` (`src/db/db_iface.c:1464`), new block before its `return (0)` at `:1615`, under `#ifdef HAVE_TYPED` | before any file is created |
| **Partitioning** | **no new code.** C6 forces `dname != NULL`, and `__db_open` already refuses a partitioned handle with a subdatabase name (`db_open.c:199-203`, BDB0637, returns `ENOENT`). A subdatabase cannot carry partition metaflags, so the meta path (`db_open.c:671-675`) cannot set `p_internal` either. `typed_refusals` asserts the existing refusal. | already fail-closed |
| **Compression** | `__typed_open`: `DB_IS_COMPRESSED(dbp)` ⇒ `EINVAL`, under `#ifdef HAVE_COMPRESSION` | compression can come from `set_bt_compress` *or* from `BTM_COMPRESS` on an existing file (`bt_open.c:245-250`), so only a post-AM-open check sees both |
| **Partial puts** (`DB_DBT_PARTIAL` on key or data) | `__typed_put_check` (3.7), per put | not an open-time property |
| **C12** `DB_DUP` / `DB_DUPSORT` | `db_set_schema` (3.4 rule 8) and again in `__typed_open` (`F_ISSET(dbp, DB_AM_DUP)`, which `__bam_metachk` / `__ham_metachk` set from the file, `bt_open.c:172-173`, `hash_open.c:199-200`) | a flag set after `db_set_schema`, or carried by the file, is caught at open |
| callback overridden after `db_set_schema` | `__typed_open`: each of `bt_compare`, `bt_prefix`, `h_compare`, `h_hash` must equal what 3.6 installed | `set_bt_compare` etc. are existing methods that cannot be intercepted without editing them |

### 3.6 Generated callbacks (installed by `db_set_schema`, through the existing setters)

| Schema | Installed |
|---|---|
| Btree, `key == NULL` or every key field `MEMCMP` | nothing: default comparator and prefix |
| Btree, otherwise | `bt_compare = __typed_bt_compare`; `bt_prefix = NULL` |
| Hash, `key == NULL` or every key field `EQUALIMAGE` | nothing |
| Hash, otherwise | `h_hash = __typed_h_hash` (per-field `hash`, combined with the same mixing `__ham_func5` uses on each field's 32-bit result, so the field split is part of the hash), `h_compare = __typed_h_compare` (0 iff every field's `equal` / `compare()==0`) |
| each `UNIQUE` | at `__typed_open`, the internal associate of `unique_db` to `dbp` with `__typed_unique_cb` and no flags (no `DB_CREATE` populate: a non-empty primary with an empty unique index is a catalog mismatch, §4) |

- `__typed_bt_compare(dbp, a, b)` is the RFC's bounded-walk comparator (§6):
  walk both records with the record codec; malformed sorts after well-formed;
  two malformed compare by `memcmp` then length; otherwise compare field by
  field with each codec's `compare` (or `memcmp` for MEMCMP fields, with the
  shorter-is-less rule).
- Every generated callback fetches its state with the slot read of 3.2. If the
  slot is 0 (some internal path cloned the callback pointer onto a fresh
  handle), it DIAGNOSTIC-asserts and falls back to the byte order of
  `__bam_defcmp` / `__ham_func5`, so it can misorder but cannot fault.
- `__typed_unique_cb(sdbp, pkey, pdata, skey)`: state from
  `sdbp->s_primary`'s slot; the constraint is the one whose `unique_db ==
  sdbp`. `field()` the value; `isnull` ⇒ `DB_DONOTINDEX` (NULLS DISTINCT) or, with
  `NULLS_NOT_DISTINCT`, a one-byte `0x00` key whose non-NULL images are all
  prefixed `0x01`; `EQUALIMAGE`/`MEMCMP` ⇒ `skey` points into `pdata` (no copy);
  else `sortkey()` into `__os_umalloc` memory with `DB_DBT_APPMALLOC`. A
  `DB_CODEC_MALFORMED` from the codec becomes `DB_CONSTRAINT_VIOLATION`, which
  fails the put through the existing callback-veto path (`db_cam.c:1522-1531`).

### 3.7 The put hook

**Site:** `src/db/db_cam.c`, in `__dbc_put` (`:2017`), inserted after
`F_CLR(dbc, DBC_ERROR);` (`:2027`) and before the comment at `:2029`, i.e.
*before* `flags` is rewritten (`:2043-2045`), because that rewrite turns
`DB_UPDATE_SECONDARY` into `DB_KEYLAST` and the hook must skip internal
secondary maintenance:

```c
#ifdef HAVE_TYPED
	if (F2_ISSET(dbp, DB2_AM_TYPED) && flags != DB_UPDATE_SECONDARY &&
	    (ret = __typed_put_check(dbc, key, data, flags)) != 0)
		return (ret);
#endif
```

It runs before `__dbc_put_primary` (`:2056-2058`), so a rejected record writes
neither secondaries nor primary, and before `CDB_LOCKING_INIT` (`:2047`),
because it touches no page. Every external put reaches it: `DB->put` calls
`__dbc_put` once per pair, including inside both bulk loops
(`db_am.c:583`, `:611`, `:638`, `:644`); `DBC->put` calls it from
`__dbc_put_pp` (`db_iface.c:2947`); a `DB_FOREIGN_NULLIFY` rewrite of a
primary does too (`db_cam.c:3232`). `DB_APPEND` on Btree/Hash fails in
`__db_put` before any cursor put (`db_am.c:561-570`). The other in-tree
callers are internal handles that are never typed (master database,
`db.c:301`, `:361`, `:375`; verify, `db_vrfyutil.c:535`; log verify,
`log_verify_int.c:437`, `log_verify_util.c:704`, `:1795`, `:1805`; partitions,
`partition.c:1059`, refused above).

`__typed_put_check(dbc, key, data, flags)`:

1. `F_ISSET(key, DB_DBT_PARTIAL) || F_ISSET(data, DB_DBT_PARTIAL)` ⇒ `EINVAL`
   ("partial puts are not supported on typed databases").
2. If `ts->nunique > 0`: under the handle's own `dbp->mutex` (per handle, and
   `MUTEX_INVALID` on a non-`DB_THREAD` handle), count `s_secondaries` entries
   whose `s_callback == __typed_unique_cb`; fewer than `nunique` ⇒ `EINVAL`
   ("UNIQUE index for constraint X was closed before its primary"). Without this
   a closed `unique_db` silently stops enforcing UNIQUE: closing a secondary
   just unlinks it (`db.c:860-861`). ponytail: list walk of ≤ 64 entries per put,
   per-handle mutex only; cache a generation number if it ever shows in a
   profile.
3. Key side, if `schema.key != NULL` and `flags != DB_CURRENT` (a `DB_CURRENT`
   put keeps the stored key, which was checked when it was written): run the
   record codec's `validate` if set, then every key-side constraint.
4. Data side, if `schema.data != NULL`: the same for the data DBT.
5. Constraint semantics: `NOT_NULL` ⇒ `field()` reports `isnull`; `MAXLEN` ⇒
   field `len` (or `DBT.size` for `DB_TYPED_WHOLE`) `> maxlen`; `VALID` ⇒ the
   record codec's `validate` and every field's codec `validate` return 0.
   `UNIQUE` is not checked here (3.6). A failure returns
   `DB_CONSTRAINT_VIOLATION` after `__db_errx(env, "constraint %s violated")`.
   `DB_CODEC_MALFORMED` from any callback becomes `DB_CONSTRAINT_VIOLATION`;
   any other non-zero callback return is passed through unchanged.

The D6 edit (`EINVAL` → `DB_KEYEXIST` on a typed primary,
`db_cam.c:1699`) is the only other line in the put path; it is commit 7 (§9).

## §4 D3's catalog

### 4.1 The fail-closed bit (it exists for Btree and Hash)

**[FOUND] Old libdb rejects an unknown access-method meta flag on Btree and
Hash.**

- Btree: `__bam_metachk` calls `__db_fchk(env, "DB->open", btm->dbmeta.flags,
  BTM_MASK)` (`src/btree/bt_open.c:156-158`). `BTM_MASK` is `0x0ff` and all
  eight bits are taken (`src/dbinc/db_page.h:100-108`).
- Hash: `__ham_metachk` calls `__db_fchk` with
  `DB_HASH_DUP | DB_HASH_SUBDB | DB_HASH_DUPSORT` (`src/hash/hash_open.c:194-197`;
  bits `0x01`, `0x02`, `0x04`, `db_page.h:131-133`).
- `__db_fchk` (`src/common/db_err.c:28-34`) returns `__db_ferr`, which logs
  "BDB0055 illegal flag specified to DB->open" and returns `EINVAL`
  (`db_err.c:60-73`).
- The same check, at the same lines, is in the `v2026.10.3` release
  (`git show v2026.10.3:src/btree/bt_open.c`, line 157;
  `git show v2026.10.3:src/hash/hash_open.c`, line 196).
- **Subdatabases go through it too.** An existing subdatabase is opened by
  `__fop_subdb_setup` → `__db_init_subdb` (`src/fileops/fop_util.c:886`), which
  reads the subdatabase's own meta page and calls `__db_meta_setup`
  (`src/db/db_open.c:391-397`), which calls `__bam_metachk` / `__ham_metachk`
  (`db_open.c:630-641`). C6 makes every typed database a subdatabase, so this
  is the path that matters.

**Bits.** In `src/dbinc/db_page.h`, unconditionally (a `#define` is invisible to
`s_sig`):

```c
#define	BTM_TYPED	0x100	/*	  Typed (RFC 0014); needs --enable-typed. */
#define	DB_HASH_TYPED	0x08	/*	  Typed (RFC 0014); needs --enable-typed. */
```

`BTM_MASK` stays `0x0ff`. The accepted mask grows **only under
`#ifdef HAVE_TYPED`**, at the two check sites:
`BTM_MASK | BTM_TYPED` at `bt_open.c:157` and `... | DB_HASH_TYPED` at
`hash_open.c:196`. On a match, `F2_SET(dbp, DB2_AM_TYPED_FILE)`.

**Written** only when a typed subdatabase is *created*: in `__bam_init_meta`
(`bt_open.c:433-450`, next to `BTM_COMPRESS`) and `__ham_init_meta`
(`hash_open.c:301-306`), `#ifdef HAVE_TYPED if (F2_ISSET(dbp, DB2_AM_TYPED))`.
The subdatabase create path builds the meta page with `__bam_init_meta`
(`bt_open.c:641`) and logs the whole page with `__db_log_page`
(`bt_open.c:642-643`, a `__crdel_metasub` record, `db.c:1340-1341`), so the bit
is recovered and replicated with no new log record. (Hash: `hash_open.c:508`,
same pattern.)

**What each reader does:**

| Reader | Result on a typed subdatabase |
|---|---|
| any release before this one, or this release built without `--enable-typed` | `DB->open` fails `EINVAL` (BDB0055), read-only or not. **Fail closed.** |
| `--enable-typed`, handle with a matching schema | opens; §4.3 checks run |
| `--enable-typed`, handle with **no** schema | `__typed_open` (§3.5) allows it only if `DB_AM_RDONLY` or the open is `DB_WRITEOPEN` (used only by `__db_subdb_remove`, `db_remove.c:406-407`; the only other reference is `fop_util.c:878`), else `EINVAL` "typed database: db_set_schema required to open for writing". This gives D8's read-only `db_dump`/`db_stat`. |
| `--enable-typed`, schema given, existing subdatabase **without** the bit | `EINVAL` "database was not created typed". Phase 1 does not convert. |

**[FOUND] Two holes, stated plainly:**

1. **`DB->rename` does not read meta flags.** `__db_subdb_rename`
   (`src/db/db_rename.c:316`) reads the subdatabase meta page only to copy its
   `uid` (`:351-354`) and never calls `__db_meta_setup`. So an old libdb can
   rename a typed subdatabase. The catalog is keyed by name (4.2), so the
   renamed database then fails to open in a typed build (`EINVAL`, "no catalog
   record") until it is renamed back. That is fail-closed for enforcement but
   it strands the data; the documentation must say "rename typed databases only
   with an `--enable-typed` libdb".
2. **`DB_TRUNCATE` skips the meta check** (`db_open.c:630`, `:639`: `(oflags &
   DB_TRUNCATE) == 0`), and the pre-truncate probe open swallows `EINVAL`
   (`db_open.c:83`). `DB_TRUNCATE` is refused with locking or transactions
   (`db_iface.c:1591-1596`). [UNSURE] whether an old libdb's `DB_TRUNCATE` of a
   subdatabase rewrites its meta page without the bit; if it does, the result
   is an *empty untyped* database plus a stale catalog record, which a typed
   open refuses (row 4 above). Nothing is written that skips a constraint.

**Heap, Queue, Recno:** refused by `db_set_schema` (§3.4 rule 2, D4).

### 4.2 Name and record format

- **Subdatabase name:** `__db_typed`, in the same file as the typed
  subdatabases (D3). One record per typed subdatabase.
- **Key:** the typed subdatabase's name, bytes without the NUL.
- **Data**, all integers big-endian (the catalog is byte-order independent; the
  meta page is not and is swapped by `__bam_mswap`):

| Off | Size | Field |
|---|---|---|
| 0 | 4 | magic `0x44425459` ("DBTY") |
| 4 | 4 | format version, **1** |
| 8 | 4 | total record length |
| 12 | 4 | `DBTYPE` |
| 16 | 2 | `ncodecs` (distinct `DB_CODEC`s, first-use order: key fields, then data fields) |
| … | | per codec: `u8 namelen`, name, `u32 version`, `u32 flags`, `u32 fixed_len`, `20` bytes fingerprint (§5) |
| … | | per side (key, then data): `u8 present`; if present: `u8 namelen`, name, `u32 version`, `u32 flags`, `u32 nfields`, `nfields × u16` codec index |
| … | | `u16 nconstraints`; each: `u32 kind`, `u32 side`, `u32 field`, `u32 maxlen`, `u32 flags`, `u8 namelen`, name |

- **Reading:** wrong magic, a length that disagrees with the DBT, or any index
  out of range ⇒ `EINVAL` "corrupt catalog record". Version `> 1` ⇒ `EINVAL`
  "catalog format N not supported" (an older typed build fails closed on a
  newer catalog).
- The RFC's "list of record-codec versions written" is **skipped**: Phase 1
  requires the record codec's name and version to equal the catalog's exactly.
  Add the list (format version 2) when Phase 2 needs data-record evolution.
- `unique_db` is not recorded: it is a handle. Its *name* is Phase 2 (FK
  persistence wants the same thing).

### 4.3 What is written when

All catalog access is from `__typed_open` (§3.5) or the two lifecycle hooks
below, on a short-lived internal handle (`__db_create_internal` +
`__db_open(cdbp, ip, txn, fname, "__db_typed", DB_BTREE, …)`), in the caller's
`txn`, closed before returning. ponytail: open/close per typed open; cache the
handle if open latency matters.

| Event | Catalog action |
|---|---|
| typed subdatabase **created** (`F_ISSET(dbp, DB_AM_CREATED)`, still set inside `__db_open`; cleared later at `db_iface.c:1426`) | create `__db_typed` if absent; `put` the record (overwrite: a non-transactional crash can leave a stale one). Same `txn`, so an aborted create leaves neither. |
| typed subdatabase **opened** | `get`; missing ⇒ `EINVAL` "no catalog record"; parse; compare (§5). No write. |
| `DB->remove` of a typed subdatabase | in `__db_subdb_remove` after `MU_REMOVE` (`db_remove.c:441-443`): `#ifdef HAVE_TYPED`, if `DB2_AM_TYPED_FILE` on `sdbp`, delete the record in `txn`. A missing record is not an error. |
| `DB->rename` of a typed subdatabase | in `__db_subdb_rename` after `MU_RENAME` (`db_rename.c:365-367`): if the subdatabase's meta page has the bit (it is already fetched at `:351`; test the flag there and remember it), `get` old, `put` new, `del` old, in `txn`. |
| `DB->truncate` | none |
| whole-file remove / rename | none: the catalog is in the file |
| recovery | none: catalog writes are ordinary logged puts (no new log record) |

**[FOUND] — new correction C13:** D3's claim that the catalog "travels with the
database through `db_dump`/`db_load`" is half true. The catalog records travel;
the meta bit does not (`db_dump`'s header carries `duplicates=`/`dupsort=`,
`src/db/db_pr.c:1819-1843`, and nothing typed). A dump/load round trip therefore
produces an **untyped** database plus an orphan catalog record. A typed open of
it fails "database was not created typed", so nothing is silently unenforced.
Making `db_load` re-create typed databases needs a header keyword and is
Phase 2. Hot backup and replication copy pages, so the bit survives them.

## §5 Multi-process codec fingerprint

### 5.1 What is hashed

One fingerprint per distinct `DB_CODEC`: SHA-1 (`__db_SHA1Init/Update/Final`,
`src/hmac/sha1.c:197-257`, built unconditionally: `dist/srcfiles.in:172`,
`dist/Makefile.in:310`), 20 bytes, over this byte stream (integers big-endian
`u32`):

1. `name` bytes, `version`, `flags` after C11 normalisation (MEMCMP ⇒ also
   EQUALIMAGE), `fixed_len`, `nvectors`;
2. each vector: `size`, then its bytes;
3. for every ordered pair `(i, j)`, `i, j < nvectors`: `sign(compare(v_i, v_j))`
   as `-1/0/1` (for a MEMCMP codec with `compare == NULL`, the sign of the
   default bytewise order, `memcmp` then shorter-first);
4. for every pair `i < j`: `equal(v_i, v_j) != 0` (or `compare == 0` if `equal`
   is NULL);
5. for every `i`: `hash(v_i)` if `hash != NULL`; `validate(v_i) == 0` if
   `validate != NULL`; if `sortkey != NULL`, its output length and bytes
   (two-call protocol of §1.2; output buffer `__os_malloc`'d once, sized by the
   first call).

≤ 256 vectors ⇒ ≤ 65 536 `compare` calls per codec per open. ponytail:
all pairs, not adjacent pairs; drop to adjacent pairs if open latency ever
matters.

**Self-checks while hashing** (`db_set_schema` fails `EINVAL` naming the codec
and the vector indices): vectors not in non-decreasing `compare` order
(the RFC's "ordered list the codec must reproduce"); `sign(c(a,b)) !=
-sign(c(b,a))`; `MEMCMP` claimed but `memcmp` sign differs on a pair;
`EQUALIMAGE` claimed but `compare == 0` on two different byte strings, or
`!= 0` on identical ones; `hash` differs on two vectors `equal` calls equal;
`sortkey` order disagrees with `compare` order on a pair. These are checks of
the codec's *claims* on the vectors, not proofs.

The vectors are **part of the codec's identity at its version**. Changing the
vector set without bumping `version` is a `DB_CODEC_MISMATCH`; bumping
`version` is an `EINVAL` (5.2). Either way the database must be rebuilt. This
is deliberate: the catalog has to compare like with like. (Upgrade path: store
the vectors themselves in catalog format 2 and re-run them through the current
codec, if applications need to grow their vector sets.)

The **record codec** has no vectors, so it is identified by name, version,
flags, `nfields` and the per-field codec indices only. Its `field()` behaviour
is not fingerprinted. The documentation must say so.

### 5.2 When it is checked, and the errors

Computed once, in `db_set_schema` (cold), and stored in the side table
(`TYPED_STATE`: change §3.1's `fp[20]` to `u_int32_t ncodecs; const DB_CODEC
**codecs; u_int8_t (*fp)[20];`, all inside the one allocation).

Checked in `__typed_open` against the catalog record (§4.2), for every
non-recovery open of an existing typed subdatabase, before any callback is
used. Written instead of checked when the subdatabase is created.

| Condition (catalog vs. passed schema) | Error |
|---|---|
| a catalog codec name is absent from the schema | `EINVAL` "typed database requires codec %s v%u" |
| same name, different `version` | `EINVAL` "typed database requires codec %s v%u, got v%u" |
| same name and version; different `flags`, `fixed_len` or fingerprint | **`DB_CODEC_MISMATCH` (-30965)** "codec %s v%u behaves differently from the codec this database was created with" |
| record codec name, version, flags, `nfields` or field→codec map differs; `DBTYPE` differs; constraint list differs (kind, side, field, maxlen, flags, name, in order) | `EINVAL` "schema differs from the database's catalog record" |
| catalog record missing / corrupt / newer format | `EINVAL` (§4.2) |

Per C5 the three new codes are -30966 (`DB_CONSTRAINT_VIOLATION`, puts, §3.7),
-30965 (`DB_CODEC_MISMATCH`, this table) and -30964 (`DB_CODEC_MALFORMED`,
callback-to-libdb only, never returned by a public call).

Not checked: by `db_recover` or any `DB_AM_RECOVER` open (D5, §3.5); by a
schema-less read-only open (§4.1), which installs no codec and so cannot
misorder anything it writes, because it writes nothing.

**Stated limit** (unchanged from the RFC): agreement on the vectors is all
this proves.

## §6 The DB_OPTREAD gate (D7)

### 6.1 What "DB_OPTREAD set" means today

**[FOUND]** It is a **process environment variable**, not a `DB_ENV` flag.
`__bam_opt_enabled()` (`src/btree/bt_search.c:100-109`, `static`) returns
`getenv("DB_NO_OPTREAD") == NULL && getenv("DB_OPTREAD") != NULL`, cached in
a function-static on first call. The only application code it reaches is the
Btree comparator: `__bam_opt_child` loads `bt_compare` (`:182`) and calls it on
unvalidated interior-page bytes (`:230-233`). The optimistic path is taken only
for `dbc->dbtype == DB_BTREE`, read-only, non-OPD, non-recnum descents
(`:844-850`), so Hash, duplicate trees and writes never see torn bytes.

### 6.2 The check

In `__typed_open` (§3.5), before the catalog is touched:

```c
if (dbp->type == DB_BTREE && ts->key_rc != NULL && !ts->key_memcmp &&
    __bam_optread_on()) {
	/* key record codec, then each key field codec in field order */
	if (!FLD_ISSET(ts->key_rc->flags, DB_CODEC_TOTAL))
		-> EINVAL, "DB_OPTREAD is set and record codec %s is not DB_CODEC_TOTAL"
	for each key-field codec c:
		if (!FLD_ISSET(c->flags, DB_CODEC_TOTAL))
			-> EINVAL, "DB_OPTREAD is set and codec %s is not DB_CODEC_TOTAL"
}
```

- `__bam_optread_on` is a new two-line non-static wrapper around
  `__bam_opt_enabled`, added to `bt_search.c` **inside `#ifdef HAVE_TYPED`**
  with a `PUBLIC: #ifdef HAVE_TYPED` prototype (the conditional-prototype form
  `s_include` already handles, e.g. `src/os/os_atomic.c:288-291`). Calling the
  same cached function, rather than repeating the `getenv` test, keeps the gate
  and the descent in agreement even if the application calls `setenv` later.
  The default build's `bt_search.c` is unchanged.
- **Data-side codecs are not gated:** they run only on the DBT being put
  (§3.7) and in the UNIQUE callback, never on page bytes.
- **MEMCMP keys are not gated:** the default comparator runs (§3.6), which is
  bounded by construction.
- The check runs per open, so a process that sets `DB_OPTREAD` after another
  process created the database is still refused.

### 6.3 What libdb itself guarantees inside `__typed_bt_compare` (C10)

The record codec's `field()` is application code, so `TOTAL` is a promise.
The generated comparator adds the one check libdb *can* make, per slice,
before any type codec sees it:

```c
/* overflow-safe: p within [d, d+size], len within the rest */
if (p < d || (size_t)(p - d) > size || len > size - (size_t)(p - d))
	-> treat the record as malformed (sorts last, §3.6)
```

That turns a lying `field()` into a misordering, not an out-of-bounds read by
a type codec. It cannot stop `field()` itself from reading out of bounds; only
`TOTAL` covers that.

## §7 Test plan

TODO.

## §8 Phase 0

TODO.

## §9 Work breakdown

TODO.
