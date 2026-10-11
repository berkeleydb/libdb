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

TODO: filled in by the next commit.

## §4 D3's catalog

TODO.

## §5 Multi-process codec fingerprint

TODO.

## §6 The DB_OPTREAD gate (D7)

TODO.

## §7 Test plan

TODO.

## §8 Phase 0

TODO.

## §9 Work breakdown

TODO.
