# RFC 0014 — Implementation specification (Phases 0 and 1)

- **Implements:** `rfc/0014-typed-data-and-constraints.md`, decisions D1–D8
  ("Maintainer decisions taken for implementation").
- **Branch:** `rfc14-typed`, rebased onto master `553b649a1` (v2026.10.5).
- **Audience:** the implementation agent. Every choice is made here. Where this
  document says "TODO-VERIFY", the implementer runs the named probe *first*; the
  spec says what to do for each outcome.
- **Citations** are `file:line` against `553b649a1`. §0-§2 were written against
  `2d8397dfe`. Between the two, `src/` changed only in `src/dbinc/lock.h`, so
  §0-§1's `src/` citations still hold. `dist/meson.build` gained 9 lines at
  `:122`, so §2.2's "uring block (`:132-151`)" is now `:141-160` and §2.3's
  `dist/meson.build:535` is now `:544`; the implementer anchors on the text.
  No `TODO-VERIFY` remains: each was resolved in place (§10).

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
put ambiguous for `DB_CURRENT` and for UNIQUE. UNIQUE forces it in any case:
`__db_associate_arg` already refuses a `DB_DUP` primary ("BDB0575 Primary
databases may not be configured with duplicates", `src/db/db_iface.c:181-185`). Refused in
`db_set_schema` and again at open (§3.5). Lift it in Phase 2 with the
generated `dup_compare`.

**C13. D3's "travels with `db_dump`/`db_load`" holds for the catalog records
but not for the meta bit.** See §4.3. A round trip yields an untyped database
that a typed open refuses. Fail-closed; `db_load` support is Phase 2.

**C14. D5 holds only for a `db_recover` built with `--enable-typed`.** A
default-build (or older) `db_recover` reaches the typed subdatabase's meta check
through `__dbreg_do_open` → `__db_open` (`src/dbreg/dbreg_util.c:701`), gets
`EINVAL` from `__db_fchk`, and `__dbreg_register_recover` forgives only `ENOENT`
(`src/dbreg/dbreg_rec.c:144-167`), so recovery fails. That is fail-closed, and
it is the correct result: a library that does not know the bit must not
write the file. It means "standalone `db_recover` works with no codecs" is true
of the typed build's utility, which links no application code, and not of
an old one. `typed_recover` tests the first; `typed_old_reader` records the
second.

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
| `DB_CONSTRAINT_VIOLATION` | **-30966** | a put that fails NOT NULL / MAXLEN / VALID | `DB_CONSTRAINT_VIOLATION: A typed-database constraint was violated` | **0216** (verified free: `git grep '"0216"'` is empty; 0207-0214 are taken in `db_err.c` / `os_csprng.c`; 0215 goes to §8.1) |
| `DB_CODEC_MISMATCH` | **-30965** | `DB->open`: same codec name+version, different fingerprint | `DB_CODEC_MISMATCH: Codec behaviour differs from the database's record` | **0217** |
| `DB_CODEC_MALFORMED` | **-30964** | codec / record-codec callbacks to libdb. libdb never returns it from a public call: the put hook maps it to `DB_CONSTRAINT_VIOLATION` | `DB_CODEC_MALFORMED: Value bytes are not well formed for their codec` | **0218** |

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
| `dist/pubdef.in` | Add, alphabetically: `DB2_AM_TYPED * I *`, `DB2_AM_TYPED_FILE * I *`, `DB_CODEC_EQUALIMAGE D I *`, `DB_CODEC_FIXED D I *`, `DB_CODEC_MALFORMED D I *`, `DB_CODEC_MEMCMP D I *`, `DB_CODEC_MISMATCH D I *`, `DB_CODEC_TOTAL D I *`, `DB_CONSTRAINT_MAXLEN D I *`, `DB_CONSTRAINT_NOT_NULL D I *`, `DB_CONSTRAINT_NULLS_NOT_DISTINCT D I *`, `DB_CONSTRAINT_UNIQUE D I *`, `DB_CONSTRAINT_VALID D I *`, `DB_CONSTRAINT_VIOLATION D I *`, `DB_TYPED_DATA D I *`, `DB_TYPED_KEY D I *`, `DB_TYPED_WHOLE D I *`. No `J` column: Java gets nothing in Phase 1. The `DB2_*` rows copy `DB2_AM_EXCL * I *` (`dist/pubdef.in:567-569`). |
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
	u_int32_t	 ncodecs;	/* distinct codecs, §4.2 order */
	const DB_CODEC	**codecs;
	u_int8_t	(*fp)[20];	/* one SHA-1 per codec (§5) */
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
8. The handle has no application callback already set: `bt_compare ==
   __bam_defcmp` and `bt_prefix == __bam_defpfx` (the defaults,
   `src/btree/bt_method.c:52-53`; `set_bt_compare` nulls `bt_prefix`,
   `:287-289`), `h_hash == NULL` and `h_compare == NULL` (`src/hash/hash_method.c:46-47`;
   the open fills a NULL `h_hash` with `__ham_func5`, `hash_open.c:105-107`,
   `:265-266`, so compare *before* open only), `dup_compare == NULL`, and
   `DB_AM_DUP` is clear (C12).

### 3.5 Phase 1 refusals (C9, C6, C12) and where each is enforced

`__typed_open` is called from `__db_open` (`src/db/db_open.c:52`) after the
access method and partition opens succeed and before the post-open test hook,
i.e. inserted between `db_open.c:266` (`#endif` of the partition block) and
`:267` (`DB_TEST_RECOVERY(... DB_TEST_POSTOPEN ...)`):

```c
#ifdef HAVE_TYPED
	if (F2_ISSET(dbp, DB2_AM_TYPED | DB2_AM_TYPED_FILE) &&
	    !F_ISSET(dbp, DB_AM_RECOVER) &&
	    (ret = __typed_open(dbp, ip, txn, fname, dname, flags)) != 0)
		goto err;
#endif
```

`DB2_AM_TYPED_FILE` is set by the meta check (§4.1), so the hook also runs for a
schema-less open of a typed file, which is where §4.1's read-only rule lives.
The `DB_AM_RECOVER` test is what keeps recovery's internal handles (schema-less,
read-write, `dbreg_util.c:677`) out of that rule; `typed_recover`'s sabotage arm
deletes it. A failure there is cleaned up by the caller exactly like any other open
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
| crash between a **non-transactional** create and its catalog put | the subdatabase exists with the bit and no record; every later typed open fails "no catalog record" (fail closed); `DB->remove` still works (its hook ignores a missing record). Transactional creates have no such window. |
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
(`TYPED_STATE.codecs` / `.fp`, §3.1, inside the one allocation).

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

### 7.1 Harness

- Every runner is `test/db/run_<name>.sh`, registered in `test/MANIFEST` as
  `<tier>	run_<name>` (the existing `db`-tier convention, `MANIFEST:464-483`;
  `run_all.sh` runs `$HERE/$2.sh`). It prints
  `VERDICT <name> PASS|FAIL <reason>` and exits non-zero on FAIL. A hang is a
  FAIL: each runner wraps its binary in the `run_with_timeout` idiom of
  `test/db/run_curadj_dup_partition.sh`.
- **Two tiers.** `db` (default build, existing CI step `ci.yml:108`) and a
  **new tier `typed`**. Typed tests need a `--enable-typed` build, and a
  runner that skips in a default build would be a vacuous green. So:
  - `test/db/run_all.sh`: `TIER=${TIER:-db}`, and the awk selector at its line
    27 becomes `$1 == tier` (with `-v tier="$TIER"`), and `hi_init "$TIER"`. One
    line each; the `db` tier is unchanged.
  - Each `typed` runner first does `grep -q '^#define HAVE_TYPED 1'
    "$BUILD/db_config.h"` and emits `VERDICT <name> FAIL not-a-typed-build` if
    absent. It never skips.
  - CI: a new job in `ci.yml` configures `--enable-typed --enable-debug
    CFLAGS=-fsanitize=address,undefined`, runs the existing `db` tier against
    it (the gates must stay green in the typed build), then `TIER=typed sh
    test/db/run_all.sh` and `./test/check_manifest.sh --tier typed`.
- **Sabotage arms** are not in CI (they would be red). Each one is written as
  a reversible `sed` or patch in `test/db/typed-sabotage.sh`, which applies
  one arm, rebuilds, runs the one named test, requires FAIL, and reverts. That
  follows `test/c/flagapi-sabotage.sh`. The implementer runs every arm on
  `/tmp/s_impl` before each commit and pastes the FAIL lines into the commit
  message. **A sabotage arm that passes means the test is wrong** (handoff).

### 7.2 Tests

| Test (`run_<name>.sh`) | Asserts | Sabotage: the exact revert that must make it FAIL | MANIFEST |
|---|---|---|---|
| `typed_footprint` | §2.4 items 1-4: default-build `.text` of `libdb-*.so` and `env_sig_print.sh` output equal master's; no `db_set_schema`/`__typed_` symbol in a default build, present in a typed one; `env_sig` equal default vs typed (D2) | (a) add an unguarded `int __typed_canary(void){return 1;}` to `db_cam.c` → item 1 FAIL. (b) delete the four `s_sig` exclusion lines and rerun `s_sig` → items 2 and 4 FAIL | `db` |
| `handle_sizes` (existing runner, second registration) | `sizeof(DB)=1744` etc. hold **in the typed build** | add `void *typed_internal;` to `struct __db` under `#ifdef HAVE_TYPED` → FAIL | `typed` |
| `typed_api` | every §3.4 rule returns `EINVAL` on its violation and a valid schema is accepted; §5.1 self-checks reject unordered vectors, a false MEMCMP, a false EQUALIMAGE; `db_get_schema` returns the deep copy (caller's arrays scribbled after the call do not change it); `db_set_schema` after open → `EINVAL` | (a) delete the `compare == NULL ⇒ MEMCMP` rule → that case FAILs. (b) delete the vector-order self-check → unordered-vectors case FAIL. (c) make the copy shallow (store the caller's `types` pointer) → deep-copy case FAIL | `typed` |
| `typed_refusals` | `EINVAL` for: `fname == NULL`; `dname == NULL`; named in-memory; Heap/Queue/Recno schema; `DB_DUP` before and after `db_set_schema`; compression (`set_bt_compress`) and a pre-existing `BTM_COMPRESS` subdatabase; `set_bt_compare` after `db_set_schema`. Partitioning: the existing `ENOENT`/BDB0637 (`db_open.c:199-203`) | (a) delete the C6 block in `__db_open_arg` → the two C6 rows FAIL. (b) delete the compression check in `__typed_open` → both compression rows FAIL. (c) delete the callback-identity check → override row FAIL | `typed` |
| `typed_notnull` | NOT NULL violated → `DB_CONSTRAINT_VIOLATION` (-30966) via `DB->put`, `DBC->put(DB_KEYFIRST)`, `DBC->put(DB_CURRENT)`, `DB_MULTIPLE_KEY` bulk (bulk stops at the bad pair; `key->doff` counts the good ones, `db_am.c:640-641`); the database is unchanged after each failure (a get returns the prior value or `DB_NOTFOUND`); a non-NULL put succeeds | delete the `__typed_put_check` call in `__dbc_put` → FAIL | `typed` |
| `typed_maxlen` | field len = maxlen succeeds; maxlen + 1 → -30966; `DB_TYPED_WHOLE` same at the DBT boundary | (a) delete the hook → FAIL. (b) change `len > maxlen` to `len > maxlen + 1` → boundary row FAIL | `typed` |
| `typed_valid` | a codec `validate` returning `DB_CODEC_MALFORMED` fails the put with -30966, **never** -30964; a record codec `validate` failure, same; an arbitrary error (`ENOMEM`) from a callback is passed through | delete the MALFORMED → VIOLATION mapping → the put returns -30964 → FAIL | `typed` |
| `typed_partial` | `DB_DBT_PARTIAL` on key or data → `EINVAL`, record unchanged | delete §3.7 step 1 → partial put succeeds → FAIL | `typed` |
| `typed_btree_cmp` | non-MEMCMP key codec (a reverse-order int): `DB_NEXT` scan is in codec order; malformed keys (field() fails) sort after all well-formed ones, in `memcmp`-then-length order; an all-MEMCMP key schema leaves `bt_compare == __bam_defcmp` (`get_bt_compare`) | do not install `__typed_bt_compare` → scan is bytewise → FAIL | `typed` |
| `typed_hash` | Hash with a case-insensitive key codec: put "abc", get "AbC" finds it; put "ABC" overwrites (one record) | do not install `h_hash` → "AbC" lands in another bucket → FAIL | `typed` |
| `typed_cmp_fuzz` | 10^6 random, truncated and torn key pairs into the installed comparator (from `get_bt_compare`) with a deliberately **lying** `field()` (returns slices past the end): no ASan/UBSan report; antisymmetry `sign(c(a,b)) == -sign(c(b,a))` holds; malformed-last holds. 60 s timeout | delete the §6.3 slice-bounds check → ASan heap-buffer-overflow → FAIL | `typed` |
| `typed_optread` | `DB_OPTREAD=1`: key codec without `TOTAL` → open `EINVAL` and the message names the codec; record codec without `TOTAL` → same; with `TOTAL` → opens; MEMCMP key without `TOTAL` → opens; `DB_NO_OPTREAD=1 DB_OPTREAD=1` → opens; a Hash schema without `TOTAL` → opens. Each case is its own process (the setting is cached, §6.1) | delete the §6.2 check → first two rows FAIL | `typed` |
| `typed_codec_mismatch` | process A creates with codec v1. B: v2 → `EINVAL`. C: v1 with a reversed `compare` → `DB_CODEC_MISMATCH` (-30965). D: schema missing the codec → `EINVAL`. E: v1 with a different `flags` → -30965. F: identical schema → opens and reads A's rows | skip the fingerprint comparison → C opens; the runner then prints a `DB_NEXT` scan under v1 showing C's inserts out of order (the reason the check exists) → FAIL | `typed` |
| `typed_catalog_lifecycle` | after create, `__db_typed` has exactly the record of §4.2 (decoded field by field); `DB->remove` of the typed subdatabase deletes it; `DB->rename` moves it and the new name opens with the schema; a create inside an **aborted** txn leaves neither subdatabase nor record; `DB->truncate` keeps it | (a) delete the remove hook → stale record → FAIL. (b) delete the rename hook → new name fails "no catalog record" → FAIL | `typed` |
| `typed_catalog_format` | catalog record with bad magic, format version 2, truncated length, codec index out of range → each open `EINVAL` | delete the version check → version-2 row FAIL | `typed` |
| `typed_rdonly_noschema` | typed subdatabase opened with no schema and `DB_RDONLY` → opens, reads; without `DB_RDONLY` → `EINVAL`; the typed build's `db_dump` exits 0 and dumps it; `db_load` of that dump yields a file a typed open refuses (C13); `db_verify` output recorded (D8, §9 commit 12) | delete the §4.1 read-only rule → write open succeeds → FAIL | `typed` |
| `typed_old_reader` | the committed fixtures `test/db/fixtures/typed_bt.db` and `typed_hash.db` (one typed subdatabase each plus `__db_typed`), opened by the **default** build, `DB_RDONLY` and not → `EINVAL` with "BDB0055"; if `LIBDB_OLD` names a `v2026.10.3` install, the same with it (that row is optional). In the `typed` tier, a companion check regenerates the fixtures and compares meta flags and catalog bytes to the committed ones, so the fixture cannot drift from the code | drop the `#ifdef HAVE_TYPED` around the widened `BTM_MASK`/hash mask → the default build opens the fixture → FAIL | `db` (+ regen check `typed`) |
| `typed_unique` | two txns insert the same UNIQUE field value under 2PL, `DB_TXN_SNAPSHOT` and `DB_TXN_SERIALIZABLE`: exactly one commits; the loser gets `DB_KEYEXIST` (D6), or `DB_LOCK_DEADLOCK` and then `DB_KEYEXIST` on retry; after both, the primary and the unique index have one row each and agree. **Control:** an untyped non-DUP secondary in the same typed build still returns `EINVAL` (D6) | (a) the RFC's arm: `__typed_unique_cb` appends a per-call counter to the key → both commit → FAIL. (b) revert the D6 edit → loser sees `EINVAL` → FAIL. (c) make D6 unconditional → control FAIL | `typed` |
| `typed_unique_mp` | the same race from two **processes** (fork, one env, `DB_REGISTER` off, `DB_INIT_LOCK`) with a barrier file | arm (a) above → FAIL | `typed` |
| `typed_unique_nulls` | NULLS DISTINCT: two NULL rows both commit, no index entry; `NULLS_NOT_DISTINCT`: second NULL → `DB_KEYEXIST`; a non-NULL value that happens to equal the NULL image does not collide | (a) return a key instead of `DB_DONOTINDEX` for NULL → NULLS DISTINCT row FAIL. (b) drop the `0x01` prefix → collision row FAIL | `typed` |
| `typed_unique_closed` | close `unique_db`, then put on the primary → `EINVAL` naming the constraint | delete §3.7 step 2 → put succeeds, duplicate stored → FAIL | `typed` |
| `typed_recover` | populate under txns; fork a writer and `kill -9` it mid-txn; run the **typed build's** `db_recover -h` (no codecs) → exit 0; reopen with the schema; full scan through a reference model: committed rows present, the uncommitted absent, NOT NULL holds, unique index ↔ primary bijection | delete `!F_ISSET(dbp, DB_AM_RECOVER)` from the §3.5 hook → recovery's schema-less read-write opens are refused → `db_recover` exits non-zero → FAIL | `typed` |
| `foreign_reassoc` (§8.1) | see §8.1 | revert the §8.1 fix | `db` |
| `foreign_disassoc_leak` (§8.2) | see §8.2 | revert the §8.2 fix | `db` |
| `r1_revive` (§8.3) | see §8.3 | revert the §8.3 fix | `db` |

Not a test: the put-path cost on **untyped** databases in a typed build (D2's
revisit condition) is a measurement, §9 commit 13.

## §8 Phase 0

Phase 0 is untyped: no `HAVE_TYPED`, every build. Each item has a `db`-tier
runner. (RFC Phase 0 item 1 moved to Phase 1, C7.)

### 8.1 `__db_associate_foreign` leaves a stale `f_primaries` entry (C8)

**[FOUND] Defect.** `__db_associate_foreign` (`src/db/db_am.c:1143`) inserts
`f_info` into `fdbp->f_primaries` (`:1174-1176`) and only then tests
`pdbp->s_foreign != NULL`, returning `EINVAL` with the entry still linked
(`:1184-1185`). `__db_associate_foreign_arg` (`src/db/db_iface.c:2169-2211`)
does not check `s_foreign`, so the second call `fdbp2->associate_foreign(fdbp2,
sdbp, ...)` on an already-associated secondary reaches that path. The entry
points at `sdbp` while `sdbp->s_foreign` still names the first foreign database.

**Hazards**, each from code read now:

1. **Wrong cascade / wrong abort.** A delete in `fdbp2` runs `__dbc_del_foreign`
   because `f_primaries` is non-empty (`db_cam.c:451-453`), walks the stale
   entry (`:3105-3108`) and applies `DB_FOREIGN_ABORT`/`CASCADE`/`NULLIFY` to
   `sdbp`'s primary, a relationship the `EINVAL` said was not created.
2. **Disables a live FK check.** If `fdbp2` closes first, `__db_refresh` walks
   its `f_primaries` and sets `f_info->dbp->s_foreign = NULL`
   (`src/db/db.c:867-874`), clearing `sdbp`'s association with the *first*
   foreign database. Inserts into `sdbp`'s primary then skip the foreign check
   (`db_cam.c:1533-1537`) with no error.
3. **Use-after-free.** If `sdbp` closes first, `__db_disassociate_foreign`
   (`db.c:1293-1318`) walks `sdbp->s_foreign`'s list (the first foreign DB), not
   `fdbp2`'s, so the stale entry survives `sdbp`'s free. A later delete in
   `fdbp2` dereferences `f_info->dbp` (`db_cam.c:3106-3107`), as does
   `fdbp2`'s close (`db.c:871`).

**Fix** (`db_am.c`): test before allocating.

```c
	if (pdbp->s_foreign != NULL)
		return (EINVAL);
	if ((ret = __os_malloc(env, sizeof(DB_FOREIGN_INFO), &f_info)) != 0)
		return (ret);
```

and delete the old test at `:1184-1185`. Give it a message:
`__db_errx(env, DB_STR("0215", "Secondary index is already associated with a
foreign database"))` (0215 is unused: `git grep '"0215"'` is empty; 0207-0214
are taken, `src/common/db_err.c`, `os_csprng.c`). The lock-free test of
`pdbp->s_foreign` matches today's code.

**Test `foreign_reassoc`** (`db` tier). Primary P, secondary S, foreign F1 and
F2, all in one env. `F1->associate_foreign(F1, S, NULL, DB_FOREIGN_CASCADE)`
→ 0; `F2->associate_foreign(F2, S, NULL, DB_FOREIGN_CASCADE)` → must be
`EINVAL`. Put `k → v` into P (S key `s`; `s` in F1 and F2). Then:
(i) delete `s` from F2 → P must still hold `k` (hazard 1); (ii) close F2,
then put a P record whose S key is absent from F1 → must be
`DB_FOREIGN_CONFLICT` (hazard 2); (iii) a fresh run: close S, then delete from
F2, under ASan → no report (hazard 3). The ASan arm runs with the
`--enable-debug CFLAGS=-fsanitize=address` build the lockmatrix tier uses.
**Sabotage:** restore the post-insert test (move the `s_foreign` check back below
`LIST_INSERT_HEAD`) → (i) finds `k` cascaded away, (ii) succeeds, (iii) reports
heap-use-after-free: each must FAIL.

### 8.2 `__db_disassociate_foreign` leaks one allocation per close (C8)

**[FOUND] Defect.** `src/db/db.c:1302-1303` mallocs `sizeof(DB_FOREIGN_INFO)`
into `tmp`; `:1309` overwrites `tmp` with `LIST_NEXT(...)` before any use. Every
`DB->close` of a secondary with `s_foreign != NULL` (called from `db.c:876-878`)
leaks one block. The malloc's only effect is an `ENOMEM` return that leaves
the list unedited.

**Fix:** delete `db.c:1302-1303`. Nothing else changes.

**Test `foreign_disassoc_leak`** (`db` tier): count allocations through
`db_env_set_func_malloc`/`db_env_set_func_free` (`src/common/os_method.c:120`,
`:74`; `__os_malloc` uses `DB_GLOBAL(j_malloc)`, `src/os/os_alloc.c:82-83`,
and `__os_free` uses `j_free`, `:190-191`). Live count = mallocs − frees,
counting `realloc` as neither. Open env, P, S, F; `associate`,
`associate_foreign`; record the live count; then 100 × { close S; reopen S;
re-associate both }; record again. **Asserts** the two counts are equal.
**Control arm** in the same binary: the same loop without `associate_foreign`
must also be flat, which proves the counter sees handle churn without false
positives. **Sabotage:** restore the two lines → the delta is exactly 100 → FAIL.
(Prefer the counter to LeakSanitizer: a leak inside a live environment is not
reported by LSan until exit, and the env closes cleanly either way.)

### 8.3 R1: an equal-key put revives a deleted item's key bytes

**Reproduces** on this branch. Probe, run on `/tmp/s_impl` (FreeBSD 14.5,
`dist/configure --enable-debug --enable-diagnostic`, tree `553b649a1`, the
reporter's `~/ws/dbsql/.agent/libdb-bugs/r1_revive.c` unmodified, linked against
`.libs/libdb-2026.0.a`):

```
libdb 2026.10.5 (October 10, 2026)
control (deleter closed)       no-env stored key: "ABC"  (expected)
another cursor on the item     no-env stored key: "abc"  (UNEXPECTED: old bytes)
same cursor deletes and puts   no-env stored key: "abc"  (UNEXPECTED: old bytes)
control (deleter closed)       txn    stored key: "ABC"  (expected)
another cursor on the item     txn    stored key: "abc"  (UNEXPECTED: old bytes)
same cursor deletes and puts   txn    stored key: "abc"  (UNEXPECTED: old bytes)
```

**[FOUND] Mechanism.** `__bamc_put` (`src/btree/bt_cursor.c:2143-2147`) finds an
exact match (the comparator says equal); with no duplicates it sets
`iiop = DB_CURRENT` (`:2174-2176`) whether or not the matched slot is deleted
(the `IS_CUR_DELETED` test at `:2162` guards only `DB_NOOVERWRITE`).
`__bam_iitem`'s `DB_CURRENT` case (`src/btree/bt_put.c:403-458`) clears the
cursor-deleted state and replaces only the **data** item at `indx + 1`; the key
item at `indx` is never written. So a slot kept only because a cursor
references it (`B_DISSET`) comes back to life with its old key bytes.

**Fix** (smallest correct one): in the `DB_CURRENT` case of `__bam_iitem`, when
the slot being revived was deleted and the new key's bytes differ from the
stored ones, replace the key item too:

```c
	case DB_CURRENT:
		... existing __bam_ca_delete ...
		if (TYPE(h) == P_LBTREE && key != NULL &&
		    B_DISSET(GET_BKEYDATA(dbp, h, indx + O_INDX)->type) &&
		    (ret = __bam_revive_key(dbc, h, indx, key)) != 0)
			return (ret);
```

`__bam_revive_key` (new, static, `bt_put.c`): if the key item is `B_KEYDATA` with
`len == key->size` and `memcmp` equal, do nothing; if it is `B_KEYDATA`,
`key->size <= cp->ovflsize` (same rule as `:232`), and the space fits, call
`__bam_ritem(dbc, h, indx, key, 0)` (`bt_put.c:703`; it logs a `__bam_repl`
record, so recovery and replication see it); otherwise return
`DB_NEEDSPLIT`-equivalent by falling back to delete-and-insert: `__bam_ditem` the
pair and redo the put as `DB_KEYFIRST` at `indx`. The space accounting at
`bt_put.c:255-274` must add `BKEYDATA_PSIZE(key->size) - BKEYDATA_PSIZE(old
len)` for this case, so the split decision is made before anything is written.
Only deleted slots change; replacing a **live** equal key keeps the stored key
bytes, which is libdb's documented behaviour.

This changes no format and no API, and makes `EQUALIMAGE` irrelevant to R1. The
RFC suggested waiting for typed EQUALIMAGE; that is not needed, because the bug
is the revival, not equality.

[UNSURE] Overflow keys (`B_OVERFLOW` key items) and the off-page-duplicate
case. The probe covers neither. The implementer must extend the test to a key
longer than `ovflsize` before choosing between the `__bam_ritem` path and the
delete-and-insert path for it. If delete-and-insert is used for every case, the
code is shorter and the special cases go away; choose that unless the test shows
a cursor-adjustment problem.

**Test `r1_revive`** (`db` tier): the reporter's six cases, each judged on the
stored key bytes, plus two **controls that must not change**: (a) a put of
"ABC" over a *live* "abc" keeps "abc" (documented replace semantics); (b)
`DB_NOOVERWRITE` over a live equal key returns `DB_KEYEXIST`. Plus (c) the same
six cases with a 2000-byte key (overflow), and (d) run `db_recover -c` on the
txn-env case and re-read: the key must still be "ABC" (proves the key change is
logged). `VERDICT r1_revive PASS` iff all six read "ABC" and all controls hold.
**Sabotage:** delete the `__bam_revive_key` call → the four non-control cases
read "abc" → FAIL. That is the current tree's behaviour, so the arm's FAIL output
is the probe output above.

## §9 Work breakdown

**Every commit:** build on `/tmp/s_impl` in a sibling build dir, in **both** a
default and an `--enable-typed --enable-debug` configuration; run the
handoff gates (`run_all.sh` `db` tier, `s_validate`, `s_execbits`,
`s_chk_copyright`, `s_chk_licence`, `env_sig_print.sh`, `run_handle_sizes.sh`)
in both, plus `TIER=typed run_all.sh` in the typed one; run the commit's
sabotage arms and paste their FAIL lines into the commit message. New files get
the SPDX header; `dist/` files need `git add -f`; runners need `git
update-index --chmod=+x`.

### Phase 0 (no `HAVE_TYPED`; may go to master on their own)

| # | Commit | Tests (tier) |
|---|---|---|
| P1 | `db: __db_disassociate_foreign leaked one DB_FOREIGN_INFO per secondary close` (§8.2) | `foreign_disassoc_leak` (`db`) |
| P2 | `db: associate_foreign linked f_info before refusing a second foreign DB` (§8.1) | `foreign_reassoc` (`db`) |
| P3 | `btree: an equal-key put over a deleted slot kept the deleted key's bytes (R1)` (§8.3) | `r1_revive` (`db`); full TCL `test` suite subset in CI, because this touches `__bam_iitem` |

### Phase 1

| # | Commit | Tests (tier) |
|---|---|---|
| 1 | `typed: --enable-typed / meson typed option, empty guarded sources, build lists` (§2): options.m4, configure.ac, regenerated configure + config.hin, meson_options.txt, meson.build, srcfiles.in, Makefile.in, Windows project files, `src/typed/*.c` containing only the guard. Also: `run_all.sh` `TIER` (§7.1), `test/db/typed-sabotage.sh` skeleton, the CI job. | `typed_footprint` (`db`), `handle_sizes` (`typed`), `run_windows_srclist`, `run_meson_autoconf_parity` (`db`, must stay green) |
| 2 | `typed: public declarations` (§1): `db.in` structs, flags, 3 error codes, `db_strerror` cases, `s_sig` exclusions, `pubdef.in`, `s_include` + `typed_ext.h`, regenerated `build_windows/db.h` | `typed_footprint` (second sabotage arm), `s_validate` (`s_chk_err`, `s_chk_pubdef`) |
| 3 | `typed: side table, db_set_schema / db_get_schema / db_record_fixed, fingerprint` (§3.1-3.4, §5.1), release in `__db_close` | `typed_api` |
| 4 | `typed: open hook, Phase 1 refusals, BTM_TYPED / DB_HASH_TYPED` (§3.5, §4.1), fixtures `test/db/fixtures/typed_{bt,hash}.db` | `typed_refusals`, `typed_old_reader` (`db` + regen check in `typed`) |
| 5 | `typed: __db_typed catalog, fingerprint check, remove/rename hooks` (§4.2-4.3, §5.2) | `typed_codec_mismatch`, `typed_catalog_lifecycle`, `typed_catalog_format`, `typed_rdonly_noschema` |
| 6 | `typed: __dbc_put hook -- NOT NULL, MAXLEN, VALID, partial-put refusal` (§3.7) | `typed_notnull`, `typed_maxlen`, `typed_valid`, `typed_partial` |
| 7 | `typed: UNIQUE through a generated secondary; DB_KEYEXIST on typed primaries (D6)` (§3.6, §3.7 step 2, `db_cam.c:1699`) | `typed_unique`, `typed_unique_mp`, `typed_unique_nulls`, `typed_unique_closed` |
| 8 | `typed: generated bt_compare / h_hash / h_compare; DB_OPTREAD gate` (§3.6, §6) | `typed_btree_cmp`, `typed_hash`, `typed_cmp_fuzz`, `typed_optread` |
| 9 | `typed: db_recover with no codecs (D5)` | `typed_recover` |
| 10 | `docs: typed databases` — `docs_src/api/c/db_set_schema.md`, `db_get_schema.md`, `db_record_fixed.md`, the three error codes in `program_errorret.md`, the fingerprint's stated limit, "rename only with a typed libdb" (§4.1), C13, C14, and a `DB_OPTREAD` cross-reference in `dbset_bt_compare.md` | `s_validate` doc checks |
| 11 | `typed: sabotage runner complete` — every arm in §7.2 present in `typed-sabotage.sh`, all run on `/tmp/s_impl`, output in the commit message | — |
| 12 | `util: say "typed database" where a utility cannot enforce it` (D8): `db_verify` and `db_dump`/`db_load` messages. **Starts with a probe:** what a default-build and a typed-build `db_verify` print for a typed subdatabase today. [UNSURE: no verify file calls `__bam_metachk` or `__ham_metachk` (grep of `src/db/db_vrfy.c`, `src/btree/bt_verify.c`, `src/hash/hash_verify.c`), so verify may accept the bit silently.] | `typed_rdonly_noschema` (verify row) |
| 13 | `bench: untyped put cost in a typed build` — `test/bench` A/B of a default vs typed build, untyped Btree puts, the D2 revisit condition; results file only | none (measurement) |

Commits 1-2 change no behaviour in any build; from 3 on, only typed builds
change. Commit 7 is the only edit in the shared put path besides the hook
itself.

---

## §10 Resolved and deferred markers

| Marker (where) | Resolution |
|---|---|
| §3 "filled in by the next commit", §4-§9 "TODO." (7) | written |
| §1.3 `DB_STR` id | 0216/0217/0218, verified free; 0215 to §8.1 |
| §1.6 `pubdef.in` `J` column | copies `DB2_AM_EXCL * I *` (`pubdef.in:567`) |
| C12 associate DUP primary | already refused, `db_iface.c:181-185` |
| §3.4 default callback pointers | `bt_method.c:52-53`, `hash_method.c:46-47` |
| **Deferred, [UNSURE]** | |
| §4.1 old-libdb `DB_TRUNCATE` of a typed subdatabase | the implementer probes it in commit 4 with the `typed_old_reader` fixture; no constraint can be skipped either way (§4.1) |
| §8.3 R1 with overflow keys | the implementer extends `r1_revive` with case (c) first; delete-and-insert for all cases is the default |
| §9 commit 12 `db_verify` behaviour on a typed subdatabase | probe first; verify calls no `*_metachk` |
| C10 / §6.3 | `TOTAL` on a record codec remains a promise; libdb bounds what `field()` returns, not what it reads |


---

## §11 Implementation deviations

Each entry names the spec text, what was built instead, and the evidence.

**V1 (P3, §8.3). The key item is replaced in place, not the pair.**
§8.3's fallback reads "`__bam_ditem` the pair and redo the put as
`DB_KEYFIRST` at `indx`". Built instead: when the revived slot's stored key
bytes differ, `__bam_iitem`'s `DB_CURRENT` case deletes and re-adds **only the
key item** at the same index (`__bam_ditem` + `__db_pitem`, both logged), then
runs the existing data step unchanged. This is the shape case 4 already uses
for data, and it keeps the index, so no `__bam_ca_di` cursor adjustment is
needed. It is used for every key type, which is the spec's own stated default
("delete-and-insert for every case"); `__bam_ritem` is not used. The helper is
`__bam_revive_chk` (decides only), not `__bam_revive_key`; the §7 sabotage
arm is therefore "delete the `__bam_revive_chk` call". Evidence
(`r1_revive`, commit "btree: an equal-key put over a deleted slot kept the
deleted key's bytes (R1)"): with a cursor pinned on the item, that cursor's
`DB_CURRENT` reads the new key and data in all 12 pinned cases. These include
overflow to overflow, keydata to overflow and back, and a 3 to 900-byte
revive on a full leaf that must split first (asserted: 1 leaf before, 2 or
more after). All txn cases survive delete-the-file plus `DB_RECOVER_FATAL`.

**V2 (P3, §8.3). A key shared by an on-page duplicate set is not replaced.**
§8.3 does not mention duplicates. With `DB_DUPSORT`, a put can revive one
deleted member of an on-page set whose key item is shared (`inp[]` equal)
with other members, some possibly live. Rewriting a shared key would mean
splitting it off the set. `__bam_revive_chk` returns "no revive" for a shared
key, which keeps today's behaviour for that case. Known ceiling (a
`ponytail:` note in `bt_put.c`): a set whose members are all deleted and
pinned, revived through one member, keeps the old key bytes. Typed databases
refuse duplicates (C12), so Phase 1 does not depend on it.

**V3 (P3, §8.3 test item (d)).** "Run `db_recover -c` on the txn-env case"
is done in-process. The test deletes the `.db` file and reopens the env with
`DB_RECOVER_FATAL`, which is what `db_recover -c` does. The file is then
rebuilt from the log alone, a stronger check than recovering over an intact
file. This avoids a dependence on the utility's path in a sibling build dir.

**V4 (Phase 1 commit 1, §2.4). The footprint gate's base is the parent of the
first `src/typed/` commit, not the merge base with master.** §2.4 says
`run_typed_footprint.sh` builds the base at `$(git merge-base HEAD master)`.
Built instead: the base defaults to the parent of the oldest commit that added
`src/typed/` (`git log --diff-filter=A -- src/typed | tail -1`, then `^`).
Reason: the branch carries Phase 0 (P1-P3), which moves `libdb-*.so` `.text`
legitimately and is approved; the merge base with master is *below* P1-P3, so
item 1 (`.text` equal) would go red for a Phase 0 change that has nothing to do
with the typed layer. The parent of the first typed commit sits *above* P1-P3
and *below* the typed layer, so it isolates exactly the typed layer's cost to a
default build -- which is what §2.4 is measuring. Evidence: on this branch
`merge-base HEAD master` = `eb63c0ca1` (before P1-P3) while the chosen base =
`f13921ff7` (P3's tip, the parent of commit 1); building both default and
measuring `.text` shows commit 1 adds 0 bytes against `f13921ff7` and a
non-zero delta against `eb63c0ca1` purely from P1-P3. `TYPED_FOOTPRINT_BASE`
still overrides. The script also no longer uses `size -m`/`__text` for macOS
(the gate host is FreeBSD/Linux, `size -A`) and ignores `CFLAGS/LDFLAGS/CPPFLAGS`
so a sanitizer CI job measures the same default build. CI (`ci.yml` typed job)
keeps `fetch-depth: 0` and lets the runner find the base; it no longer passes
`TYPED_FOOTPRINT_BASE=$(git merge-base ...)`, which would reintroduce the
Phase 0 false red. A later master merge that moves `.text` makes item 1 red for
the wrong reason; the `ponytail:` note in the script names the fix (set
`TYPED_FOOTPRINT_BASE` to that merge minus `src/typed`).

**V5 (Phase 1 commit 1, gate `s_validate` on FreeBSD).** The gate
`cd dist && sh ./s_validate` fails on the FreeBSD host at `s_chk_windef`
("missing items in libdb.def file", the four `db_env_set_func_{assert,free,
malloc,realloc}`), while it passes on Linux. Not a regression: master's
identical `s_chk_windef` fails the same way under BSD sed. Root cause: the
derived list used `sed -e '/db_env_set_func_\(assert\|free\|malloc\|
realloc\)/p' -e '/db_env_set_func_/d'` in one invocation, relying on GNU
sed's print-then-delete order; BSD sed deletes all four. Fixed by rewriting
that step as an explicit keep-list under `sed -n` (`.../p` for the four, plus
`/^db_env_set_func_/!p` for everything outside the family), which is byte-
identical to the old GNU-sed output (verified: 14-line sorted `t2`, `cmp`
equal) and produces the same four lines under both seds. This is the only way
the handoff's `s_validate` gate can be green on the host it is told to run on;
it changes no library behaviour and no exported symbol, so the default build
stays byte-identical.

**V6 (Phase 1 commit 1, test `handle_sizes` typed tier).** §7.2's sabotage for
the typed `handle_sizes` is "add `void *typed_internal;` to `struct __db`
under `#ifdef HAVE_TYPED` → FAIL". As written in commit 1, `handle_sizes.c`
`#include`s the *public* `db.h` and the runner compiled it with no `HAVE_TYPED`
define, so a field guarded by `#ifdef HAVE_TYPED` was invisible to the test
even against a typed build's `db.h`: the sabotage PASSed (a vacuous green).
Fixed: `run_handle_sizes.sh` now compiles the consumer with `-DHAVE_TYPED`
when `$BUILD/db_config.h` has it, so the test measures the struct the way the
library's own `.c` files see it -- which is the only size that matters, since
these structs are library-allocated and opaque (A1). This is also the exact
ABI hazard D2 forbids: a field added under `#ifdef HAVE_TYPED` to a public
struct makes the library (built with the macro) and an application (built
without) disagree on `sizeof(DB)`. On the real typed layer no such field
exists, so the flag leaves `sizeof(DB)=1744` unchanged (verified) and only a
wrong typed build trips it. Evidence: unsabotaged typed build with
`-DHAVE_TYPED` -> `DB actual=1744 ... PASS`; with the sabotage field ->
`DB actual=1752 ... FAIL 1 handle size(s) changed`. The sabotage arm
`handle_sizes_typed` was added to `test/db/typed-sabotage.sh`; because `db.h`
is generated from `db.in` by `config.status` (not by `make`), the arm edits
`db.in` and re-runs `config.status` in the build dir before the test
recompiles.

**V7 (Phase 1 commit 2, §1.1). The typed struct block goes after the `DBTYPE`
enum, not after `struct __db_dbt`.** §1.1 places the four typed structs in a
"new block after `struct __db_dbt`". Built there, `db.h` does not compile:
`struct __db_schema` has a by-value `DBTYPE type` member, and `DBTYPE` is a
`typedef enum` defined ~1100 lines *below* `__db_dbt` (`db.in`'s enum, now at
the `DB_BTREE=1 ... DB_UNKNOWN=5` block). `DB *` in `struct __db_constraint`
is fine (forward-typedef'd early), but `DBTYPE` is not. Observed:
`db.h:323: error: unknown type name 'DBTYPE'`, failing every engine `.c`.
Built instead: the block is inserted immediately after the `} DBTYPE;` enum,
which has `DBTYPE`, `DBT`, `DB`, and `u_int8_t` all in scope. Also fixed a
`-Wcomment` warning: the record-codec `field()` comment read `*p/*len`, which
clang parsed as a nested `/*`; reworded to "the returned (p, len)".

**V8 (Phase 1 commit 2, §1.6 s_sig gate). `env_sig.c` is pre-existing-stale,
so `git diff --exit-code` cannot be the D2 gate; the exclusions are proven
differentially instead.** §1.6's gate is `cd dist && sh s_sig && git
diff --exit-code ../src/env/env_sig.c`. On this branch (and identically on
master) a bare `sh s_sig` already rewrites `env_sig.c`: the committed file is
missing ~11 non-typed structs added to the tree without regeneration
(`__cq_part`, `__bam_rsnap`, `__db_aio_op`, ...) and carries an SPDX header
`s_sig` does not emit. Regenerating would pull in those structs, move the
runtime signature (which the footprint gate pins at `0xe7904e69`), and even
break the build (some stale structs are not visible to `env_sig.c`'s include
set) -- a large, non-typed change that does not belong in commit 2. So
`env_sig.c` is left as committed, and D2 is proven two ways that do not depend
on it being in sync: (1) the footprint gate shows the runtime `env_sig` equal
across the typed-free base, the default head, and the typed build
(`0xe7904e69`); (2) differentially, `s_sig` WITH the four C2 exclusions emits
`(42 + 115)` and no `__ADD(__db_codec|__db_record_codec|__db_constraint|
__db_schema)`, while WITHOUT them it emits `(42 + 119)` and those four lines --
so the exclusions are exactly what keep the typed structs out of the
signature. The `typed_footprint` second sabotage arm (`footprint_sig`) injects
those four `__ADD` lines into the committed `env_sig.c` directly (not via a
full `s_sig` regen, for the same build-break reason) and the footprint gate's
item 2 (base vs head signature) goes red: `2(sig 0xe7904e69->0xf9ff4329)`.

**V9 (Phase 1 commit 2, §2.4 footprint item 4). The s_sig sabotage trips item
2, not item 4.** §2.4's second arm says dropping the `s_sig` exclusion makes
"items 2 and 4" fail. Item 4 compares the default and `--enable-typed` builds
of the *same* tree, which share one `env_sig.c`; a signature edit moves both
equally, so item 4 stays green. The regression shows up in item 2 (the
typed-free base vs head), which is the measurement that actually encodes D2:
"the typed layer did not move the signature versus a tree without it." The
`footprint_sig` arm therefore asserts item 2.

**V10 (Phase 1 commit 2, two validate scripts and one not_public entry).**
Three gate scripts needed a one-line typed-aware edit, each a false
positive the typed declarations would otherwise cause; none changes library
behaviour:
- `dist/validate/s_chk_ext_method_calls` extracts every `(*name)` function
  pointer from `db.in` and forbids `->name(` calls in the engine. The RFC 0014
  codec callbacks (`compare`, `equal`, `hash`, `validate`, `sortkey`, `field`)
  are application callbacks the library is *meant* to invoke (the `dup_compare`
  / backup-callback category already excluded); `compare` collided with
  `partition.c`'s internal `ka->compare()`. Excluded by exact member name.
- `dist/validate/s_chk_windef` compares `ext_prot.in` exports against
  `build_windows/libdb.def`. The three typed entry points are now in
  `ext_prot.in` but are not exported on Windows (the whole layer is
  `HAVE_TYPED`-guarded and Windows never sets it, §2.3), so they are dropped
  from the derived list rather than added to `libdb.def`.
- `dist/validate/s_chk_pubdef` demands a `pubdef.in` row for every `DB_*`/`DB2_*`
  `#define`. `DB2_TYPED_SLOT_SHIFT` is an internal bit position (not an API
  constant and not in §1.6's pubdef list), so it joins `not_public` with a
  reason, like `DB_CURSOR_NPART`. The function-like macro `DB2_TYPED_SLOT(dbp)`
  is not extracted by the check's greedy sed, so it needs nothing.

**V11 (Phase 1 commit 2, footprint TYPED_API detection).** The footprint
runner auto-set `TYPED_API=1` once `grep -q 'db_set_schema'` matched
`src/typed/typed.c`. Commit 2 adds an `EXTERN: ... db_set_schema` comment there
(the prototype source) while the *definition* is still commit 3, so the old
grep flipped `TYPED_API` on a commit that cannot yet export the symbol, making
item 3 demand a typed export that does not exist. Changed to match a
definition (`^db_set_schema[ (]`, K&R column 0), which the comment does not
satisfy; `TYPED_API` stays 0 through commit 2 (verified: item 3 reports "not
yet applicable").

**V12 (Phase 1 commit 4, test `typed_old_reader` in the `db` tier).** §7.2
registers `typed_old_reader` in the `db` tier and describes it only as "the
default build refuses the fixture (BDB0055)". The handoff gate, however, runs
the whole `db` tier in BOTH a default and an `--enable-typed` build each
commit. Run against the typed library the plain refusal assertion is a false
red: a typed build KNOWS `BTM_TYPED`, so it opens the fixture (the read-only
rule is §4.1, commit 5). Built instead: the runner compiles the reader with
`-DHAVE_TYPED` iff `$BUILD/db_config.h` has it (the `run_handle_sizes.sh`
precedent), and the reader asserts the opposite outcome under that guard --
refusal (EINVAL/BDB0055) without the macro, a successful schema-less open with
it. Neither branch is a skip, so neither is vacuous: against the default build
the only pass is a BDB0055 refusal, against the typed build the only pass is an
open. The `typed_old_reader_mask` sabotage still runs against the default build
(no `-DHAVE_TYPED`), where dropping the mask guard makes the default library
open the fixture and the `#else` branch FAILs. Evidence (commit 4): db-tier vs
default -> "refused EINVAL/BDB0055" 2/2 PASS; db-tier vs typed -> "opened
(typed build knows the bit)" 2/2 PASS; mask sabotage vs default -> "RDONLY
ret=0 ... FAIL". The typed-tier regen companion compares the committed and
regenerated fixtures' `DBMETA.flags` word (file offset 32816 = subdatabase meta
page 32768 + struct offset 48), which carries `BTM_TYPED`/`DB_HASH_TYPED`, so
the fixture cannot drift from the code (`20010000` for the Btree, `0a000000`
for the Hash).

**V13 (Phase 1 commit 4, fixtures carry the meta bit but no catalog record).**
§9 commit 4 lists "the on-disk fixtures" and §7.2's `typed_old_reader` names
"one typed subdatabase each plus `__db_typed`". The committed fixtures carry
the `BTM_TYPED`/`DB_HASH_TYPED` meta bit (commit 4) but NOT a `__db_typed`
catalog subdatabase, because the catalog writer is commit 5. This is sufficient
for every assertion `typed_old_reader` makes in commit 4: the refusal a reader
without the bits gives is driven entirely by the unknown meta flag
(`__db_fchk`/BDB0055), which `__db_typed` would not change. Commit 5
regenerates the fixtures with the catalog present and extends the regen
companion to the catalog bytes; the committed fixtures are refreshed then. The
`db_set_schema`-created fixture in commit 4 is a typed subdatabase that a
commit-5 typed open would reject with "no catalog record" -- which is why the
fixtures are regenerated in commit 5, not reused.

**V14 (Phase 1 commit 5, fixtures refreshed; typed_old_reader RW now refused).**
As foreseen in V13, commit 5 regenerated test/db/fixtures/typed_{bt,hash}.db so
each now carries its `__db_typed` catalog record (the files grew from 2 to 3
meta pages; the typed subdatabase meta flags stay at file offset 32816 =
`0x00000120` Btree / `0x0000000a` Hash, so the typed-tier regen byte-compare is
unchanged). Commit 5 also adds the §4.1 read-only rule, which changes what a
typed build does with a SCHEMA-LESS open of a typed subdatabase: in commit 4 it
opened read-write, in commit 5 it opens only read-only (DB_RDONLY) or for the
internal DB_WRITEOPEN probe, and refuses a plain read-write schema-less open
with EINVAL (not BDB0055). `typed_old_reader`'s `#ifdef HAVE_TYPED` branch was
updated to match: RDONLY must open, read-write must be refused with EINVAL and
NOT a BDB0055 (a BDB0055 would mean the typed build fails to recognise its own
bit). The default-build branch is unchanged (BDB0055 refusal, both modes), and
the `typed_old_reader_mask` sabotage still runs against the default build.

**V15 (Phase 1 commit 5, internal typed prototypes live in typed_int.h, not
the generated typed_ext.h).** `__typed_catalog_write` / `__typed_catalog_check`
take a `TYPED_STATE *`, and `__typed_state` returns one. `TYPED_STATE` lives
only in `src/typed/typed_int.h` (C2/C3 keep it out of any header `s_sig`
scans), so a `PUBLIC:`-generated prototype naming it in `src/dbinc_auto/
typed_ext.h` -- which `db_int.h` includes everywhere -- would be an
unresolvable type in every engine TU. Built instead: the two catalog
functions are declared directly in `typed_int.h` (included only by the three
`src/typed/*.c` files) with forward `struct __db_thread_info`/`struct __db_txn`
decls, and `__typed_state` is `static` to typed.c (commit 3). Only the typed
functions that take purely public types (`__typed_release`,
`__typed_fingerprint`, `__typed_memcmp_order`, `__typed_codec_cmp`,
`__typed_open`, `__typed_catalog_remove`, `__typed_catalog_rename`) carry
`PUBLIC:` markers and reach the generated header; the remove/rename pair is
reached that way from db_remove.c / db_rename.c, which do not include
typed_int.h.

**V16 (Phase 1 commit 7, typed_unique is sequential; the concurrent race is
typed_unique_mp).** §7.2's typed_unique says "two txns insert the same UNIQUE
field value ... exactly one commits; the loser gets DB_KEYEXIST, or
DB_LOCK_DEADLOCK and then DB_KEYEXIST on retry". A genuinely interleaved
two-txn race in ONE process hangs: txn B blocks on txn A's write lock on the
unique-index key, and with no background deadlock-detector thread and no
competing waiter nothing re-invokes detection to expire B's lock timeout
(detection runs on each conflicting REQUEST, not on an already-parked waiter),
so B waits forever. Built instead: typed_unique runs A to COMMIT first, then B
in a fresh transaction inserts the same value and must get DB_KEYEXIST --
across all three isolation levels (2PL, DB_TXN_SNAPSHOT, DB_TXN_SERIALIZABLE,
env opened DB_MULTIVERSION), proving the level does not change the outcome, plus
the untyped-secondary control (still EINVAL, D6 is conditional). The genuinely
concurrent check-then-insert race is typed_unique_mp, which forks + execs two
real processes that block on each other's unique-index lock and resolve to
exactly one winner and one DB_KEYEXIST loser (verified stable over repeated
runs). This matches the handoff: "typed_unique_mp must use real processes
(fork+exec), not threads".

**V17 (Phase 1 commit 7, the single-process UNIQUE counter sabotage faults
rather than committing twice).** §7.2's typed_unique arm (a) is
"__typed_unique_cb appends a per-call counter to the key -> both commit".
Built as a per-row key disambiguator (append the primary key, so it also
breaks the cross-process typed_unique_mp arm, where a per-process static
counter would not -- each process starts its counter at 0 and the keys still
collide). Against the single-process typed_unique the sabotaged callback faults
(signal 11) rather than cleanly committing twice; a crash is still a correct
"sabotage detected" (non-zero exit, test FAILs), and the behaviour of
deliberately corrupted callback code is undefined by construction. The clean,
informative failure is the typed_unique_mp arm ("FAIL 3 checks: winners=2").
Both arms FAIL when applied and PASS when restored. The two UNIQUE sabotage
arms that matter for correctness are typed_unique_d6 (revert DB_KEYEXIST ->
EINVAL: loser sees EINVAL) and typed_unique_d6uncond (make D6 unconditional:
the untyped-secondary control breaks), both of which FAIL cleanly.

**V18 (Phase 1 commit 8, §3.6). The generated callbacks are installed in
`__typed_open`, not in `db_set_schema` "through the existing setters".** §3.6's
table header reads "installed by `db_set_schema`, through the existing
setters". Built instead: `db_set_schema` installs nothing; `__typed_open`
writes `bt->bt_compare`/`bt->bt_prefix` (Btree, non-MEMCMP key) and
`ht->h_hash`/`ht->h_compare` (Hash, non-EQUALIMAGE key) directly on the
internal struct, after the identity check proves the slots still hold the
defaults. Two reasons the setter path cannot be used: (1) `bt_internal` /
`h_internal` may not exist at `db_set_schema` time -- they are allocated by
`__bam_db_create` / `__ham_db_create`, which `db_create(dbp, dbenv, 0)` (type
unknown) defers to open; `db_set_schema`'s own rule-8 check already guards
`dbp->bt_internal != NULL`, confirming it can be NULL there. (2) `__bam_set_bt_compare`
/ `__ham_set_h_compare` both call `DB_ILLEGAL_AFTER_OPEN`, which fails inside
`__typed_open` (the handle is mid-open). Writing the fields directly is exactly
what those setters do minus the open guard they cannot pass here, and the
identity check (commit 4, updated here) proves no application callback is being
overwritten. For Hash the install also matches `__ham_open`'s behaviour, which
only fills a NULL `h_hash` with `__ham_func5`; re-pointing it at
`__typed_h_hash` after open is the same contract an application re-setting its
hash on every open satisfies, and the newly created table is empty so no record
was bucketed by the wrong hash. Evidence (`typed_btree_cmp`, `typed_hash`):
with the install present a reverse-order key scans 3,2,1 and a case-insensitive
Hash finds "AbC" through "abc"; the sabotage arms `typed_btree_cmp_noinstall`
and `typed_hash_nohash` (which replace the install with the default) FAIL as
required.

**V19 (Phase 1 commit 8, test `typed_codec_mismatch`). The commit-5 test's
placeholder `field()` is replaced with a real one, because commit 8's
comparator reads it.** `typed_codec_mismatch` (commit 5) built its record codec
with `rc->field` set to a cast of `mkrc` itself ("non-NULL" placeholder): commit
5 installed no comparator, so nothing ever called `field()`. Commit 8 installs
`__typed_bt_compare` on the non-MEMCMP `oint` key, and case F (identical schema,
reads A's row) now calls `field()` -- the placeholder pointer crashed with
signal 11. Fixed in commit 8 (the commit that changes the behaviour): the test
now uses a real single-field locator (`recfield`, field 0 = whole record). This
is the only pre-existing typed test with a fake `field` on a non-MEMCMP Btree
key; every other typed Btree-key test uses a MEMCMP key codec (default
comparator, no `field()` read) or a valid `field`, so none other needed a
change. Evidence: `typed_codec_mismatch` 6/6 PASS after the fix, including case
F's read; the `typed_codec_mismatch_fp` sabotage (skip the fingerprint
comparison) still FAILs.

**V20 (Phase 1 commit 8, `typed_hash` forces a low fill factor so the sabotage
is deterministic).** §7.2's `typed_hash` sabotage is "do not install `h_hash` ->
'AbC' lands in another bucket". With libdb's default fill factor a 5000-key
table has only ~6 buckets, so `__ham_func5("abc") % 6` and `__ham_func5("AbC") %
6` collide often and the differently-cased get still finds the record through
`h_compare` alone -- the sabotage passed vacuously (observed). Built instead:
`typed_hash` calls `set_h_ffactor(dbp, 2)` and populates 5000 distinct keys,
giving ~2836 buckets (measured), so a byte-sensitive hash deterministically
sends "abc" and "AbC" to different buckets. With the real `__typed_h_hash`
installed both case-forms hash identically and the get succeeds (test PASS);
with the sabotage the get misses (sabotage FAIL, verified: "FAIL [2]: get AbC
finds the abc record"). The count assertions are made relative to the populated
baseline.
