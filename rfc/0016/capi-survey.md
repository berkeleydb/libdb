<!-- Backing evidence for RFC 0016. The ABI facts in §8 and the flag/return-code
     values were RE-VERIFIED on a FreeBSD HEAD build at commit 5e00a8b5e
     (DB_CALVER 2026.10.7, the first LTS) on 2026-10-16: struct sizes DB=1744
     DBC=552 DB_ENV=2088 DB_TXN=336 DBT=40 DB_LSN=8 DB_SEQUENCE=288 DB_LOGC=152
     DB_MPOOLFILE=312; soname libdb-2026.0.so; db_version maj=2026 min=0 patch=9.
     All flag/opcode/DB_* constants in this file were printed from that build and
     match. The structs are OPAQUE to a binding, so the sizes are informational;
     the binding depends on method-pointer signatures and the constants. -->

# libdb Public C API Survey — for Rust & Zig binding design

Scope: the public C API a language binding must cover, mapped from the public
header template `src/dbinc/db.in` and the generated `build_unix/db.h`.
Every structural claim cites `file:line`. "db.in" = `src/dbinc/db.in`;
"db.h" = `build_unix/db.h` (the generated public header — flags and top-level
prototypes that db.in holds as `@...@` substitution tokens appear only there).

Version / ABI context (read from the built tree):
- `build_unix/db.h:45-49`: family=11 release=2 **MAJOR=2026 MINOR=0 PATCH=9**;
  `DB_CALVER "2026.10.1"` (db.h:56); `DB_VERSION_STRING "libdb 2026.10.1
  (October 6, 2026)"` (db.h:57). NOTE: `meson.build:15` declares `version:
  '2026.10.7'`, so the checked-in `build_unix/` artifacts are a few patch
  releases behind HEAD. The MAJOR/MINOR/PATCH triple (NOT CALVER) is the
  format/ABI compat level (db.in:48-55 comment).

===============================================================================
## 1. Handle types and their method tables
===============================================================================

libdb is a C object system: each handle is a heap struct whose public surface is
a block of function pointers ("methods"), every one taking its own handle as the
first argument. The library allocates and owns these structs; an application
calls `db_env_create`/`db_create` (or a handle's factory method) to obtain a
pointer, calls methods through the pointers, and calls a `close`/`commit`/`abort`
method to destroy. The structs are OPAQUE to a binding (see §8): never embed or
allocate them, never read/write fields except on DBT and the 2-3 documented
public sub-fields.

Top-level C entry points (generated prototype block db.h:3096-3133; all confirmed
exported in `build_unix/.libs/libdb-2026.0.so` via `nm -D`):
- `int  db_env_create(DB_ENV **, u_int32_t);`                   (db.h:3127)
- `int  db_create(DB **, DB_ENV *, u_int32_t);`                 (db.h:3098)
- `int  db_sequence_create(DB_SEQUENCE **, DB *, u_int32_t);`   (db.h:3137)
- `char *db_strerror(int);`                                      (db.h:3099)
- `char *db_version(int *, int *, int *);`                       (db.h:3128)
- `char *db_full_version(int *, int *, int *, int *, int *);`    (db.h:3129)
- `int  log_compare(const DB_LSN *, const DB_LSN *);`            (db.h:3130)
- `int  db_copy(DB_ENV *, const char *, const char *, const char *);` (db.h:3096)
- `int  db_get_multiple(DB *, DB_TXN *, DBT *, DBT *, int *, u_int32_t, u_int32_t);` (db.h:3097)
- process-global I/O override hooks `db_env_set_func_*` (db.h:3100-3126).

### DB_ENV — database environment  (struct __db_env, db.in:2316)
Created: `db_env_create(&dbenv, 0)`. Opened: `dbenv->open(dbenv, home, flags, mode)`.
Destroyed: `dbenv->close(dbenv, flags)` / `dbenv->remove(...)`; after either the
handle is dead regardless of return (program_scope.md).
Public method block: db.in:2471-2712 (`/* DB_ENV PUBLIC HANDLE LIST BEGIN/END */`).
Selected signatures (verbatim):
- `int (*open)(DB_ENV *, const char *, u_int32_t, int);`           (db.in:2599)
- `int (*close)(DB_ENV *, u_int32_t);`                              (db.in:2475)
- `int (*remove)(DB_ENV *, const char *, u_int32_t);`              (db.in:2600)
- `int (*txn_begin)(DB_ENV *, DB_TXN *parent, DB_TXN **, u_int32_t);`(db.in:2706)
- `int (*txn_checkpoint)(DB_ENV *, u_int32_t, u_int32_t, u_int32_t);`(db.in:2707)
- `int (*txn_recover)(DB_ENV *, DB_PREPLIST *, long, long *, u_int32_t);`(db.in:2708)
- `int (*log_cursor)(DB_ENV *, DB_LOGC **, u_int32_t);`             (db.in:2559)
- `int (*memp_fcreate)(DB_ENV *, DB_MPOOLFILE **, u_int32_t);`      (db.in:2575)
- `int (*lock_get/lock_put/lock_vec/lock_detect/lock_id)`          (db.in:2548-2556)
- `int (*failchk)(DB_ENV *, u_int32_t);`                           (db.in:2483)
- `void (*err)(const DB_ENV *, int, const char *, ...);`           (db.in:2481)
- `void (*errx)(const DB_ENV *, const char *, ...);`               (db.in:2482)
- `rep_*` / `repmgr_*` replication families                        (db.in:2601-2705)
One-entry PRIVATE list (`prdbt`) follows the END marker (db.in:2744-2746) — ignore.

### DB — a database  (struct __db, db.in:1439)
Created: `db_create(&db, dbenv_or_NULL, 0)` (NULL dbenv => DB makes a private env).
Opened: `db->open(db, txn, file, database, DBTYPE, flags, mode)`.
Destroyed: `db->close(db, flags)` / `db->remove(...)` / `db->rename(...)`.
Public method block: db.in:1639-1774.
Core signatures (verbatim):
- `int (*open)(DB *, DB_TXN *, const char *file, const char *database, DBTYPE, u_int32_t, int);` (db.in:1714)
- `int (*close)(DB *, u_int32_t);`                                  (db.in:1645)
- `int (*get)(DB *, DB_TXN *, DBT *key, DBT *data, u_int32_t);`     (db.in:1654)
- `int (*put)(DB *, DB_TXN *, DBT *key, DBT *data, u_int32_t);`     (db.in:1717)
- `int (*del)(DB *, DB_TXN *, DBT *, u_int32_t);`                   (db.in:1649)
- `int (*exists)(DB *, DB_TXN *, DBT *, u_int32_t);`                (db.in:1652)
- `int (*cursor)(DB *, DB_TXN *, DBC **, u_int32_t);`               (db.in:1648)
- `int (*pget)(DB *, DB_TXN *, DBT *key, DBT *pkey, DBT *data, u_int32_t);` (db.in:1716)
- `int (*associate)(DB *, DB_TXN *, DB *secondary, int (*)(DB *, const DBT *, const DBT *, DBT *), u_int32_t);` (db.in:1640)
- `int (*associate_foreign)(DB *, DB *, int (*)(DB *, const DBT *, DBT *, const DBT *, int *), u_int32_t);` (db.in:1642)
- `int (*join)(DB *, DBC **, DBC **, u_int32_t);`                   (db.in:1711)
- plus `sync`/`truncate`/`compact`/`stat`/`verify`/`upgrade` and a large
  `get_*`/`set_*` config family                                    (db.in:1655-1773)
PRIVATE list (`dump`, `db_am_remove`, `db_am_rename`) at db.in after the END marker.
WARNING: `stored_get`/`stored_close` and **`alt_close`** sit after the END marker
(`int (*alt_close)(DB *, u_int32_t)` "Alternative handle close function, used by
C++ API", db.in ~1790). A binding that owns the handle lifecycle may use the same
seam (see §cxx).

### DBC — cursor  (struct __dbc, db.in:2044)
Created: `db->cursor(db, txn, &dbc, flags)` or `dbc->dup(dbc, &newdbc, flags)`.
Destroyed: `dbc->close(dbc)`.
Public method block: db.in:2113-2124:
- `int (*close)(DBC *);`                                            (db.in:2114)
- `int (*get)(DBC *, DBT *key, DBT *data, u_int32_t);`             (db.in:2119)
- `int (*put)(DBC *, DBT *key, DBT *data, u_int32_t);`             (db.in:2122)
- `int (*del)(DBC *, u_int32_t);`                                  (db.in:2117)
- `int (*count)(DBC *, db_recno_t *, u_int32_t);`                  (db.in:2116)
- `int (*dup)(DBC *, DBC **, u_int32_t);`                          (db.in:2118)
- `int (*pget)(DBC *, DBT *, DBT *, DBT *, u_int32_t);`            (db.in:2121)
- `int (*cmp)(DBC *, DBC *, int *, u_int32_t);`                    (db.in:2115)
- `int (*get_priority/set_priority)`                               (db.in:2120,2123)
Deprecated `c_close`/`c_get`/`c_put`/... aliases exist (db.in:2127-2133) — do not bind.

### DB_TXN — transaction  (struct __db_txn, db.in:822)
Created: `dbenv->txn_begin(dbenv, parent_or_NULL, &txn, flags)`,
`dbenv->cdsgroup_begin(...)`, or recovered via `dbenv->txn_recover(...)`.
Resolved/destroyed: `txn->commit(txn, flags)` / `txn->abort(txn)`;
`txn->discard(txn, flags)` for a recovered-but-unresolved txn. After commit/abort
the handle is dead (program_scope.md).
Public method block: db.in:925-937:
- `int (*commit)(DB_TXN *, u_int32_t);`                             (db.in:927)
- `int (*abort)(DB_TXN *);`                                        (db.in:926)
- `int (*discard)(DB_TXN *, u_int32_t);`                           (db.in:928)
- `int (*prepare)(DB_TXN *, u_int8_t *gid);`                       (db.in:932)
- `u_int32_t (*id)(DB_TXN *);`                                     (db.in:931)
- `int (*set_name)(DB_TXN *, const char *);`                       (db.in:934)
- `int (*set_priority)(DB_TXN *, u_int32_t);`                      (db.in:935)
- `int (*set_timeout)(DB_TXN *, db_timeout_t, u_int32_t);`         (db.in:936)
PRIVATE: `set_txn_lsnp` (db.in:939-941) — ignore.

### DB_SEQUENCE — 64-bit sequence  (struct __db_sequence, db.in:1245)
Created: `db_sequence_create(&seq, db, 0)`. Opened: `seq->open(seq, txn, key_dbt, flags)`.
Destroyed: `seq->close(seq, flags)`; removed: `seq->remove(seq, txn, flags)`.
Public method block: db.in:1259-1279:
- `int (*open)(DB_SEQUENCE *, DB_TXN *, DBT *, u_int32_t);`         (db.in:1270)
- `int (*get)(DB_SEQUENCE *, DB_TXN *, int32_t delta, db_seq_t *, u_int32_t);` (db.in:1261)
- `int (*close)(DB_SEQUENCE *, u_int32_t);`                        (db.in:1260)
- `int (*remove)(DB_SEQUENCE *, DB_TXN *, u_int32_t);`            (db.in:1272)
- `int (*initial_value)(DB_SEQUENCE *, db_seq_t);`                (db.in:1269)
- `set_cachesize`(db.in:1273)/`get_cachesize`(db.in:1263)/`set_flags`/`get_flags`/
  `set_range`/`get_range`/`get_db`/`get_key`/`stat`/`stat_print`.
(`db_seq_t` is signed 64-bit; sequences require 64-bit integral support, db.in:124.)

### DB_LOGC — log cursor  (struct __db_log_cursor, db.in:516)
Created: `dbenv->log_cursor(dbenv, &logc, flags)`. Destroyed: `logc->close(logc, flags)`.
Public method block: db.in:535-539:
- `int (*close)(DB_LOGC *, u_int32_t);`                            (db.in:536)
- `int (*get)(DB_LOGC *, DB_LSN *, DBT *, u_int32_t);`            (db.in:537)
- `int (*version)(DB_LOGC *, u_int32_t *, u_int32_t);`            (db.in:538)

### DB_MPOOLFILE — buffer-pool file  (struct __db_mpoolfile, db.in:636)
Created: `dbenv->memp_fcreate(dbenv, &mpf, flags)`. Opened: `mpf->open(...)`.
Destroyed: `mpf->close(mpf, flags)`.
Public method block: db.in:684-708:
- `int (*open)(DB_MPOOLFILE *, const char *, u_int32_t, int, size_t);`    (db.in:697)
- `int (*get)(DB_MPOOLFILE *, db_pgno_t *, DB_TXN *, u_int32_t, void *);` (db.in:686)
- `int (*put)(DB_MPOOLFILE *, void *, DB_CACHE_PRIORITY, u_int32_t);`     (db.in:698)
- `int (*close)(DB_MPOOLFILE *, u_int32_t);`                              (db.in:685)
- plus get_*/set_* for clear_len/fileid/flags/ftype/lsn_offset/maxsize/pgcookie/priority.

### Minor handles (same pattern)
- DB_CHANNEL (struct __db_channel, db.in:2744) — repmgr messaging:
  close/send_msg/send_request/set_timeout (db.in:2749-2755).
- DB_SITE (struct __db_site, db.in:2758) — repmgr site:
  get_address/get_config/get_eid/set_config/remove/close (db.in:2765-2772).
- DB_LOCK/DB_LOCKREQ/DB_PREPLIST/DB_KEY_RANGE/DB_COMPACT/DB_HEAP_RID — plain data
  structs the app allocates by value and passes in; not method tables.

===============================================================================
## 2. DBT — the data carrier  (struct __db_dbt, db.in:225-249; identical db.h:226-249)
===============================================================================

Verified `sizeof(DBT)==40` on LP64 (handle_sizes run, §8). Fields:
```
void      *data;    /* key/data pointer                         db.in:226 */
u_int32_t  size;    /* key/data length                          db.in:227 */
u_int32_t  ulen;    /* length of USER buffer (USERMEM)          db.in:230 */
u_int32_t  dlen;    /* partial: length of the sub-record        db.in:231 */
u_int32_t  doff;    /* partial: offset of the sub-record        db.in:232 */
void      *app_data;/* application scratch / bulk cookie        db.in:234 */
u_int32_t  flags;   /* DB_DBT_* below                           db.in:248 */
```
`app_data` is a PUBLIC field (distinct from the "app_private" on handles); the
Java layer reuses it to cache a locked byte[] (java_callbacks.i:606,619).

DB_DBT_* flags (db.in:236-247):
- `DB_DBT_APPMALLOC   0x001`  callback-allocated memory (set by app in a 2ndary cb).
- `DB_DBT_BULK        0x002`  *internal* (bulk write buffer); set by DB_MULTIPLE_WRITE_INIT.
- `DB_DBT_DUPOK       0x004`  *internal*.
- `DB_DBT_ISSET       0x008`  *internal*.
- `DB_DBT_MALLOC      0x010`  DB malloc()s the return buffer; app owns & frees it.
- `DB_DBT_MULTIPLE    0x020`  DBT references multiple records (bulk get).
- `DB_DBT_PARTIAL     0x040`  partial get/put using dlen+doff.
- `DB_DBT_REALLOC     0x080`  DB realloc()s the return buffer across calls; app frees.
- `DB_DBT_READONLY    0x100`  do not update this DBT.
- `DB_DBT_STREAMING   0x200`  *internal*.
- `DB_DBT_USERCOPY    0x400`  return via a user copy callback (advanced; see §5 note).
- `DB_DBT_USERMEM     0x800`  return into app buffer at `data`, capacity `ulen`.

Memory-ownership model the binding must encode (the single hardest DBT concern):
- Default (no flag) on a get: DB returns a pointer into DB-managed memory valid
  only until the next op on that cursor/handle in that thread. Copy out eagerly,
  or require DB_THREAD + USERMEM.
- USERMEM: app provides `data`+`ulen`; too small => `DB_BUFFER_SMALL` and DB sets
  `size` to the needed length (retry with a bigger buffer). Natural fit for a safe
  binding over a caller-owned slice.
- MALLOC/REALLOC: DB allocates; the binding frees with the matching allocator (the
  one set via `set_alloc`, else libc).
- On input (put/key), DB only reads `data[0..size)`; a binding may point at a
  borrowed slice for the call's duration.
- A DBT can legally be `data==NULL, size==0`; a binding's memcpy wrapper MUST guard
  length 0 (this exact shape caused real UB in the library's own log_put.c).

Bulk retrieval macros (one call returns many records in one buffer; walk them —
pure pointer arithmetic over `dbt->data`/`dbt->ulen`, sentinel `(u_int32_t)-1`; a
binding can reimplement them natively):
- `DB_MULTIPLE_INIT(ptr, dbt)`                               (db.in:1875)
- `DB_MULTIPLE_NEXT(ptr, dbt, retdata, retdlen)`             (db.in:1879)
- `DB_MULTIPLE_KEY_NEXT(ptr, dbt, k, klen, d, dlen)`         (db.in:1894)
- `DB_MULTIPLE_RECNO_NEXT(ptr, dbt, recno, d, dlen)`         (db.in:1910)
- write side: `DB_MULTIPLE_WRITE_INIT`(db.in:1925),
  `DB_MULTIPLE_RESERVE_NEXT`(db.in:1933), `DB_MULTIPLE_WRITE_NEXT`(db.in:1949),
  and `DB_MULTIPLE_KEY_*` variants.
Buffer must be u_int32_t-aligned and the cursor get must set `DB_MULTIPLE 0x800`
(db.h) or `DB_MULTIPLE_KEY 0x4000` (db.h).

===============================================================================
## 3. Error reporting
===============================================================================

Dual convention on every method: return `int` where
- `0`  = success;
- `>0` = a system errno (EINVAL, ENOMEM, EACCES, ...);
- `<0` = a DB_* code in the reserved range -30,800 .. -30,999 (db.in:1376 comment).
`char *db_strerror(int)` maps either kind to a message (db.h:3099; exported).

DB (public) return codes (db.in:1392-1424):
```
DB_BUFFER_SMALL      -30999  user memory too small (USERMEM retry signal)   db.in:1392
DB_DONOTINDEX        -30998  "null" return from a secondary-key callback    db.in:1393
DB_FOREIGN_CONFLICT  -30997  foreign-key constraint triggered               db.in:1394
DB_HEAP_FULL         -30996  no free space in a heap db                     db.in:1395
DB_KEYEMPTY          -30995  record deleted/never created (queue/recno)     db.in:1396
DB_KEYEXIST          -30994  key/data pair already exists (NOOVERWRITE)     db.in:1397
DB_LOCK_DEADLOCK     -30993  deadlock; txn must abort and retry             db.in:1398
DB_LOCK_NOTGRANTED   -30992  lock unavailable (NOWAIT / timeout)            db.in:1399
DB_LOG_BUFFER_FULL   -30991                                                 db.in:1400
DB_LOG_VERIFY_BAD    -30990                                                 db.in:1401
DB_NOSERVER          -30989                                                 db.in:1402
DB_NOTFOUND          -30988  key/data not found; also cursor EOF            db.in:1403
DB_OLD_VERSION       -30987                                                 db.in:1404
DB_PAGE_NOTFOUND     -30986                                                 db.in:1405
DB_REP_* (DUPMASTER..WOULDROLLBACK)  -30985..-30974  replication status     db.in:1406-1417
DB_RUNRECOVERY       -30973  PANIC: env unusable, must run recovery         db.in:1418
DB_SECONDARY_BAD     -30972  secondary index corrupt                       db.in:1419
DB_TIMEOUT           -30971  timed out on read consistency                 db.in:1420
DB_VERIFY_BAD        -30970  verify failed                                 db.in:1421
DB_VERSION_MISMATCH  -30969  env version mismatch (recreate env)           db.in:1422
DB_SNAPSHOT_CONFLICT -30968  SSI conflicting snapshot update               db.in:1423
DB_SNAPSHOT_UNSAFE   -30967  SSI potential anomaly                         db.in:1424
```
DB (private) codes -30899..-30887 (db.in:1427-1440): DB_ALREADY_ABORTED,
DB_CHKSUM_FAIL, DB_DELETED, DB_EVENT_NOT_HANDLED, DB_NEEDSPLIT, DB_REP_BULKOVF,
DB_REP_LOGREADY, DB_REP_NEWMASTER, DB_REP_PAGEDONE, DB_SURPRISE_KID, DB_SWAPBYTES,
DB_TXN_CKP, DB_VERIFY_FATAL — internal; a binding generally should not surface them.

Binding guidance: 0=>Ok; `<0 && in DB range`=>typed enum; `>0`=>OS errno. Demote the
expected control-flow negatives: DB_NOTFOUND=>Ok(None), DB_KEYEXIST=>put outcome,
DB_BUFFER_SMALL=>grow-and-retry, DB_LOCK_DEADLOCK=>retry signal — not panics.

Error/message/prefix hooks (route DB diagnostics into the host language):
- `void (*set_errcall)(DB_ENV *, void (*)(const DB_ENV *, const char *errpfx, const char *msg));` (db.in:2655)
- `void (*set_errfile)(DB_ENV *, FILE *);`                          (db.in:2657)
- `void (*set_errpfx)(DB_ENV *, const char *);`                     (db.in:2658)
- `void (*set_msgcall)(DB_ENV *, void (*)(const DB_ENV *, const char *));` (db.in:2688)
- `void (*set_msgfile)(DB_ENV *, FILE *);`                          (db.in:2690)
- `int  (*set_paniccall)(DB_ENV *, void (*)(DB_ENV *, int errval));` (db.in:2691)
  DB mirrors all of these on the DB handle (set_errcall db.in:1737, set_errfile 1739,
  set_errpfx 1740, set_msgcall 1753, set_paniccall 1756).

===============================================================================
## 4. Access methods and open flags
===============================================================================

DBTYPE (enum, db.in:1301-1307): `DB_BTREE=1, DB_HASH=2, DB_RECNO=3, DB_QUEUE=4,
DB_UNKNOWN=5, DB_HEAP=6` (DB_UNKNOWN = detect on open).

DB->open() flags (db.h, hex verified):
`DB_CREATE 0x00000001`(db.h:2882), `DB_RDONLY 0x00000400`(db.h:2991),
`DB_EXCL 0x00000004`(db.h:2895), `DB_THREAD 0x00000020`(db.h:3055),
`DB_AUTO_COMMIT 0x00000100`(db.h:2872), `DB_TRUNCATE 0x00020000`(db.h:3057),
`DB_NOMMAP 0x00000010`(db.h:2977), `DB_MULTIVERSION 0x00000008`(db.h:2967),
`DB_READ_UNCOMMITTED 0x00000200`, `DB_READ_COMMITTED 0x00000400`,
`DB_FCNTL_LOCKING 0x00000800` (undocumented).

DB->set_flags() access-method flags (db.h): `DB_DUP 0x10`, `DB_DUPSORT 0x02`,
`DB_RECNUM 0x40`, `DB_RENUMBER 0x80`, `DB_REVSPLITOFF 0x100`, `DB_INORDER 0x20`,
`DB_CHKSUM 0x08`, `DB_ENCRYPT 0x01`, `DB_TXN_NOT_DURABLE 0x04`.

DB_ENV->open() subsystem/behavior flags (db.h):
- init: `DB_INIT_MPOOL 0x400`(db.h:2912), `DB_INIT_LOCK 0x100`, `DB_INIT_LOG 0x200`,
  `DB_INIT_TXN 0x2000`, `DB_INIT_REP 0x1000`, `DB_INIT_MUTEX 0x800`, `DB_INIT_CDB 0x80`.
- recovery: `DB_RECOVER 0x02`(db.h:2996), `DB_RECOVER_FATAL 0x20000`, `DB_CREATE 0x01`,
  `DB_USE_ENVIRON 0x04`(db.h:3070), `DB_USE_ENVIRON_ROOT 0x08`.
- memory/registration: `DB_PRIVATE 0x10000`(db.h:2988), `DB_SYSTEM_MEM 0x80000`(db.h:3054),
  `DB_REGISTER 0x40000`(db.h:2999), `DB_LOCKDOWN 0x4000`, `DB_FAILCHK 0x10`, `DB_THREAD 0x20`.
(Typical transactional env open:
 `DB_CREATE|DB_INIT_MPOOL|DB_INIT_LOCK|DB_INIT_LOG|DB_INIT_TXN`.)

Cursor get() operation codes (db.in:1335-1371; op VALUES (first opcode DB_AFTER=1 at db.in:1335) masked by
`DB_OPFLAGS_MASK 0x000000ff` db.in:1368, so modifiers OR in above the mask):
`DB_FIRST 7, DB_LAST 15, DB_NEXT 16, DB_PREV 23, DB_NEXT_DUP 17, DB_NEXT_NODUP 18,
DB_PREV_DUP 24, DB_PREV_NODUP 25, DB_CURRENT 6, DB_SET 26, DB_SET_RANGE 27,
DB_SET_RECNO 28, DB_GET_RECNO 11, DB_GET_BOTH 8, DB_GET_BOTH_RANGE 10,
DB_JOIN_ITEM 12, DB_CONSUME 4, DB_CONSUME_WAIT 5`. Modifier bit OR'd in:
`DB_RMW 0x2000`(db.h), `DB_MULTIPLE 0x800`, `DB_MULTIPLE_KEY 0x4000`.

put() flags/opcodes (db.in:1335-1369): `DB_NOOVERWRITE 20, DB_APPEND 2,
DB_NODUPDATA 19, DB_OVERWRITE_DUP 21, DB_KEYFIRST 13, DB_KEYLAST 14, DB_AFTER 1,
DB_BEFORE 3, DB_CURRENT 6` (AFTER/BEFORE/KEYFIRST/KEYLAST/CURRENT are cursor-put only).

===============================================================================
## 5. Callbacks the app supplies  (they re-enter the host language)
===============================================================================

Each installs a C function pointer on a handle; when DB fires it, the binding
routes back into the host language. For each: how installed + exact C signature.
(The `self`-recovery trick is in §cxx/§java.)

- bt_compare (btree key order):
  `db->set_bt_compare(DB *, int (*)(DB *, const DBT *, const DBT *))`   (db.in:1724)
- dup_compare (duplicate order):
  `db->set_dup_compare(DB *, int (*)(DB *, const DBT *, const DBT *))`  (db.in:1734)
  (also public FIELD `int (*dup_compare)(DB *, const DBT *, const DBT *)` db.in:1449)
- h_hash:  `db->set_h_hash(DB *, u_int32_t (*)(DB *, const void *, u_int32_t))` (db.in:1746)
- h_compare: `db->set_h_compare(DB *, int (*)(DB *, const DBT *, const DBT *))` (db.in:1743)
- bt_prefix: `db->set_bt_prefix(DB *, size_t (*)(DB *, const DBT *, const DBT *))` (db.in:1730)
- bt_compress/decompress: `db->set_bt_compress(DB *, int (*)(...6 DBTs...), int (*)(...))` (db.in:1726)
- append_recno: `db->set_append_recno(DB *, int (*)(DB *, DBT *, db_recno_t))` (db.in:1723)
  (also public FIELD `int (*db_append_recno)(DB *, DBT *, db_recno_t)` db.in:1447)
- secondary-index key extractor (DB->associate), the callback type is
  `int (*)(DB *secondary, const DBT *key, const DBT *data, DBT *result)` passed to
  `db->associate(...)` (db.in:1640); return DB_DONOTINDEX to skip indexing a record.
  foreign variant `int (*)(DB *, const DBT *, DBT *, const DBT *, int *changed)`
  via `db->associate_foreign(...)` (db.in:1642).
- db feedback (open/upgrade/verify progress):
  `db->set_feedback(DB *, void (*)(DB *, int opcode, int percent))` (db.in:1741)
  (also public FIELD `void (*db_feedback)(DB *, int, int)` db.in:1448)
- env feedback: `dbenv->set_feedback(DB_ENV *, void (*)(DB_ENV *, int, int))` (db.in:2661)
- recovery dispatch (custom log records):
  `dbenv->set_app_dispatch(DB_ENV *, int (*)(DB_ENV *, DBT *, DB_LSN *, db_recops))` (db.in:2641)
  (`db_recops` enum db.in:802-810: DB_TXN_ABORT=0, APPLY=1, BACKWARD_ROLL=3,
   FORWARD_ROLL=4, PRINT=7; public FIELD `app_dispatch` db.in:2335)
- failchk liveness (needed by dbenv->failchk):
  `dbenv->set_isalive(DB_ENV *, int (*)(DB_ENV *, pid_t, db_threadid_t, u_int32_t))` (db.in:2664)
  `dbenv->set_thread_id(DB_ENV *, void (*)(DB_ENV *, pid_t *, db_threadid_t *))` (db.in:2694)
  `dbenv->set_thread_id_string(DB_ENV *, char *(*)(DB_ENV *, pid_t, db_threadid_t, char *buf))` (db.in:2696)
  (public FIELDs: is_alive db.in:2342, thread_id db.in:2343, thread_id_string db.in:2344;
   buffer length `DB_THREADID_STRLEN 128` db.in:285)
- error/message/panic callbacks: see §3 (set_errcall / set_msgcall / set_paniccall).
- event notification:
  `dbenv->set_event_notify(DB_ENV *, void (*)(DB_ENV *, u_int32_t event, void *info))` (db.in:2659)
  (public FIELD `db_event_func` db.in:2336; event codes DB_EVENT_* db.in:1079-1101:
   PANIC=0, REG_ALIVE=1, REG_PANIC=2, REP_* 3..20, WRITE_FAILED=21,
   NO_SUCH_EVENT=0xffffffff db.in:1101)
- allocator override (who frees MALLOC/REALLOC DBTs):
  `db->set_alloc(DB *, void*(*)(size_t), void*(*)(void*,size_t), void(*)(void*))` (db.in:1721)
  `dbenv->set_alloc(DB_ENV *, ...)` (db.in:2639)
- replication transport: `dbenv->rep_set_transport(DB_ENV *, int,
  int (*)(DB_ENV *, const DBT *, const DBT *, const DB_LSN *, int, u_int32_t))` (db.in:2619)
NOTE: DB_DBT_USERCOPY's per-env copy callback is NOT a public setter — there is no
`set_usercopy` in db.h; only an internal `__dbt_usercopy` symbol exists (confirmed by
grep of db.h and `nm -D`). Treat USERCOPY as advanced / omit for a v1 binding.

===============================================================================
## 6. Config setters that matter for a getting-started binding (grouped; not exhaustive)
===============================================================================

Memory pool: `dbenv->set_cachesize(DB_ENV *, u_int32_t gbytes, u_int32_t bytes, int ncache)`
(db.in:2644); set_cache_max, set_mp_mmapsize, set_mp_pagesize, set_mp_max_openfd,
set_mp_tablesize, set_mp_mtxcount (within db.in:2644-2690). Per-DB:
`db->set_pagesize(DB *, u_int32_t)` (db.in:1755), `db->set_cachesize` (db.in:1732),
`db->set_priority` (db.in:1760).

Locking: `dbenv->set_lk_detect(DB_ENV *, u_int32_t)` (db.in:2672) with modes
DB_LOCK_DEFAULT=1, EXPIRE=2, MAXLOCKS=3, MAXWRITE=4, MINLOCKS=5, MINWRITE=6,
OLDEST=7, RANDOM=8, YOUNGEST=9 (db.in:299-308); set_lk_max_locks(db.in:2674),
set_lk_max_lockers(db.in:2673), set_lk_max_objects(db.in:2675),
set_lk_partitions(db.in:2676), set_lk_conflicts, and
`set_timeout(..., DB_SET_LOCK_TIMEOUT)` (db.in:2698).

Logging: set_lg_bsize(db.in:2666), set_lg_max(db.in:2669), set_lg_dir(db.in:2667),
set_lg_regionmax(db.in:2670), set_lg_filemode(db.in:2668),
`dbenv->log_set_config(DB_ENV *, u_int32_t, int)` (db.in:2570) with DB_LOG_* flags
(DB_LOG_AUTO_REMOVE, DB_LOG_IN_MEMORY, DB_LOG_DSYNC, DB_LOG_ZERO — db.h).

Transactions: `dbenv->set_tx_max(DB_ENV *, u_int32_t)` (db.in:2700), set_tx_timestamp,
`set_timeout(..., DB_SET_TXN_TIMEOUT)` (db.in:2698).

Mutexes: mutex_set_align(db.in:2591), mutex_set_increment(db.in:2592),
mutex_set_init(db.in:2593), mutex_set_max(db.in:2594), mutex_set_tas_spins(db.in:2595).

Flags on/off: `dbenv->set_flags(DB_ENV *, u_int32_t flags, int onoff)` (db.in:2662)
toggling behavior bits (DB_AUTO_COMMIT, DB_TXN_NOSYNC, DB_TXN_WRITE_NOSYNC,
DB_MULTIVERSION, DB_DIRECT_DB, DB_NOMMAP, DB_REGION_INIT, DB_TXN_SNAPSHOT, ...).
Per-DB: `db->set_flags(DB *, u_int32_t)` (db.in:1742).
Encryption: `dbenv->set_encrypt(DB_ENV *, const char *, u_int32_t)` (db.in:2654),
`db->set_encrypt` (db.in:1736).

===============================================================================
## 7. Transaction + cursor lifecycle rules a safe binding MUST enforce
===============================================================================

Authoritative: docs_src/guides/programmer_reference/program_scope.md and
docs_src/api/c/*.md. Quoting:

- Free-threaded only with DB_THREAD. DB_ENV: "free-threaded if the DB_THREAD flag
  is specified to the DB_ENV->open() method"; DB: "free-threaded if DB_THREAD is
  specified to DB->open() ... or if the environment ... is free-threaded"
  (program_scope.md). WITHOUT DB_THREAD a handle may not be shared across threads.
  => A Send/Sync Rust binding must require DB_THREAD on the handle; even then
  DB_TXN/DBC/DB_LOGC/DB_MPOOLFILE are NOT free-threaded (next point).
- DB_TXN "is not free-threaded. Transactions may span threads, but only serially,
  that is, the application must serialize access to the TXN handles."
  (program_scope.md). Each of DBC/DB_LOGC/DB_MPOOLFILE is likewise "not
  free-threaded" (program_scope.md). => make these !Sync (or guard so one thread
  at a time).
- Cursor must be closed before its txn resolves: "All cursors opened within the
  transaction must be closed before the transaction is committed. If they are not
  closed, they will be closed by this function..." (docs_src/api/c/txncommit.md:19);
  "If the cursor is to be used to perform operations on behalf of a transaction,
  the cursor must be opened and closed within the context of that single
  transaction." (program_scope.md). => tie a cursor's lifetime to its transaction
  (Rust: borrow the txn; Zig: document + assert).
- Handle-dead-after-close, every handle: after close/commit/abort/remove/rename
  "the handle may not be accessed again, regardless of the method's return"
  (program_scope.md per handle; dbcclose.md:19; txncommit.md:21). => consume self on
  close (Rust) / null the pointer (Zig).
- A DB opened inside a txn: pass the txn to `db->open`; parent txn may issue no ops
  while a child txn is live except txn_begin/abort/commit (program_scope.md).
- Ownership ordering: DB_ENV must outlive every DB/TXN; DB must outlive its cursors
  and the txns that operate on it (program_scope.md). So:
  DB_ENV ⊃ DB ⊃ {DBC, txn-scoped ops}; DB_ENV ⊃ DB_TXN.

===============================================================================
## 8. ABI facts a binding's build needs
===============================================================================

- Soname: `libdb-2026.0.so` (verified `readelf -d` SONAME; libtool .la dlname =
  libdb-2026.0.so; static archive libdb-2026.0.a). The autoconf/libtool build names
  the lib `libdb-<MAJOR.MINOR>` = 2026.0 (frozen format level, NOT CALVER).
  The meson build declares `library('db', ...)` (dist/meson.build:682) with no
  explicit soversion, so a meson artifact links as plain `-ldb` (libdb.so). A
  binding's build script should probe for BOTH `db` and `db-2026.0`.
- Installed public header: `db.h` (generated by `custom_target('db.h', ...)`,
  dist/meson.build:350, assembled from src/dbinc/db.in + src/dbinc_auto/
  {api_flags,ext_def,ext_prot}.in). Bindings `#include <db.h>` and link `-ldb`.
- Opaque-struct sizes on LP64, RECORDED and VERIFIED by running the in-tree
  test/db/handle_sizes.c against the built lib (ran:
  `LD_LIBRARY_PATH=build_unix/.libs ./build_unix/handle_sizes`):
      DB     = 1744   DBC   =  552   DB_ENV = 2088
      DB_TXN =  336   DBT   =   40   DB_LSN =    8
  => "VERDICT handle_sizes PASS all 6 handle sizes match the recorded values".
- Handles ARE opaque to a binding. test/db/handle_sizes.c:17-30 records the project
  decision ("A1 option (b)"): handle structs are INTERNAL — the library allocates
  them (db_create does `__os_calloc(env,1,sizeof(*dbp))`), db.in has zero by-value
  DB/DBC members in any public struct, "so no application allocates or embeds one
  and the soname tracks only the exported-function ABI." CONFIRMED in db.in:
  struct __db marks "Public: owned by the application" (db.in:1441) vs "Private:
  owned by DB" (db.in:1454); struct __db_env: "The user owns this structure, the
  library owns the ENV structure" (db.in:2284). => treat DB*/DB_ENV*/DBC*/DB_TXN*/
  DB_SEQUENCE*/DB_LOGC*/DB_MPOOLFILE* as OPAQUE POINTERS, call only through method
  pointers, and poke fields ONLY on DBT and the documented public sub-fields:
  DB.app_private (db.in:1451), DB_ENV.app_private (db.in:2364), and the DBT fields.
- Version at compile time: DB_VERSION_MAJOR/MINOR/PATCH + FAMILY/RELEASE macros
  (db.h:45-49), DB_VERSION_STRING (db.h:57), DB_CALVER (db.h:56). At run time:
  `db_version(&maj,&min,&patch)` / `db_full_version(&family,&release,&maj,&min,&patch)`
  (both exported). Compare compiled-against MAJOR/MINOR with runtime db_version() to
  catch a header/lib skew.

===============================================================================
## cxx — the C++ trampoline (how a C callback recovers the language object)
===============================================================================

Mechanism (Rust and Zig need the identical trick): each handle has a `void *` field
reserved for the host-language wrapper; the wrapper stores a pointer to itself there
at creation, and the C callback casts the handle's field back.

Stash field per handle (THE answer to "where to put self"):
- DB          -> `db->api_internal`     (set: cxx_db.cpp:154; recover: db_cxx.h:344-347 `Db::get_Db`)
- DB_ENV      -> `dbenv->api1_internal` (set: cxx_env.cpp:537; recover: db_cxx.h:797 `get_DbEnv`)
                 (`api2_internal` is reserved for Java — see §java)
- DB_TXN      -> `txn->api_internal`    (set: cxx_txn.cpp:58; recover: db_cxx.h:1169-1172)
- DB_SEQUENCE -> `seq->api_internal`    (set: cxx_seq.cpp:49,56; recover: db_cxx.h:1082-1085)
- DBT         -> NO stash; `Dbt` is layout-compatible with `DBT` and reinterpret-cast
                 both ways (`get_Dbt`/`get_DBT`, db_cxx.h:1276-1281). A binding does
                 the same: a Rust `#[repr(C)]` / Zig `extern struct` mirroring DBT.
Field names confirmed in db.in: DB.api_internal db.in:1630; DB_ENV.api1_internal
db.in:2365, api2_internal db.in:2366; DB_TXN.api_internal db.in:920;
DB_SEQUENCE.api_internal db.in:1257.

The intercept macro is the whole pattern in miniature (cxx_db.cpp:426-439):
```
extern "C" _rettype _db_<name>_intercept_c (DB *cthis, ...args...) {
    Db *cxxthis = Db::get_Db(cthis);          /* = (Db*)cthis->api_internal */
    return (*cxxthis-><name>_callback_)(cxxthis, ...wrapped args...);
}
```
`set_*` stores the host callback on the wrapper and installs the C intercept only
when the host callback is non-NULL (DB_SET_CALLBACK, cxx_db.cpp:441-448). The
associate callback is special: the C++ wrapper stores it on the SECONDARY Db (its
first arg is the secondary), cxx_db.cpp:471-476. `alt_close` lets the wrapper's
close run on handle close (cxx_db.cpp:841-849). int->exception funnels non-zero
returns through `DbEnv::runtime_error(dbenv, caller, err, policy)` (db_cxx.h:650);
policy is per-handle (DB_CXX_NO_EXCEPTIONS). A Rust/Zig binding maps the int to a
Result/error-union instead of throwing.

===============================================================================
## java — the JNI/SWIG trampoline (same field, different payload)
===============================================================================

Same stash fields, payload is a JNI global ref (jobject), not a C++ `this`:
- `#define DB_INTERNAL(db)        ((db)->api_internal)`       (java_util.i:68; db_java_wrap.c:206)
- `#define DB_ENV_INTERNAL(dbenv) ((dbenv)->api2_internal)`   (java_util.i:67; db_java_wrap.c:205)
So C++ uses `dbenv->api1_internal` and Java uses `dbenv->api2_internal` — they
coexist on one DB_ENV. On a DB handle BOTH use `api_internal`, so a given DB can be
wrapped by only one of them. A Rust and a Zig binding should likewise claim
`api_internal` for DB and pick ONE of api1/api2_internal for DB_ENV (and document it).
The compare trampoline: `__dbj_am_compare(DB *db, const DBT *d1, const DBT *d2,
jmethodID m)` does `jobject jdb = (jobject)DB_INTERNAL(db);` then
CallNonvirtualIntMethod (java_callbacks.i:593-647); it caches a locked byte[] in
`dbt->app_data` (java_callbacks.i:606,619) — evidence app_data is the DBT's
app-owned slot. Callbacks are declared via the JAVA_CALLBACK macro
(java_callbacks.i:1088+ for associate/append_recno/bt_compare/h_hash/...); DBT is a
value type with Java byte[] <-> data/size typemaps (java_typemaps.i).

===============================================================================
## Binding-design implications (what most shapes an idiomatic binding)
===============================================================================

1. Opaque handles + method pointers. Treat DB*, DB_ENV*, DBC*, DB_TXN*,
   DB_SEQUENCE*, DB_LOGC*, DB_MPOOLFILE* as opaque pointers; never define the
   struct bodies, never read fields except on DBT. Call everything through
   `(*handle->method)(handle, ...)`. The library allocates/frees the structs
   (handle_sizes.c:17-30 "A1"), so the ABI you depend on is the method-pointer
   SIGNATURES, not struct layout — which is why the recorded sizes can change under
   a stable soname. Wrap each handle in a newtype owning the pointer whose
   Drop/deinit calls close/commit exactly once.

2. DBT memory ownership is the ergonomic crux. Model three regimes: borrowed-in
   (put/key: point at a caller slice for the call only), USERMEM (caller slice +
   ulen, retry on DB_BUFFER_SMALL by growing), MALLOC/REALLOC (DB allocates; free
   with the set_alloc allocator). Default get returns a pointer into DB-owned memory
   valid only until the next op — copy out eagerly. Expose DB_MULTIPLE_* as a native
   zero-copy iterator. Guard size==0/data==NULL in any memcpy wrapper.

3. The callback trampoline field. Stash the host callback object in
   `handle->api_internal` (DB, DB_TXN, DB_SEQUENCE) / `dbenv->api1_internal` or
   `api2_internal` (DB_ENV); install a small `extern "C"` thunk that casts the field
   back and invokes the host callback. DBT needs no stash — mirror it as a
   repr(C)/extern struct and reinterpret-cast. Every §5 callback uses this one trick.

4. The int / DB_* / errno error split. One decoder: 0=>Ok; <0 in -30,800..-30,999
   => typed DB error; >0 => OS errno. Demote the expected negatives (DB_NOTFOUND=>
   Ok(None), DB_KEYEXIST=>put outcome, DB_BUFFER_SMALL=>grow-and-retry,
   DB_LOCK_DEADLOCK=>retry signal) so the API surfaces only genuine failures as
   errors; wire set_errcall/set_msgcall to capture DB's diagnostic strings.
