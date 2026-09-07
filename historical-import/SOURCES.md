# Where the historical tarballs come from

`build_historical.py` rebuilds this orphan branch from a local mirror of the
upstream release tarballs. The mirror is not committed (the tarballs are large
and are not ours to redistribute); `SHA256SUMS` pins exactly what the mirror must
contain, and the script verifies against it.

Set the mirror location with `BDB_MIRROR` (default `~/bdb-mirror`), the working
repository with `LIBDB_HISTORICAL_REPO` (default `~/libdb-historical`), and the
GNU patch binary with `PATCH` if `gpatch` is not on `PATH`.

## Primary source

Most releases came from Oracle's Berkeley DB download archive. Oracle has
withdrawn several of the oldest ones, so the pre-2.7 releases are recovered from
the Debian source archive, which mirrored them at the time.

## Recovered from the Debian archive

These two were requested in
[issue #135](https://github.com/berkeleydb/libdb/issues/135) and are the only
2.x releases besides 2.7.7 that we have provenance for:

| Version | Source URL | SHA-256 (of the fetched file) |
|---|---|---|
| 2.3.16 | <https://archive.debian.org/debian/dists/hamm/hamm/source/libs/db_2.3.16.orig.tar.gz> | `9716681cd849df0b91951b64cfd0886b7921c081f97515c648bc23edff78f122` |
| 2.4.14 | <https://archive.debian.org/debian/dists/slink/main/source/libs/db_2.4.14.orig.tar.gz> | `3fdb686fbce712ab905b8a4dd404635891a5187ea838910a4efbe609fa563beb` |

Debian names these `db_X.Y.Z.orig.tar.gz`; rename to `db-X.Y.Z.tar.gz` in the
mirror so they match `SHA256SUMS` and the `tar` key in `build_historical.py`.
Both extract to a `db-X.Y.Z/` root, like the other 2.x/3.x tarballs.

Verified contents:

```
db-2.3.16/include/db.h:  DB_VERSION_STRING "Sleepycat Software: DB 2.3.16 ..."
db-2.4.14/include/db.h:  DB_VERSION_STRING "Sleepycat Software: DB 2.4.14 ..."
```

Neither tarball carries a release date the script can parse out of `README`, so
both use an explicit `date` in `RELEASES`, taken from the newest file mtime
inside the tarball: **1998-01-22** for 2.3.16 and **1998-06-02** for 2.4.14.
Those order correctly between 1.86 (1996) and 2.7.7.

## Fetching

```sh
mkdir -p "${BDB_MIRROR:-$HOME/bdb-mirror}"
cd "${BDB_MIRROR:-$HOME/bdb-mirror}"
curl -sSLo db-2.3.16.tar.gz \
  https://archive.debian.org/debian/dists/hamm/hamm/source/libs/db_2.3.16.orig.tar.gz
curl -sSLo db-2.4.14.tar.gz \
  https://archive.debian.org/debian/dists/slink/main/source/libs/db_2.4.14.orig.tar.gz
sha256sum -c /path/to/historical-import/SHA256SUMS --ignore-missing
```

## Rebuilding

`build_historical.py` is idempotent: it deletes the `historical` branch and every
tag it manages, then replays the whole timeline. `master` and the `v5.3.x` release
tags are never touched. Because it rewrites the branch, **the maintainer runs it
and force-pushes**; adding a release here is only the first half of the change.
