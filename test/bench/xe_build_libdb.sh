#!/bin/sh
#
# xe_build_libdb.sh -- configure + build libdb for the cross-engine TPROC
# benchmark, and PROVE which library came out.
#
# The "prove which library you linked" requirement exists because a hardcoded
# -ldb-5.3 once resolved to Debian's 2013 Berkeley DB behind a benchmark's
# back.  test/bench/Makefile now globs the built .so; this script additionally
# records db_version() from the freshly built tree.
set -e

SRC=${1:-/nvme/libdb}
BUILD="$SRC/build_unix"

cd "$BUILD"
if [ ! -f db.h ]; then
	../dist/configure \
		--enable-o_direct \
		--prefix="$SRC/install" \
		CFLAGS="-O2 -g -fno-omit-frame-pointer" >configure.log 2>&1 \
	|| { tail -40 configure.log; exit 1; }
fi
make -j "$(nproc)" >build.log 2>&1 || { tail -60 build.log; exit 1; }

echo "## io_uring configured in?"
grep -E 'HAVE_IO_URING|HAVE_O_DIRECT|HAVE_AIO' db_config.h || true
echo
echo "## built libraries"
ls -l "$BUILD"/.libs/libdb-*.so 2>&1
echo
echo "## db_version() from the built tree"
cat > /tmp/xe_ver.c <<'EOF'
#include <stdio.h>
#include <db.h>
int main(void) {
	int a, b, c;
	printf("%s\n", db_version(&a, &b, &c));
	printf("major=%d minor=%d patch=%d\n", a, b, c);
	printf("DB_VERSION_STRING=%s\n", DB_VERSION_STRING);
	return 0;
}
EOF
SO=$(ls "$BUILD"/.libs/libdb-*.so | head -1)
cc -O0 -I"$BUILD" /tmp/xe_ver.c "$SO" -Wl,-rpath,"$BUILD/.libs" -o /tmp/xe_ver
/tmp/xe_ver
