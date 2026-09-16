# Build the benchmark drivers against an Autoconf build tree:
#   make BDB=../../build_unix
#
# The TPROC-* drivers are HammerDB-style workloads (not the TPC benchmarks);
# each shares bdb_bench.h for the safety-feature toggle framework.
BDB ?= ../../build_unix
CFLAGS ?= -O2 -pthread
INCLUDES = -I$(BDB)
# Link the build tree's shared object BY PATH, discovered from the tree itself.
#
# This used to read "-L$(BDB)/.libs -ldb-5.3".  That spelled the library's
# version into the harness, and when 2bad51f4f renamed the fork's soname to
# libdb-2026.0.so the name -ldb-5.3 stopped matching anything in the build
# tree -- so it resolved to the *distro* /lib/x86_64-linux-gnu/libdb-5.3.so
# (Berkeley DB 5.3.28, 2013) instead, silently, with -rpath powerless because
# the soname recorded in the binary was the system library's.  Every number
# published by commit_bench described Oracle's 2013 code, not this fork.
#
# A wildcard cannot name the wrong library: it can only find what this build
# tree actually produced, and if the tree produced nothing the build fails
# loudly instead of falling back to whatever the system has installed.
# -rpath is still required so the runtime lookup stays inside the tree.
BDB_SO := $(wildcard $(BDB)/.libs/libdb-*.so)
LIBS = $(BDB_SO) -Wl,-rpath,$(abspath $(BDB)/.libs)

ifeq ($(BDB_SO),)
$(error no libdb-*.so in $(BDB)/.libs -- build libdb first (cd $(BDB) && \
  ../dist/configure && make).  Refusing to fall back to a system libdb, \
  which is how the 5.3.28 measurement leak happened)
endif
ifneq ($(words $(BDB_SO)),1)
$(error $(words $(BDB_SO)) candidate libraries in $(BDB)/.libs ($(BDB_SO)) -- \
  ambiguous; clean the build tree so the measured library is unambiguous)
endif

BENCHES = scale_bench scale_iso tproc_c tproc_b tproc_h lock_bench ssi_abort_bench \
	  commit_bench fsync_probe

all: $(BENCHES)

scale_bench: scale_bench.c
	$(CC) $(CFLAGS) $(INCLUDES) scale_bench.c $(LIBS) -o $@

scale_iso: scale_iso.c
	$(CC) $(CFLAGS) $(INCLUDES) scale_iso.c $(LIBS) -o $@

lock_bench: lock_bench.c
	$(CC) $(CFLAGS) $(INCLUDES) lock_bench.c $(LIBS) -o $@

# commit_bench measures durable-commit throughput, fsyncs-per-commit and
# per-phase latency percentiles.  Built against a library configured
# --enable-handoff-trace it also dumps the group-commit handoff histograms; the
# hook is a weak symbol, so the same source serves both builds.
#
# fsync_probe measures the bare device fsync latency.  NOTE that this is not a
# term to subtract from commit_bench's per-round time: the probe appends to a
# growing file while the log flush usually syncs an already-sized one, so they
# are different operations.  Use the in-path fsync_us histogram from
# --enable-handoff-trace for that comparison instead.
commit_bench: commit_bench.c
	$(CC) $(CFLAGS) $(INCLUDES) commit_bench.c $(LIBS) -o $@

fsync_probe: fsync_probe.c
	$(CC) $(CFLAGS) fsync_probe.c -o $@

# -lm: the Zipfian generator uses pow().  glibc's libm is not linked by default
# on GCC/Linux, and the undefined `pow` broke the harness-smoke job.
ssi_abort_bench: ssi_abort_bench.c
	$(CC) $(CFLAGS) $(INCLUDES) ssi_abort_bench.c $(LIBS) -lm -o $@

tproc_c: tproc_c.c bdb_bench.h
	$(CC) $(CFLAGS) $(INCLUDES) tproc_c.c $(LIBS) -o $@

tproc_b: tproc_b.c bdb_bench.h
	$(CC) $(CFLAGS) $(INCLUDES) tproc_b.c $(LIBS) -o $@

tproc_h: tproc_h.c bdb_bench.h
	$(CC) $(CFLAGS) $(INCLUDES) tproc_h.c $(LIBS) -o $@

clean:; rm -f $(BENCHES)

# Reproducible regression harness (see README.md): measure, then compare.
baseline: all
	./run_bench.sh

.PHONY: baseline

.PHONY: all clean
