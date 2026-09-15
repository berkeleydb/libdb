# Build the benchmark drivers against an Autoconf build tree:
#   make BDB=../../build_unix
#
# The TPROC-* drivers are HammerDB-style workloads (not the TPC benchmarks);
# each shares bdb_bench.h for the safety-feature toggle framework.
BDB ?= ../../build_unix
CFLAGS ?= -O2 -pthread
INCLUDES = -I$(BDB)
# -rpath is not optional: without it the loader silently picks up a
# system-installed libdb-5.3 (e.g. /lib64/libdb-5.3.so) and every measurement
# describes that library instead of this build tree.
LIBS = -L$(BDB)/.libs -ldb-5.3 -Wl,-rpath,$(abspath $(BDB)/.libs)

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
# commit-latency percentiles; fsync_probe measures the bare device fsync
# latency, which is the denominator commit_bench's flush-round rate is
# compared against.
commit_bench: commit_bench.c
	$(CC) $(CFLAGS) $(INCLUDES) commit_bench.c $(LIBS) -o $@

fsync_probe: fsync_probe.c
	$(CC) $(CFLAGS) fsync_probe.c -o $@

ssi_abort_bench: ssi_abort_bench.c
	$(CC) $(CFLAGS) $(INCLUDES) ssi_abort_bench.c $(LIBS) -o $@

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
