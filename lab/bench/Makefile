# Build the benchmark drivers against an Autoconf build tree:
#   make BDB=../../build_unix
#
# The TPROC-* drivers are HammerDB-style workloads (not the TPC benchmarks);
# each shares bdb_bench.h for the safety-feature toggle framework.
BDB ?= ../../build_unix
CFLAGS ?= -O2 -pthread
INCLUDES = -I$(BDB)
LIBS = -L$(BDB)/.libs -ldb-5.3

BENCHES = scale_bench tproc_c tproc_b tproc_h

all: $(BENCHES)

scale_bench: scale_bench.c
	$(CC) $(CFLAGS) $(INCLUDES) scale_bench.c $(LIBS) -o $@

tproc_c: tproc_c.c bdb_bench.h
	$(CC) $(CFLAGS) $(INCLUDES) tproc_c.c $(LIBS) -o $@

tproc_b: tproc_b.c bdb_bench.h
	$(CC) $(CFLAGS) $(INCLUDES) tproc_b.c $(LIBS) -o $@

tproc_h: tproc_h.c bdb_bench.h
	$(CC) $(CFLAGS) $(INCLUDES) tproc_h.c $(LIBS) -o $@

clean:; rm -f $(BENCHES)

.PHONY: all clean
