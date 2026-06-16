# Build against an Autoconf build tree: make BDB=../../build_unix
BDB ?= ../../build_unix
CFLAGS ?= -O2 -pthread
scale_bench: scale_bench.c
	$(CC) $(CFLAGS) scale_bench.c -I$(BDB) -L$(BDB)/.libs -ldb-5.3 -o $@
clean:; rm -f scale_bench
