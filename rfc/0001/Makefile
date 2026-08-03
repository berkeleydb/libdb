# Standalone build for the adaptive-controller prototype (no BDB deps).
CFLAGS ?= -O2 -Wall -Wextra -std=c99

test_adaptive: adaptive.c test_adaptive.c adaptive.h
	$(CC) $(CFLAGS) adaptive.c test_adaptive.c -o $@

.PHONY: check clean
check: test_adaptive
	./test_adaptive

clean:
	rm -f test_adaptive
