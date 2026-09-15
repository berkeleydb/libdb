/*
 * fsync_probe -- raw fdatasync/fsync latency on this filesystem, and the
 * maximum achievable serial fsync rate, so we can tell how much of libdb's
 * per-round cost is device time vs software handoff.
 */
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

static double
now(void)
{
	struct timespec t;
	(void)clock_gettime(CLOCK_MONOTONIC, &t);
	return (t.tv_sec + t.tv_nsec / 1e9);
}

static int
cmpd(const void *a, const void *b)
{
	double x = *(const double *)a, y = *(const double *)b;
	return (x < y ? -1 : x > y ? 1 : 0);
}

int
main(int argc, char **argv)
{
	char buf[4096];
	double *lat, t0, t1;
	int fd, i, n;
	const char *path;

	path = argc > 1 ? argv[1] : "./fsync_probe.dat";
	n = argc > 2 ? atoi(argv[2]) : 300;
	memset(buf, 'z', sizeof(buf));
	if ((fd = open(path, O_RDWR | O_CREAT | O_TRUNC, 0644)) < 0) {
		perror("open");
		return (1);
	}
	/* Preallocate so we are not measuring metadata growth. */
	if (ftruncate(fd, (off_t)n * (off_t)sizeof(buf)) != 0) {
		perror("ftruncate");
		return (1);
	}
	(void)fsync(fd);
	if ((lat = malloc((size_t)n * sizeof(double))) == NULL)
		return (1);

	for (i = 0; i < n; i++) {
		if (write(fd, buf, sizeof(buf)) != (ssize_t)sizeof(buf)) {
			perror("write");
			return (1);
		}
		t0 = now();
		if (fdatasync(fd) != 0) {
			perror("fdatasync");
			return (1);
		}
		t1 = now();
		lat[i] = (t1 - t0) * 1e6;
	}
	qsort(lat, (size_t)n, sizeof(double), cmpd);
	printf("fdatasync n=%d p50_us=%.1f p90_us=%.1f p99_us=%.1f "
	    "min_us=%.1f max_us=%.1f -> max_serial_rate=%.0f/s\n",
	    n, lat[n / 2], lat[n * 90 / 100], lat[n * 99 / 100],
	    lat[0], lat[n - 1], 1e6 / lat[n / 2]);

	/* Same again with full fsync (what libdb actually calls). */
	for (i = 0; i < n; i++) {
		if (write(fd, buf, sizeof(buf)) != (ssize_t)sizeof(buf)) {
			perror("write");
			return (1);
		}
		t0 = now();
		if (fsync(fd) != 0) {
			perror("fsync");
			return (1);
		}
		t1 = now();
		lat[i] = (t1 - t0) * 1e6;
	}
	qsort(lat, (size_t)n, sizeof(double), cmpd);
	printf("fsync     n=%d p50_us=%.1f p90_us=%.1f p99_us=%.1f "
	    "min_us=%.1f max_us=%.1f -> max_serial_rate=%.0f/s\n",
	    n, lat[n / 2], lat[n * 90 / 100], lat[n * 99 / 100],
	    lat[0], lat[n - 1], 1e6 / lat[n / 2]);
	(void)close(fd);
	(void)unlink(path);
	return (0);
}
