#!/bin/sh
# lsc_attrib.sh -- attribute the convoy: per-page waiter counts across samples,
# split rate from the log, and btree shape.
set -e
S=$HOME/wt-lockscope/build_unix
D=$(cat /tmp/lsc-repro/envdir)
O=/tmp/lsc-repro

echo "=== waiters per page, all 5 samples (page, n_wait, n_held)"
for i in 1 2 3 4 5; do
	awk -v s=$i '
	    /page +[0-9]+$/ {
		pg=$NF; st=$4
		if (st=="WAIT") w[pg]++; else if (st=="HELD") h[pg]++
	    }
	    END {
		for (p in w) printf "s%s wait pg=%s n=%d\n", s, p, w[p]
		tw=0; for (p in w) tw+=w[p]
		th=0; for (p in h) th+=h[p]
		printf "s%s TOTAL wait=%d held=%d distinct_wait_pages=%d\n", s, tw, th, length(w)
	    }' $O/objects.$i.txt | sort -t= -k3 -rn | head -6
done

echo
echo "=== btree shape after the run"
$S/db_stat -h $D -d bench.db 2>&1 | head -30

echo
echo "=== log record census (split rate)"
$S/db_printlog -h $D 2>/dev/null | grep -oE '^\[[0-9]+\]\[[0-9]+\]__[a-z_0-9]+' \
    | sed 's/.*\]//' | sort | uniq -c | sort -rn | head -20

echo
echo "=== lock stats (final)"
grep -E 'conflicts|requested|released|deadlock|Maximum number of locks|lockers|objects|st_' $O/lockstat.final.txt | head -25
echo ATTRIB_DONE
