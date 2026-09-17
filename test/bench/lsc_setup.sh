#!/bin/sh
# lsc_setup.sh -- OS tuning + driver build for the btree lock-scope study.
set -e
WT=$HOME/wt-lockscope

# OS tuning (idempotent).
sudo sh -c 'echo never > /sys/kernel/mm/transparent_hugepage/enabled' 2>/dev/null || true
sudo sh -c 'echo 0 > /proc/sys/kernel/randomize_va_space' 2>/dev/null || true
for c in /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor; do
	[ -w "$c" ] && sudo sh -c "echo performance > $c" 2>/dev/null || true
done

cd $WT/test/bench
make BDB=../../build_unix commit_bench fsync_probe 2>&1 | tail -3
ls -la commit_bench fsync_probe
echo SETUP_OK
