#!/bin/sh
# rdl_run_where.sh -- run rdl_where under gdb, counting __db_lget / __lock_get
# calls across exactly ONE point read, and dumping which page ends up locked.
#
# The stat counter in rdl_count.c counts lock REQUESTS; this distinguishes
# "__db_lget never called for interior pages" from "called and short-circuited".
set -e
W=/home/admin/rdl-wt
B=$W/bu
S=/tmp/rdl-where
OUT=/tmp/rdl_where.bin
MODE=${1:-plain}
case $MODE in
plain) ARG= ;;
snapshot) ARG=-m ;;
serializable) ARG=-s ;;
*) echo "usage: $0 plain|snapshot|serializable"; exit 2 ;;
esac
rm -f $OUT
gcc -O0 -g -o $OUT $W/test/bench/rdl_where.c -I$B -L$B -ldb -lpthread -luring
test -x $OUT || { echo "COMPILE FAILED"; exit 1; }
mkdir -p $S
find $S -mindepth 1 -delete
mkdir -p $S/env

cat >/tmp/rdl_where.gdb <<'EOF'
set pagination off
set confirm off
set breakpoint pending on
# Arm the counters only across the single measured read.
break rdl_where.c:97
commands
  silent
  printf "GDB armed\n"
  break __db_lget
  commands
    silent
    printf "LGET pgno=%u mode=%d action=%d\n", pgno, mode, action
    continue
  end
  break __lock_get
  commands
    silent
    printf "LOCKGET mode=%d\n", mode
    continue
  end
  continue
end
run -h /tmp/rdl-where/env ARGPLACEHOLDER
quit
EOF
sed -i "s|ARGPLACEHOLDER|$ARG|" /tmp/rdl_where.gdb

echo "=== MODE=$MODE ==="
timeout 900 gdb -q -batch -x /tmp/rdl_where.gdb $OUT 2>&1 |
    grep -E "RDLW|LGET|LOCKGET|GDB armed|FAIL|Locker|READ|WRITE|page" |
    sed -n '1,80p'
