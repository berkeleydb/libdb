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
set $nlget = 0
set $nlockget = 0
# Arm the counters only across the single measured read.
break rdl_mark_begin
commands
  silent
  printf "GDB armed\n"
  break __db_lget
  commands
    silent
    set $nlget = $nlget + 1
    printf "LGET #%d pgno=%u mode=%d action=%d\n", $nlget, pgno, mode, action
    # Prove the locked page is the LEAF: in the __bam_search frame, `level` is
    # the level of the page we are descending FROM, and the gate at
    # bt_search.c:942 is `level - 1 == LEAFLEVEL`, so level must be 2 here
    # (LEAFLEVEL is 1).  This is the mechanical form of the claim; page numbers
    # alone cannot distinguish interior from leaf.
    up
    printf "LGETFRAME level=%d pg=%u getlock=%d lock_mode=%d slevel=%d\n", level, pg, getlock, lock_mode, slevel
    down
    continue
  end
  break __lock_get_internal
  commands
    silent
    set $nlockget = $nlockget + 1
    printf "LOCKGETINT #%d mode=%d\n", $nlockget, lock_mode
    continue
  end
  # Show the page number the descent settled on, and its level/type.
  break rdl_mark_end
  commands
    silent
    printf "COUNTS lget=%d lock_get_internal=%d\n", $nlget, $nlockget
    continue
  end
  continue
end
run -h /tmp/rdl-where/env ARGPLACEHOLDER
printf "FINAL lget=%d lock_get_internal=%d\n", $nlget, $nlockget
quit
EOF
sed -i "s|ARGPLACEHOLDER|$ARG|" /tmp/rdl_where.gdb

echo "=== MODE=$MODE ==="
timeout 900 gdb -q -batch -x /tmp/rdl_where.gdb $OUT 2>&1 |
    grep -E "RDLW|LGET|LGETFRAME|LOCKGETINT|COUNTS|FINAL|armed|FAIL|Error" |
    sed -n '1,80p'
