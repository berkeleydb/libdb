#!/bin/bash
# pinrm_setup.sh -- create the five worktrees (baseline + 4 branches) on the box
# and build each into its own build dir.  Idempotent: skips existing builds.
set -u
R=/home/admin/libdb
cd "$R" || exit 1

# Import the branch commits from the bundle (fetch, not clone, so no rm needed).
git fetch /home/admin/pinrm.bundle \
  'refs/heads/rm/bhpin-r1:refs/heads/rm/bhpin-r1' \
  'refs/heads/rm/rsnap-ml-r:refs/heads/rm/rsnap-ml-r' \
  'refs/heads/rm/lock-readpath:refs/heads/rm/lock-readpath' \
  'refs/heads/rm/mpool-pin-port:refs/heads/rm/mpool-pin-port' 2>&1 | tail -6

mk() {  # mk <name> <ref>
  local n=$1
  local ref=$2
  local wt=/home/admin/wt-$n
  if [ ! -d "$wt" ]; then
    git worktree add --detach "$wt" "$ref" >/dev/null 2>&1 || {
      echo "WORKTREE-FAIL $n"; return 1; }
  fi
  ( cd "$wt" && git checkout -f -- build_windows/ 2>/dev/null; true )
  echo "WORKTREE-OK $n $(cd $wt && git log --oneline -1)"
}

mk base    v2026.09.6
mk bhpin   rm/bhpin-r1
mk rsnap   rm/rsnap-ml-r
mk lockrp  rm/lock-readpath
mk mpoolp  rm/mpool-pin-port

build() {  # build <name> [extra CFLAGS]
  local n=$1; shift
  local wt=/home/admin/wt-$n
  local b=/home/admin/wt-$n/build_unix
  if [ -f "$b/.libs/libdb-2026.0.so" ]; then
    echo "BUILD-CACHED $n"; return 0
  fi
  mkdir -p "$b"
  ( cd "$b" && ../dist/configure --enable-o_direct LIBS=-luring \
      CFLAGS="-O2 -g -fno-omit-frame-pointer $*" >conf.log 2>&1 \
    && make -j96 >make.log 2>&1 ) || { echo "BUILD-FAIL $n (see $b/make.log)"; return 1; }
  [ -f "$b/.libs/libdb-2026.0.so" ] || { echo "BUILD-NOLIB $n"; return 1; }
  echo "BUILD-OK $n"
}

build base
build bhpin
build rsnap
build lockrp
build mpoolp -DMPOOL_HOTFIELDS_ISOLATED=1
echo SETUP-DONE
