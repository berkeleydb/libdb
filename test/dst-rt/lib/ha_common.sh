# Copyright (c) 2026 libdb contributors.  All rights reserved.
# SPDX-License-Identifier: Sleepycat
#
# ha_common.sh -- shared machinery for the RFC 0024 HA scenarios (S4-S8).
#
# Sourced by scenarios/s4_repmgr_netfaults.sh etc.  It layers on fi_env.sh
# (tool discovery, fi_build_node, fi_skip) and fi_net.sh (netns topology), and
# adds: build the node + oracle; bring up an N-site repmgr group, one rtnode
# per netns; drive the ctl FIFOs; and the HA oracle (rules 4-5: convergence and
# single-master-per-generation).  Rules 1-3/7 are rtcheck's job.
#
# A scenario does:
#   . "$(dirname "$0")/../lib/ha_common.sh"
#   ha_need_netns                 # SKIP cleanly if no netns/sudo
#   ha_group_up 3 "quorum" ""     # nsites, ack policy, extra rtnode args
#   ha_load a 200 $SEED           # run load on the current master
#   net_partition "a b|c"; ...    # faults via fi_net.sh
#   ha_oracle                     # rules 4-5 + rtcheck rules 1-3/7
#   ha_group_down

set -u

HA_DIR=$(CDPATH= cd -- "$(dirname -- "${BASH_SOURCE[0]:-$0}")" && pwd)
. "$HA_DIR/fi_env.sh"
. "$HA_DIR/fi_net.sh"

HA_PORT_BASE=${HA_PORT_BASE:-7000}
HA_NODES=""            # node names, set by ha_group_up
HA_RUN=""              # run dir (ctl/ev/ack live here)
HA_ACKPOL="quorum"
HA_KILLED=""           # names of nodes we killed (for the oracle)

# SKIP the whole scenario unless netns is usable.
ha_need_netns() {
	fi_have libdb || fi_skip "no libdb library under BUILD=$BUILD"
	fi_have netns  || fi_skip "no network-namespace permission (need root or passwordless sudo)"
	fi_have tc     || fi_skip "no tc (iproute2) for netem"
}

# Build rtnode + rtcheck once into $BUILD.
ha_build() {
	[ -x "$BUILD/rtnode" ]  || fi_build_node rtnode  || fi_skip "cannot build rtnode"
	[ -x "$BUILD/rtcheck" ] || fi_build_node rtcheck || fi_skip "cannot build rtcheck"
}

ha_send() { echo "$2" > "$HA_RUN/ctl.$1"; }

# Return the node currently reporting master (last ROLE master wins), or empty.
ha_master() {
	local n last=""
	for n in $HA_NODES; do
		grep -q 'ROLE master' "$HA_RUN/ev.$n.log" 2>/dev/null && last=$n
	done
	echo "$last"
}

# Wait up to $1 seconds for some node to be master and all live nodes READY.
ha_wait_master() {
	local deadline=$(( $(date +%s) + ${1:-30} )) m
	while [ "$(date +%s)" -lt "$deadline" ]; do
		m=$(ha_master)
		if [ -n "$m" ]; then echo "$m"; return 0; fi
		sleep 1
	done
	echo ""
	return 1
}

# ha_group_up NSITES ACKPOL "extra rtnode args"
# Node a is the group creator and highest priority (preferred master).
ha_group_up() {
	local nsites="$1"; HA_ACKPOL="${2:-quorum}"; local extra="${3:-}"
	local names="" i c
	for i in $(seq 1 "$nsites"); do
		c=$(printf "\\$(printf '%03o' $((96 + i)))"); names="$names $c"
	done
	HA_NODES=$(echo $names)

	HA_RUN=$(fi_mkrun "s-${RT_RUN:-run}")
	export LD_LIBRARY_PATH="${LD_LIBRARY_PATH:-}:$(cd "$BUILD/.libs" 2>/dev/null && pwd)"

	net_up "$nsites" $HA_NODES

	local node ip home port peers first=1
	for node in $HA_NODES; do
		ip=$(net_ip "$node"); port=$HA_PORT_BASE
		home="$HA_RUN/$node"; mkdir -p "$home"
		mkfifo "$HA_RUN/ctl.$node" 2>/dev/null || true
		peers=""
		local other
		for other in $HA_NODES; do
			[ "$other" != "$node" ] && \
			    peers="$peers --peer $(net_ip "$other"):$port"
		done
		local creator="" ackpol="--ack-policy $HA_ACKPOL"
		[ "$first" = 1 ] && creator="--group-creator"
		net_run "$node" env LD_LIBRARY_PATH="$LD_LIBRARY_PATH" \
		    "$BUILD/rtnode" --home "$home" --ctl "$HA_RUN/ctl.$node" \
		    --ev "$HA_RUN/ev.$node.log" --node "$node" \
		    --ack "$HA_RUN/ack.$node.log" \
		    --site "$ip:$port" $peers $creator $ackpol $extra \
		    >"$HA_RUN/out.$node" 2>&1 &
		first=0
	done
	# Give the creator a head start so it owns the group, then the rest.
	sleep 3
	local m; m=$(ha_wait_master 40)
	[ -n "$m" ] || { echo "ha_group_up: no master elected"; return 1; }
	# wait for the others to finish startup sync
	sleep 4
	echo "ha_group_up: master=$m nodes=[$HA_NODES] run=$HA_RUN"
	return 0
}

# Drive a load of N ops with SEED on the current master.
ha_load() {
	local n="$1" seed="$2" m
	m=$(ha_master)
	[ -n "$m" ] || { echo "ha_load: no master"; return 1; }
	ha_send "$m" "load $n $seed"
	# wait for LOADDONE
	local deadline=$(( $(date +%s) + 60 ))
	while [ "$(date +%s)" -lt "$deadline" ]; do
		grep -q "LOADDONE" "$HA_RUN/ev.$m.log" 2>/dev/null && break
		sleep 1
	done
	echo "ha_load: $n ops on $m seed=$seed acks=$(grep -c ' ACK$' "$HA_RUN/ack.$m.log" 2>/dev/null || echo 0)"
}

# Kill a node hard (SIGKILL), recording it for the oracle.
ha_kill() {
	local node="$1" pid
	pid=$(cat "$HA_RUN/pid.$node" 2>/dev/null)
	# rtnode has no pid file; kill by name inside the netns.
	net_run "$node" pkill -9 -f "rtnode .*--node $node" 2>/dev/null || true
	HA_KILLED="$HA_KILLED $node"
	echo "ha_kill: $node"
}

# Restart a previously-killed node (fresh rtnode, DB_RECOVER on open).
ha_restart() {
	local node="$1" ip port home peers other
	ip=$(net_ip "$node"); port=$HA_PORT_BASE; home="$HA_RUN/$node"
	peers=""
	for other in $HA_NODES; do
		[ "$other" != "$node" ] && peers="$peers --peer $(net_ip "$other"):$port"
	done
	net_run "$node" env LD_LIBRARY_PATH="$LD_LIBRARY_PATH" \
	    "$BUILD/rtnode" --home "$home" --ctl "$HA_RUN/ctl.$node" \
	    --ev "$HA_RUN/ev.$node.log" --node "$node" \
	    --ack "$HA_RUN/ack.$node.log" \
	    --site "$ip:$port" $peers --ack-policy "$HA_ACKPOL" \
	    >>"$HA_RUN/out.$node" 2>&1 &
	HA_KILLED=$(echo "$HA_KILLED" | sed "s/\b$node\b//")
	sleep 3
	echo "ha_restart: $node"
}

# quiesce every live node and settle.
ha_quiesce() {
	local n
	for n in $HA_NODES; do
		case " $HA_KILLED " in *" $n "*) continue;; esac
		ha_send "$n" quiesce
	done
	sleep 3
}

# --- HA oracle: rules 4 (convergence) and 5 (single master per gen) --------
# rules 1-3/7 are delegated to rtcheck across all homes.
HA_VIOL=0

# rule 5: at most one node reporting master per election generation.
ha_check_single_master() {
	# collect "gen node" from every "ROLE master gen=N" across ev logs.
	local tmp n; tmp=$(mktemp)
	for n in $HA_NODES; do
		[ -f "$HA_RUN/ev.$n.log" ] || continue
		sed -n 's/^ROLE master gen=\([0-9]*\).*/\1 '"$n"'/p' \
		    "$HA_RUN/ev.$n.log" >> "$tmp"
	done
	# a violation is a generation with >1 distinct master node.
	local bad
	bad=$(awk '{seen[$1]=seen[$1] " " $2} END{for(g in seen){
		n=split(seen[g],a," "); u=0; delete d;
		for(i=1;i<=n;i++){if(!(a[i] in d)){d[a[i]]=1;u++}}
		if(u>1) print g": "seen[g]}}' "$tmp")
	rm -f "$tmp"
	if [ -n "$bad" ]; then
		echo "VIOLATION (rule 5, two masters in a generation):"
		echo "$bad" | sed 's/^/  gen /'
		HA_VIOL=$((HA_VIOL + 1))
	else
		echo "rule 5 (single master per gen): OK"
	fi
}

# rule 4: after heal + quiesce, every LIVE home has the same db_dump and the
# same log LSN.  Convergence is an EVENTUAL property, so we poll up to
# HA_CONVERGE_WAIT seconds (a just-restarted node needs time to catch up);
# killed-but-not-restarted nodes are excluded.
HA_CONVERGE_WAIT=${HA_CONVERGE_WAIT:-45}
ha_check_convergence() {
	local live="" n
	for n in $HA_NODES; do
		case " $HA_KILLED " in *" $n "*) continue;; esac
		live="$live $n"
	done
	local deadline=$(( $(date +%s) + HA_CONVERGE_WAIT ))
	local first ref_dump d mism diffnode
	while :; do
		first=""; ref_dump=""; mism=0; diffnode=""
		for n in $live; do
			d=$(net_run "$n" env LD_LIBRARY_PATH="$LD_LIBRARY_PATH" \
			    "$BUILD/db_dump" -p -h "$HA_RUN/$n" rt.db 2>/dev/null | \
			    grep -v '^VERSION\|^format\|^type\|^db_pagesize\|^HEADER\|^DATA' | sort)
			if [ -z "$first" ]; then
				first=$n; ref_dump="$d"
			elif [ "$d" != "$ref_dump" ]; then
				mism=1; diffnode=$n
			fi
		done
		[ "$mism" = 0 ] && break
		[ "$(date +%s)" -ge "$deadline" ] && break
		sleep 3
	done
	if [ "$mism" = 0 ]; then
		echo "rule 4 (convergence, identical db_dump across [$live]): OK"
	else
		echo "VIOLATION (rule 4): $diffnode db_dump still differs from $first after ${HA_CONVERGE_WAIT}s"
		HA_VIOL=$((HA_VIOL + 1))
	fi
}

# rule 1/2 at the key level, independent of rtcheck (teeth even if rtcheck is
# unavailable).  An ACKed put's key MUST be present in the converged master
# dump; an op whose txn outcome is FAIL must not add a NEW key that no ACK
# produced.  Value-hash matching is rtcheck's job (binary-safe); here we gate on
# key presence, which is what a lost acked write looks like.
ha_check_durability() {
	local m; m=$(ha_master)
	[ -n "$m" ] || { echo "note: no master for durability check"; return; }
	local ack="$HA_RUN/ack.$m.log"
	[ -f "$ack" ] || { echo "note: no ack log for $m"; return; }
	# keys of ACKed puts: a 'txn ... ACK' line's txnid, joined to its 'put' line.
	local acked_keys dumpf miss=0 k
	dumpf=$(mktemp)
	net_run "$m" env LD_LIBRARY_PATH="$LD_LIBRARY_PATH" \
	    "$BUILD/db_dump" -p -h "$HA_RUN/$m" rt.db 2>/dev/null > "$dumpf"
	# ACKed txnids
	local acktxn; acktxn=$(awk '$3=="txn" && $NF=="ACK"{print $2}' "$ack" | sort -u)
	# keys for those txnids from put lines
	acked_keys=$(awk 'NR==FNR{a[$1]=1;next} $3=="put" && ($2 in a){print $5}' \
	    <(echo "$acktxn") "$ack" | sort -u)
	local nack; nack=$(echo "$acked_keys" | grep -c . )
	for k in $acked_keys; do
		[ -n "$k" ] || continue
		# db_dump -p prints key on its own line prefixed with a space.
		grep -qxF " $k" "$dumpf" || miss=$((miss + 1))
	done
	rm -f "$dumpf"
	if [ "$miss" = 0 ]; then
		echo "rule 1 (durability, $nack ACKed keys present on master $m): OK"
	else
		echo "VIOLATION (rule 1, durability): $miss of $nack ACKed keys MISSING on master $m"
		HA_VIOL=$((HA_VIOL + 1))
	fi
}

# rtcheck across all homes for rules 1-3/7.
ha_check_rtcheck() {
	local acks="" homes="" n
	for n in $HA_NODES; do
		[ -f "$HA_RUN/ack.$n.log" ] && acks="$acks --ack $HA_RUN/ack.$n.log"
		case " $HA_KILLED " in *" $n "*) continue;; esac
		homes="$homes --home $HA_RUN/$n"
	done
	# rtcheck runs as root (env homes are root-owned under netns sudo).
	local out
	out=$(sudo env LD_LIBRARY_PATH="$LD_LIBRARY_PATH" \
	    "$BUILD/rtcheck" $acks $homes 2>&1)
	echo "$out" | sed 's/^/  rtcheck: /'
	echo "$out" | grep -q '^PASS' || HA_VIOL=$((HA_VIOL + 1))
}

ha_oracle() {
	HA_VIOL=0
	ha_check_single_master
	ha_check_convergence
	ha_check_durability
	ha_check_rtcheck
	if [ "$HA_VIOL" = 0 ]; then
		echo "ORACLE: PASS"
		return 0
	fi
	echo "ORACLE: FAIL ($HA_VIOL violation group(s)); evidence kept in $HA_RUN"
	return 1
}

ha_group_down() {
	local n
	for n in $HA_NODES; do ha_send "$n" exit 2>/dev/null || true; done
	sleep 1
	for n in $HA_NODES; do
		net_run "$n" pkill -9 -f "rtnode .*--node $n" 2>/dev/null || true
	done
	net_down
}
