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
[ -f "$HA_DIR/fi_fiu.sh" ] && . "$HA_DIR/fi_fiu.sh"

HA_PORT_BASE=${HA_PORT_BASE:-7000}
HA_NODES=""            # node names, set by ha_group_up
HA_RUN=""              # run dir (ctl/ev/ack live here)
HA_ACKPOL="quorum"
HA_KILLED=""           # names of nodes we killed (for the oracle)
HA_FIU_NODE=""         # a node name to launch under a libfiu plan (S6)
HA_FIU_PLAN=""         # the fi_fiu plan for that node
HA_FIU_SEED=""         # FIU_PRNG_SEED for replay
HA_FIU_PROB=""         # probability override

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

ha_send() { timeout 5 sh -c "echo '$2' > '$HA_RUN/ctl.$1'" 2>/dev/null || true; }

# ha_tls_gen_certs -- make a CA and one cert/key per node under $HA_RUN (and the
# node homes).  Call AFTER ha_group_up's run dir exists is not possible (certs
# must exist before the node starts), so the scenario sets HA_RUN itself... no:
# instead the scenario calls ha_tls_setup BEFORE ha_group_up, which makes the
# run dir, the CA and per-node certs, and sets HA_TLS=1.  Echoes each node's
# sha256 fingerprint (for pinning).
ha_tls_setup() {
	local nsites="$1"; shift 2>/dev/null || true
	HA_RUN=$(fi_mkrun "s-${RT_RUN:-run}")
	local subj="/CN=dstrt"
	# CA
	openssl req -x509 -newkey rsa:2048 -nodes -keyout "$HA_RUN/ca.key" \
	    -out "$HA_RUN/ca.crt" -days 2 -subj "$subj-ca" >/dev/null 2>&1
	local i c home
	for i in $(seq 1 "$nsites"); do
		c=$(printf "\\$(printf '%03o' $((96 + i)))")
		home="$HA_RUN/$c"; mkdir -p "$home"
		openssl req -newkey rsa:2048 -nodes -keyout "$home/node.key" \
		    -out "$home/node.csr" -subj "$subj-$c" >/dev/null 2>&1
		openssl x509 -req -in "$home/node.csr" -CA "$HA_RUN/ca.crt" \
		    -CAkey "$HA_RUN/ca.key" -CAcreateserial -days 2 \
		    -out "$home/node.crt" >/dev/null 2>&1
	done
	HA_TLS=1
	# reuse HA_RUN in ha_group_up instead of making a new one.
	HA_RUN_PRESET="$HA_RUN"
}

# echo a node's cert sha256 fingerprint (hex, no colons) for pinning.
ha_tls_fpr() {
	openssl x509 -in "$HA_RUN/$1/node.crt" -noout -fingerprint -sha256 2>/dev/null \
	    | sed 's/.*=//; s/://g' | tr 'A-F' 'a-f'
}

# Return the node currently reporting master.  A node's current role is the
# LAST ROLE line in its own ev log; among nodes whose last role is master, the
# one with the highest generation is the live master (a stale pre-failover
# "ROLE master" is superseded by a later "ROLE client" in the same log).
ha_master() {
	local n role gen best="" bestgen=-1
	for n in $HA_NODES; do
		[ -f "$HA_RUN/ev.$n.log" ] || continue
		role=$(grep -oE 'ROLE (master|client)' "$HA_RUN/ev.$n.log" 2>/dev/null | tail -1)
		[ "$role" = "ROLE master" ] || continue
		gen=$(sed -n 's/^ROLE master gen=\([0-9]*\).*/\1/p' "$HA_RUN/ev.$n.log" 2>/dev/null | tail -1)
		gen=${gen:-0}
		if [ "$gen" -ge "$bestgen" ]; then bestgen=$gen; best=$n; fi
	done
	echo "$best"
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

# _ha_launch_node NODE FIRST -- write DB_CONFIG and start one rtnode in NODE's
# netns.  FIRST=1 marks the group creator (highest priority).
HA_EXTRA=""
_ha_launch_node() {
	local node="$1" first="$2"
	local ip home port peers other prio creator ackpol
	ip=$(net_ip "$node"); port=$HA_PORT_BASE
	home="$HA_RUN/$node"; mkdir -p "$home"
	mkfifo "$HA_RUN/ctl.$node" 2>/dev/null || true
	# DB_CONFIG: elections on, heartbeats + election/ack/retry timeouts
	# (RFC 0024 S4).  rtnode sets none of these, so a clean partition would
	# never trigger an election without them.  node a (first) gets the
	# highest priority so it is the preferred master.  (Persisted in the
	# home, so a restart reuses it.)
	prio=100; [ "$first" = 1 ] && prio=200
	if [ ! -f "$home/DB_CONFIG" ]; then
		cat > "$home/DB_CONFIG" <<-EOF
			rep_set_config db_repmgr_conf_elections on
			rep_set_priority $prio
			rep_set_timeout db_rep_heartbeat_send 500000
			rep_set_timeout db_rep_heartbeat_monitor 1500000
			rep_set_timeout db_rep_election_timeout 1000000
			rep_set_timeout db_rep_election_retry 1000000
			rep_set_timeout db_rep_connection_retry 1000000
			rep_set_timeout db_rep_ack_timeout 1000000
		EOF
		# S7: TLS directives (RFC 0018) for this node, if configured.
		if [ -n "${HA_TLS:-}" ]; then
			echo "repmgr_tls cert $home/node.crt key $home/node.key ca $HA_RUN/ca.crt flags require,verify" >> "$home/DB_CONFIG"
			local pin
			for pin in ${HA_TLS_PINS:-}; do
				echo "repmgr_tls_pin $pin" >> "$home/DB_CONFIG"
			done
		fi
	fi
	peers=""
	for other in $HA_NODES; do
		[ "$other" != "$node" ] && peers="$peers --peer $(net_ip "$other"):$port"
	done
	creator=""; [ "$first" = 1 ] && creator="--group-creator"
	ackpol="--ack-policy $HA_ACKPOL"
	# repmgr timeouts, us (RFC 0024 S4): rtnode is the source of truth and
	# the reproduce line shows them.  A DB_CONFIG is still written above as a
	# fallback; the command line wins.
	local tmo="--hb-send ${HA_HB_SEND:-500000} --hb-mon ${HA_HB_MON:-1500000} --elect-to ${HA_ELECT_TO:-1000000} --conn-retry ${HA_CONN_RETRY:-1000000} --ack-to ${HA_ACK_TO:-1000000}"
	# S6: one node may run under a libfiu I/O plan (fiu-run -x ... --).  The
	# ack log is kept out of fault scope by the raw-syscall writer, so its
	# own writes are never failed.  To keep the (nested) quoting of the
	# fiu-run -c arguments sane, write the whole invocation to a per-node
	# launch script and exec that inside the netns.
	local fiu=""
	if [ "$node" = "$HA_FIU_NODE" ] && [ -n "$HA_FIU_PLAN" ] && \
	   command -v fi_fiu_cmd >/dev/null 2>&1; then
		fiu=$(fi_fiu_cmd "$HA_FIU_PLAN" "${HA_FIU_SEED:-1}" "${HA_FIU_PROB:-}")
	fi
	local ls="$HA_RUN/launch.$node.sh"
	{
		echo "#!/bin/sh"
		echo "export LD_LIBRARY_PATH='$LD_LIBRARY_PATH'"
		# $fiu is either empty or "env ... fiu-run -x -c '...' -c '...'";
		# append '--' when present so the node binary follows the plan.
		if [ -n "$fiu" ]; then
			printf '%s -- ' "$fiu"
		fi
		echo "'$BUILD/rtnode' --home '$home' --ctl '$HA_RUN/ctl.$node' --ev '$HA_RUN/ev.$node.log' --node '$node' --ack '$HA_RUN/ack.$node.log' --site '$ip:$port' $peers $creator $ackpol $tmo $HA_EXTRA"
	} > "$ls"
	net_run "$node" sh "$ls" >>"$HA_RUN/out.$node" 2>&1 &
}

# wait up to $2 s for NODE to report READY (startup sync done).
_ha_wait_ready() {
	local node="$1" deadline=$(( $(date +%s) + ${2:-30} ))
	while [ "$(date +%s)" -lt "$deadline" ]; do
		grep -q 'READY' "$HA_RUN/ev.$node.log" 2>/dev/null && return 0
		sleep 1
	done
	return 1
}

# ha_group_up NSITES ACKPOL "extra rtnode args"
# Node a is the group creator and highest priority (preferred master).  Nodes
# start SEQUENTIALLY -- the creator forms the group and becomes master, then
# each client joins and finishes initial sync before the next starts -- because
# a client that calls repmgr_start before the group exists gets DB_REP_UNAVAIL
# and panics (the join race).
ha_group_up() {
	local nsites="$1"; HA_ACKPOL="${2:-quorum}"; HA_EXTRA="${3:-}"
	local names="" i c
	for i in $(seq 1 "$nsites"); do
		c=$(printf "\\$(printf '%03o' $((96 + i)))"); names="$names $c"
	done
	HA_NODES=$(echo $names)

	# reuse the run dir ha_tls_setup already created (so the certs match),
	# else make a fresh one.
	if [ -n "${HA_RUN_PRESET:-}" ]; then
		HA_RUN="$HA_RUN_PRESET"; HA_RUN_PRESET=""
	else
		HA_RUN=$(fi_mkrun "s-${RT_RUN:-run}")
	fi
	export LD_LIBRARY_PATH="${LD_LIBRARY_PATH:-}:$(cd "$BUILD/.libs" 2>/dev/null && pwd)"

	net_up "$nsites" $HA_NODES

	local node first=1
	for node in $HA_NODES; do
		_ha_launch_node "$node" "$first"
		if [ "$first" = 1 ]; then
			# the creator must form the group before any client joins,
			# or a client gets DB_REP_UNAVAIL and panics; wait for it to
			# be master.
			local cm; cm=$(ha_wait_master 30)
			[ -n "$cm" ] || { echo "ha_group_up: creator never became master"; return 1; }
		else
			# let this client finish joining + initial sync before the
			# next one starts (sequential joins avoid the join race).
			_ha_wait_ready "$node" 30
		fi
		first=0
	done
	local m; m=$(ha_wait_master 40)
	[ -n "$m" ] || { echo "ha_group_up: no master elected"; return 1; }
	sleep 3
	echo "ha_group_up: master=$m nodes=[$HA_NODES] run=$HA_RUN"
	return 0
}

# Drive a load of N ops with SEED on the current master.
ha_load() {
	local n="$1" seed="$2" m
	m=$(ha_master)
	[ -n "$m" ] || { echo "ha_load: no master"; return 1; }
	ha_load_on "$m" "$n" "$seed"
}

# Drive a load of N ops with SEED on a SPECIFIC node (used after a failover).
ha_load_on() {
	local node="$1" n="$2" seed="$3"
	ha_send "$node" "load $n $seed"
	local deadline=$(( $(date +%s) + 60 ))
	while [ "$(date +%s)" -lt "$deadline" ]; do
		grep -q "LOADDONE" "$HA_RUN/ev.$node.log" 2>/dev/null && break
		sleep 1
	done
	echo "ha_load: $n ops on $node seed=$seed acks=$(grep -c ' ACK$' "$HA_RUN/ack.$node.log" 2>/dev/null || echo 0)"
}

# Kill a node hard (SIGKILL), recording it for the oracle.
ha_kill() {
	local node="$1"
	# SIGKILL by the unique per-run home path (ip netns exec shares the host
	# PID namespace, so a host-side pkill finds the node's rtnode and this
	# never blocks on a ctl FIFO).
	sudo pkill -9 -f "rtnode --home $HA_RUN/$node " 2>/dev/null || true
	HA_KILLED="$HA_KILLED $node"
	echo "ha_kill: $node"
}

# Restart a previously-killed node (fresh rtnode, DB_RECOVER on open; its
# DB_CONFIG persists in the home).  A restart is never the group creator.
ha_restart() {
	local node="$1"
	_ha_launch_node "$node" 0
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

# rule 1 at the key level, independent of rtcheck (teeth even if rtcheck is
# unavailable, and the check that gives the S5 sabotage arm its teeth).  Every
# key an ACK log marks ACKed -- from ANY node, since a failed-over write was
# acked on the old master -- MUST be present in the SURVIVING master's
# converged data.  A lost acked write (e.g. ACKS_NONE + partition + kill) shows
# up here as a missing key.  Value-hash matching is rtcheck's job.
ha_check_durability() {
	local m; m=$(ha_master)
	[ -n "$m" ] || { echo "note: no master for durability check"; return; }
	local dumpf miss=0 nack=0 k n ack acktxn acked_keys
	dumpf=$(mktemp)
	net_run "$m" env LD_LIBRARY_PATH="$LD_LIBRARY_PATH" \
	    "$BUILD/db_dump" -p -h "$HA_RUN/$m" rt.db 2>/dev/null > "$dumpf"
	for n in $HA_NODES; do
		ack="$HA_RUN/ack.$n.log"
		[ -f "$ack" ] || continue
		acktxn=$(awk '$3=="txn" && $7=="ACK"{print $2}' "$ack" | sort -u)
		[ -n "$acktxn" ] || continue
		acked_keys=$(awk 'NR==FNR{a[$1]=1;next} $3=="put" && ($2 in a){print $5}' \
		    <(echo "$acktxn") "$ack" | sort -u)
		for k in $acked_keys; do
			[ -n "$k" ] || continue
			nack=$((nack + 1))
			grep -qxF " $k" "$dumpf" || {
				miss=$((miss + 1))
				[ "$miss" -le 5 ] && echo "  lost acked key (from $n): $k"
			}
		done
	done
	rm -f "$dumpf"
	if [ "$nack" -eq 0 ]; then
		echo "VIOLATION (vacuous): 0 ACKed keys to check -- the oracle verified nothing"
		HA_VIOL=$((HA_VIOL + 1))
	elif [ "$miss" = 0 ]; then
		echo "rule 1 (durability, $nack ACKed keys present on surviving master $m): OK"
	else
		echo "VIOLATION (rule 1, durability): $miss of $nack ACKed keys MISSING on surviving master $m"
		HA_VIOL=$((HA_VIOL + 1))
	fi
}

# rtcheck across all homes for rules 1-3/7.  Every violation it reports is a
# HARD failure now that ack lines carry (gen, LSN) and rtcheck orders by it:
# missing-key (lost acked write), a WRONG VALUE (a lost/overwritten write --
# the whole point of the HA tests), recovery failure and DB->verify failure.
# A vacuous pass (0 acked ops checked) is itself a failure.
ha_check_rtcheck() {
	local acks="" homes="" n
	for n in $HA_NODES; do
		[ -f "$HA_RUN/ack.$n.log" ] && acks="$acks --ack $HA_RUN/ack.$n.log"
		case " $HA_KILLED " in *" $n "*) continue;; esac
		homes="$homes --home $HA_RUN/$n"
	done
	local out
	out=$(sudo env LD_LIBRARY_PATH="$LD_LIBRARY_PATH" \
	    "$BUILD/rtcheck" $acks $homes 2>&1)
	echo "$out" | grep -E '^PASS|^FAIL|VIOLATION' | head -20 | sed 's/^/  rtcheck: /'
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
	# Best-effort graceful exit, but never block: a stuck rtnode (inside a
	# repmgr call) may not be reading its ctl FIFO, and `echo > fifo` would
	# hang forever.  timeout the write, then kill by pattern on the host
	# (ip netns exec shares the host PID namespace, so a host-side pkill
	# sees the rtnode; killing by the unique per-run home path is precise).
	for n in $HA_NODES; do
		timeout 3 sh -c "echo exit > '$HA_RUN/ctl.$n'" 2>/dev/null || true
	done
	sleep 1
	sudo pkill -9 -f "rtnode --home $HA_RUN/" 2>/dev/null || true
	sleep 1
	# second sweep for any that caught a SIGTERM-ignore window
	sudo pkill -9 -f "rtnode --home $HA_RUN/" 2>/dev/null || true
	net_down
}
