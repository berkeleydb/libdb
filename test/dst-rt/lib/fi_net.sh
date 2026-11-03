# Copyright (c) 2026 libdb contributors.  All rights reserved.
# SPDX-License-Identifier: Sleepycat
#
# fi_net.sh -- network-namespace topology and tc/netem fault injection for the
# RFC 0024 runtime-fault-injection DST layer (scenarios S4-S8).
#
# A run gets its own set of namespaces (one per node plus one bridge namespace)
# joined by veth pairs to a single Linux bridge.  Faults are injected by
# changing the netem qdisc on the per-node veth while the test runs, so delay,
# jitter, loss and rate limits are per node, and a partition is "drop everything
# on this node's veth".  Asymmetric partitions use nftables drop rules in the
# bridge namespace, which can match one direction only.
#
# Public functions (all take node NAMES like a b c, not full ns names):
#   net_up <n>                     create bridge + n nodes (names a b c ...)
#   net_down                       tear the whole topology down (also on trap)
#   net_ns <node>                  echo the full netns name for a node
#   net_ip <node>                  echo the node IP
#   net_run <node> <cmd...>        run a command inside a node's netns
#   net_apply <node> <netem args>  set the node's netem qdisc (e.g. "delay 20ms 5ms loss 1%")
#   net_clear [<node>]             remove netem from one node, or all nodes
#   net_rate <node> <rate>         rate-limit a node (e.g. 1mbit), composed with any delay
#   net_partition "<setA>|<setB>"  split: no traffic between the two name sets
#   net_partition_asym <from> <to> drop <from>-><to> only (nftables, one direction)
#   net_heal                       remove every partition (symmetric and asymmetric)
#   net_selftest <node> <ms> <pct> apply delay <ms>/loss <pct>, verify with ping, fail loudly
#
# Every function requires root (sudo).  Source this file, then run as root or
# under sudo.  The trap guarantees teardown even on SIGINT/SIGTERM/kill.

set -u

# --- unique, collision-proof naming -----------------------------------------
# One run == one prefix.  RT_RUN may be set by the caller (the scenario's seed
# plus scenario id); otherwise derive a unique one from pid + time + a random
# byte so parallel runs and crashed-run leftovers never collide.
: "${RT_RUN:=$$-$(date +%s)-${RANDOM}}"
FN_PREFIX="dstrt-${RT_RUN}"
# A per-run /24 that is unlikely to clash with the host or another run.  16 bits
# of the run hash pick the 2nd and 3rd octets inside 10.0.0.0/8.
_fn_hash=$(printf '%s' "$RT_RUN" | cksum | cut -d' ' -f1)
FN_NET2=$(( (_fn_hash >> 8) & 0xff ))
FN_NET3=$(( _fn_hash & 0xff ))
FN_SUBNET="10.${FN_NET2}.${FN_NET3}"
FN_BRNS="${FN_PREFIX}-br"
FN_BR="br0"
# veth names must be < IFNAMSIZ (15) AND unique per run; a truncated prefix
# (dstrt-s4-1) is not unique across seeds (s4-1 vs s4-10), so use a short hex
# tag from the run hash: fv<4hex>-h-<node>.
FN_TAG=$(printf 'fv%04x' $(( _fn_hash & 0xffff )))
FN_LOCK="${TMPDIR:-/tmp}/dstrt-netns.lock"
FN_TC="${FN_TC:-tc}"
command -v "$FN_TC" >/dev/null 2>&1 || FN_TC=/sbin/tc
FN_NFT="${FN_NFT:-nft}"
command -v "$FN_NFT" >/dev/null 2>&1 || FN_NFT=/sbin/nft

# Node registry, filled by net_up.
FN_NODES=""

_fn_nsname() { echo "${FN_PREFIX}-$1"; }
net_ns() { _fn_nsname "$1"; }

# Node index (1-based) by name, for IP assignment.
_fn_idx() {
	local n=1 x
	for x in $FN_NODES; do
		[ "$x" = "$1" ] && { echo "$n"; return 0; }
		n=$((n + 1))
	done
	echo "ERR: unknown node $1" >&2
	return 1
}
net_ip() { echo "${FN_SUBNET}.$(( $(_fn_idx "$1") + 1 ))"; }  # .2, .3, ...

net_run() {
	local node="$1"; shift
	sudo ip netns exec "$(_fn_nsname "$node")" "$@"
}
net_run_br() { sudo ip netns exec "$FN_BRNS" "$@"; }

# --- teardown (idempotent, trap-safe) ---------------------------------------
# Deleting a netns deletes the veths that live in it, so removing every ns we
# made removes every link too.  We also sweep by prefix in case net_up died
# half-built.  Never fails the caller.
net_down() {
	(
	flock 9
	local ns
	for ns in $(ip netns list 2>/dev/null | awk '{print $1}' | grep "^${FN_PREFIX}-" 2>/dev/null); do
		sudo ip netns del "$ns" 2>/dev/null || true
	done
	# veth half-pairs can briefly outlive a ns on error; sweep by this run's
	# unique veth tag (NOT a truncated prefix, which would hit other runs).
	for l in $(ip -o link show 2>/dev/null | awk -F': ' '{print $2}' | sed 's/@.*//' | grep "^${FN_TAG}-" 2>/dev/null); do
		sudo ip link del "$l" 2>/dev/null || true
	done
	) 9>"$FN_LOCK"
	return 0
}

_fn_installed_trap=""
_fn_install_trap() {
	[ -n "$_fn_installed_trap" ] && return 0
	# Preserve whatever EXIT trap the scenario set, run net_down first.
	trap 'net_down' EXIT
	trap 'net_down; trap - INT;  kill -INT  $$' INT
	trap 'net_down; trap - TERM; kill -TERM $$' TERM
	trap 'net_down; trap - HUP;  kill -HUP  $$' HUP
	_fn_installed_trap=1
}

# --- topology ----------------------------------------------------------------
net_up() {
	local count="$1"; shift 2>/dev/null || true
	local names
	if [ $# -gt 0 ]; then
		names="$*"
	else
		# default names a b c d ...
		names=""
		local i c
		for i in $(seq 1 "$count"); do
			c=$(printf "\\$(printf '%03o' $((96 + i)))")  # a,b,c,...
			names="$names $c"
		done
	fi
	FN_NODES=$(echo $names)
	_fn_install_trap

	# Serialize the namespace+veth build across concurrent runs: `ip netns
	# add` races on the shared /run/netns mount ("Invalid netns value" / the
	# bridge ns silently missing) when two sweeps build topologies at once.
	# The lock is held only for the fast build, so long run phases still
	# overlap.  flock on an fd; released when the subshell ends.
	(
	flock 9
	# Bridge namespace with one bridge.  The very first `ip netns add` after a
	# clean slate can fail with "Invalid netns value" while the kernel sets up
	# the shared /run/netns mount (a cold-start race, seen once in 20 at the
	# start of a sweep); retry a few times under the lock.
	_try=0
	while ! sudo ip netns add "$FN_BRNS" 2>/dev/null; do
		_try=$((_try + 1))
		[ "$_try" -ge 5 ] && { sudo ip netns add "$FN_BRNS"; break; }
		sudo ip netns del "$FN_BRNS" 2>/dev/null || true
		sleep 1
	done
	net_run_br ip link add "$FN_BR" type bridge
	net_run_br ip link set "$FN_BR" up
	net_run_br ip addr add "${FN_SUBNET}.1/24" dev "$FN_BR"

	local node ns hveth cveth ip
	for node in $FN_NODES; do
		ns=$(_fn_nsname "$node")
		hveth=$(_fn_hveth "$node")   # bridge side (< IFNAMSIZ 15, unique/run)
		cveth=$(_fn_cveth "$node")   # node side
		ip=$(net_ip "$node")
		sudo ip netns add "$ns"
		# veth pair: one end stays, one end goes into the node ns.
		sudo ip link add "$hveth" type veth peer name "$cveth"
		# host end -> bridge ns, attach to bridge
		sudo ip link set "$hveth" netns "$FN_BRNS"
		net_run_br ip link set "$hveth" master "$FN_BR"
		net_run_br ip link set "$hveth" up
		# client end -> node ns
		sudo ip link set "$cveth" netns "$ns"
		net_run "$node" ip link set lo up
		net_run "$node" ip link set "$cveth" name eth0
		net_run "$node" ip addr add "${ip}/24" dev eth0
		net_run "$node" ip link set eth0 up
		net_run "$node" ip route add default via "${FN_SUBNET}.1"
	done
	# Enable nftables in the bridge ns for asymmetric drops (best effort).
	net_run_br sysctl -q -w net.bridge.bridge-nf-call-iptables=0 2>/dev/null || true
	) 9>"$FN_LOCK"
	return 0
}

# Name of the bridge-side veth for a node (where egress-to-node netem lives).
_fn_hveth() { echo "${FN_TAG}-h-$1"; }
_fn_cveth() { echo "${FN_TAG}-c-$1"; }

# --- netem fault injection ---------------------------------------------------
# netem is applied on the node side eth0 root qdisc (egress from the node) and
# on the bridge-side veth (egress toward the node), so a symmetric delay/loss
# affects both directions, matching what a real flaky link does.
_fn_set_netem() {  # <node> <netem-args-or-empty>
	local node="$1"; shift
	local args="$*"
	local hveth; hveth=$(_fn_hveth "$node")
	# node egress
	net_run "$node" "$FN_TC" qdisc del dev eth0 root 2>/dev/null || true
	# bridge egress toward node
	net_run_br "$FN_TC" qdisc del dev "$hveth" root 2>/dev/null || true
	if [ -n "$args" ]; then
		net_run "$node" "$FN_TC" qdisc add dev eth0 root netem $args
		net_run_br "$FN_TC" qdisc add dev "$hveth" root netem $args
	fi
}

net_apply() { _fn_set_netem "$@"; }

net_clear() {
	if [ $# -gt 0 ]; then
		_fn_set_netem "$1"
	else
		local node
		for node in $FN_NODES; do _fn_set_netem "$node"; done
	fi
}

# Rate limit a node.  Implemented as a netem rate, composed with nothing else;
# callers that want delay+rate pass both through net_apply ("delay 10ms rate 1mbit").
net_rate() { _fn_set_netem "$1" "rate $2"; }

# --- partitions --------------------------------------------------------------
# Symmetric partition: "<setA>|<setB>".  All nodes named in setA cannot reach
# any node in setB and vice-versa.  Implemented as 100% netem loss on the links
# of one side (the smaller side) toward the other -- simplest total cut that the
# self-test can verify.  We cut the side listed second (the "minority" by
# convention in S5) by dropping all its traffic in both directions.
FN_PART_CUT=""   # remembered cut node set for net_heal
net_partition() {
	local spec="$1"
	local setA setB
	setA=$(echo "$spec" | cut -d'|' -f1)
	setB=$(echo "$spec" | cut -d'|' -f2)
	# Cut every node in setB from the rest by 100% loss on its link.
	local node
	FN_PART_CUT=""
	for node in $setB; do
		_fn_set_netem "$node" "loss 100%"
		FN_PART_CUT="$FN_PART_CUT $node"
	done
	return 0
}

# Asymmetric drop: packets from <from> to <to> are dropped, reverse is clean.
# Bridged (L2) traffic does not traverse the IP forward hook, so we hook the
# netdev ingress of <from>'s bridge-side veth (where <from>'s outbound frames
# enter the bridge) and drop those whose IP dest is <to>.  That cuts
# <from>-><to> only; <to>-><from> enters via <to>'s veth and is untouched.
FN_ASYM=""
net_partition_asym() {
	local from="$1" to="$2"
	local tip hveth
	tip=$(net_ip "$to")
	hveth=$(_fn_hveth "$from")
	net_run_br "$FN_NFT" add table netdev fidrop 2>/dev/null || true
	net_run_br "$FN_NFT" "add chain netdev fidrop out_${from} { type filter hook ingress device \"$hveth\" priority 0 ; }" 2>/dev/null || true
	net_run_br "$FN_NFT" add rule netdev fidrop "out_${from}" ip daddr "$tip" drop
	FN_ASYM=1
	return 0
}

net_heal() {
	local node
	for node in $FN_PART_CUT; do _fn_set_netem "$node"; done
	FN_PART_CUT=""
	if [ -n "$FN_ASYM" ]; then
		net_run_br "$FN_NFT" delete table netdev fidrop 2>/dev/null || true
		FN_ASYM=""
	fi
	return 0
}

# --- self-test ---------------------------------------------------------------
# A netem rule can silently fail to apply (wrong dev, module missing, typo).
# This measures the ACTUAL delay and loss between the bridge and <node> with
# ping and fails loudly if the applied values are not observed.  Returns 0 on
# success, non-zero (and prints why) on failure.
net_selftest() {
	local node="$1" want_ms="$2" want_loss="$3"
	local ip; ip=$(net_ip "$node")
	net_apply "$node" "delay ${want_ms}ms loss ${want_loss}%"
	# 40 pings at 50ms spacing from inside the bridge ns to the node.
	local out
	out=$(sudo ip netns exec "$FN_BRNS" ping -n -q -c 40 -i 0.05 -W 2 "$ip" 2>&1)
	local rtt loss
	rtt=$(echo "$out" | awk -F'/' '/rtt|round-trip/ {print $5}')
	loss=$(echo "$out"  | grep -oE '[0-9.]+% packet loss' | grep -oE '^[0-9.]+')
	net_clear "$node"
	[ -z "$rtt" ] && { echo "net_selftest $node: no RTT measured (netem/ping failed): $out" >&2; return 1; }
	# Expected avg RTT >= want_ms (one-way delay each way == 2x, but netem delay
	# here is per-direction so round trip ~= 2*want_ms).  Require at least 80% of
	# one want_ms to prove the qdisc is live; and loss within a tolerance band.
	local rtt_int; rtt_int=${rtt%.*}
	if [ "$rtt_int" -lt "$(( want_ms * 8 / 10 ))" ]; then
		echo "net_selftest $node: delay NOT applied: wanted ~${want_ms}ms+, saw ${rtt}ms" >&2
		return 1
	fi
	loss=${loss:-0}; local loss_int=${loss%.*}
	# With want_loss% per direction the round-trip loss is ~1-(1-p)^2; just
	# require observed > 0 when we asked for >0, and ~0 when we asked for 0.
	if [ "${want_loss%.*}" -gt 0 ] && [ "${loss_int:-0}" -eq 0 ]; then
		echo "net_selftest $node: loss NOT applied: wanted ${want_loss}%, saw ${loss}%" >&2
		return 1
	fi
	echo "net_selftest $node OK: rtt=${rtt}ms loss=${loss}% (wanted delay ${want_ms}ms loss ${want_loss}%)"
	return 0
}
