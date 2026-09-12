#!/bin/sh
# run.sh START END MOD "OPENSET"  -- attack when (i%MOD) is in OPENSET, else block.
# Halts on any prediction mismatch or fight end.
i=$1; end=$2; mod=$3; set_=$4
while [ "$i" -le "$end" ]; do
  m=$((i % mod)); act=block; pred=STRIKING
  for s in $set_; do [ "$m" -eq "$s" ] && { act=attack; pred=OPEN; }; done
  out=$(./p.sh "$act")
  actual=STRIKING; case "$out" in *"was OPEN"*) actual=OPEN;; esac
  if [ "$actual" != "$pred" ]; then echo "MISMATCH at $i (pred $pred): $out"; exit 1; fi
  case "$out" in *OVER*|*"already over"*) echo "$i: $out"; exit 0;; esac
  i=$((i+1))
done
echo "done through $end"; ./p.sh state
