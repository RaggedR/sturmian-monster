#!/bin/sh
# up.sh [--bot afterone|afterhh] — the monster, and the player.
set -e
cd "$(dirname "$0")"
./down.sh >/dev/null 2>&1 || true

export STREAM_PLAYER="claude"
[ "$1" = "--bot" ] && STREAM_PLAYER="${2:-afterone}"

deno run --quiet --allow-net monster.server.ts > /tmp/stream-monster.log 2>&1 &
echo "$!" >> /tmp/stream.pids
# Scoped --allow-run: the only binary this server may ever spawn.
deno run --quiet --allow-net --allow-run=claude --allow-env \
  player.server.ts > /tmp/stream-player.log 2>&1 &
echo "$!" >> /tmp/stream.pids

sleep 1
echo "up: monster :8811   player :8812   (player = $STREAM_PLAYER)"
echo "    deno run --allow-net referee.ts [seed]"
