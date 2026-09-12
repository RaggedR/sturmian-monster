#!/bin/sh
[ -f /tmp/stream.pids ] && while read -r p; do kill "$p" 2>/dev/null || true; done < /tmp/stream.pids
rm -f /tmp/stream.pids
