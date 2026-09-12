#!/bin/sh
exec deno run --quiet --allow-net play.ts "$@"
