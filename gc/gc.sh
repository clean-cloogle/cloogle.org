#!/usr/bin/env bash
CACHE_SIZE=1000
INTERVAL=600
CACHE_DIR=/var/cache

cd "$CACHE_DIR"

while :; do
	n="$(($(ls -l | wc -l)-1))"
	if [ "$n" -gt "$CACHE_SIZE" ]; then
		ls -1tu | tail -n "$((n-CACHE_SIZE))" | xargs -P$(nproc) -r rm -v
	else
		echo "$n / $CACHE_SIZE cache entries used"
	fi
	sleep $INTERVAL
done
