#!/usr/bin/env bash
CACHE_SIZE=2000
INTERVAL=600
CACHE_DIR=/var/cache

cd "$CACHE_DIR"

while :; do
	# Long term cache
	cd lt
	n="$(ls -1 | wc -l)"
	if [ "$n" -gt "$CACHE_SIZE" ]; then
		ls -1tu | tail -n "$((n-CACHE_SIZE))" | xargs -P$(nproc) -r rm -v
	else
		echo "$n / $CACHE_SIZE cache entries used"
	fi
	cd ..
	# Brief cache
	cd brief
	n="$(ls -1 | wc -l)"
	echo "$n entries removed from brief cache"
	if [ "$n" -gt "0" ]; then rm *; fi
	cd ..
	# Wait
	sleep $INTERVAL
done
