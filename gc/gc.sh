#!/usr/bin/env bash
CACHE_SIZE=1000
INTERVAL=600
CACHE_DIR=/var/cache

cd "$CACHE_DIR"

while :; do
	# Long term cache
	cd lt
	n="$(($(ls -1 | wc -l)-1))"
	if [ "$n" -gt "$CACHE_SIZE" ]; then
		ls -1tu | tail -n "$((n-CACHE_SIZE))" | xargs -P$(nproc) -r rm -v
	else
		echo "$n / $CACHE_SIZE cache entries used"
	fi
	cd ..
	# Brief cache
	cd brief
	echo "$(($(ls -1 | wc -l)-1)) entries removed from brief cache"
	rm *
	cd ..
	# Wait
	sleep $INTERVAL
done
