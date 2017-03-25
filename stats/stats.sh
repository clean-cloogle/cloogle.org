#!/bin/sh
tail -n 0 -f /var/log/cloogle.log | jq -c --unbuffered .request
