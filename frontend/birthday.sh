#!/bin/bash

month="$(date +%m)"
day="$(date +%d)"

if [[ "$month" == "02" ]] && [[ "$day" > "15" ]] && [[ "$day" < "24" ]]; then
	patch < birthday.patch
fi
