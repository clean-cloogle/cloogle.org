#!/bin/bash
shopt -s globstar
for f in /opt/clean/lib/**/*.[id]cl; do
	enc="$(file -bi "$f" | grep -Po '(?<=charset=).*')"
	if [ "$enc" != 'us-ascii' -a "$enc" != 'binary' -a "$enc" != 'utf-8' ]; then
		iconv -f "$enc" -t utf-8 < "$f" > "$f.tmp"
		mv "$f.tmp" "$f"
		echo "converted $f from $enc to utf-8"
	fi
done
