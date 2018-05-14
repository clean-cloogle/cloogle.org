#!/bin/bash

DEST="$1"

rm -rf "$DEST"
mkdir -p "$DEST"
# Fetch Clean distribution
curl -sSL http://ftp.cs.ru.nl/Clean/builds/linux-x64/clean-bundle-complete-linux-x64-latest.tgz |\
	tar -xz --exclude=exe -C "$DEST" --strip-components=2 clean-bundle-complete/lib

exec 5< <(jq '.[]' < libs.json | jq '.[]' | jq -cMr '.name,.fetch_url[0,1],.path')

while read lib <&5
do
	read fetch_method <&5
	read fetch_url <&5
	read path <&5

	case "$fetch_method" in
		"SVN")
			rm -rf "$DEST/$lib"
			if [[ "$path" == "null" ]]; then
				echo "Fetching $fetch_url..."
				svn checkout -q "$fetch_url" "$DEST/$lib"
			else
				echo "Fetching $fetch_url/$path..."
				svn checkout -q "$fetch_url/$path" "$DEST/$lib"
			fi
			;;
		"Git")
			rm -rf "$DEST/$lib"
			if [[ "$path" == "null" ]]; then
				echo "Fetching $fetch_url..."
				git clone -q "$fetch_url" "$DEST/$lib"
			else
				echo "Fetching $fetch_url/$path..."
				git clone -q "$fetch_url" "/tmp/$lib"
				mv "/tmp/$lib/$path" "$DEST/$lib"
				rm -rf "/tmp/$lib"
			fi
			;;
		"CleanDistribution")
			;;
		*)
			echo "Unknown fetch method '$fetch_method'"
			exit 1
			;;
	esac
done
