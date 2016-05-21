#!/bin/sh

# Changes {{{COMMIT} in FILE to information about the latest commit.
# May be useful to put in a git hook for index.html.

escape_sed() {
	sed -e 's/\//\\\//g' -e 's/\&/\\\&/g'
}

if [ 1 -ne $# ]; then
	echo "Usage: $0 FILE"
	exit 1
fi

FILE="$1"
COMMIT_INFO="$(git log -1 --decorate --pretty=oneline --no-color | escape_sed)"

echo "$COMMIT_INFO"

sed -i "s/{{{COMMIT}}}/$COMMIT_INFO/g" "$FILE"
