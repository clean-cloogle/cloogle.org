#!/bin/bash
echo "Pulling new commits..."

git checkout frontend/index.html
git pull origin master
git submodule update --init --recursive

echo "Updating containers..."

sudo docker-compose build --force-rm --no-cache --pull
sudo docker-compose up -d

echo "All done."

CLEAR_CACHE=""
if [ "$1" == "--clear-cache" ]; then
	CLEAR_CACHE="yes"
else if [ "$1" == "--no-clear-cache" ]; then
	CLEAR_CACHE="no"
fi; fi

if [ "$CLEAR_CACHE" == "" ]; then
	echo
	read -p "Do you want to clear the caches? (y/[n]) " confirm
	case "$confirm" in
		y|Y ) CLEAR_CACHE="yes";;
		* ) CLEAR_CACHE="no";;
	esac
fi

if [ "$CLEAR_CACHE" == "yes" ]; then
	echo "Clearing the cache..."
	sudo bash -c 'rm -f cache/*/*'
else
	echo "Not clearing the cache."
fi
