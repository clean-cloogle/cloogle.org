#!/bin/bash
echo "Pulling new commits..."

git checkout frontend/index.html
git pull origin master
git submodule init
git submodule update

echo "Updating containers..."

sudo docker-compose build
sudo docker-compose up -d

echo "All done."

echo
read -p "Do you want to clear the caches? (y/[n]) " confirm
case "$confirm" in
	y|Y ) echo "Clearing the cache..."; sudo bash -c 'rm -f cache/*/*';;
	* ) echo "Not clearing the cache.";;
esac
