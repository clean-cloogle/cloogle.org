#!/bin/bash
CLEAR_CACHE=-1
INTERACTIVE=1

for arg do
	case "$arg" in
		--interactive ) INTERACTIVE=1;;
		--no-interactive ) INTERACTIVE=0;;
		--clear-cache ) CLEAR_CACHE=1;;
		--no-clear-cache ) CLEAR_CACHE=0;;
		-h|--help )
			echo "Usage: $0 [--[no-]interactive] [--[no-]clear-cache]"
			exit;;
		* )
			echo "Unknown argument '$arg'; use -h for help"
			exit -1;;
	esac
done

if [[ $INTERACTIVE -eq 0 ]] && [[ $CLEAR_CACHE -lt 0 ]]; then
	echo "When using --no-interactive you must use either --no-clear-cache or --clear-cache."
	exit -1
fi

echo "Pulling new commits..."

git checkout frontend/index.html
if [ $INTERACTIVE -eq 0 ]; then
	git pull origin master
else
	git pull --no-edit origin master
fi
git submodule update --init --recursive

echo "Updating containers..."

sudo docker-compose build --force-rm --no-cache --pull
sudo docker-compose up -d
sudo docker image prune -f

echo "All done."

if [ $CLEAR_CACHE -lt 0 ]; then
	echo
	read -p "Do you want to clear the caches? (y/[n]) " confirm
	case "$confirm" in
		y|Y ) CLEAR_CACHE=1;;
		* ) CLEAR_CACHE=0;;
	esac
fi

if [ $CLEAR_CACHE -eq 1 ]; then
	echo "Clearing the cache..."
	sudo bash -c 'rm -f cache/*/*'
else
	echo "Not clearing the cache."
fi
