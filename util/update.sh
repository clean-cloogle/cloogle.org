#!/bin/bash
CLEAR_CACHE=-1
INTERACTIVE=1

escape_sed() {
	sed -e 's/\//\\\//g' -e 's/\&/\\\&/g'
}

while [ $# -gt 0 ]; do
	case "$1" in
		--interactive ) INTERACTIVE=1;;
		--no-interactive ) INTERACTIVE=0;;
		--clear-cache ) CLEAR_CACHE=1;;
		--no-clear-cache ) CLEAR_CACHE=0;;
		--huginn ) HUGINN="$2"; shift;;
		--release-repo ) RELEASE_REPO="$2"; shift;;
		--github-token ) GITHUB_TOKEN="$2"; shift;;
		-h|--help )
			echo "Usage: $0 [--[no-]interactive] [--[no-]clear-cache] [--huginn HUGINN_ADDRESS] [--release-repo USER/REPO] [--github-token TOKEN]"
			exit;;
		* )
			echo "Unknown argument '$1'; use -h for help"
			exit -1;;
	esac
	shift
done

if [[ $INTERACTIVE -eq 0 ]] && [[ $CLEAR_CACHE -lt 0 ]]; then
	echo "When using --no-interactive you must use either --no-clear-cache or --clear-cache."
	exit -1
fi

echo "Pulling new commits..."

git checkout frontend/index.php
if [ $INTERACTIVE -eq 0 ]; then
	git pull origin master
else
	git pull --no-edit origin master
fi
git submodule update --init --recursive

COMMIT_INFO="$(git log -2 --decorate --pretty=oneline --no-color | tail -1 | escape_sed)"
sed -i "s/{{{COMMIT}}}/$COMMIT_INFO/g" "frontend/index.php"

echo "Updating containers..."

sudo docker-compose build --force-rm --no-cache --pull &> /tmp/cloogle-build.log
RES=$?
cat /tmp/cloogle-build.log
if [ $RES -ne 0 ]; then
	if [[ "$HUGINN" != "" ]]; then
		curl -s -d text="Cloogle build failed." "$HUGINN"; echo
	fi
	echo "--> Cloogle build failed."
	exit $RES
fi
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

RESULTS="$(grep -A11 -F '| Table' /tmp/cloogle-build.log | sed 's/\x1b\[[0-9;]*m//g')"
if [[ "$HUGINN" != "" ]]; then
	curl -s -d text="Cloogle build succeeded." "$HUGINN"; echo
fi
if [[ "$RELEASE_REPO" != "" ]]; then
	timeout -k 10 10 sudo docker-compose exec -T backend cat types.json > /tmp/types.json
	DATE="$(date +%Y-%m-%d)"
	ID="$(curl -s -X POST \
		-H "Content-Type:application/json" \
		-H "Authorization: token $GITHUB_TOKEN" "https://api.github.com/repos/$RELEASE_REPO/releases" \
		-d "{\"tag_name\":\"$DATE\",\"name\":\"$DATE\",\"body\":\"Automatic update on $DATE.\\n\\n${RESULTS//$'\n'/\\n}\"}" | jq .id)"
	if [[ "$ID" == "null" ]]; then
		echo "Could not create release '$DATE'"
	else
		curl -X POST \
			-H "Content-Type:text/json" \
			--data-binary @/tmp/types.json \
			"https://uploads.github.com/repos/$RELEASE_REPO/releases/$ID/assets?name=types.json&access_token=$GITHUB_TOKEN"
		echo
	fi
fi
echo "--> Cloogle build succeeded."
echo "$RESULTS"
echo -en "\033[0m"
rm /tmp/cloogle-build.log
