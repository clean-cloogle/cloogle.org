FROM camilstaps/clean:nightly

COPY backend /usr/src/cloogle
COPY libs.json /usr/src/cloogle/libs.json
COPY util/fetch_libs.sh /usr/src/cloogle/fetch_libs.sh
WORKDIR /usr/src/cloogle

RUN install_clean.sh bundle-complete 2018-09-08 \
	&& PACKAGES="patch jq" \
	&& apt-get update -qq\
	&& apt-get install -qq $PACKAGES --no-install-recommends\
	&& make distclean CloogleServer builddb\
	&& ./fetch_libs.sh /opt/clean/lib\
	&& make types.json\
	&& rm -rf \
		Cloogle \
		Clean\ System\ Files \
		clean-compiler \
		*.dcl *.icl \
		Dockerfile \
		Makefile \
	&& rm -rf /opt/clean \
	&& apt-get remove --purge -qq $PACKAGES \
	&& apt-get autoremove -qq \
	&& rm -rf /var/lib/apt/lists \
	&& uninstall_clean.sh

EXPOSE 31215

ENTRYPOINT "./serve"
CMD []
