FROM camilstaps/clean:2.4-itasks

# Move StdEnv implementation modules to builddb root
RUN mv /opt/clean/StdEnv/*.icl /opt/clean/lib/StdEnv

RUN PACKAGES="subversion ca-certificates" \
	&& apt-get update \
	&& apt-get install -qq subversion ca-certificates \
# Cleanup
	&& ADDED_PACKAGES=`apt-mark showauto` \
	&& apt-get remove --purge -qq $PACKAGES $ADDED_PACKAGES\
	&& rm -rf /var/lib/apt/lists/*

COPY . /usr/src/cloogle
WORKDIR /usr/src/cloogle

RUN PACKAGES="make subversion ca-certificates gcc" \
	&& apt-get update \
	&& apt-get install -qq $PACKAGES \
	&& svn checkout https://svn.cs.ru.nl/repos/SoccerFun/src /opt/clean/lib/SoccerFun \
	&& svn checkout https://svn.cs.ru.nl/repos/clean-libraries/trunk/Libraries/ObjectIO/ObjectIO /opt/clean/lib/ObjectIO \
	&& make distclean \
	&& make \
# Cleanup
	&& rm -rf \
		CleanPrettyPrint \
		CleanTypeUnifier \
		Clean\ System\ Files \
		clean-compiler \
		clean-platform \
		*.dcl *.icl \
		Dockerfile \
		Makefile \
	&& rm -rf /opt/clean \
	&& ADDED_PACKAGES=`apt-mark showauto` \
	&& apt-get remove --purge -qq $PACKAGES $ADDED_PACKAGES \
	&& rm -rf /var/lib/apt/lists/*

EXPOSE 31215

ENTRYPOINT "./serve"
CMD []
