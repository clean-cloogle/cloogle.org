FROM camilstaps/clean:nightly

COPY . /usr/src/cloogle
WORKDIR /usr/src/cloogle

RUN install_clean.sh bundle-complete 2017-10-31 \
	&& PACKAGES="patch" \
	&& apt-get update -qq\
	&& apt-get install -qq $PACKAGES --no-install-recommends \
	&& make distclean CloogleServer builddb\
	&& rm -r /opt/clean/lib/*\
	&& svn checkout https://svn.cs.ru.nl/repos/SoccerFun/src /opt/clean/lib/SoccerFun \
	&& mv /opt/clean/lib/SoccerFun/*/*.[id]cl /opt/clean/lib/SoccerFun \
	&& bash -c 'rm -r /opt/clean/lib/SoccerFun/{Game,Gui,StdLibExt,StdReferee,StdTeam,afbeeldingen,sound}' \
	&& svn checkout https://svn.cs.ru.nl/repos/clean-libraries/trunk/Libraries/ObjectIO/ObjectIO /opt/clean/lib/ObjectIO \
	&& svn checkout https://svn.cs.ru.nl/repos/clean-libraries/trunk/Libraries/MersenneTwister /opt/clean/lib/MersenneTwister \
	&& svn checkout https://svn.cs.ru.nl/repos/clean-compiler/trunk /opt/clean/lib/clean-compiler \
	&& svn checkout https://svn.cs.ru.nl/repos/clean-ide/trunk /opt/clean/lib/clean-ide \
	&& git clone https://gitlab.science.ru.nl/mlubbers/CleanSerial /opt/clean/lib/CleanSerial \
	&& git clone https://github.com/clean-cloogle/Cloogle /opt/clean/lib/Cloogle \
	&& git clone https://github.com/clean-cloogle/libcloogle /opt/clean/lib/libcloogle \
	&& git clone https://github.com/clean-cloogle/CleanTypeUnifier /opt/clean/lib/CleanTypeUnifier \
	&& git clone https://github.com/clean-cloogle/CleanPrettyPrint /opt/clean/lib/CleanPrettyPrint \
	&& git clone https://github.com/camilstaps/CleanInotify /opt/clean/lib/CleanInotify \
	&& git clone https://github.com/camilstaps/CleanSnappy /opt/clean/lib/CleanSnappy \
	&& curl -sSL ftp://ftp.cs.ru.nl/pub/Clean/builds/linux-x64/clean-bundle-complete-linux-x64-latest.tgz |\
	 	tar -xz -C /opt/clean --strip-components=1\
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
