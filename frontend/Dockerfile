FROM php:7.2-rc-apache

# PHP dependencies
RUN docker-php-source extract \
	&& apt-get update \
	&& apt-get install -y php-net-socket \
	&& docker-php-ext-install sockets mysqli \
	&& docker-php-source delete

# Clean libraries, for /src
RUN apt-get update && apt-get install -y subversion ca-certificates git

RUN mkdir -p /opt/clean && cd /opt/clean && \
	curl -sSL ftp://ftp.cs.ru.nl/pub/Clean/builds/linux-x64/clean-bundle-complete-linux-x64-latest.tgz \
		| tar -xz --strip-components=1

RUN svn checkout https://svn.cs.ru.nl/repos/SoccerFun/src /opt/clean/lib/SoccerFun \
	&& mv /opt/clean/lib/SoccerFun/*/*.[id]cl /opt/clean/lib/SoccerFun \
	&& bash -c 'rm -r /opt/clean/lib/SoccerFun/{Game,Gui,StdLibExt,StdReferee,StdTeam,afbeeldingen,sound}' \
	&& bash -c 'rm /opt/clean/lib/SoccerFun/{Team_,RefereeCoach_,Buffer,KeeperChallenger}*' \
	&& svn checkout https://svn.cs.ru.nl/repos/clean-libraries/trunk/Libraries/MersenneTwister /opt/clean/lib/MersenneTwister \
	&& svn checkout https://svn.cs.ru.nl/repos/clean-libraries/trunk/Libraries/ObjectIO/ObjectIO /opt/clean/lib/ObjectIO \
	&& git clone https://gitlab.science.ru.nl/mlubbers/CleanSerial /opt/clean/lib/CleanSerial \
	&& git clone https://github.com/clean-cloogle/Cloogle /opt/clean/lib/Cloogle \
	&& git clone https://github.com/clean-cloogle/libcloogle /opt/clean/lib/libcloogle \
	&& git clone https://github.com/clean-cloogle/CleanTypeUnifier /opt/clean/lib/CleanTypeUnifier \
	&& git clone https://github.com/clean-cloogle/CleanPrettyPrint /opt/clean/lib/CleanPrettyPrint \
	&& git clone https://github.com/camilstaps/CleanInotify /opt/clean/lib/CleanInotify \
	&& git clone https://github.com/camilstaps/CleanSnappy /opt/clean/lib/CleanSnappy

# Pygments, for highlighting
RUN apt-get update && apt-get install -y python3.4 python3-pip mercurial

RUN hg clone https://bitbucket.org/camilstaps/pygments-main-3 /opt/pygments \
	&& pip3 install -e /opt/pygments

COPY iconv.sh .
RUN bash iconv.sh && rm iconv.sh

COPY logo.svg /var/www/logo.svg
RUN cd /var/www \
	&& apt-get install -qq imagemagick librsvg2-bin \
	&& rsvg-convert -w 400 logo.svg > logo.png \
	&& convert -resize 200x logo.png logo.png \
	&& apt-get remove -qq imagemagick librsvg2-bin \
	&& apt-get autoremove -qq
