FROM php:7.2-rc-apache

# PHP dependencies
RUN docker-php-source extract \
	&& apt-get update \
	&& apt-get install -y php-net-socket \
	&& docker-php-ext-install sockets mysqli \
	&& docker-php-source delete

COPY logo.svg /var/www/logo.svg
COPY birthday.patch /var/www/birthday.patch
COPY birthday.sh /var/www/birthday.sh
RUN cd /var/www \
	&& apt-get update -qq \
	&& apt-get install -qq imagemagick librsvg2-bin \
	&& ./birthday.sh \
	&& rsvg-convert -w 400 logo.svg > logo.png \
	&& convert -resize 200x logo.png logo.png \
	&& apt-get remove -qq imagemagick librsvg2-bin \
	&& apt-get autoremove -qq

# Clean libraries, for /src
RUN apt-get update -qq && apt-get install -qq subversion ca-certificates git

RUN mkdir -p /opt/clean && cd /opt/clean && \
	curl -sSL ftp://ftp.cs.ru.nl/pub/Clean/builds/linux-x64/clean-bundle-complete-linux-x64-latest.tgz \
		| tar -xz --strip-components=1

RUN svn checkout https://svn.cs.ru.nl/repos/SoccerFun/src /opt/clean/lib/SoccerFun \
	&& svn checkout https://svn.cs.ru.nl/repos/clean-libraries/trunk/Libraries/MersenneTwister /opt/clean/lib/MersenneTwister \
	&& svn checkout https://svn.cs.ru.nl/repos/clean-libraries/trunk/Libraries/ObjectIO/ObjectIO /opt/clean/lib/ObjectIO \
	&& svn checkout https://svn.cs.ru.nl/repos/clean-ide/trunk /opt/clean/lib/clean-ide \
	&& svn checkout https://svn.cs.ru.nl/repos/clean-compiler/trunk /opt/clean/lib/clean-compiler \
	&& git clone https://gitlab.science.ru.nl/mlubbers/CleanSerial /opt/clean/lib/CleanSerial \
	&& git clone https://github.com/clean-cloogle/Cloogle /opt/clean/lib/Cloogle \
	&& git clone https://github.com/clean-cloogle/libcloogle /opt/clean/lib/libcloogle \
	&& git clone https://github.com/clean-cloogle/CleanTypeUnifier /opt/clean/lib/CleanTypeUnifier \
	&& git clone https://github.com/clean-cloogle/CleanPrettyPrint /opt/clean/lib/CleanPrettyPrint \
	&& git clone https://github.com/camilstaps/CleanInotify /opt/clean/lib/CleanInotify \
	&& git clone https://github.com/camilstaps/CleanSnappy /opt/clean/lib/CleanSnappy \
	&& git clone https://github.com/clean-cloogle/clean-irc /opt/clean/lib/clean-irc \
	&& git clone https://github.com/dopefishh/clean-selectloop /tmp/clean-selectloop \
		&& mv /tmp/clean-selectloop/libraries /opt/clean/lib/clean-selectloop \
	&& rm -r /opt/clean/lib/StdEnv \
	&& git clone https://github.com/clean-cloogle/StdEnv-doc /opt/clean/lib/StdEnv

# Pygments, for highlighting
RUN apt-get update -qq \
	&& apt-get install -qq python3.5 python3-pip \
	&& pip3 install pygments pygments-lexer-clean

COPY index_common_problems.py .
RUN ./index_common_problems.py && rm index_common_problems.py

COPY iconv.sh .
RUN bash iconv.sh && rm iconv.sh
