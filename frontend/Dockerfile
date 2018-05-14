FROM php:7.2-rc-apache

# PHP dependencies
RUN docker-php-source extract \
	&& apt-get update \
	&& apt-get install -y php-net-socket \
	&& docker-php-ext-install sockets mysqli \
	&& docker-php-source delete

COPY frontend/logo.svg /var/www/logo.svg
COPY frontend/birthday.patch /var/www/birthday.patch
COPY frontend/birthday.sh /var/www/birthday.sh
RUN cd /var/www \
	&& apt-get update -qq \
	&& apt-get install -qq imagemagick librsvg2-bin \
	&& ./birthday.sh \
	&& rsvg-convert -w 400 logo.svg > logo.png \
	&& convert -resize 200x logo.png logo.png \
	&& apt-get remove -qq imagemagick librsvg2-bin \
	&& apt-get autoremove -qq

# Clean installation with indexed libraries, for /src and /doc
COPY util /tmp/util
COPY libs.json /var/libs.json
RUN apt-get update -qq && apt-get install -qq subversion ca-certificates git jq\
	&& mkdir -p /opt/clean\
	&& curl -s http://ftp.cs.ru.nl/Clean/builds/linux-x64/clean-base-linux-x64-latest.tgz\
		| tar xz -C /opt/clean --strip-components=1\
	&& cd /tmp/util\
	&& ln -s /var/libs.json\
	&& ./fetch_libs.sh /opt/clean/lib\
	&& apt-get remove -qq subversion ca-certificates git jq\
	&& apt-get autoremove -qq

# Pygments, for highlighting
RUN apt-get update -qq \
	&& apt-get install -qq python3.5 python3-pip \
	&& pip3 install pygments pygments-lexer-clean

COPY frontend/index_common_problems.py .
RUN apt-get update -qq && apt-get install -qq ca-certificates git\
	&& ./index_common_problems.py\
	&& apt-get remove -qq ca-certificates git\
	&& apt-get autoremove -qq

COPY frontend/iconv.sh .
RUN bash iconv.sh && rm iconv.sh
