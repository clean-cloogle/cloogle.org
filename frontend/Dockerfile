FROM php:apache

# PHP dependencies
RUN docker-php-source extract \
	&& apt-get update \
	&& apt-get install -y php-net-socket\
	&& docker-php-ext-install sockets mysqli\
	&& docker-php-source delete

# Clean libraries, for /srv/view.php
RUN apt-get update && apt-get install -y subversion ca-certificates

RUN mkdir -p /opt/clean && cd /opt/clean &&\
	curl -sSL http://ftp.cs.ru.nl/Clean/nightly/clean-itasks-linux64-20160531.tar.gz\
		| tar --strip-components=1 -xz &&\
	make -j &&\
	mv StdEnv/*.icl lib/StdEnv

RUN svn checkout https://svn.cs.ru.nl/repos/SoccerFun/src /opt/clean/lib/SoccerFun

# Pygments, for highlighting
RUN apt-get update && apt-get install -y python3.4 python3-pip mercurial

RUN hg clone https://camilstaps@bitbucket.org/camilstaps/pygments-main-3 /opt/pygments &&\
	pip3 install -e /opt/pygments
