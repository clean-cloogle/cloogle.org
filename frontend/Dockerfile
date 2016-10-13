FROM php:5.6-apache

RUN docker-php-source extract \
	&& apt-get update \
	&& apt-get install -y php-net-socket\
	&& docker-php-ext-install sockets mysqli\
	&& docker-php-source delete
