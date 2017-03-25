#!/bin/sh
[ -f "/srv/ssl/cert.pem" ] && [ -f "/srv/ssl/key.pem" ] && SSL="--ssl --sslcert=/srv/ssl/cert.pem --sslkey=/srv/ssl/key.pem"
/usr/bin/websocketd --port=31216 $SSL ./stats.sh
