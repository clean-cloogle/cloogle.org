#!/bin/bash
if [[ -f "/srv/ssl/cert.pem" ]] && [[ -f "/srv/ssl/key.pem" ]]; then
	node server.js /var/log/cloogle.log /srv/ssl/cert.pem /srv/ssl/key.pem
else
	node server.js /var/log/cloogle.log
fi
