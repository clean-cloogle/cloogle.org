# Installation:
# docker build -t cloogle-stats .
#
# Usage:
# docker run \
#   -d \
#   --net=host \
#   --name cloogle-stats \
#   -v /path/to/cloogle/cloogle.log:/var/log/cloogle.log \
#   -v /path/to/cert.pem:/srv/ssl/cert.pem \
#   -v /path/to/key.pem:/srv/ssl/key.pem \
#   cloogle-stats

FROM node:argon

RUN mkdir -p /usr/src/cloogle-stats
WORKDIR /usr/src/cloogle-stats

COPY package.json /usr/src/cloogle-stats
RUN npm install

COPY server.js /usr/src/cloogle-stats
COPY entrypoint.sh /usr/src/cloogle-stats

EXPOSE 31216

ENTRYPOINT ["./entrypoint.sh"]
