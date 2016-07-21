# Installation:
# docker build -t cloogle-stats .
#
# Usage:
# docker run \
#   -d \
#   --net=host \
#   --name cloogle-stats \
#   -v /path/to/cloogle/cloogle.log:/var/log/cloogle.log \
#   cloogle-stats \
#   /var/log/cloogle.log

FROM node:argon

RUN mkdir -p /usr/src/cloogle-stats
WORKDIR /usr/src/cloogle-stats

COPY package.json /usr/src/cloogle-stats
RUN npm install

COPY server.js /usr/src/cloogle-stats

EXPOSE 31216

ENTRYPOINT ["node", "server.js"]
