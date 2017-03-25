FROM debian:jessie

RUN apt-get update && apt-get install -qq wget jq
RUN wget -q -O/tmp/t.deb https://github.com/joewalnes/websocketd/releases/download/v0.2.12/websocketd-0.2.12_amd64.deb && \
	dpkg -i /tmp/t.deb && rm /tmp/t.deb
RUN apt-get purge -qqy wget

EXPOSE 31216

COPY entrypoint.sh .
COPY stats.sh .
ENTRYPOINT ./entrypoint.sh
