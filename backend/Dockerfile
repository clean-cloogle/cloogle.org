# Installation:
# docker build -t cloogle . 
#
# Usage:
# docker run \
#   -d \
#   --net=host \
#   --name=cloogle \
#   -v /path/to/cloogle/cloogle.log:/usr/src/cloogle/cloogle.log \
#   cloogle

FROM camilstaps/clean:2.4-itasks

RUN apt-get update && apt-get install -y subversion ca-certificates

COPY . /usr/src/cloogle
WORKDIR /usr/src/cloogle

RUN cat env/envs.linux64 >> /opt/clean/etc/IDEEnvs

RUN svn checkout https://svn.cs.ru.nl/repos/SoccerFun/src /opt/clean/lib/SoccerFun

RUN make

EXPOSE 31215

ENTRYPOINT "./serve"
CMD []
