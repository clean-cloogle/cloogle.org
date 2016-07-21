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

COPY . /usr/src/cloogle
WORKDIR /usr/src/cloogle

RUN make

EXPOSE 31215

ENTRYPOINT "./serve"
CMD []
