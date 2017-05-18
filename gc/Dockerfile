FROM bash

RUN apk add --update coreutils findutils && rm -rf /var/cache/apk/*

COPY gc.sh /

CMD ["bash", "/gc.sh"]
