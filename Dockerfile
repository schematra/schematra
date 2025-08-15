FROM alpine
RUN apk update \
    && apk add build-base bash git curl openssl-dev hiredis hiredis-dev \
    && rm -rf /var/cache/apk/*
ARG CHICKEN_VERSION
ENV CHICKEN_VERSION=${CHICKEN_VERSION:-5.4.0}
RUN curl -LO "https://code.call-cc.org/releases/${CHICKEN_VERSION}/chicken-${CHICKEN_VERSION}.tar.gz" \
    && tar xzf "chicken-${CHICKEN_VERSION}.tar.gz" \
    && cd "chicken-${CHICKEN_VERSION}" \
    && make && make install
# FIX: install sendfile-2.x
RUN cd /tmp && git clone -b sendfile-2.x https://github.com/certainty/chicken_sendfile.git \
    && cd chicken_sendfile && chicken-install
# Now install schematra
COPY . /schematra
RUN cd /schematra \
    && chicken-install
RUN cd /schematra && csc schematra-web.scm

CMD ["/schematra/schematra-web"]
