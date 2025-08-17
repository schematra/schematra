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
# install dependencies
COPY parse-deps.scm schematra.egg /schematra/
RUN chicken-install $(csi -s /schematra/parse-deps.scm)
# Now install schematra
COPY . /schematra

WORKDIR /schematra
RUN chicken-install
RUN csc -O2 -d0 schematra-web.scm

CMD ["./schematra-web"]
