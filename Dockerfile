# syntax=docker/dockerfile:1-labs
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
# install dependencies earlier - faster builds for the common case
WORKDIR /schematra
COPY deps.txt /schematra/
RUN chicken-install $(cat deps.txt)

# Now install schematra & core eggs
COPY --parents eggs .

# Install all the eggs
RUN bash -c "pushd eggs/chiccup && chicken-install && popd \
          && pushd eggs/schematra && chicken-install && popd \
          && pushd eggs/schematra-session && chicken-install && popd \
          && pushd eggs/schematra-csrf && chicken-install && popd \
          && pushd eggs/oauthtoothy && chicken-install"

CMD ["csi"]
