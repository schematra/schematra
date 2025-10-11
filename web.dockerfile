FROM ghcr.io/schematra/schematra:sha-1437a12

COPY web/*.scm web/*.egg /schematra/web/
COPY web/public /schematra/web/public/
COPY web/blog /schematra/web/blog/
RUN cd /schematra/web && chicken-install

WORKDIR /schematra/web
CMD ["web"]
