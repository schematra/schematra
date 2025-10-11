FROM ghcr.io/schematra/schematra:sha-1437a12

COPY web/*.scm web/*.egg ./web/
COPY web/public ./web/public/
COPY web/blog ./web/blog/
RUN cd web && chicken-install

WORKDIR /schematra/web
CMD ["web"]
