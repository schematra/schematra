FROM ghcr.io/schematra/schematra:latest

COPY web/*.scm web/*.egg ./web/
COPY web/public ./web/public/
COPY web/blog ./web/blog/
RUN cd web && chicken-install

WORKDIR /schematra/web
CMD ["web"]
