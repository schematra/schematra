FROM ghcr.io/schematra/schematra:latest

COPY web/*.scm web/*.egg /schematra/web/
COPY web/public /schematra/web/public/
COPY web/blog /schematra/web/blog/
# install custom setup.defaults to add eggs.rolando.cl
RUN cat <<EOS > /usr/local/share/chicken/setup.defaults
(version 2)
(map (srfi-4 ->))
(server "http://eggs.rolando.cl/henrietta.cgi")
(server "http://code.call-cc.org/cgi-bin/henrietta.cgi")
EOS
RUN cd /schematra/web && chicken-install

WORKDIR /schematra/web
CMD ["web"]
