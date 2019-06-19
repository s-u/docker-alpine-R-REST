FROM docker.io/alpine:3.9

MAINTAINER Simon Urbanek <simon.urbanek@R-project.org>

RUN apk add --no-cache R curl
RUN curl -sL http://dune.urbanek.info/docker/Rserve_1.8-6-alpine.6.9.tar.gz | tar fxz - -C /usr/lib/R/library

COPY rs.conf scripts.R run.sh /root/

EXPOSE 8080

CMD /root/run.sh
