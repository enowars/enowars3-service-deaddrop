# Build stage 0
FROM erlang:alpine

RUN mkdir -p /buildroot
WORKDIR /buildroot

COPY Makefile Makefile
COPY erlang.mk erlang.mk
COPY relx.config relx.config
COPY config config

RUN apk add --no-cache --update make git
RUN make deps

# Build stage 0
FROM erlang:alpine

RUN mkdir -p /buildroot
WORKDIR /buildroot

COPY --from=0 /buildroot /buildroot
COPY src src

RUN apk add --no-cache --update curl make

RUN make rel

# Build stage 2
FROM alpine

RUN apk add --no-cache --update make ncurses-libs

# Install the released application
COPY --from=1 /buildroot/_rel /srv

# Expose relevant ports
EXPOSE 8080

CMD ["/srv/msq_ctf_service_release/bin/msq_ctf_service_release", "console"]
