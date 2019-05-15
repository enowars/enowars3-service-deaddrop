# Build stage 0
FROM erlang:alpine

# Reset working directory
RUN mkdir -p /buildroot
WORKDIR /buildroot

# Copy our Erlang test application
COPY src src
COPY Makefile Makefile
COPY erlang.mk erlang.mk
COPY relx.config relx.config
COPY config config

RUN apk add --no-cache --update make git curl
RUN make rel

# Build stage 1
FROM alpine

RUN apk add --no-cache --update make ncurses-libs

# Install the released application
COPY --from=0 /buildroot/_rel /srv

# Expose relevant ports
EXPOSE 8080

# CMD ["sh"]
CMD ["/srv/msq_ctf_service_release/bin/msq_ctf_service_release", "console"]
