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

# Build stage 1
FROM erlang:alpine

RUN mkdir -p /buildroot
WORKDIR /buildroot

COPY --from=0 /buildroot /buildroot
COPY src/add_topic_handler.erl src/
COPY src/file_handler.erl src/
COPY src/msq_ctf_service_app.erl src/
COPY src/msq_ctf_service_sup.erl src/
COPY src/publish_handler.erl src/
COPY src/subscribe_handler.erl src/
COPY src/subscriber_pool.erl src/
COPY src/topics_handler.erl src/

RUN apk add --no-cache --update curl make

RUN make rel

# Build stage 2
FROM alpine

RUN apk add --no-cache --update ncurses-libs

# Install the released application
COPY --from=1 /buildroot/_rel /srv

# Expose relevant ports
EXPOSE 8080

CMD ["/srv/msq_ctf_service_release/bin/msq_ctf_service_release", "console"]
