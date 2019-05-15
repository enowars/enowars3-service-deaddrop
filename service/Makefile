PROJECT = msq_ctf_service
PROJECT_DESCRIPTION = A simple, buggy message queue.
PROJECT_VERSION = 0.1.0

DEPS = cowboy
dep_cowboy_commit = 2.6.3

BUILD_DEPS = reload_mk
DEP_PLUGINS = cowboy, reload_mk

.PHONY: up
up:
	docker-compose up --build -d

.PHONY: down
down:
	docker-compose down

include erlang.mk
