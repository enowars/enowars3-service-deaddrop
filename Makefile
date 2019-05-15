PROJECT = msq_ctf_service
PROJECT_DESCRIPTION = A simple, buggy message queue.
PROJECT_VERSION = 0.1.0

DEPS = cowboy
dep_cowboy_commit = 2.6.3

BUILD_DEPS = reload_mk
DEP_PLUGINS = cowboy, reload_mk

docker-build:
	docker build -t msq .

docker-run:
	docker run -dit --name msq -p 8080:8080 msq

docker-clean:
	docker stop $$(docker ps -a -q)
	docker rm $$(docker ps -a -q)

.PHONY: test
test:
	sh test/smoke-test

.PHONY: do-clean
ultra-clean:
	find . -name '*.log' -delete
	find . -name '*.core' -delete
	${MAKE} clean
	${MAKE} distclean
	rm -rf -- .data
	rm -rf -- checker/.data

include erlang.mk
