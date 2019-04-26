PROJECT = msq_ctf_service
PROJECT_DESCRIPTION = A simple, buggy message queue.
PROJECT_VERSION = 0.1.0

DEPS = cowboy
dep_cowboy_commit = 2.6.3

DEP_PLUGINS = cowboy

include erlang.mk
