SERVICE_DIR=	./service
CHECKER_CMD=	python3 ./checker/checker.py

EXAMPLE_FLAG=	ENO6QMAAAeETi6mGPeJgd83rWfM2U3bcg8KZLsICovytDw=

# Service up.
.PHONY: su
su:
	${MAKE} -C ${SERVICE_DIR} up

# Service down.
.PHONY: sd
sd:
	${MAKE} -C "${SERVICE_DIR}" down

.PHONY: test
test:
	${MAKE} sd
	${MAKE} su

	sleep 1

	${CHECKER_CMD} run havoc
	${CHECKER_CMD} run putflag --flag "${EXAMPLE_FLAG}"
	${CHECKER_CMD} run getflag --flag "${EXAMPLE_FLAG}"

	${MAKE} sd

.PHONY: ultra-clean
ultra-clean:
	find . -name '*.log' -delete
	find . -name '*.core' -delete
	${MAKE} -C service distclean
	rm -rf -- .data
	rm -rf -- checker/.data
