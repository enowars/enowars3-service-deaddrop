SERVICE_DIR=	./service
CHECKER_CMD=	python3 ./checker/checker.py

FLAG=	ENO6QMAAAeETi6mGPeJgd83rWfM2U3bcg8KZLsICovytDw=
NOISE=	=This=Is=Neither=A=Flag=Nor=A=Love=Song===31337

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
	${CHECKER_CMD} run putflag --flag "${FLAG}"
	${CHECKER_CMD} run getflag --flag "${FLAG}"
	${CHECKER_CMD} run putnoise --flag "${NOISE}"
	${CHECKER_CMD} run getnoise --flag "${NOISE}"

	${MAKE} sd

.PHONY: ultra-clean
ultra-clean:
	find . -name '*.log' -delete
	find . -name '*.core' -delete
	${MAKE} -C service distclean
	rm -rf -- .data
	rm -rf -- checker/.data
