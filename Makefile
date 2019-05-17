SERVICE_DIR=	./service
CHECKER_CMD=	python3 ./checker/checker.py

.PHONY: test
test:
	${MAKE} -C "${service}" down
	${MAKE} -C "${service}" up

	sleep 1

	${CHECKER_CMD} run havoc
	${CHECKER_CMD} run putflag

	cd -- "${SERVICE_DIR}" && ${MAKE} down

.PHONY: ultra-clean
ultra-clean:
	find . -name '*.log' -delete
	find . -name '*.core' -delete
	${MAKE} -C service distclean
	rm -rf -- .data
	rm -rf -- checker/.data
