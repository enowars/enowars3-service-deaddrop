SERVICE_DIR=	./service
CHECKER_CMD=	python3 ./checker/checker.py

.PHONY: test
test:
	${MAKE} -C "${SERVICE_DIR}" down
	${MAKE} -C "${SERVICE_DIR}" up

	sleep 1

	${CHECKER_CMD} run havoc
	${CHECKER_CMD} run putflag

	${MAKE} -C "${SERVICE_DIR}" down

.PHONY: ultra-clean
ultra-clean:
	find . -name '*.log' -delete
	find . -name '*.core' -delete
	${MAKE} -C service distclean
	rm -rf -- .data
	rm -rf -- checker/.data
