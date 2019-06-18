SERVICE_DIR=	./service
TEST_TEAMID=	eeee
TEST_SERVICE_IP_ADDRESS=	[fd00:1337:${TEST_TEAMID}:ba17::1]
CHECKER_CMD=	python3 ./checker/checker.py run -a "${TEST_SERVICE_IP_ADDRESS}"

FLAG=	ENO6QMAAAeETi6mGPeJgd83rWfM2U3bcg8KZLsICovytDw=
NOISE=	=This=Is=Neither=A=Flag=Nor=A=Love=Song===31337

${SERVICE_DIR}/docker-compose.yml: ${SERVICE_DIR}/docker-compose.yml.template
	sed 's/TEAMID/${TEST_TEAMID}/g' ${SERVICE_DIR}/docker-compose.yml.template > ${SERVICE_DIR}/docker-compose.yml

.PHONY: se
se:
	docker exec -it service_queue_1 sh

.PHONY: sa
sa:
	docker attach service_queue_1

# Service up.
.PHONY: su
su: ${SERVICE_DIR}/docker-compose.yml
	${MAKE} -C ${SERVICE_DIR} up

# Service down.
.PHONY: sd
sd: ${SERVICE_DIR}/docker-compose.yml
	${MAKE} -C "${SERVICE_DIR}" down

.PHONY: cu
cu:
	cd checker && docker-compose up --build -d

.PHONY: cd
cd:
	cd checker && docker-compose down

TEST_DELAY=	2

.PHONY: test-setup-service
test-setup-service:
	sed 's/TEAMID/${TEST_TEAMID}/g' ${SERVICE_DIR}/docker-compose.yml.template > ${SERVICE_DIR}/docker-compose.yml
	${MAKE} sd
	${MAKE} su

.PHONY: test-setup-checker
test-setup-checker:
	sed 's/TEAMID/${TEST_TEAMID}/g' ${SERVICE_DIR}/docker-compose.yml.template > ${SERVICE_DIR}/docker-compose.yml
	${MAKE} cd
	${MAKE} cu

.PHONY: test-teardown-service
test-teardown-service:
	${MAKE} sd

.PHONY: test-teardown-checker
test-teardown-checker:
	${MAKE} cu

.PHONY: do-test-smoke
do-test-smoke:
	${CHECKER_CMD} havoc
	${CHECKER_CMD} putflag --flag "${FLAG}"
	${CHECKER_CMD} getflag --flag "${FLAG}"
	${CHECKER_CMD} putnoise --flag "${NOISE}"
	${CHECKER_CMD} getnoise --flag "${NOISE}"
	${CHECKER_CMD} havoc

.PHONY: test-smoke
test-smoke:
	${MAKE} test-setup-service
	sleep ${TEST_DELAY}
	${MAKE} do-test-smoke
	${MAKE} test-teardown-service

.PHONY: do-test-exploit
do-test-exploit:
	${CHECKER_CMD} putflag --flag "${FLAG}"
	${CHECKER_CMD} putnoise --flag "${NOISE}"
	${CHECKER_CMD} exploit --flag "${FLAG}"

.PHONY: test-exploit
test-exploit:
	${MAKE} test-setup-service
	sleep ${TEST_DELAY}
	${MAKE} do-test-exploit
	${MAKE} test-teardown-service

.PHONY: do-test-checker
do-test-checker:
	echo ok

.PHONY: test-checker
test-checker:
	${MAKE} test-setup-service
	${MAKE} test-setup-checker
	sleep ${TEST_DELAY}
	${MAKE} do-test-checker
	${MAKE} test-teardown-checker
	${MAKE} test-teardown-service

.PHONY: test
test:
	${MAKE} test-smoke
	${MAKE} test-exploit

CLIENT=	./scripts/client
.PHONY: debug-restart-and-watch
debug-restart-and-watch:
	${MAKE} sd
	${MAKE} su
	sleep 2

	${CLIENT} a '- priv'
	${CLIENT} a pub
	${CLIENT} p '- priv' 'private message 1'
	${CLIENT} p 'pub' 'public message 1'

	${MAKE} sa

.PHONY: clean
clean:
	find . -name '*.log' -delete
	find . -name '*.core' -delete

.PHONY: ultra-clean
ultra-clean:
	${MAKE} clean
	${MAKE} -C service distclean
	rm -rf -- .data
	rm -rf -- checker/.data
	${MAKE} -f Makefile.release release-clean
