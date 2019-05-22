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

RELEASE_BRANCH=	release
REPOSITORY_URL=	ssh://git@git.eno.host:7999/blue/service-messagequeue.git
RELEASE_DIR=	./new-release

.PHONY: release-clone
release-clone:
	git clone ${REPOSITORY_URL} ${RELEASE_DIR}

.PHONY: release-update
release-update:
	${MAKE} -C ${RELEASE_DIR} -f ${PWD}/Makefile do-release-update

.PHONY: do-release-update
do-release-update:
	git checkout ${RELEASE_BRANCH}
	git rm -r .

	# Docker files
	git checkout --force master service/Dockerfile
	git checkout --force master service/docker-compose.yml.template
	# Build infrastructure
	git checkout --force master ${SERVICE_DIR}/Makefile
	git checkout --force master ${SERVICE_DIR}/config
	git checkout --force master ${SERVICE_DIR}/erlang.mk
	git checkout --force master ${SERVICE_DIR}/relx.config
	# Source of the service
	git checkout --force master ${SERVICE_DIR}/src

	git status

.PHONY: release-qa
release-qa:
	sed 's/TEAMID/eeee/g' ${SERVICE_DIR}/docker-compose.yml.template > ${SERVICE_DIR}/docker-compose.yml
	${MAKE} -C ${SERVICE_DIR} up
	${MAKE} -C ${SERVICE_DIR} down
	rm -f -- ${SERVICE_DIR}/docker-compose.yml

.PHONY: release-push
release-push:
	${MAKE} -C ${RELEASE_DIR} -f ${PWD}/Makefile do-release-push

.PHONY: do-release-push
do-release-push:
	git commit --message="Release the service (based on $$(git log -n 1 --pretty=format:"%H"))"
	git push --set-upstream origin ${RELEASE_BRANCH}

.PHONY: release-clean
release-clean:
	rm -rf -- ${RELEASE_DIR}

.PHONY: ultra-clean
ultra-clean:
	find . -name '*.log' -delete
	find . -name '*.core' -delete
	${MAKE} -C service distclean
	rm -rf -- .data
	rm -rf -- checker/.data
	rm -rf -- ${RELEASE_DIR}
